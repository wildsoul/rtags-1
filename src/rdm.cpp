#include <rct/EventLoop.h>
#include <rct/Log.h>
#include "RTags.h"
#include "Server.h"
#include <rct/Rct.h>
#include <rct/Thread.h>
#include <rct/ThreadPool.h>
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>
#endif

void sigSegvHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
    String trace = Rct::backtrace();
    if (!trace.isEmpty()) {
        fprintf(stderr, "%s", trace.constData());
    }
    fflush(stderr);
    _exit(1);
}

static Path socketFile;

void sigIntHandler(int)
{
    unlink(socketFile.constData());
    _exit(1);
}

#define EXCLUDEFILTER_DEFAULT "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*"
void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                         Display this page.\n"
            "  --include-path|-I [arg]           Add additional include path to clang.\n"
            "  --include|-i [arg]                Add additional include directive to clang.\n"
            "  --define|-D [arg]                 Add additional define directive to clang.\n"
            "  --log-file|-L [arg]               Log to this file.\n"
            "  --append|-A                       Append to log file.\n"
            "  --verbose|-v                      Change verbosity, multiple -v's are allowed.\n"
            "  --clear-projects|-C               Clear all projects.\n"
            "  --disable-sighandler|-s           Disable signal handler to dump stack for crashes..\n"
            "                                    Note that this might not play well with clang's signal handler.\n"
            "  --clang-includepath|-P            Use clang include paths by default.\n"
            "  --no-Wall|-W                      Don't use -Wall.\n"
            "  --Wlarge-by-value-copy|-r [arg]   Use -Wlarge-by-value-copy=[arg] when invoking clang.\n"
            "  --silent|-S                       No logging to stdout.\n"
            "  --exclude-filter|-x [arg]         Files to exclude from rdm, default \"" EXCLUDEFILTER_DEFAULT "\".\n"
            "  --no-rc|-N                        Don't load any rc files.\n"
            "  --ignore-printf-fixits|-F         Disregard any clang fixit that looks like it's trying to fix format for printf and friends.\n"
            "  --rc-file|-c [arg]                Use this file instead of ~/.rdmrc.\n"
            "  --data-dir|-d [arg]               Use this directory to store persistent data (default ~/.rtags).\n"
            "  --socket-file|-n [arg]            Use this file for the server socket (default ~/.rdm).\n"
            "  --no-current-project|-o           Don't restore the last current project on startup.\n"
            "  --allow-multiple-builds|-m        Without this setting different flags for the same compiler will be merged for each source file.\n"
            "  --unload-timer|-u [arg]           Number of minutes to wait before unloading non-current projects (disabled by default).\n"
            "  --no-clang-thread|-O              Don't run clang thread (for fixits)\n"
            "  --disable-plugin|-p [arg]         Don't load this plugin\n");
}

int main(int argc, char** argv)
{
    Rct::findExecutablePath(*argv);

    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "include-path", required_argument, 0, 'I' },
        { "include", required_argument, 0, 'i' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "no-builtin-includes", no_argument, 0, 'U' },
        { "no-Wall", no_argument, 0, 'W' },
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "clear-projects", no_argument, 0, 'C' },
        { "enable-sighandler", no_argument, 0, 's' },
        { "silent", no_argument, 0, 'S' },
        { "exclude-filter", required_argument, 0, 'x' },
        { "socket-file", required_argument, 0, 'n' },
        { "rc-file", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { "data-dir", required_argument, 0, 'd' },
        { "ignore-printf-fixits", no_argument, 0, 'F' },
        { "Wlarge-by-value-copy", required_argument, 0, 'r' },
        { "allow-multiple-builds", no_argument, 0, 'm' },
        { "no-current-project", no_argument, 0, 'o' },
        { "no-clang-thread", no_argument, 0, 'O' },
        { "disable-plugin", required_argument, 0, 'p' },
        { 0, 0, 0, 0 }
    };
    const String shortOptions = Rct::shortOptions(opts);
    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOptions.contains('a' + i))
                unused.append('a' + i);
            if (!shortOptions.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (int i=0; opts[i].name; ++i) {
            if (opts[i].name) {
                if (!opts[i].val) {
                    printf("No shortoption for %s\n", opts[i].name);
                } else if (opts[i].name[0] != opts[i].val) {
                    printf("Not ideal option for %s|%c\n", opts[i].name, opts[i].val);
                }
            }
        }
        return 0;
    }


    List<String> argCopy;
    List<char*> argList;
    {
        bool norc = false;
        Path rcfile = Path::home() + ".rdmrc";
        opterr = 0;
        while (true) {
            const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
            if (c == -1)
                break;
            switch (c) {
            case 'N':
                norc = true;
                break;
            case 'c':
                rcfile = optarg;
                break;
            default:
                break;
            }
        }
        opterr = 1;
        argList.append(argv[0]);
        if (!norc) {
            char *rc;
            int size = Path("/etc/rdmrc").readAll(rc);
            if (rc) {
                argCopy = String(rc, size).split('\n');
                delete[] rc;
            }
            if (!rcfile.isEmpty()) {
                size = rcfile.readAll(rc);
                if (rc) {
                    List<String> split = String(rc, size).split('\n');
                    argCopy.append(split);
                    delete[] rc;
                }
            }
            const int s = argCopy.size();
            for (int i=0; i<s; ++i) {
                String &arg = argCopy.at(i);
                if (!arg.isEmpty() && !arg.startsWith('#') && !arg.startsWith(' '))
                    argList.append(arg.data());
            }
        }
        for (int i=1; i<argc; ++i) {
            argList.append(argv[i]);
        }

        optind = 1;
    }

    Server::Options serverOpts;
    serverOpts.socketFile = String::format<128>("%s.rdm", Path::home().constData());
    serverOpts.options = Server::Wall;
    serverOpts.excludeFilters = String(EXCLUDEFILTER_DEFAULT).split(';');
    serverOpts.dataDir = String::format<128>("%s.rtags", Path::home().constData());

    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    assert(Path::home().endsWith('/'));
    int argCount = argList.size();
    char **args = argList.data();
    bool noSigHandler = false;
    while (true) {
        const int c = getopt_long(argCount, args, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'N':
        case 'c':
            // ignored
            break;
        case 'S':
            logLevel = -1;
            break;
        case 'x':
            serverOpts.excludeFilters += String(optarg).split(';');
            break;
        case 'n':
            serverOpts.socketFile = optarg;
            break;
        case 'd':
            serverOpts.dataDir = String::format<128>("%s", Path::resolved(optarg).constData());
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'm':
            serverOpts.options |= Server::AllowMultipleBuildsForSameCompiler;
            break;
        case 'o':
            serverOpts.options |= Server::NoStartupCurrentProject;
            break;
        case 'F':
            serverOpts.options |= Server::IgnorePrintfFixits;
            break;
        case 'U':
            serverOpts.options |= Server::NoBuiltinIncludes;
            break;
        case 'W':
            serverOpts.options &= ~Server::Wall;
            break;
        case 'C':
            serverOpts.options |= Server::ClearProjects;
            break;
        case 's':
            noSigHandler = true;
            break;
        case 'r': {
            int large = atoi(optarg);
            if (large <= 0) {
                fprintf(stderr, "Can't parse argument to -r %s\n", optarg);
                return 1;
            }
            serverOpts.defaultArguments.append("-Wlarge-by-value-copy=" + String(optarg)); // ### not quite working
            break; }
        case 'D':
            serverOpts.defaultArguments.append("-D" + String(optarg));
            break;
        case 'I':
            serverOpts.defaultArguments.append("-I" + String(optarg));
            break;
        case 'i':
            serverOpts.defaultArguments.append("-include");
            serverOpts.defaultArguments.append(optarg);
            break;
        case 'A':
            logFlags |= Log::Append;
            break;
        case 'L':
            logFile = optarg;
            break;
        case 'v':
            if (logLevel >= 0)
                ++logLevel;
            break;
        case '?':
            usage(stderr);
            return 1;
        }
    }
    if (!noSigHandler)
        signal(SIGSEGV, sigSegvHandler);

    if (optind < argCount) {
        fprintf(stderr, "rdm: unexpected option -- '%s'\n", args[optind]);
        return 1;
    }

    signal(SIGINT, sigIntHandler);

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return 1;
    }
    EventLoop loop;

    shared_ptr<Server> server(new Server);
    ::socketFile = serverOpts.socketFile;
    if (!serverOpts.dataDir.endsWith('/'))
        serverOpts.dataDir.append('/');
    if (!server->init(serverOpts)) {
        cleanupLogging();
        return 1;
    }

    loop.run();
    cleanupLogging();
    return 0;
}
