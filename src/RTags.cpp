#include "RTags.h"
#include "CompileMessage.h"
#include "CreateOutputMessage.h"
#include "QueryMessage.h"
#include <dirent.h>
#include <fnmatch.h>
#include <rct/Messages.h>

/* Same behavior as rtags-default-current-project() */

namespace RTags {
enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2
};
static inline Path findAncestor(Path path, const char *fn, unsigned flags)
{
    Path ret;
    int slash = path.size();
    const int len = strlen(fn) + 1;
    struct stat st;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    dirent *direntBuf = 0, *entry = 0;
    if (flags & Wildcard)
        direntBuf = reinterpret_cast<struct dirent *>(malloc(sizeof(buf)));

    memcpy(buf, path.constData(), path.size() + 1);
    while ((slash = path.lastIndexOf('/', slash - 1)) > 0) { // We don't want to search in /
        if (!(flags & Wildcard)) {
            memcpy(buf + slash + 1, fn, len);
            if (!stat(buf, &st)) {
                buf[slash + 1] = '\0';
                ret = buf;
                if (flags & Shallow) {
                    break;
                }
            }
        } else {
            buf[slash + 1] = '\0';
            DIR *dir = opendir(buf);
            bool found = false;
            if (dir) {
                while (!readdir_r(dir, direntBuf, &entry) && entry) {
                    const int l = strlen(entry->d_name) + 1;
                    switch (l - 1) {
                    case 1:
                        if (entry->d_name[0] == '.')
                            continue;
                        break;
                    case 2:
                        if (entry->d_name[0] == '.' && entry->d_name[1] == '.')
                            continue;
                        break;
                    }
                    assert(buf[slash] == '/');
                    assert(l + slash + 1 < static_cast<int>(sizeof(buf)));
                    memcpy(buf + slash + 1, entry->d_name, l);
                    if (!fnmatch(fn, buf, 0)) {
                        ret = buf;
                        ret.truncate(slash + 1);
                        found = true;
                        break;
                    }
                }
            }
            closedir(dir);
            if (found && flags & Shallow)
                break;
        }
    }
    if (flags & Wildcard)
        free(direntBuf);

    if (!ret.isEmpty() && !ret.endsWith('/'))
        ret.append('/');
    return ret;
}

struct Entry {
    const char *name;
    const unsigned flags;
};

static inline Path checkEntry(const Entry *entries, const Path &path, const Path &home)
{
    for (int i=0; entries[i].name; ++i) {
        Path p = findAncestor(path, entries[i].name, entries[i].flags);
        if ((p.isEmpty() || p == home) && (entries[i].flags & Wildcard)) {
            const int len = strlen(entries[i].name);
            if (entries[i].name[len - 1] == '*') {
                const String name(entries[i].name, len - 1);
                p = findAncestor(path, name.constData(), entries[i].flags & ~Wildcard);
            }
        }
        if (!p.isEmpty() && p != home) {
            if (!p.compare("./") || !p.compare("."))
                error() << "1" << path << "=>" << p << entries[i].name;
            return p;
        }
    }
    return Path();
}

Path findProjectRoot(const Path &path)
{
    assert(path.isAbsolute());
    static const Path home = Path::home();
    const Entry before[] = {
        { "GTAGS", 0 },
        { "CMakeLists.txt", 0 },
        { "configure", 0 },
        { ".git", 0 },
        { ".svn", 0 },
        { "*.pro", Wildcard },
        { "scons.1", 0 },
        { "*.scons", Wildcard },
        { "SConstruct", 0 },
        { "autogen.*", Wildcard },
        { "GNUMakefile*", Wildcard },
        { "INSTALL*", Wildcard },
        { "README*", Wildcard },
        { 0, 0 }
    };
    {
        const Path ret = checkEntry(before, path, home);
        if (!ret.isEmpty())
            return ret;
    }
    {
        const Path configStatus = findAncestor(path, "config.status", 0);
        if (!configStatus.isEmpty()) {
            FILE *f = fopen((configStatus + "config.status").constData(), "r");
            Path ret;
            if (f) {
                char line[1024];
                enum { MaxLines = 10 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1)
                        break;
                    char *configure = strstr(line, "/configure");
                    if (configure) {
                        char *end = configure + 10;
                        while (--configure >= line) {
                            Path conf(configure, end - configure);
                            if (!conf.isAbsolute())
                                conf.resolve();
                            if (conf.isFile()) {
                                ret = conf.parentDir();
                                if (ret == home)
                                    ret.clear();
                                break;
                            }
                        }
                    }
                    if (!ret.isEmpty())
                        break;
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    {
        const Path cmakeCache = findAncestor(path, "CMakeCache.txt", 0);
        if (!cmakeCache.isEmpty()) {
            FILE *f = fopen((cmakeCache + "Makefile").constData(), "r");
            if (f) {
                Path ret;
                char line[1024];
                enum { MaxLines = 256 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1) {
                        break;
                    }
                    if (!strncmp(line, "CMAKE_SOURCE_DIR", 16)) {
                        char *dir = line + 16;
                        while (*dir && (*dir == ' ' || *dir == '='))
                            ++dir;
                        if (dir != home) {
                            ret = dir;
                            ret += '/';
                            if (!Path(ret + "CMakeLists.txt").isFile())
                                ret.clear();
                        }
                        break;
                    }
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    const Entry after[] = {
        { "Makefile*", Wildcard },
        { 0, 0 }
    };

    {
        const Path ret = checkEntry(after, path, home);
        if (!ret.isEmpty())
            return ret;
    }

    return Path();
}

void initMessages()
{
#ifndef GRTAGS
    Messages::registerMessage<QueryMessage>();
    Messages::registerMessage<CompileMessage>();
    Messages::registerMessage<CreateOutputMessage>();
#endif
}
}
