#include "Server.h"

#include "Client.h"
#include "CompileMessage.h"
#include "CompletionMessage.h"
#include "Filter.h"
#include "Indexer.h"
#include "LogObject.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "QueryMessage.h"
#include "RTags.h"
#include <rct/Connection.h>
#include <rct/Event.h>
#include <rct/EventLoop.h>
#include <rct/Log.h>
#include <rct/Message.h>
#include <rct/Messages.h>
#include <rct/Path.h>
#include <rct/Process.h>
#include <rct/Rct.h>
#include <rct/RegExp.h>
#include <rct/SHA256.h>
#include <rct/SocketClient.h>
#include <rct/SocketServer.h>
#include <rct/ThreadPool.h>
#include <stdio.h>

void *UnloadTimer = &UnloadTimer;
Server::Options Server::sOptions;
RTagsPluginFactory Server::sPluginFactory;
Server::Server()
    : mServer(0), mVerbose(false)
{
}

Server::~Server()
{
    mCurrentProject.reset();
    clear();
    delete ThreadPool::instance();
    Messages::cleanup();
    sPluginFactory.cleanup();
}

void Server::clear()
{
    Path::rm(sOptions.socketFile);
    delete mServer;
    mServer = 0;
    mProjects.clear();
}

bool Server::init(const Options &options)
{
    {
        List<Path> plugins = Rct::executablePath().parentDir().files(Path::File);
        for (int i=0; i<plugins.size(); ++i) {
            if (sPluginFactory.addPlugin(plugins.at(i))) {
                error() << "Loaded plugin" << plugins.at(i);
            } else {
                const char *ext = plugins.at(i).extension();
                if (ext && !strcmp(plugins.at(i).extension(), "so"))
                    error() << "Plugin error" << sPluginFactory.error();
            }
        }
    }
    RTags::initMessages();

    if (!ThreadPool::instance())
        new ThreadPool(sOptions.threadPoolSize, sOptions.threadPoolStackSize);

    sOptions = options;
    if (options.options & NoBuiltinIncludes) {
        sOptions.defaultArguments.append("-nobuiltininc");
        sOptions.defaultArguments.append("-nostdinc++");
    }

    if (options.options & Wall)
        sOptions.defaultArguments.append("-Wall");
    sOptions.defaultArguments << "-fspell-checking";
    error() << "using args:" << String::join(sOptions.defaultArguments, " ");

    if (sOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mServer = new SocketServer;
        if (mServer->listenUnix(sOptions.socketFile)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            enum { Timeout = 1000 };
            Client client;
            if (client.connectToServer(sOptions.socketFile, Timeout)) {
                QueryMessage msg(QueryMessage::Shutdown);
                client.send(&msg, Timeout);
            }
        }
        sleep(1);
        Path::rm(sOptions.socketFile);
    }
    if (!mServer) {
        error("Unable to listen on %s", sOptions.socketFile.constData());
        return false;
    }

    mServer->clientConnected().connect(this, &Server::onNewConnection);

    reloadProjects();
    if (!(sOptions.options & NoStartupCurrentProject)) {
        Path current = Path(sOptions.dataDir + ".currentProject").readAll(1024);
        if (current.size() > 1) {
            current.chop(1);
            if (!setCurrentProject(current)) {
                error() << "Can't restore project" << current;
                unlink((sOptions.dataDir + ".currentProject").constData());
            }
        }
    }
    return true;
}

shared_ptr<Project> Server::addProject(const Path &path)
{
    bool remove = false;
    {
        shared_ptr<Project> &project = mProjects[path];
        if (!project) {
            project.reset(new Project(path));
            shared_ptr<Indexer> indexer = sPluginFactory.createIndexer(sOptions.indexPlugin, project);
            if (!indexer) {
                error("Can't load indexer from plugin");
                project.reset();
                remove = true;
            }
            if (indexer) {
                project->setIndexer(indexer);
                return project;
            }
        }
    }
    if (remove)
        mProjects.remove(path);
    return shared_ptr<Project>();
}

int Server::reloadProjects()
{
    mProjects.clear();
    List<Path> projects = sOptions.dataDir.files(Path::File);
    const Path home = Path::home();
    for (int i=0; i<projects.size(); ++i) {
        Path path = projects.at(i).fileName();
        Server::decodePath(path);
        if (path.isAbsolute() && path.isDir())
            addProject(path);
    }
    return mProjects.size();
}

void Server::onNewConnection()
{
    while (true) {
        SocketClient *client = mServer->nextClient();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(this, &Server::onNewMessage);
        // client->disconnected().connect(conn, &Connection::onLoop);
    }
}

void Server::onNewMessage(Message *message, Connection *connection)
{
    ClientMessage *m = static_cast<ClientMessage*>(message);
    const String raw = m->raw();
    if (!raw.isEmpty()) {
        if (message->messageId() != CompileMessage::MessageId) {
            error() << raw;
        } else {
            warning() << raw;
        }
    } else if (message->messageId() != CompileMessage::MessageId) {
        updateProject(m->projects());
    }

    switch (message->messageId()) {
    case CompileMessage::MessageId:
        handleCompileMessage(static_cast<CompileMessage*>(message), connection);
        break;
    case CompletionMessage::MessageId:
        handleCompletionMessage(static_cast<CompletionMessage*>(message), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<QueryMessage*>(message), connection);
        break;
    case ResponseMessage::MessageId:
        assert(0);
        connection->finish();
        break;
    default:
        error("Unknown message: %d", message->messageId());
        connection->finish();
        break;
    }
}

void Server::handleCompileMessage(CompileMessage *message, Connection *conn)
{
    conn->finish(); // nothing to wait for
    GccArguments args;
    if (!args.parse(message->arguments(), message->path()) || args.language() == GccArguments::NoLang)
        return;

    Path srcRoot;
    if (updateProject(message->projects())) {
        srcRoot = currentProject()->path();
    } else if (!message->projects().isEmpty()) {
        srcRoot = message->projects().first();
    } else {
        srcRoot = args.projectRoot();
    }
    List<Path> inputFiles = args.inputFiles();
    if (srcRoot.isEmpty()) {
        error("Can't find project root for %s", String::join(inputFiles, ", ").constData());
        return;
    }

    debug() << inputFiles << "in" << srcRoot;
    const int count = inputFiles.size();
    int filtered = 0;
    if (!sOptions.excludeFilters.isEmpty()) {
        for (int i=0; i<count; ++i) {
            Path &p = inputFiles[i];
            if (Filter::filter(p, sOptions.excludeFilters) == Filter::Filtered) {
                warning() << "Filtered out" << p;
                p.clear();
                ++filtered;
            }
        }
    }
    if (filtered == count) {
        warning("no input file?");
        return;
    }

    shared_ptr<Project> project = mProjects.value(srcRoot);
    if (!project) {
        project = addProject(srcRoot);
        assert(project);
    }

    if (!mCurrentProject.lock()) {
        setCurrentProject(project);
    } else {
        loadProject(project);
    }

    for (int i=0; i<count; ++i) {
        project->indexFile(inputFiles.at(i), args);
    }
}

void Server::handleCompletionMessage(CompletionMessage *message, Connection *conn)
{
    updateProject(message->projects());
    const Location loc = message->location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project) {
        conn->finish();
        return;
    }
    if (!project->indexer()->codeCompleteAt(loc, message->contents(), conn))
        conn->finish();
    // not calling finish, could be async
}

void Server::handleQueryMessage(QueryMessage *message, Connection *conn)
{
    conn->setSilent(message->flags() & QueryMessage::Silent);
    updateProject(message->projects());

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::JobCount:
        jobCount(*message, conn);
        break;
    case QueryMessage::FixIts:
        fixIts(*message, conn);
        break;
    case QueryMessage::LogOutput:
        logOutput(*message, conn);
        break;
    case QueryMessage::Builds:
        builds(*message, conn);
        break;
    case QueryMessage::IsIndexing:
        isIndexing(*message, conn);
        break;
    case QueryMessage::RemoveFile:
        removeFile(*message, conn);
        break;
    case QueryMessage::FindFile:
        findFile(*message, conn);
        break;
    case QueryMessage::DumpFile:
        dumpFile(*message, conn);
        break;
    case QueryMessage::Dependencies:
        dependencies(*message, conn);
        break;
    case QueryMessage::DeleteProject:
    case QueryMessage::UnloadProject:
        removeProject(*message, conn);
        break;
    case QueryMessage::ReloadProjects:
        reloadProjects(*message, conn);
        break;
    case QueryMessage::Project:
        project(*message, conn);
        break;
    case QueryMessage::Reindex: {
        reindex(*message, conn);
        break; }
    case QueryMessage::ClearProjects:
        clearProjects(*message, conn);
        break;
    case QueryMessage::CursorInfo:
        cursorInfo(*message, conn);
        break;
    case QueryMessage::Shutdown:
        shutdown(*message, conn);
        break;
    case QueryMessage::FollowLocation:
        followLocation(*message, conn);
        break;
    case QueryMessage::ReferencesLocation:
        referencesForLocation(*message, conn);
        break;
    case QueryMessage::ReferencesName:
        referencesForName(*message, conn);
        break;
    case QueryMessage::ListSymbols:
    case QueryMessage::LocalSymbols:
        listSymbols(*message, conn);
        break;
    case QueryMessage::FindSymbols:
        findSymbols(*message, conn);
        break;
    case QueryMessage::Status:
        status(*message, conn);
        break;
    case QueryMessage::IsIndexed:
        isIndexed(*message, conn);
        break;
    case QueryMessage::HasFileManager:
        hasFileManager(*message, conn);
        break;
    case QueryMessage::PreprocessFile:
        preprocessFile(*message, conn);
        break;
    case QueryMessage::ReloadFileManager:
        reloadFileManager(*message, conn);
        break;
    }
}

static bool isFiltered(const Location &loc, const QueryMessage &query)
{
    if (query.minLine() == -1) {
        assert(query.maxLine() == -1);
        return false;
    }
    return loc.line() >= query.minLine() && loc.line() <= query.maxLine();
}

void Server::followLocation(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    Project::Cursor cursor = project->indexer()->cursor(loc);
    if (query.flags() & QueryMessage::DeclarationOnly && cursor.isDefinition() && cursor.target.isValid())
        cursor = project->indexer()->cursor(cursor.target);

    if (cursor.location.isValid() && !isFiltered(cursor.location, query))
        conn->write(cursor.target.toString(query.flags()).constData());
    conn->finish();
}

void Server::isIndexing(const QueryMessage &, Connection *conn)
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->indexer()->isIndexing()) {
            conn->write("1");
            conn->finish();
            return;
        }
    }
    conn->write("0");
    conn->finish();
}


void Server::removeFile(const QueryMessage &query, Connection *conn)
{
    // Path path = query.path();
    const Match match(query.query());
    shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project) {
        project = currentProject();
        if (!project) {
            error("No project");
            conn->finish();
            return;
        }
    }

    const int count = project->remove(match);
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Removed %d files", count);
    } else {
        conn->write("No matches");
    }
    conn->finish();
}

void Server::logOutput(const QueryMessage &query, Connection *conn)
{
    new LogObject(conn, query.query().toULongLong());
}

void Server::findFile(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->fileManager()) {
        error("No project");
        conn->finish();
        return;
    }

    const String q = query.query();
    RegExp regExp;
    String pattern;
    if (!q.isEmpty()) {
        if (query.flags() & QueryMessage::MatchRegexp) {
            regExp = q;
        } else {
            pattern = q;
        }
    }

    const Path srcRoot = project->path();

    enum Mode {
        All,
        RegExp,
        Pattern
    } mode = All;
    String::CaseSensitivity cs = String::CaseSensitive;
    if (regExp.isValid()) {
        mode = RegExp;
    } else if (!pattern.isEmpty()) {
        mode = Pattern;
    }
    if (query.flags() & QueryMessage::MatchCaseInsensitive)
        cs = String::CaseInsensitive;

    String out;
    out.reserve(PATH_MAX);
    if (query.flags() & QueryMessage::AbsolutePath) {
        out.append(srcRoot);
        assert(srcRoot.endsWith('/'));
    }
    const FilesMap& dirs = project->filesMap();
    FilesMap::const_iterator dirit = dirs.begin();
    bool foundExact = false;
    const int patternSize = pattern.size();
    List<String> matches;
    const bool preferExact = query.flags() & QueryMessage::FindFilePreferExact;
    while (dirit != dirs.end()) {
        const Path &dir = dirit->first;
        if (dir.size() < srcRoot.size()) {
            continue;
        } else {
            out.append(dir.constData() + srcRoot.size(), dir.size() - srcRoot.size());
        }

        const Set<String> &files = dirit->second;
        for (Set<String>::const_iterator it = files.begin(); it != files.end(); ++it) {
            const String &key = *it;
            out.append(key);
            bool ok;
            switch (mode) {
            case All:
                ok = true;
                break;
            case RegExp:
                ok = regExp.indexIn(out) != -1;
                break;
            case Pattern:
                if (!preferExact) {
                    ok = out.contains(pattern, cs);
                } else {
                    const int outSize = out.size();
                    const bool exact = (outSize > patternSize && out.endsWith(pattern) && out.at(outSize - (patternSize + 1)) == '/');
                    if (exact) {
                        ok = true;
                        if (!foundExact) {
                            matches.clear();
                            foundExact = true;
                        }
                    } else {
                        ok = !foundExact && out.contains(pattern, cs);
                    }
                }
                break;
            }
            if (ok) {
                if (preferExact && !foundExact) {
                    matches.append(out);
                } else {
                    if (!conn->write(out)) {
                        conn->finish();
                        return;
                    }
                }
            }
            out.chop(key.size());
        }
        out.chop(dir.size() - srcRoot.size());
        ++dirit;
    }
    for (List<String>::const_iterator it = matches.begin(); it != matches.end(); ++it) {
        if (!conn->write(*it))
            break;
    }
    conn->finish();
}

void Server::dumpFile(const QueryMessage &query, Connection *conn)
{
    const Path& path = query.query();
    const uint32_t fileId = Location::fileId(path);
    const Location loc(fileId, 1, 1);

    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project || !project->isValid()) {
        conn->write<256>("%s is not indexed", path.constData());
        conn->finish();
        return;
    }
    const SourceInformation c = project->sourceInfo(path);
    if (c.isNull()) {
        conn->write<256>("%s is not indexed", path.constData());
        conn->finish();
        return;
    }

    project->indexer()->dump(c, conn);
    conn->finish(); // this might block the main thread too long
}

void Server::cursorInfo(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(loc);

    if (!project) {
        conn->finish();
        return;
    }

    Project::Cursor cursor = project->indexer()->cursor(loc);
    if (cursor.location.isValid())
        conn->write(project->toString(cursor, query.flags()));
    conn->finish();
}

void Server::dependencies(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project) {
        conn->finish();
        return;
    }

    const Path srcRoot = project->path();

    Set<Path> dependencies = project->indexer()->dependencies(path, Indexer::DependsOnArg);
    dependencies.remove(path);
    if (!dependencies.isEmpty()) {
        conn->write<64>("%s is depended on by:", path.constData());
        for (Set<Path>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            conn->write<64>("  %s", it->constData());
        }
    }
    dependencies = project->indexer()->dependencies(path, Indexer::ArgDependsOn);
    if (!dependencies.isEmpty()) {
        conn->write<64>("%s depends on:", path.constData());
        for (Set<Path>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            conn->write<64>("  %s", path.constData());
        }
    }
    conn->finish();
}


void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    const Path path = Path::resolved(query.query());
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project) {
        conn->finish();
        return;
    }

    const String out = project->indexer()->fixits(path);
    if (!out.isEmpty())
        conn->write(out);

    conn->finish();
}

void Server::referencesForLocation(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }
    project->indexer()->references(loc, query.flags(), query.pathFilters(), conn);
    conn->finish();
}

void Server::referencesForName(const QueryMessage& query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }
    const Set<Project::Cursor> cursors = project->indexer()->findCursors(query.query(), query.pathFilters());
    for (Set<Project::Cursor>::const_iterator it = cursors.begin(); it != cursors.end(); ++it) {
        project->indexer()->references(it->location, query.flags(), query.pathFilters(), conn);
    }
    conn->finish();
}


struct Node
{
    Node(const Location &loc = Location(), bool def = false)
        : location(loc), definition(def)
    {}

    Location location;
    bool definition;
    bool operator<(const Node &node) const
    {
        if (definition != node.definition)
            return definition;
        return location < node.location;
    }
};

void Server::findSymbols(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }
    List<Project::Cursor> cursors = project->indexer()->findCursors(query.query(), query.pathFilters()).toList();
    if (!cursors.isEmpty()) {
        List<Node> nodes(cursors.size());
        for (int i=0; i<cursors.size(); ++i)
            nodes[i] = Node(cursors.at(i).location, cursors.at(i).isDefinition());
        std::sort(nodes.begin(), nodes.end());
        for (int i=0; i<nodes.size(); ++i) {
            conn->write(nodes.at(i).location.toString(query.flags()));
        }
    }

    conn->finish();
}

void Server::listSymbols(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    Set<String> strings;
    if (query.type() == QueryMessage::LocalSymbols) {
        Set<Project::Cursor> cursors = project->indexer()->cursors(query.query());
        for (Set<Project::Cursor>::const_iterator it = cursors.begin(); it != cursors.end(); ++it) {
            switch (it->kind) {
            case Project::Cursor::Reference:
            case Project::Cursor::EnumValue:
            case Project::Cursor::Namespace:
                break;
            default:
                strings.insert(it->symbolName);
                break;
            }
        }
    } else {
        strings = project->indexer()->listSymbols(query.query(), query.pathFilters());
    }

    const bool elispList = query.flags() & QueryMessage::ElispList;

    int max = query.max();
    if (elispList) {
        String out = "(list ";
        for (Set<String>::const_iterator it = strings.begin(); it != strings.end(); ++it) {
            if (!max--) // max could start at -1
                break;
            out += '"' + *it + '"';
        }
        out += ')';
        conn->write(out);
    } else {
        List<String> sorted = strings.toList();
        if (query.flags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<String>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        int count = sorted.size();
        if (max != -1)
            count = max;
        for (int i=0; i<count; ++i) {
            conn->write(sorted.at(i));
        }
    }

    conn->finish();
}

void Server::status(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }
    project->indexer()->status(query.query(), conn, query.flags());
    conn->finish();
}

void Server::isIndexed(const QueryMessage &query, Connection *conn)
{
    int ret = 0;
    const Match match = query.match();
    shared_ptr<Project> project = updateProjectForLocation(match);
    if (project) {
        bool indexed = false;
        if (project->match(match, &indexed))
            ret = indexed ? 1 : 2;
    }

    error("=> %d", ret);
    conn->write<16>("%d", ret);
    conn->finish();
}

void Server::reloadFileManager(const QueryMessage &, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (project) {
        conn->write<512>("Reloading files for %s", project->path().constData());
        conn->finish();
        project->fileManager()->reload();
    } else {
        conn->write("No current project");
        conn->finish();
    }
}


void Server::hasFileManager(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (project && project->fileManager() && (project->fileManager()->contains(path) || project->match(path))) {
        error("=> 1");
        conn->write("1");
    } else {
        error("=> 0");
        conn->write("0");
    }
    conn->finish();
}

void Server::preprocessFile(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project || !project->isValid()) {
        conn->write("No project");
        conn->finish();
        return;
    }

    const SourceInformation c = project->sourceInfo(path);
    if (c.isNull()) {
        conn->write("No arguments for " + path);
        conn->finish();
        return;
    }
    if (c.builds.size() <= query.buildIndex()) {
        conn->write<512>("No build for for index %d (max %d) for %s",
                         query.buildIndex(), c.builds.size() - 1, path.constData());
        conn->finish();
        return;
    }

    Preprocessor* pre = new Preprocessor(c, query.buildIndex(), conn);
    pre->preprocess();
}

void Server::jobCount(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        conn->write<128>("Running with %d jobs", sOptions.threadPoolSize);
    } else {
        const int jobCount = query.query().toLongLong();
        if (jobCount <= 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query.query().constData(), jobCount);
        } else {
            sOptions.threadPoolSize = jobCount;
            ThreadPool::instance()->setConcurrentJobs(jobCount);
            conn->write<128>("Changed jobs to %d", jobCount);
        }
    }
    conn->finish();
}

void Server::clearProjects()
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it)
        it->second->unload();
    Rct::removeDirectory(sOptions.dataDir);
    mCurrentProject.reset();
    unlink((sOptions.dataDir + ".currentProject").constData());
    mProjects.clear();
}

void Server::reindex(const QueryMessage &query, Connection *conn)
{
    Match match = query.match();
    shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project) {
        project = currentProject();
        if (!project || !project->isValid()) {
            error("No project");
            conn->finish();
            return;
        }
    }

    const int count = project->reindex(match);
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
    } else {
        conn->write("No matches");
    }
    conn->finish();
}

void Server::loadProject(const shared_ptr<Project> &project)
{
    assert(project);
    if (!project->isValid()) {
        assert(!project->isValid());
        project->init();
        project->restore();
    }
}

shared_ptr<Project> Server::setCurrentProject(const Path &path) // lock always held
{
    ProjectsMap::iterator it = mProjects.find(path);
    if (it != mProjects.end()) {
        setCurrentProject(it->second);
        return it->second;
    }
    return shared_ptr<Project>();
}

shared_ptr<Project> Server::setCurrentProject(const shared_ptr<Project> &project)
{
    if (project && project != mCurrentProject.lock()) {
        mCurrentProject = project;
        Path::mkdir(sOptions.dataDir);
        FILE *f = fopen((sOptions.dataDir + ".currentProject").constData(), "w");
        if (f) {
            if (!fwrite(project->path().constData(), project->path().size(), 1, f) || !fwrite("\n", 1, 1, f)) {
                error() << "error writing to" << (sOptions.dataDir + ".currentProject");
                fclose(f);
                unlink((sOptions.dataDir + ".currentProject").constData());
            } else {
                fclose(f);
            }
        } else {
            error() << "error opening" << (sOptions.dataDir + ".currentProject") << "for write";
        }

        if (!project->isValid())
            loadProject(project);
        return project;
    }
    return shared_ptr<Project>();
}

shared_ptr<Project> Server::updateProjectForLocation(const Location &location)
{
    return updateProjectForLocation(location.path());
}

shared_ptr<Project> Server::updateProjectForLocation(const Path &path)
{
    return updateProjectForLocation(Match(path));
}

shared_ptr<Project> Server::updateProjectForLocation(const Match &match)
{
    shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    if (cur && cur->match(match))
        return cur;

    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(match)) {
            return setCurrentProject(it->second->path());
        }
    }
    return shared_ptr<Project>();
}

void Server::removeProject(const QueryMessage &query, Connection *conn)
{
    const bool unload = query.type() == QueryMessage::UnloadProject;

    const Match match = query.match();
    ProjectsMap::iterator it = mProjects.begin();
    while (it != mProjects.end()) {
        ProjectsMap::iterator cur = it++;
        if (match.match(cur->first)) {
            if (mCurrentProject.lock() == it->second) {
                mCurrentProject.reset();
                unlink((sOptions.dataDir + ".currentProject").constData());
            }
            cur->second->unload();
            Path path = cur->first;
            conn->write<128>("%s project: %s", unload ? "Unloaded" : "Deleted", path.constData());
            if (!unload) {
                Server::encodePath(path);
                Path::rm(sOptions.dataDir + path);
                mProjects.erase(cur);
            }
        }
    }
    conn->finish();
}

void Server::reloadProjects(const QueryMessage &query, Connection *conn)
{
    const int old = mProjects.size();
    const int cur = reloadProjects();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

bool Server::selectProject(const Match &match, Connection *conn)
{
    shared_ptr<Project> selected;
    bool error = false;
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        assert(it->second);
        if (it->second->match(match)) {
            if (error) {
                if (conn)
                    conn->write(it->first);
            } else if (selected) {
                error = true;
                if (conn) {
                    conn->write<128>("Multiple matches for %s", match.pattern().constData());
                    conn->write(selected->path());
                    conn->write(it->first);
                }
                selected.reset();
            } else {
                selected = it->second;
            }
        }
    }
    if (selected) {
        if (setCurrentProject(selected) && conn)
            conn->write<128>("Selected project: %s for %s", selected->path().constData(), match.pattern().constData());
        return true;
    } else if (!error && conn) {
        conn->write<128>("No matches for %s", match.pattern().constData());
    }
    return false;
}

bool Server::updateProject(const List<String> &projects)
{
    for (int i=0; i<projects.size(); ++i) {
        if (selectProject(projects.at(i), 0)) {
            const Path p = Path::resolved(projects.at(i));
            if (p.isFile() && p != mCurrentSourceFile)
                mCurrentSourceFile = p;

            return true;
        }
    }
    return false;
}

void Server::project(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        const shared_ptr<Project> current = mCurrentProject.lock();
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            conn->write<128>("%s%s%s",
                             it->first.constData(),
                             it->second->isValid() ? " (loaded)" : "",
                             it->second == current ? " <=" : "");
        }
    } else {
        Path selected;
        bool error = false;
        const Match match = query.match();
        const ProjectsMap::const_iterator it = mProjects.find(match.pattern());
        bool ok = false;
        unsigned long long index = query.query().toULongLong(&ok);
        if (it != mProjects.end()) {
            selected = it->first;
        } else {
            for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                assert(it->second);
                if (ok) {
                    if (!index) {
                        selected = it->first;
                    } else {
                        --index;
                    }
                }
                if (match.match(it->first)) {
                    if (error) {
                        conn->write(it->first);
                    } else if (!selected.isEmpty()) {
                        error = true;
                        conn->write<128>("Multiple matches for %s", match.pattern().constData());
                        conn->write(selected);
                        conn->write(it->first);
                        selected.clear();
                    } else {
                        selected = it->first;
                    }
                }
            }
        }
        if (!selected.isEmpty()) {
            shared_ptr<Project> current = mCurrentProject.lock();
            if (!current || selected != current->path()) {
                setCurrentProject(selected);
                conn->write<128>("Selected project: %s for %s", selected.constData(), match.pattern().constData());
            }
        } else if (!error) {
            conn->write<128>("No matches for %s", match.pattern().constData());
        }
    }
    conn->finish();
}

void Server::clearProjects(const QueryMessage &query, Connection *conn)
{
    clearProjects();
    conn->write("Cleared projects");
    conn->finish();
}

void Server::shutdown(const QueryMessage &query, Connection *conn)
{
    EventLoop::instance()->exit();
    conn->write("Shutting down");
    conn->finish();
}

void Server::builds(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    if (!path.isEmpty()) {
        shared_ptr<Project> project = updateProjectForLocation(path);
        if (project) {
            const SourceInformation info = project->sourceInfo(path);
            if (!info.isNull())
                conn->write(info.toString());
        }
    } else if (shared_ptr<Project> project = currentProject()) {
        const SourceInformationMap infos = project->sourceInfos();
        for (SourceInformationMap::const_iterator it = infos.begin(); it != infos.end(); ++it) {
            conn->write(it->second.toString());
        }
    } else {
        conn->write("No project");
    }
    conn->finish();
}

void Server::timerEvent(TimerEvent *e)
{
    if (e->userData() == UnloadTimer) {
        shared_ptr<Project> cur = mCurrentProject.lock();
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            if (it->second->isValid() && it->second != cur && !it->second->indexer()->isIndexing()) {
                it->second->unload();
            }
        }
    }
}

bool Server::encodePath(Path &path)
{
    int size = path.size();
    enum { EncodedUnderscoreLength = 12 };
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        switch (ch) {
        case '/':
            ch = '_';
            break;
        case '_':
            path.replace(i, 1, "<underscore>");
            size += EncodedUnderscoreLength - 1;
            i += EncodedUnderscoreLength - 1;
            break;
        case '<':
            if (i + EncodedUnderscoreLength <= size && !strncmp(&ch + 1, "underscore>", EncodedUnderscoreLength - 1)) {
                error("Invalid folder name %s", path.constData());
                return false;
            }
            break;
        }
    }
    return true;
}

void Server::decodePath(Path &path)
{
    int size = path.size();
    enum { EncodedUnderscoreLength = 12 };
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        switch (ch) {
        case '_':
            ch = '/';
            break;
        case '<':
            if (i + EncodedUnderscoreLength <= size && !strncmp(&ch + 1, "underscore>", EncodedUnderscoreLength - 1)) {
                path.replace(i, EncodedUnderscoreLength, "_");
                size -= EncodedUnderscoreLength - 1;
            }
            break;
        }
    }
}

bool Server::loadFileIds()
{
    MutexLocker lock(&Location::sMutex);
    const Path p = sOptions.dataDir + "fileids";
    FILE *f = fopen(p.constData(), "r");
    if (!f)
        return false;
    Deserializer s(f);
    uint32_t fileSize;
    s >> fileSize;
    if (fileSize != static_cast<uint32_t>(Rct::fileSize(f))) {
        fclose(f);
        return false;
    }
    uint32_t version;
    s >> version;
    if (version != Server::DatabaseVersion) {
        fclose(f);
        return false;
    }
    uint32_t size;
    s >> size;
    Path path;
    uint32_t fileId;
    while (size--) {
        s >> path >> fileId;
        Location::sPathsToIds[path] = fileId;
        Location::sIdsToPaths[fileId] = path;
    }
    fclose(f);
    return true;
}

bool Server::saveFileIds()
{
    MutexLocker lock(&Location::sMutex);
    const Path p = sOptions.dataDir + "fileids";
    FILE *f = fopen(p.constData(), "w");
    if (!f)
        return false;
    Serializer s(f);
    s << static_cast<uint32_t>(0);
    s << static_cast<uint32_t>(Server::DatabaseVersion);
    s << static_cast<uint32_t>(Location::sPathsToIds.size());
    for (Map<Path, uint32_t>::const_iterator it = Location::sPathsToIds.begin();
         it != Location::sPathsToIds.end(); ++it) {
        s << it->first << it->second;
    }
    const uint32_t size = ftell(f);
    fseek(f, 0, SEEK_SET);
    s << size;
    fclose(f);
    return true;
}
