#include "Server.h"

#include "Client.h"
#include "CompileMessage.h"
#include "CompletionMessage.h"
#include "Database.h"
#include "Filter.h"
#include "IndexerJob.h"
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
#include <stdio.h>

void *UnloadTimer = &UnloadTimer;
Server *Server::sInstance = 0;
Server::Server()
    : mServer(0), mVerbose(false), mJobId(0), mThreadPool(0)
{
    assert(!sInstance);
    sInstance = this;
}

Server::~Server()
{
    clear();
    assert(sInstance == this);
    sInstance = 0;
    Messages::cleanup();
}

void Server::clear()
{
    if (mThreadPool) {
        mThreadPool->clearBackLog();
        delete mThreadPool;
        mThreadPool = 0;
    }
    Path::rm(mOptions.socketFile);
    delete mServer;
    mServer = 0;
    mProjects.clear();
}

bool Server::init(const Options &options)
{
    {
        List<Path> plugins = Rct::executablePath().parentDir().files(Path::File);
        for (int i=0; i<plugins.size(); ++i) {
            if (mPluginFactory.addPlugin(plugins.at(i))) {
                error() << "Loaded plugin" << plugins.at(i);
                //} else {
                //error() << "Plugin error" << mPluginFactory.error();
            }
        }
    }
    RTags::initMessages();

    mThreadPool = new ThreadPool(options.threadCount, options.stackSize);

    mOptions = options;
    if (options.options & NoBuiltinIncludes) {
        mOptions.defaultArguments.append("-nobuiltininc");
        mOptions.defaultArguments.append("-nostdinc++");
    }

    if (options.options & UnlimitedErrors)
        mOptions.defaultArguments.append("-ferror-limit=0");
    if (options.options & Wall)
        mOptions.defaultArguments.append("-Wall");
    if (options.options & SpellChecking)
        mOptions.defaultArguments << "-fspell-checking";
    error() << "using args:" << String::join(mOptions.defaultArguments, " ");

    if (mOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mServer = new SocketServer;
        if (mServer->listenUnix(mOptions.socketFile)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            enum { Timeout = 1000 };
            Client client;
            if (client.connectToServer(mOptions.socketFile, Timeout)) {
                QueryMessage msg(QueryMessage::Shutdown);
                client.send(&msg, Timeout);
            }
        }
        sleep(1);
        Path::rm(mOptions.socketFile);
    }
    if (!mServer) {
        error("Unable to listen on %s", mOptions.socketFile.constData());
        return false;
    }

    mServer->clientConnected().connect(this, &Server::onNewConnection);
    reloadProjects();
    if (!(mOptions.options & NoStartupCurrentProject)) {
        Path current = Path(mOptions.dataDir + ".currentProject").readAll(1024);
        if (current.size() > 1) {
            current.chop(1);
            if (!setCurrentProject(current)) {
                error() << "Can't restore project" << current;
                unlink((mOptions.dataDir + ".currentProject").constData());
            }
        }
    }

    return true;
}

shared_ptr<Project> Server::addProject(const Path &path)
{
    shared_ptr<Project> &project = mProjects[path];
    if (!project) {
        project.reset(new Project(path));
        if (!project->database()) {
            error("Can't load plugin");
            project.reset();
        }

        return project;
    }
    return shared_ptr<Project>();
}

int Server::reloadProjects()
{
    mProjects.clear();
    List<Path> projects = mOptions.dataDir.files(Path::File);
    const Path home = Path::home();
    for (int i=0; i<projects.size(); ++i) {
        Path file = projects.at(i);
        Path p = file.mid(mOptions.dataDir.size());
        Server::decodePath(p);
        if (p.isDir()) {
            bool remove = false;
            if (FILE *f = fopen(file.constData(), "r")) {
                Deserializer in(f);
                int version;
                in >> version;

                if (version == Server::DatabaseVersion) {
                    int fs;
                    in >> fs;
                    if (fs != Rct::fileSize(f)) {
                        error("%s seems to be corrupted, refusing to restore. Removing.",
                              file.constData());
                        remove = true;
                    } else {
                        addProject(p);
                    }
                } else {
                    remove = true;
                    error() << file << "has wrong format. Got" << version << "expected" << Server::DatabaseVersion << "Removing";
                }
                fclose(f);
            }
            if (remove) {
                Path::rm(file);
            }
        }
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
        conn->destroyed().connect(this, &Server::onConnectionDestroyed);
        // client->disconnected().connect(conn, &Connection::onLoop);
    }
}

void Server::onConnectionDestroyed(Connection *o)
{
    Map<int, Connection*>::iterator it = mPendingLookups.begin();
    const Map<int, Connection*>::const_iterator end = mPendingLookups.end();
    while (it != end) {
        if (it->second == o) {
            mPendingLookups.erase(it++);
        } else {
            ++it;
        }
    }
}

void Server::onNewMessage(Message *message, Connection *connection)
{
    if (mOptions.unloadTimer)
        mUnloadTimer.start(shared_from_this(), mOptions.unloadTimer * 1000 * 60, SingleShot, UnloadTimer);

    ClientMessage *m = static_cast<ClientMessage*>(message);
    const String raw = m->raw();
    if (!raw.isEmpty()) {
        if (message->messageId() != CompileMessage::MessageId) {
            error() << raw;
        } else {
            warning() << raw;
        }
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
    if (args.parse(message->arguments(), message->path())) {
        processSourceFile(args, message->projects());
    }
}

void Server::handleCompletionMessage(CompletionMessage *message, Connection *conn)
{
    printf("[%s] %s:%d: void Server::handleCompletionMessage(CompletionMessage *message, Connection *conn) [after]\n", __func__, __FILE__, __LINE__);
    updateProject(message->projects());
    const Location loc = message->location();
    if (loc.isNull()) {
        printf("[%s] %s:%d: if (loc.isNull()) { [after]\n", __func__, __FILE__, __LINE__);
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project) {
        printf("[%s] %s:%d: if (!project) { [after]\n", __func__, __FILE__, __LINE__);
        conn->finish();
        return;
    }
    printf("[%s] %s:%d: } [after]\n", __func__, __FILE__, __LINE__);
    shared_ptr<Database> database = project->database();
    database->codeCompleteAt(loc, message->contents(), conn);
    conn->finish();
}

void Server::handleQueryMessage(QueryMessage *message, Connection *conn)
{
    conn->setSilent(message->flags() & QueryMessage::Silent);
    updateProject(message->projects());

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
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
    case QueryMessage::JobCount:
        jobCount(*message, conn);
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
        removeProject(*message, conn);
        break;
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

int Server::nextId()
{
    ++mJobId;
    if (!mJobId)
        ++mJobId;
    return mJobId;
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

    shared_ptr<Database> database = project->database();
    Database::Cursor cursor = database->cursor(loc);
    if (query.flags() & QueryMessage::DeclarationOnly && cursor.isDefinition() && cursor.target.isValid())
        cursor = database->cursor(cursor.target);

    if (cursor.location.isValid() && !isFiltered(cursor.location, query))
        conn->write(cursor.target.key(query.keyFlags()).constData());
    conn->finish();
}

void Server::isIndexing(const QueryMessage &, Connection *conn)
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->isIndexing()) {
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
    const FilesMap& dirs = project->files();
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
    const Path path = query.query();
    const Location loc(path, 1, 1);

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

    shared_ptr<IndexerJob> job(new IndexerJob(project->database(), IndexerJob::Dump, c, conn));
    startJob(job);
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

    shared_ptr<Database> db = project->database();
    Database::Cursor cursor = db->cursor(loc);
    if (cursor.location.isValid())
        conn->write(cursor.toString(query.keyFlags()));
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

    // const bool absolute = (queryFlags() & QueryMessage::AbsolutePath);
    Set<Path> dependencies = project->dependencies(path, Project::DependsOnArg);
    dependencies.remove(path);
    // if (!absolute && path.startsWith(srcRoot))
    //     absolute.remove(0, srcRoot.size());
    if (!dependencies.isEmpty()) {
        conn->write<64>("%s is depended on by:", path.constData());
        for (Set<Path>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            conn->write<64>("  %s", it->constData());
        }
    }
    dependencies = project->dependencies(path, Project::ArgDependsOn);
    if (!dependencies.isEmpty()) {
        conn->write<64>("%s depends on:", path.constData());
        for (Set<Path>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            conn->write<64>("  %s", path.constData());
        }
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

static inline void writeNodes(const QueryMessage &query, List<Node> &locations, Connection *conn)
{
    std::sort(locations.begin(), locations.end());
    const unsigned keyFlags = query.keyFlags();
    for (int i=0; i<locations.size(); ++i) {
        conn->write(locations.at(i).location.key(keyFlags));
    }
}

static void references(const QueryMessage &query, const Set<Database::Cursor> &cursors, const shared_ptr<Database> &db, Connection *conn)
{
    assert(!cursors.isEmpty());
    List<Node> locations;
    const bool references = !(query.flags() & QueryMessage::FindVirtuals);
    for (Set<Database::Cursor>::const_iterator it = cursors.begin(); it != cursors.end(); ++it) {
        for (Set<Location>::const_iterator loc = it->references.begin(); loc != it->references.end(); ++it) {
            if (query.flags() & QueryMessage::AllReferences) {
                locations.append(Node(*loc, false));
            } else {
                const Database::Cursor c = db->cursor(*loc);
                if (c.isValid() && (c.kind == Database::Cursor::Reference) == references) {
                    locations.append(Node(c.location, c.isDefinition()));
                }
            }
        }
    }
    writeNodes(query, locations, conn);
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
    shared_ptr<Database> database = project->database();
    const Database::Cursor cursor = database->cursor(loc);
    if (!cursor.isValid()) {
        conn->finish();
        return;
    }

    Set<Database::Cursor> cursors;
    cursors.insert(cursor);
    references(query, cursors, database, conn);
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
    shared_ptr<Database> database = project->database();
    Set<Database::Cursor> cursors = database->findCursors(query.query(), query.pathFilters());
    if (!cursors.isEmpty())
        references(query, cursors, database, conn);
    conn->finish();
}

static inline bool compareCursorPreferDefinition(const Database::Cursor &l, const Database::Cursor &r)
{
    if (l.isDefinition() != r.isDefinition())
        return l.isDefinition();
    return l.location < r.location;
}

void Server::findSymbols(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }
    shared_ptr<Database> database = project->database();
    List<Database::Cursor> cursors = database->findCursors(query.query(), query.pathFilters()).toList();
    if (!cursors.isEmpty()) {
        List<Node> nodes(cursors.size());
        for (int i=0; i<cursors.size(); ++i)
            nodes[i] = Node(cursors.at(i).location, cursors.at(i).isDefinition());
        writeNodes(query, nodes, conn);
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

    shared_ptr<Database> db = project->database();
    const Set<String> strings = db->listSymbols(query.query(), query.pathFilters());

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
    project->database()->status(query.query(), conn);
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

void Server::clearProjects()
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it)
        it->second->unload();
    Rct::removeDirectory(mOptions.dataDir);
    mCurrentProject.reset();
    unlink((mOptions.dataDir + ".currentProject").constData());
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

void Server::startJob(const shared_ptr<ThreadPool::Job> &job)
{
    mThreadPool->start(job);
}

void Server::processSourceFile(const GccArguments &args, const List<String> &projects)
{
    if (args.language() == GccArguments::NoLang || mOptions.ignoredCompilers.contains(args.compiler())) {
        return;
    }
    Path srcRoot;
    if (updateProject(projects)) {
        srcRoot = currentProject()->path();
    } else if (!projects.isEmpty()) {
        srcRoot = projects.first();
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
    if (!mOptions.excludeFilters.isEmpty()) {
        for (int i=0; i<count; ++i) {
            Path &p = inputFiles[i];
            if (Filter::filter(p, mOptions.excludeFilters) == Filter::Filtered) {
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
    loadProject(project);

    if (!mCurrentProject.lock())
        mCurrentProject = project;


    for (int i=0; i<count; ++i) {
        project->index(inputFiles.at(i), args);
    }
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
        FILE *f = fopen((mOptions.dataDir + ".currentProject").constData(), "w");
        if (f) {
            if (!fwrite(project->path().constData(), project->path().size(), 1, f) || !fwrite("\n", 1, 1, f)) {
                error() << "error writing to" << (mOptions.dataDir + ".currentProject");
                fclose(f);
                unlink((mOptions.dataDir + ".currentProject").constData());
            } else {
                fclose(f);
            }
        } else {
            error() << "error opening" << (mOptions.dataDir + ".currentProject") << "for write";
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
        if (cur->second->match(match)) {
            if (mCurrentProject.lock() == it->second) {
                mCurrentProject.reset();
                unlink((mOptions.dataDir + ".currentProject").constData());
            }
            cur->second->unload();
            Path path = cur->first;
            conn->write<128>("%s project: %s", unload ? "Unloaded" : "Deleted", path.constData());
            if (!unload) {
                Server::encodePath(path);
                Path::rm(mOptions.dataDir + path);
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
        if (selectProject(projects.at(i), 0))
            return true;
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
                if (it->second->match(match)) {
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

void Server::jobCount(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        conn->write<128>("Running with %d jobs", mOptions.threadCount);
    } else {
        const int jobCount = query.query().toLongLong();
        if (jobCount <= 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query.query().constData(), jobCount);
        } else {
            mOptions.threadCount = jobCount;
            mThreadPool->setConcurrentJobs(jobCount);
            conn->write<128>("Changed jobs to %d", jobCount);
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
            if (it->second->isValid() && it->second != cur && !it->second->isIndexing()) {
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
