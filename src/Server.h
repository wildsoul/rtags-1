#ifndef Server_h
#define Server_h

#include "QueryMessage.h"
#include "CompileMessage.h"
#include "FileManager.h"
#include "QueryMessage.h"
#include "RTagsPluginFactory.h"
#include <rct/Connection.h>
#include <rct/EventReceiver.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Map.h>
#include <rct/String.h>

class Connection;
class Message;
class ErrorMessage;
class CompileMessage;
class CompletionMessage;
class SocketServer;
class GccArguments;
class Job;
class TimerEvent;
class Project;
class IndexerJob;
class Server : public EventReceiver
{
public:
    enum { DatabaseVersion = 31 };

    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x000,
        NoBuiltinIncludes = 0x001,
        Validate = 0x002,
        ClearProjects = 0x004,
        Wall = 0x008,
        IgnorePrintfFixits = 0x010,
        UnlimitedErrors = 0x020,
        SpellChecking = 0x040,
        AllowMultipleBuildsForSameCompiler = 0x080,
        NoStartupCurrentProject = 0x100
    };
    struct Options {
        Options() : options(0), unloadTimer(0), stackSize(0) {}
        Path socketFile, dataDir;
        unsigned options;
        int unloadTimer, stackSize;
        List<String> defaultArguments, excludeFilters;
        Set<Path> ignoredCompilers;
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    RTagsPluginFactory &factory() { return mPluginFactory; }
    static bool encodePath(Path &path);
    static void decodePath(Path &path);
private:
    bool selectProject(const Match &match, Connection *conn);
    bool updateProject(const List<String> &projects);

    virtual void timerEvent(TimerEvent *event);

    void clear();
    void onNewConnection();
    signalslot::Signal2<int, const List<String> &> &complete() { return mComplete; }
    shared_ptr<Project> setCurrentProject(const Path &path);
    shared_ptr<Project> setCurrentProject(const shared_ptr<Project> &project);
    void processSourceFile(const GccArguments &args, const List<String> &projects);
    void onNewMessage(Message *message, Connection *conn);
    void clearProjects();
    void handleCompileMessage(CompileMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCompletionMessage(CompletionMessage *message, Connection *conn);
    void isIndexing(const QueryMessage &, Connection *conn);
    void removeFile(const QueryMessage &query, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
    void dependencies(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void listSymbols(const QueryMessage &query, Connection *conn);
    void status(const QueryMessage &query, Connection *conn);
    void isIndexed(const QueryMessage &query, Connection *conn);
    void hasFileManager(const QueryMessage &query, Connection *conn);
    void reloadFileManager(const QueryMessage &query, Connection *conn);
    void preprocessFile(const QueryMessage &query, Connection *conn);
    void findFile(const QueryMessage &query, Connection *conn);
    void logOutput(const QueryMessage &query, Connection *conn);
    void dumpFile(const QueryMessage &query, Connection *conn);
    void removeProject(const QueryMessage &query, Connection *conn);
    void reloadProjects(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    void builds(const QueryMessage &query, Connection *conn);
    void reindex(const QueryMessage &query, Connection *conn);
    shared_ptr<Project> updateProjectForLocation(const Match &match);
    shared_ptr<Project> updateProjectForLocation(const Location &location);
    shared_ptr<Project> updateProjectForLocation(const Path &path);
    shared_ptr<Project> currentProject() const
    {
        return mCurrentProject.lock();
    }
    int reloadProjects();
    shared_ptr<Project> addProject(const Path &path);
    void loadProject(const shared_ptr<Project> &project);

    typedef Map<Path, shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer *mServer;
    bool mVerbose;

    signalslot::Signal2<int, const List<String> &> mComplete;

    Timer mUnloadTimer;

    RTagsPluginFactory mPluginFactory;

    friend class CommandProcess;
};

#endif
