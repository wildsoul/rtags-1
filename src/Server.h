#ifndef Server_h
#define Server_h

#include "CompileMessage.h"
#include "FileManager.h"
#include "QueryMessage.h"
#include "QueryMessage.h"
#include "RTagsPluginFactory.h"
#include "SourceInformation.h"
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
class Server : public EventReceiver
{
public:
    enum { DatabaseVersion = 32 };

    Server();
    ~Server();
    enum Option {
        NoOptions = 0x000,
        NoBuiltinIncludes = 0x001,
        ClearProjects = 0x002,
        Wall = 0x004,
        IgnorePrintfFixits = 0x008,
        AllowMultipleBuildsForSameCompiler = 0x010,
        NoStartupCurrentProject = 0x020
    };
    struct Options {
        Options() : options(0), threadPoolSize(0), threadPoolStackSize(0) {}
        Path socketFile, dataDir;
        unsigned options;
        int threadPoolSize, threadPoolStackSize;
        List<String> defaultArguments, excludeFilters;
        String indexPlugin, diagnosticPlugin;
    };
    bool init(const Options &options);
    Path currentSourceFile() const { return mCurrentSourceFile; }
    static const Options &options() { return sOptions; }
    static RTagsPluginFactory &factory() { return sPluginFactory; }
    static bool encodePath(Path &path);
    static void decodePath(Path &path);
    static bool loadFileIds();
    static bool saveFileIds();

private:
    bool selectProject(const Match &match, Connection *conn);
    bool updateProject(const List<String> &projects);

    virtual void timerEvent(TimerEvent *event);

    void clear();
    void onNewConnection();
    signalslot::Signal2<int, const List<String> &> &complete() { return mComplete; }
    shared_ptr<Project> setCurrentProject(const Path &path);
    shared_ptr<Project> setCurrentProject(const shared_ptr<Project> &project);
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
    void fixIts(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void jobCount(const QueryMessage &query, Connection *conn);
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

    static Options sOptions;
    SocketServer *mServer;
    bool mVerbose;

    Path mCurrentSourceFile;

    signalslot::Signal2<int, const List<String> &> mComplete;

    static RTagsPluginFactory sPluginFactory;

    friend class CommandProcess;
};

#endif
