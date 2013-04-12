#ifndef RClient_h
#define RClient_h

#include <rct/List.h>
#include <rct/String.h>
#include "Client.h"
class RCCommand;
class Client;
class QueryCommand;
class RClient
{
public:
    RClient();
    ~RClient();
    bool exec();
    bool parse(int &argc, char **argv);

    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }
    int timeout() const { return mTimeout; }

    const Set<String> &pathFilters() const { return mPathFilters; }
    int minLine() const { return mMinLine; }
    int maxLine() const { return mMaxLine; }

    const Map<Path, String> &unsavedFiles() const { return mUnsavedFiles; }

    const List<String> &rdmArgs() const { return mRdmArgs; }
    const List<String> &projects() const { return mProjects; }

    String socketFile() const { return mSocketFile; }

    String context() const { return mContext; }

    unsigned queryFlags() const { return mQueryFlags; }

    int argc() const { return mArgc; }
    char **argv() const { return mArgv; }
private:
    QueryCommand *addQuery(QueryMessage::Type t, const String &query = String());

    void addLog(int level);
    void addCompile(const Path &cwd, const String &args);

    unsigned mQueryFlags;
    int mMax, mLogLevel, mTimeout, mMinLine, mMaxLine, mConnectTimeout;
    String mContext;
    Set<String> mPathFilters;
    Map<Path, String> mUnsavedFiles;
    List<RCCommand*> mCommands;
    List<String> mRdmArgs;
    String mSocketFile;
    List<String> mProjects;

    int mArgc;
    char **mArgv;
};

#endif

