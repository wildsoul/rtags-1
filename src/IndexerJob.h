#ifndef IndexerJob_h
#define IndexerJob_h

#include "RTags.h"
#include "Job.h"
#include <rct/ThreadPool.h>
#include <rct/Mutex.h>

class IndexerJob : public ThreadPool::Job, public enable_shared_from_this<IndexerJob>
{
public:
    enum Type {
        Makefile,
        Dirty,
        Dump
    };
    IndexerJob(const shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation, Connection *conn = 0);
    const SourceInformation &sourceInformation() const { return mSourceInformation; }
    time_t parseTime() const { return mParseTime; }
    Type type() const { return mType; }
    enum AbortMode {
        Unilaterally,
        OnlyIfStarted
    };
    enum State {
        NotStarted,
        Running,
        Finished,
        Aborted
    };
    State state() const;
    State abort(AbortMode mode);
protected:
    virtual void execute();

    weak_ptr<Project> mProject;
    const Type mType;
    SourceInformation mSourceInformation;
    Connection *mConnection;

    Map<uint32_t, bool> mVisitedFiles;
    StopWatch mTimer;
    time_t mParseTime;
    State mState;
};

#endif
