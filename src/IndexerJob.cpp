#include "IndexerJob.h"
#include <rct/StopWatch.h>
#include "Project.h"
#include "Database.h"

IndexerJob::IndexerJob(const shared_ptr<Project> &project, Type type,
                       const SourceInformation &sourceInformation, Connection *connection)
    : ThreadPool::Job(), mType(type), mSourceInformation(sourceInformation),
      mTimer(StopWatch::Microsecond), mParseTime(0), mState(NotStarted)
{
    assert((connection != 0) == (type == Dump));
}

void IndexerJob::execute()
{
    mTimer.restart();
    shared_ptr<Database> db;
    {
        MutexLocker lock(&mutex());
        if (mState == Aborted)
            return;
        shared_ptr<Project> project = mProject.lock();
        if (!project) {
            mState = Aborted;
            return;
        }
        db = project->database();
        assert(db);
        mState = Running;
    }
    if (mType == Dump) {
        assert(mConnection);
        db->dump(mSourceInformation, mConnection);
    } else {
        const int symbols = db->index(mSourceInformation);
        mParseTime = time(0);
        shared_ptr<Project> project;
        {
            MutexLocker lock(mutex());
            if (mState != Running)
                return;
            project = mProject.lock();
            if (!project) {
                mState = Aborted;
                return;
            }
            mState = Finished;
        }
        project->onJobFinished(shared_from_this(), symbols, mTimer.elapsed());
    }
}

IndexerJob::State IndexerJob::abort(AbortMode mode)
{
    MutexLocker lock(mutex());
    switch (mState) {
    case NotStarted:
        if (mode == OnlyIfStarted)
            break;
    case Started:
        mState = Aborted;
        break;
    case Finished:
    case Aborted:
        break;
    }
    return mState;
}
