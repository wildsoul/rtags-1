#include "IndexerJob.h"
#include <rct/StopWatch.h>
#include "Project.h"
#include "Database.h"
#include <rct/MutexLocker.h>

IndexerJob::IndexerJob(const shared_ptr<Database> &db, Type type,
                       const SourceInformation &sourceInformation, Connection *connection)
    : ThreadPool::Job(), mDatabase(db), mType(type), mSourceInformation(sourceInformation),
      mParseTime(0), mSymbolCount(-1), mElapsed(-1)
{
    assert(!sourceInformation.sourceFile.isEmpty());
    assert((connection != 0) == (type == Dump));
}

void IndexerJob::run()
{
    StopWatch timer(StopWatch::Millisecond);
    timer.start();
    assert(mDatabase);
    if (mType == Dump) {
        assert(mConnection);
        mDatabase->dump(mSourceInformation, mConnection);
    } else {
        mSymbolCount = mDatabase->index(mSourceInformation);
        mParseTime = time(0);
        mElapsed = timer.elapsed();
        mFinished(shared_from_this());
    }
}
