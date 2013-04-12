#ifndef IndexerJob_h
#define IndexerJob_h

#include <rct/ThreadPool.h>
#include <rct/Mutex.h>
#include <rct/SignalSlot.h>
#include "SourceInformation.h"

class Database;
class Connection;
class IndexerJob : public ThreadPool::Job, public enable_shared_from_this<IndexerJob>
{
public:
    enum Type {
        Makefile,
        Dirty,
        Dump
    };
    IndexerJob(const shared_ptr<Database> &project, Type type,
               const SourceInformation &sourceInformation,
               Connection *conn = 0);
    const SourceInformation &sourceInformation() const { return mSourceInformation; }
    time_t parseTime() const { return mParseTime; }
    int symbolCount() const { return mSymbolCount; }
    int elapsed() const { return mElapsed; }
    Type type() const { return mType; }
    uint32_t fileId() const { return mFileId; }
    Path path() const { return mSourceInformation.sourceFile; }
    signalslot::Signal1<shared_ptr<IndexerJob> > &finished() { return mFinished; }
protected:
    virtual void execute();
private:
    const uint32_t mFileId;
    shared_ptr<Database> mDatabase;
    const Type mType;
    SourceInformation mSourceInformation;
    Connection *mConnection;

    Map<uint32_t, bool> mVisitedFiles;
    time_t mParseTime;
    int mSymbolCount, mElapsed;

    signalslot::Signal1<shared_ptr<IndexerJob> > mFinished;
};

#endif
