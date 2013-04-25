#ifndef ClangCompletionJob_h
#define ClangCompletionJob_h

#include "ClangProject.h"
#include "Location.h"
#include <clang-c/Index.h>
#include <rct/Mutex.h>
#include <rct/Path.h>
#include <rct/ThreadPool.h>

class Connection;
class ClangCompletionJob : public ThreadPool::Job
{
public:
    ClangCompletionJob(const shared_ptr<UnitCache::Unit> &unit, const Location &location,
                       const String &unsaved, Connection *conn);
    virtual void run();
    void onConnectionDestroyed(Connection*);
private:
    shared_ptr<UnitCache::Unit> mUnit;
    const Location mLocation;
    const String mUnsaved;
    Connection *mConnection;
    Mutex mMutex;
};

#endif
