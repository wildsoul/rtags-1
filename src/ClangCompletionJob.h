#ifndef ClangCompletionJob_h
#define ClangCompletionJob_h

#ifdef CLANG_CAN_REPARSE
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
    ClangCompletionJob(const shared_ptr<UnitCache::Unit> &unit, const Location &location, const String &unsaved);
    virtual void run();
    signalslot::Signal1<ClangCompletionJob*> &finished() { return mFinished; }
    signalslot::Signal3<ClangCompletionJob*, String, String> &completion() { return mCompletion; }
private:
    shared_ptr<UnitCache::Unit> mUnit;
    const Location mLocation;
    const String mUnsaved;
    signalslot::Signal3<ClangCompletionJob*, String, String> mCompletion;
    signalslot::Signal1<ClangCompletionJob*> mFinished;
};
#endif

#endif
