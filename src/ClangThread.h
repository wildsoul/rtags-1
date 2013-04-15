#ifndef ClangThread_h
#define ClangThread_h

#include "SourceInformation.h"
#include <clang-c/Index.h>
#include <rct/Map.h>
#include <rct/Mutex.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <rct/Thread.h>
#include <rct/WaitCondition.h>

class ClangThread : public Thread
{
public:
    ClangThread();
    virtual void run();

    void stop();
    void index(const SourceInformation &sourceInfo);
    String fixIts(const Path &path) const;
private:
    void diagnose(const SourceInformation &sourceInfo, CXTranslationUnit unit);
    mutable Mutex mMutex;
    WaitCondition mCondition;
    bool mDone;
    Map<Path, String> mFixIts;
    SourceInformation mPending;
};

#endif
