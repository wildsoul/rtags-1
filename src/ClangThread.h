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
    bool isIdle() const { MutexLocker lock(&mMutex); error() << "state is" << mState; return mState == Idle; }
private:
    void diagnose(const SourceInformation &sourceInfo, CXTranslationUnit unit, const Set<Path> &files);
    mutable Mutex mMutex;
    WaitCondition mCondition;
    enum State {
        Idle,
        Parsing,
        Done
    } mState;
    SourceInformation mPending;

    struct FixIt
    {
        inline FixIt(uint32_t s = 0, uint32_t e = 0, const String &t = String())
            : start(s), end(e), text(t)
        {
        }
        inline bool operator<(const FixIt &other) const
        {
            return start < other.start;
        }
        inline bool operator==(const FixIt &other) const
        {
            return (start == other.start && end == other.end && text == other.text);
        }

        uint32_t start, end;
        String text;
    };
    Map<Path, Set<FixIt> > mFixIts;
};

#endif
