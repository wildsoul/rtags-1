#ifndef Project_h
#define Project_h

#include <rct/Path.h>
#include "RTags.h"
#include "Match.h"
#include <rct/RegExp.h>
#include <rct/EventReceiver.h>
#include <rct/ReadWriteLock.h>
#include <rct/FileSystemWatcher.h>
#include "IndexerJob.h"

class FileManager;
class TimerEvent;
class Database;
class Project : public EventReceiver
{
public:
    Project(const Path &path);
    bool isValid() const;
    void init();
    bool restore();
    bool save();
    void unload();

    shared_ptr<FileManager> fileManager() const { return mFileManager; }
    shared_ptr<Database> database() const { return mDatabase; }

    Path path() const { return mPath; }
    bool match(const Match &match, bool *indexed = 0) const;

    const FilesMap &files() const { return mFiles; }
    FilesMap &files() { return mFiles; }

    bool isIndexed(uint32_t fileId) const;

    void index(const SourceInformation &args, IndexerJob::Type type);
    bool index(const Path &sourceFile, const Path &compiler = Path(), const List<String> &args = List<String>());
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(uint32_t fileId) const;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };
    Set<uint32_t> dependencies(uint32_t fileId, DependencyMode mode) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    void onJobFinished(shared_ptr<IndexerJob> job);
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool isIndexing() const { return !mJobs.isEmpty(); }
    int dirty(const Set<uint32_t> &files);
private:
    void onFileModified(const Path &);
    void addDependencies(const DependencyMap &hash, Set<uint32_t> &newFiles);

    shared_ptr<FileManager> mFileManager;
    shared_ptr<Database> mDatabase;

    const Path mPath;

    FilesMap mFiles;

    int mJobCounter;
    Map<uint32_t, shared_ptr<IndexerJob> > mJobs;
    struct PendingJob
    {
        SourceInformation source;
        IndexerJob::Type type;
    };
    Map<uint32_t, PendingJob> mPendingJobs;

    StopWatch mTimer;

    DependencyMap mDependencies;
    SourceInformationMap mSources;

    FileSystemWatcher mWatcher;
    Set<Path> mWatchedPaths;
};

#endif
