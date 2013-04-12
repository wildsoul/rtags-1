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

typedef Map<Path, Set<String> > FilesMap;
typedef Map<Path, SourceInformation> SourceInformationMap;
typedef Map<Path, Set<Path> > DependencyMap;
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

    bool isIndexed(const Path &path) const;

    void index(const SourceInformation &args, IndexerJob::Type type);
    bool index(const Path &sourceFile, const Path &compiler = Path(), const List<String> &args = List<String>());
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(const Path &path) const;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };
    Set<Path> dependencies(const Path &path, DependencyMode mode) const;
    void setDependencies(const Path &path, const Set<Path> &dependencies);
    int reindex(const Match &match);
    int remove(const Match &match);
    void onJobFinished(shared_ptr<IndexerJob> job);
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool isIndexing() const { return !mJobs.isEmpty(); }
    int dirty(const Set<Path> &files);
    virtual void timerEvent(TimerEvent *e);
private:
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);

    shared_ptr<FileManager> mFileManager;
    shared_ptr<Database> mDatabase;

    const Path mPath;

    FilesMap mFiles;

    int mJobCounter;
    Map<Path, shared_ptr<IndexerJob> > mJobs;
    StopWatch mTimer;

    DependencyMap mDependencies;
    SourceInformationMap mSources;

    FileSystemWatcher mWatcher;
    Set<Path> mWatchedPaths, mModifiedFiles;
    Timer mModifiedFilesTimer;
};

#endif
