#ifndef Project_h
#define Project_h

#include "GccArguments.h"
#include "Match.h"
#include "SourceInformation.h"
#include <rct/EventReceiver.h>
#include <rct/FileSystemWatcher.h>
#include <rct/Path.h>

typedef Map<Path, Set<String> > FilesMap;
typedef Map<Path, SourceInformation> SourceInformationMap;
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

    enum Type {
        Index,
        Dirty,
        Dump,
        Restore
    };
    
    void index(const SourceInformation &args, Type type);
    bool index(const Path &sourceFile, const GccArguments &args);
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(const Path &path) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    SourceInformationMap sources() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool isIndexing() const;
    int dirty(const Set<Path> &files);
    virtual void timerEvent(TimerEvent *e);
private:
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);

    shared_ptr<FileManager> mFileManager;
    shared_ptr<Database> mDatabase;

    const Path mPath;

    FilesMap mFiles;

    SourceInformationMap mSources;

    FileSystemWatcher mWatcher;
    Set<Path> mWatchedPaths, mModifiedFiles;
    Timer mModifiedFilesTimer, mSaveTimer;
};

#endif
