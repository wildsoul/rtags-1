#ifndef Project_h
#define Project_h

#include "GccArguments.h"
#include "Match.h"
#include "SourceInformation.h"
#include <rct/EventReceiver.h>
#include <rct/FileSystemWatcher.h>
#include <rct/SignalSlot.h>
#include <rct/Path.h>

typedef Map<Path, Set<String> > FilesMap;
typedef Map<Path, SourceInformation> SourceInformationMap;
class FileManager;
class TimerEvent;
class Connection;
class Indexer;
class Project : public EventReceiver
{
public:
    Project(const Path &path);
    bool isValid() const;
    void init();
    bool restore();
    bool save();
    void startSaveTimer();
    void unload();
    void activate();

    shared_ptr<FileManager> fileManager() const { return mFileManager; }

    Path path() const { return mPath; }
    bool match(const Match &match, bool *indexed = 0) const;

    const FilesMap &filesMap() const { return mFilesMap; }
    FilesMap &filesMap() { return mFilesMap; }

    bool isIndexed(const Path &path) const;

    enum Type {
        Index,
        Dirty,
        Dump,
        Restore
    };

    void setIndexer(shared_ptr<Indexer> indexer) { mIndexer = indexer; }
    shared_ptr<Indexer> indexer() const { return mIndexer; }

    void indexFile(const SourceInformation &args, Type type);
    bool indexFile(const Path &sourceFile, const GccArguments &args);
    SourceInformationMap sourceInfos() const;
    void setSourceInfos(const SourceInformationMap &map);
    SourceInformation sourceInfo(const Path &path) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    SourceInformationMap sources() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    int dirtyFiles(const Set<Path> &files);
    virtual void timerEvent(TimerEvent *e);
    signalslot::Signal2<const shared_ptr<Project> &, const SourceInformation &> &sourceIndexed() { return mSourceIndexed; }

    typedef Set<Location> References;

    class Cursor
    {
    public:
        Cursor() : kind(Invalid), start(-1), end(-1) {}
        enum Kind {
            Invalid,
            File,
            MemberFunctionDefinition,
            MemberFunctionDeclaration,
            MethodDefinition,
            MethodDeclaration,
            Class,
            ClassForwardDeclaration,
            Namespace,
            Struct,
            StructForwardDeclaration,
            Variable,
            Argument,
            Field,
            Enum,
            EnumValue,
            Union,
            Macro,
            Reference
        };

        static char kindToChar(Kind kind);
        static const char *kindToString(Kind kind);
        bool isDefinition() const;
        bool isValid() const { return kind != Invalid; }
        bool isInvalid() const { return kind == Invalid; }
        bool isNull() const { return kind == Invalid; }
        bool isEmpty() const { return kind == Invalid; }

        Location location;
        String symbolName;
        Location target;
        Kind kind;
        int start, end;

        bool operator==(const Cursor &other) const { return !compare(other); }
        bool operator<(const Cursor &other) const { return compare(other) < 0; }
        bool operator>(const Cursor &other) const { return !compare(other) > 0; }
        int compare(const Cursor &other) const
        {
            if (isDefinition() != other.isDefinition())
                return -1;
            return location.compare(other.location);
        }

    };
    String toString(const Cursor &cursor, unsigned flags) const;

private:
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);
    void onFileAdded(const Path &path);

    shared_ptr<FileManager> mFileManager;
    shared_ptr<Indexer> mIndexer;

    const Path mPath;

    FilesMap mFilesMap;

    SourceInformationMap mSources;

    FileSystemWatcher mWatcher;
    Set<Path> mWatchedPaths, mModifiedFiles, mFilesToRemove;
    Timer mModifiedFilesTimer, mSaveTimer, mRemoveTimer;
    signalslot::Signal2<const shared_ptr<Project> &, const SourceInformation &> mSourceIndexed;
};

#endif
