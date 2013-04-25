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

    void index(const SourceInformation &args, Type type);
    bool index(const Path &sourceFile, const GccArguments &args);
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(const Path &path) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    SourceInformationMap sources() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    int dirty(const Set<Path> &files);
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
    virtual Cursor cursor(const Location &location) const = 0;
    virtual void references(const Location& location, unsigned queryFlags,
                            const List<Path> &pathFilter, Connection *conn) const = 0;
    virtual void status(const String &query, Connection *conn, unsigned queryFlags) const = 0;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const = 0;
    virtual int index(const SourceInformation &sourceInformation) = 0;
    virtual void remove(const Path &sourceFile) = 0;
    virtual bool isIndexing() const = 0;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn
    };

    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const = 0;
    enum FilesMode {
        SourceFiles = 0x1,
        HeaderFiles = 0x2,
        AllFiles = SourceFiles | HeaderFiles
    };
    virtual Set<Path> files(int mode) const = 0;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const = 0;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const = 0;
    virtual Set<Cursor> cursors(const Path &path) const = 0;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn) = 0;
    virtual String fixits(const Path &path) const = 0;

private:
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);

    shared_ptr<FileManager> mFileManager;

    const Path mPath;

    FilesMap mFilesMap;

    SourceInformationMap mSources;

    FileSystemWatcher mWatcher;
    Set<Path> mWatchedPaths, mModifiedFiles;
    Timer mModifiedFilesTimer, mSaveTimer;
    signalslot::Signal2<const shared_ptr<Project> &, const SourceInformation &> mSourceIndexed;
};

#endif
