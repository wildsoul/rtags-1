#ifndef ProjectClang_h
#define ProjectClang_h

#include "Project.h"
#include "UsrMap.h"
#include <clang-c/Index.h>
#include <rct/EventReceiver.h>
#include <rct/Map.h>
#include <rct/Mutex.h>
#include <rct/String.h>
#include <rct/ThreadPool.h>

class ClangUnit;

typedef Map<uint32_t, Set<Location> > UsrSet;
typedef Map<uint32_t, Set<uint32_t> > DependSet;
typedef Map<uint32_t, Set<uint32_t> > VirtualSet;

struct CursorInfo
{
    uint32_t usr;
    int start, end;
    Project::Cursor::Kind kind;

    int length() const { return end - start; }
};

template <> inline Serializer &operator<<(Serializer &s, const CursorInfo &b)
{
    s << b.usr << b.start << static_cast<uint32_t>(b.kind);
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, CursorInfo &b)
{
    uint32_t kind;
    s >> b.usr >> b.start >> b.end >> kind;
    b.kind = static_cast<Project::Cursor::Kind>(kind);
    return s;
}

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

class ClangProject : public Project
{
public:
    ClangProject(const Path &path);
    virtual ~ClangProject();

    bool save();
    bool load();

    virtual Cursor cursor(const Location &location) const;
    virtual void references(const Location& location, unsigned queryFlags,
                            const List<Path> &pathFilter, Connection *conn) const;
    virtual void status(const String &query, Connection *conn, unsigned queryFlags) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const;
    virtual int index(const SourceInformation &sourceInformation);
    virtual void remove(const Path &sourceFile);
    virtual bool isIndexing() const;

    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const;

    virtual Set<Path> files(int mode = AllFiles) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> cursors(const Path &path) const;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn);
    virtual String fixits(const Path &path) const;

    static LockingUsrMap& usrMap() { return umap; }

private:
    char locationType(const Location& location) const;
    void writeReferences(const uint32_t usr, const Set<uint32_t>& pathSet, Connection* conn, unsigned keyFlags) const;
    void writeDeclarations(const uint32_t usr, const Set<uint32_t>& pathSet, Connection* conn, unsigned keyFlags) const;

private:
    Map<uint32_t, ClangUnit*> units;
    ThreadPool *pool;
    CXIndex cidx;
    CXIndexAction caction;

    mutable Mutex mutex;
    int pendingJobs, jobsProcessed;
    StopWatch timer;
    Map<Location, uint32_t> incs;
    DependSet depends, reverseDepends;
    Map<String, Set<uint32_t> > names; // name->usr
    Map<Location, CursorInfo> usrs;    // location->usr
    UsrSet decls, defs, refs;          // usr->locations
    VirtualSet virtuals;               // usr->usrs
    Map<Path, Set<FixIt> > fixIts;

    static LockingUsrMap umap;

    friend class ClangUnit;
    friend class ClangParseJob;
};
#endif
