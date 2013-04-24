#ifndef DatabaseClang_h
#define DatabaseClang_h

#include "Database.h"
#include "UsrMap.h"
#include <clang-c/Index.h>
#include <rct/EventReceiver.h>
#include <rct/Map.h>
#include <rct/Mutex.h>
#include <rct/ThreadPool.h>

class ClangUnit;

typedef Map<uint32_t, Set<Location> > UsrSet;
typedef Map<uint32_t, Set<uint32_t> > DependSet;
typedef Map<uint32_t, Set<uint32_t> > VirtualSet;

struct CursorInfo
{
    uint32_t usr;
    int start, end;
    Database::Cursor::Kind kind;
};

class DatabaseClang : public Database, public EventReceiver
{
public:
    DatabaseClang(const Path &path);
    virtual ~DatabaseClang();

    void save();
    bool load();

    virtual Cursor cursor(const Location &location) const;
    virtual void references(const Location& location, unsigned queryFlags,
                            const List<Path> &pathFilter, Connection *conn) const;
    virtual void status(const String &query, Connection *conn) const;
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

    static LockingUsrMap& usrMap() { return umap; }

private:
    void writeReferences(const uint32_t usr, Connection* conn) const;
    void writeDeclarations(const uint32_t usr, Connection* conn) const;

private:
    Map<Path, ClangUnit*> units;
    ThreadPool pool;
    CXIndex cidx;
    CXIndexAction caction;

    mutable Mutex mutex;
    int pendingJobs;
    Map<Location, uint32_t> incs;
    DependSet depends, reverseDepends;
    Map<String, Set<uint32_t> > names; // name->usr
    Map<Location, CursorInfo> usrs;    // location->usr
    UsrSet decls, defs, refs;          // usr->locations
    VirtualSet virtuals;               // usr->usrs
    static LockingUsrMap umap;

    friend class ClangUnit;
};
#endif
