#ifndef DatabaseClang_h
#define DatabaseClang_h

#include "Database.h"
#include <rct/Map.h>
#include <rct/Mutex.h>
#include <rct/ThreadPool.h>
#include <clang-c/Index.h>

class ClangUnit;

typedef Map<String, Set<Location> > UsrSet;
typedef Map<String, Set<String> > VirtualSet;
typedef Map<Path, Set<Path> > DependSet;

struct CursorInfo
{
    String usr;
    int start, end;
    Database::Cursor::Kind kind;
};

class DatabaseClang : public Database
{
public:
    DatabaseClang();
    virtual ~DatabaseClang();

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

private:
    void writeReferences(const String& usr, Connection* conn) const;
    void writeDeclarations(const String& usr, Connection* conn) const;

private:
    Map<Path, ClangUnit*> units;
    ThreadPool pool;
    CXIndex cidx;
    CXIndexAction caction;

    mutable Mutex mutex;
    int pendingJobs;
    Map<Location, Path> incs;
    DependSet depends, reverseDepends;
    Map<String, String> names;  // name->usr
    Map<Location, CursorInfo> usrs; // location->usr
    UsrSet decls, defs, refs;   // usr->locations
    VirtualSet virtuals; // usr->usrs
    
    friend class ClangUnit;
};
#endif
