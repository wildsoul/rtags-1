#ifndef DatabaseClang_h
#define DatabaseClang_h

#include "Database.h"

class DatabaseClang : public Database
{
public:
    DatabaseClang() {}
    virtual ~DatabaseClang() {}

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
};
#endif
