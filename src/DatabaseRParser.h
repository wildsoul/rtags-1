#ifndef DatabaseRParser_h
#define DatabaseRParser_h

#include "Database.h"

class DatabaseRParser : public Database
{
public:
    DatabaseRParser() {}
    virtual ~DatabaseRParser() {}

    virtual Cursor cursor(const Location &location) const;
    virtual void status(const String &query, Connection *conn) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const;
    virtual int index(const SourceInformation &sourceInformation);
    virtual Set<Path> dependencies(const Path &path) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> cursors(const Path &path) const;
};

#endif
