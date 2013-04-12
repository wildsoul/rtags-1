#include "DatabaseRParser.h"

Database::Cursor DatabaseRParser::cursor(const Location &location) const
{
    return Cursor();
}

void DatabaseRParser::status(const String &query, Connection *conn) const
{
}

void DatabaseRParser::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
}

int DatabaseRParser::index(const SourceInformation &sourceInformation)
{
    return -1;
}

Set<Path> DatabaseRParser::dependencies(const Path &path) const
{
    return Set<Path>();
}

Set<String> DatabaseRParser::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    return Set<String>();
}

Set<Database::Cursor> DatabaseRParser::findCursors(const String &string, const List<Path> &pathFilter) const
{
    return Set<Cursor>();
}

Set<Database::Cursor> DatabaseRParser::cursors(const Path &path) const
{
    return Set<Cursor>();
}
