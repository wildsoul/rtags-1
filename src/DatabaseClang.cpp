#include "DatabaseClang.h"
#include "RTagsPlugin.h"
#include <clang-c/Index.h>

Database::Cursor DatabaseClang::cursor(const Location &location) const
{
}

void DatabaseClang::references(const Location& location, unsigned queryFlags,
                               const List<Path> &pathFilter, Connection *conn) const
{
}

void DatabaseClang::status(const String &query, Connection *conn) const
{
}

void DatabaseClang::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
}

int DatabaseClang::index(const SourceInformation &sourceInformation)
{
}

void DatabaseClang::remove(const Path &sourceFile)
{
}

bool DatabaseClang::isIndexing() const
{
}

Set<Path> DatabaseClang::dependencies(const Path &path, DependencyMode mode) const
{
}

Set<Path> DatabaseClang::files(int mode) const
{
}

Set<String> DatabaseClang::listSymbols(const String &string, const List<Path> &pathFilter) const
{
}

Set<Database::Cursor> DatabaseClang::findCursors(const String &string, const List<Path> &pathFilter) const
{
}

Set<Database::Cursor> DatabaseClang::cursors(const Path &path) const
{
}

bool DatabaseClang::codeCompleteAt(const Location &location, const String &source, Connection *conn)
{
}

class DatabaseClangPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<Database> createDatabase()
    {
        return shared_ptr<Database>(new DatabaseClang);
    }
};

extern "C" RTagsPlugin* createInstance()
{
    return new DatabaseClangPlugin;
}
