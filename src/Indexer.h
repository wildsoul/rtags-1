#ifndef INDEXER_H
#define INDEXER_H

#include "Project.h"
#include "Location.h"
#include "SourceInformation.h"
#include <rct/EventReceiver.h>
#include <rct/Tr1.h>
#include <rct/Path.h>

class Connection;

class Indexer : public EventReceiver
{
public:
    Indexer(shared_ptr<Project> project) : mProject(project) { }

    virtual Project::Cursor cursor(const Location &location) const = 0;
    virtual void references(const Location& location, unsigned queryFlags,
                            const List<Path> &pathFilter, Connection *conn) const = 0;
    virtual void status(const String &query, Connection *conn, unsigned queryFlags) const = 0;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const = 0;
    virtual void index(const SourceInformation &sourceInformation, Project::Type type) = 0;
    virtual void remove(const Path &sourceFile) = 0;
    virtual void diagnose(const SourceInformation &sourceInformation) { }
    virtual bool isIndexing() const = 0;
    virtual bool restore(Deserializer &/* deserializer */) { return true; }
    virtual bool save(Serializer &/* serializer */) { return true; }
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
    virtual Set<Project::Cursor> findCursors(const String &string, const List<Path> &pathFilter) const = 0;
    virtual Set<Project::Cursor> cursors(const Path &path) const = 0;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn) = 0;
    virtual String fixits(const Path &path) const = 0;
    virtual void dirty(const Set<Path> &files) = 0;

protected:
    shared_ptr<Project> mProject;
};

#endif
