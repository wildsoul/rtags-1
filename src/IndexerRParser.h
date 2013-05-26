#ifndef INDEXERRPARSER_H
#define INDEXERRPARSER_H

#include "Indexer.h"
#include "Project.h"
#include <cplusplus/AST.h>
#include <cppmodelmanager.h>
#include <FindUsages.h>
#include <rct/Path.h>
#include <LookupContext.h>
#include <QApplication>
#include <QObject>
#include <QPointer>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QQueue>

class RParserThread;

class DocumentParser : public QObject
{
    Q_OBJECT
public:
    DocumentParser(QPointer<CppTools::Internal::CppModelManager> mgr,
                   RParserThread* parser,
                   QObject* parent = 0);
    ~DocumentParser();

    QByteArray debugScope(CPlusPlus::Scope* scope, const QByteArray& src);

private slots:
    void onDocumentUpdated(CPlusPlus::Document::Ptr doc);

public:
    int symbolCount;
    QPointer<CppTools::Internal::CppModelManager> manager;
    RParserThread* rparser;
};

class IndexerRParser : public QThread, public Indexer
{
public:
    IndexerRParser(shared_ptr<Project> project);
    virtual ~IndexerRParser();

    virtual bool save(Serializer &serializer);
    virtual bool restore(Deserializer &deserializer);

    virtual Project::Cursor cursor(const Location &location) const;
    virtual void status(const String &query, Connection *conn, unsigned queryFlags) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const;
    virtual void index(const SourceInformation &sourceInformation, Project::Type type);
    virtual void references(const Location& location, unsigned flags, const List<Path> &pathFilters, Connection *conn) const;
    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const;
    virtual Set<Path> files(int mode) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Project::Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Project::Cursor> cursors(const Path &path) const;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn);
    virtual String fixits(const Path &path) const;
    virtual bool isIndexing() const;
    virtual void remove(const Path &sourceFile);
    virtual void dirty(const Set<Path>& files);
    virtual void activate();
};

#endif
