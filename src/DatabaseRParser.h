#ifndef DatabaseRParser_h
#define DatabaseRParser_h

#include "Database.h"
#include <cplusplus/AST.h>
#include <cppmodelmanager.h>
#include <FindUsages.h>
#include <QObject>
#include <QPointer>

class DocumentParser : public QObject
{
    Q_OBJECT
public:
    DocumentParser(QPointer<CppTools::Internal::CppModelManager> mgr, QObject* parent = 0);
    ~DocumentParser();

    QByteArray tokenForAst(CPlusPlus::AST* ast, CPlusPlus::TranslationUnit* unit, const QByteArray& src);
    QByteArray debugScope(CPlusPlus::Scope* scope, const QByteArray& src);
    QList<CPlusPlus::Usage> findUsages(CPlusPlus::Symbol* symbol, const QByteArray& unpreprocessedSource);

private slots:
    void onDocumentUpdated(CPlusPlus::Document::Ptr doc);

public:
    int symbolCount;
    QPointer<CppTools::Internal::CppModelManager> manager;
    QSet<QString> seen;
};

class DocumentParser;
class RParserUnit;
class DatabaseRParser : public Database
{
public:
    DatabaseRParser();
    virtual ~DatabaseRParser();

    virtual Cursor cursor(const Location &location) const;
    virtual void status(const String &query, Connection *conn) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const;
    virtual int index(const SourceInformation &sourceInformation);
    virtual Set<Path> dependencies(const Path &path) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> cursors(const Path &path) const;

private:
    friend class RParserUnit;

    RParserUnit* findUnit(const Path& path);
    CPlusPlus::Symbol* findSymbol(CPlusPlus::Document::Ptr doc, const Location& loc,
                                  const QByteArray& src) const;

    Map<Path, RParserUnit*> units;
    DocumentParser* parser;
    QPointer<CppTools::Internal::CppModelManager> manager;
};

#endif
