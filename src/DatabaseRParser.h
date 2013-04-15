#ifndef DatabaseRParser_h
#define DatabaseRParser_h

#include "Database.h"
#include <cplusplus/AST.h>
#include <cppmodelmanager.h>
#include <FindUsages.h>
#include <LookupContext.h>
#include <QObject>
#include <QPointer>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QQueue>

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
class RParserJob;
class DatabaseRParser : public QThread, public Database
{
public:
    DatabaseRParser();
    virtual ~DatabaseRParser();

    virtual Cursor cursor(const Location &location) const;
    virtual void status(const String &query, Connection *conn) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const;
    virtual int index(const SourceInformation &sourceInformation);
    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> cursors(const Path &path) const;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn);
    virtual bool isIndexing() const;
    virtual void remove(const SourceInformation &sourceInformation);

    enum State { Starting,
                 Indexing,
                 CollectingNames,
                 Idle };

protected:
    void run();

private:
    void changeState(State st);
    enum WaitMode { GreaterOrEqual, Equal };
    void waitForState(WaitMode m, State st) const;

    void processJob(RParserJob* job);
    void collectNames(const Set<Path>& files);
    int symbolCount(const Path& file);

    friend class RParserUnit;

    RParserUnit* findUnit(const Path& path);
    CPlusPlus::Symbol* findSymbol(CPlusPlus::Document::Ptr doc, const Location& srcLoc,
                                  const QByteArray& src, CPlusPlus::LookupContext& ctx,
                                  Location& loc) const;
    void dirty(const Set<Path>& files);

    mutable QMutex mutex;
    mutable QWaitCondition wait;
    QWaitCondition jobsAvailable;
    State state;
    QQueue<RParserJob*> jobs;
    Map<Path, RParserUnit*> units;
    struct RParserName
    {
        Set<Path> paths;
        Set<String> names;
        void merge(const RParserName& other);
    };
    Map<String, RParserName> names;
    DocumentParser* parser;
    QPointer<CppTools::Internal::CppModelManager> manager;

    friend class FindSymbols;
};

#endif
