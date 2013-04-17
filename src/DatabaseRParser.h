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

class DatabaseRParser;

class DocumentParser : public QObject
{
    Q_OBJECT
public:
    DocumentParser(QPointer<CppTools::Internal::CppModelManager> mgr,
                   DatabaseRParser* parser,
                   QObject* parent = 0);
    ~DocumentParser();

    QByteArray debugScope(CPlusPlus::Scope* scope, const QByteArray& src);

private slots:
    void onDocumentUpdated(CPlusPlus::Document::Ptr doc);

public:
    int symbolCount;
    QPointer<CppTools::Internal::CppModelManager> manager;
    DatabaseRParser* rparser;
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
    virtual void references(const Location& location, unsigned flags, const List<Path> &pathFilters, Connection *conn) const;
    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const;
    virtual Set<Path> files(int mode) const;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    virtual Set<Cursor> cursors(const Path &path) const;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn);
    virtual bool isIndexing() const;
    virtual void remove(const Path &sourceFile);

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
    Map<QString, QString> headerToSource;
    DocumentParser* parser;
    QPointer<CppTools::Internal::CppModelManager> manager;

    friend class DocumentParser;
    friend class RParserUnit;
    friend class FindSymbols;
};

#endif
