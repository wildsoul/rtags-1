#include "DatabaseRParser.h"
#include "SourceInformation.h"
#include "RTagsPlugin.h"
#include <rct/Log.h>
#include <QMutexLocker>

#include <searchsymbols.h>
#include <ASTPath.h>
#include <DependencyTable.h>
#include <cplusplus/SymbolVisitor.h>

using namespace CppTools;
using namespace CppTools::Internal;

static inline String fromQString(const QString& str)
{
    const QByteArray& utf8 = str.toUtf8();
    return String(utf8.constData(), utf8.size());
}

static inline String symbolName(const CPlusPlus::Symbol* symbol)
{
    static CPlusPlus::Overview overview;

    String symbolName = fromQString(overview.prettyName(symbol->name()));
    if (symbolName.isEmpty()) {
        String type;
        if (symbol->isNamespace()) {
            type = "namespace";
        } else if (symbol->isEnum()) {
            type = "enum";
        } else if (const CPlusPlus::Class *c = symbol->asClass())  {
            if (c->isUnion())
                type = "union";
            else if (c->isStruct())
                type = "struct";
            else
                type = "class";
        } else {
            type = "symbol";
        }
        symbolName = "<anonymous ";
        symbolName += type;
        symbolName += '>';
    }
    return symbolName;
}

class ReallyFindScopeAt: protected CPlusPlus::SymbolVisitor
{
    CPlusPlus::TranslationUnit *_unit;
    unsigned _line;
    unsigned _column;
    CPlusPlus::Scope *_scope;
    unsigned _foundStart;
    unsigned _foundEnd;

public:
    /** line and column should be 1-based */
    ReallyFindScopeAt(CPlusPlus::TranslationUnit *unit, unsigned line, unsigned column)
        : _unit(unit), _line(line), _column(column), _scope(0),
          _foundStart(0), _foundEnd(0)
    {
    }

    CPlusPlus::Scope *operator()(CPlusPlus::Symbol *symbol)
    {
        accept(symbol);
        return _scope;
    }

protected:
    bool process(CPlusPlus::Scope *symbol)
    {
        CPlusPlus::Scope *scope = symbol;

        for (unsigned i = 0; i < scope->memberCount(); ++i) {
            accept(scope->memberAt(i));
        }

        unsigned startLine, startColumn;
        _unit->getPosition(scope->startOffset(), &startLine, &startColumn);

        if (_line > startLine || (_line == startLine && _column >= startColumn)) {
            unsigned endLine, endColumn;
            _unit->getPosition(scope->endOffset(), &endLine, &endColumn);

            if (_line < endLine || (_line == endLine && _column < endColumn)) {
                if (!_scope || (scope->startOffset() >= _foundStart &&
                                scope->endOffset() <= _foundEnd)) {
                    _foundStart = scope->startOffset();
                    _foundEnd = scope->endOffset();
                    _scope = scope;
                }
            }
        }

        return false;
    }

    using CPlusPlus::SymbolVisitor::visit;

    virtual bool visit(CPlusPlus::UsingNamespaceDirective *) { return false; }
    virtual bool visit(CPlusPlus::UsingDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::NamespaceAlias *) { return false; }
    virtual bool visit(CPlusPlus::Declaration *) { return false; }
    virtual bool visit(CPlusPlus::Argument *) { return false; }
    virtual bool visit(CPlusPlus::TypenameArgument *) { return false; }
    virtual bool visit(CPlusPlus::BaseClass *) { return false; }
    virtual bool visit(CPlusPlus::ForwardClassDeclaration *) { return false; }

    virtual bool visit(CPlusPlus::Enum *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::Function *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::Namespace *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::Class *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::Block *symbol)
    { return process(symbol); }

    // Objective-C
    virtual bool visit(CPlusPlus::ObjCBaseClass *) { return false; }
    virtual bool visit(CPlusPlus::ObjCBaseProtocol *) { return false; }
    virtual bool visit(CPlusPlus::ObjCForwardClassDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::ObjCForwardProtocolDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::ObjCPropertyDeclaration *) { return false; }

    virtual bool visit(CPlusPlus::ObjCClass *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::ObjCProtocol *symbol)
    { return process(symbol); }

    virtual bool visit(CPlusPlus::ObjCMethod *symbol)
    { return process(symbol); }
};

class FindSymbols : public CPlusPlus::SymbolVisitor
{
public:
    enum Mode { Cursors, ListSymbols };

    FindSymbols(Mode m) : mode(m) { }

    bool preVisit(CPlusPlus::Symbol* symbol);

    void operator()(CPlusPlus::Symbol* symbol);

    Set<CPlusPlus::Symbol*> symbols() { return syms; }
    Map<String, DatabaseRParser::RParserName> symbolNames() { return names; };

private:
    Mode mode;
    Set<CPlusPlus::Symbol*> syms;
    Map<String, DatabaseRParser::RParserName> names;
};

void DatabaseRParser::RParserName::merge(const RParserName& other)
{
    paths += other.paths;
    names += other.names;
}

bool FindSymbols::preVisit(CPlusPlus::Symbol* symbol)
{
    if (mode == Cursors) {
        syms.insert(symbol);
    } else {
        static CPlusPlus::Overview overview;

        DatabaseRParser::RParserName cur;
        cur.paths.insert(Path(symbol->fileName()));

        QVector<int> seps;
        String symbolName;
        QList<const CPlusPlus::Name*> fullName = CPlusPlus::LookupContext::fullyQualifiedName(symbol);
        foreach(const CPlusPlus::Name* name, fullName) {
            if (!symbolName.isEmpty()) {
                symbolName.append("::");
                seps.append(symbolName.size());
            }
            symbolName.append(fromQString(overview.prettyName(name)));
        }
        if (symbolName.isEmpty())
            return true;

        cur.names.insert(symbolName);
        names[symbolName].merge(cur);

        foreach(int s, seps) {
            const String& sub = symbolName.mid(s);
            cur.names.insert(sub);
            names[sub].merge(cur);
        }
    }
    return true;
}

void FindSymbols::operator()(CPlusPlus::Symbol* symbol)
{
    syms.clear();
    names.clear();
    accept(symbol);
}

static inline bool nameMatch(CPlusPlus::Symbol* symbol, const String& name, const CPlusPlus::Overview& overview)
{
    String current;
    QList<const CPlusPlus::Name*> fullName = CPlusPlus::LookupContext::fullyQualifiedName(symbol);
    while (!fullName.isEmpty()) {
        const CPlusPlus::Name* n = fullName.takeLast();
        if (!current.isEmpty())
            current.prepend("::");
        current.prepend(fromQString(overview.prettyName(n)));
        if (current.startsWith(name))
            return true;
    }
    return false;
}

static inline CPlusPlus::Symbol *canonicalSymbol(CPlusPlus::Scope *scope, const QString &code,
                                                 CPlusPlus::TypeOfExpression &typeOfExpression)
{
    const QList<CPlusPlus::LookupItem> results =
        typeOfExpression(code.toUtf8(), scope, CPlusPlus::TypeOfExpression::Preprocess);

    for (int i = results.size() - 1; i != -1; --i) {
        const CPlusPlus::LookupItem &r = results.at(i);
        CPlusPlus::Symbol *decl = r.declaration();

        if (! (decl && decl->enclosingScope()))
            break;

        if (CPlusPlus::Class *classScope = r.declaration()->enclosingScope()->asClass()) {
            const CPlusPlus::Identifier *declId = decl->identifier();
            const CPlusPlus::Identifier *classId = classScope->identifier();

            if (classId && classId->isEqualTo(declId))
                continue; // skip it, it's a ctor or a dtor.

            else if (CPlusPlus::Function *funTy = r.declaration()->type()->asFunctionType()) {
                if (funTy->isVirtual())
                    return r.declaration();
            }
        }
    }

    for (int i = 0; i < results.size(); ++i) {
        const CPlusPlus::LookupItem &r = results.at(i);

        if (r.declaration())
            return r.declaration();
    }

    return 0;
}

DocumentParser::DocumentParser(QPointer<CppModelManager> mgr, QObject* parent)
    : QObject(parent), symbolCount(0), manager(mgr)
{
}

DocumentParser::~DocumentParser()
{
    const CPlusPlus::Snapshot& snapshot = manager->snapshot();
    CPlusPlus::Snapshot::iterator it = snapshot.begin();
    const CPlusPlus::Snapshot::const_iterator end = snapshot.end();
    while (it != end) {
        it.value()->releaseSourceAndAST();
        ++it;
    }
}

QByteArray DocumentParser::debugScope(CPlusPlus::Scope* scope, const QByteArray& src)
{
    return src.mid(scope->startOffset(), scope->endOffset() - scope->startOffset());
}

static inline QList<CPlusPlus::Usage> findUsages(QPointer<CppModelManager> manager,
                                                 CPlusPlus::Symbol* symbol,
                                                 const QByteArray& unpreprocessedSource)
{
    const CPlusPlus::Identifier *symbolId = symbol->identifier();
    if (!symbolId) {
        error("no symbol id in findUsages");
        return QList<CPlusPlus::Usage>();
    }

    const CPlusPlus::Snapshot& snapshot = manager->snapshot();

    QList<CPlusPlus::Usage> usages;

    // ### Use QFuture for this?

    CPlusPlus::Snapshot::const_iterator snap = snapshot.begin();
    const CPlusPlus::Snapshot::const_iterator end = snapshot.end();
    while (snap != end) {
        CPlusPlus::Document::Ptr doc = snap.value();
        const CPlusPlus::Control* control = doc->control();
        if (control->findIdentifier(symbolId->chars(), symbolId->size())) {
            CPlusPlus::LookupContext lookup(doc, snapshot);
            CPlusPlus::FindUsages find(lookup);
            find(symbol);
            usages.append(find.usages());
        }
        ++snap;
    }

    return usages;
}

void DocumentParser::onDocumentUpdated(CPlusPlus::Document::Ptr doc)
{
    // seems I need to keep this around
    doc->keepSourceAndAST();

    const QFileInfo info(doc->fileName());
    const QString canonical = info.canonicalFilePath();

    CPlusPlus::TranslationUnit *translationUnit = doc->translationUnit();
    CPlusPlus::Parser parser(translationUnit);
    CPlusPlus::Namespace *globalNamespace = doc->globalNamespace();
    CPlusPlus::Bind bind(translationUnit);
    if (!translationUnit->ast())
        return; // nothing to do.

    if (CPlusPlus::TranslationUnitAST *ast = translationUnit->ast()->asTranslationUnit())
        bind(ast, globalNamespace);
    else if (CPlusPlus::StatementAST *ast = translationUnit->ast()->asStatement())
        bind(ast, globalNamespace);
    else if (CPlusPlus::ExpressionAST *ast = translationUnit->ast()->asExpression())
        bind(ast, globalNamespace);
    else if (CPlusPlus::DeclarationAST *ast = translationUnit->ast()->asDeclaration())
        bind(ast, globalNamespace);
    //error("bound %s with mgr %p", qPrintable(doc->fileName()), manager.data());
}

class RParserUnit
{
public:
    SourceInformation info;

    void reindex(QPointer<CppModelManager> manager);
};

template<typename T>
static inline QStringList toQStringList(const T& t)
{
    QStringList list;
    typename T::const_iterator it = t.begin();
    const typename T::const_iterator end = t.end();
    while (it != end) {
        list << QString::fromStdString(*it);
        ++it;
    }
    return list;
}

void RParserUnit::reindex(QPointer<CppModelManager> manager)
{
    CppPreprocessor preprocessor(manager);
    const QString srcPath = QString::fromStdString(info.sourceFile.parentDir());
    static QStringList incs = QStringList() << QLatin1String("/usr/include") << QLatin1String("/usr/include/c++/4.6") << srcPath;
    List<SourceInformation::Build>::const_iterator build = info.builds.begin();
    const List<SourceInformation::Build>::const_iterator end = info.builds.end();
    while (build != end) {
        //error() << "reindexing" << info.sourceFile << build->includePaths << build->defines;
        preprocessor.setIncludePaths(toQStringList(build->includePaths) + incs);
        preprocessor.addDefinitions(toQStringList(build->defines));
        preprocessor.run(QString::fromStdString(info.sourceFile));
        preprocessor.resetEnvironment();
        ++build;
    }
}

RParserUnit* DatabaseRParser::findUnit(const Path& path)
{
    Map<Path, RParserUnit*>::const_iterator unit = units.find(path);
    if (unit == units.end())
        return 0;
    return unit->second;
}

static inline Database::Cursor::Kind symbolKind(const CPlusPlus::Symbol* sym)
{
    if (sym->asEnum()) {
        //error("enum");
        return Database::Cursor::Enum;
    } else if (sym->asFunction()) {
        //error("function");
        return Database::Cursor::MemberFunctionDeclaration;
    } else if (sym->asNamespace()) {
        //error("namespace");
        return Database::Cursor::Namespace;
    } else if (sym->asTemplate()) {
        //error("template");
    } else if (sym->asNamespaceAlias()) {
        //error("namespaceAlias");
    } else if (sym->asForwardClassDeclaration()) {
        //error("forward class");
        return Database::Cursor::Class;
    } else if (sym->asClass()) {
        //error("class");
        return Database::Cursor::Class;
    } else if (sym->asUsingNamespaceDirective()) {
        //error("using 1");
    } else if (sym->asUsingDeclaration()) {
        //error("using 2");
    } else if (sym->asDeclaration()) {
        //error("decl");
        return Database::Cursor::Variable; // ### ???
    } else if (sym->asArgument()) {
        //error("arg");
        return Database::Cursor::Variable;
    } else if (sym->asTypenameArgument()) {
        //error("typename");
    } else if (sym->asBaseClass()) {
        //error("baseclass");
    } else if (sym->asQtPropertyDeclaration()) {
    } else if (sym->asQtEnum()){
    } else if (sym->asObjCBaseClass()) {
    } else if (sym->asObjCBaseProtocol()) {
    } else if (sym->asObjCClass()) {
    } else if (sym->asObjCForwardClassDeclaration()) {
    } else if (sym->asObjCProtocol()) {
    } else if (sym->asObjCForwardProtocolDeclaration()) {
    } else if (sym->asObjCMethod()) {
    } else if (sym->asObjCPropertyDeclaration()) {
    }
    return Database::Cursor::Invalid;
}

static inline Location makeLocation(CPlusPlus::Symbol* sym)
{
    return Location(sym->fileName(), sym->line(), sym->column());
}

static inline Database::Cursor makeCursor(const CPlusPlus::Symbol* sym,
                                          const CPlusPlus::TranslationUnit* unit)
{
    Database::Cursor cursor;
    cursor.location = Location(sym->fileName(), sym->line(), sym->column());
    const CPlusPlus::Token& token = unit->tokenAt(sym->sourceLocation());
    cursor.start = token.begin();
    cursor.end = token.end();
    cursor.kind = symbolKind(sym);
    cursor.symbolName = symbolName(sym);
    return cursor;
}

CPlusPlus::Symbol* DatabaseRParser::findSymbol(CPlusPlus::Document::Ptr doc,
                                               const Location& srcLoc,
                                               const QByteArray& src,
                                               CPlusPlus::LookupContext& lookup,
                                               Location& loc) const
{
    const unsigned line = srcLoc.line();
    const unsigned column = srcLoc.column();

    // First, try to find the symbol outright:
    CPlusPlus::Symbol* sym = 0;
    {
        CPlusPlus::Symbol* candidate = doc->lastVisibleSymbolAt(line, column);
        if (candidate) {
            const CPlusPlus::Identifier* id = candidate->identifier();
            if (id) {
                // ### fryktelig
                if (candidate->line() == line && candidate->column() <= column &&
                    candidate->column() + id->size() >= column) {
                    // yes
                    sym = candidate;
                    loc = makeLocation(sym);
                    error("found outright");
                }
            }
        }
        // no
    }

    if (!sym) {
        // See if we can parse it:
        CPlusPlus::TypeOfExpression typeofExpression;
        typeofExpression.init(doc, manager->snapshot(), lookup.bindings());
        typeofExpression.setExpandTemplates(true);

        CPlusPlus::TranslationUnit* unit = doc->translationUnit();
        ReallyFindScopeAt really(unit, line, column);
        CPlusPlus::Scope* scope = really(doc->globalNamespace());
        if (!scope)
            scope = doc->globalNamespace();

        CPlusPlus::ASTPath path(doc);
        QList<CPlusPlus::AST*> asts = path(line, column);
        while (!asts.isEmpty()) {
            CPlusPlus::AST* ast = asts.takeLast();

            const CPlusPlus::Token& start = unit->tokenAt(ast->firstToken());
            const CPlusPlus::Token& last = unit->tokenAt(ast->lastToken() - 1);
            const QByteArray expression = src.mid(start.begin(), last.end() - start.begin());

            error("trying expr '%.20s' in scope %p", qPrintable(expression), scope);

            sym = canonicalSymbol(scope, expression, typeofExpression);
            if (sym) {
                unsigned startLine, startColumn;
                //unsigned endLine, endColumn;
                const CPlusPlus::StringLiteral* file;

                unit->getTokenStartPosition(ast->firstToken(), &startLine, &startColumn, &file);
                //unit->getTokenEndPosition(ast->lastToken() - 1, &endLine, &endColumn, 0);
                loc = Location(file->chars(), startLine, startColumn);

                static int faen = 0;
                if (++faen >= 10)
                    abort();
                error("got it!");
                break;
            }
        }
    }

    return sym;
}

Database::Cursor DatabaseRParser::cursor(const Location &location) const
{
    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(location.path()));
    if (!doc)
        return Cursor();
    const QByteArray& src = doc->utf8Source();

    Cursor cursor;

    CPlusPlus::LookupContext lookup(doc, manager->snapshot());
    CPlusPlus::Symbol* sym = findSymbol(doc, location, src, lookup, cursor.location);
    if (!sym) {
        error() << "no symbol whatsoever for" << location;
        return Cursor();
    }

    cursor.target = makeLocation(sym); // will be overridden if we find a symbol in the usages
    if (cursor.location == cursor.target) {
        // declaration
        cursor.kind = symbolKind(sym);
    } else {
        // possible reference
        cursor.kind = Cursor::Reference;
    }

    cursor.symbolName = symbolName(sym);

    bool added = false;
    const QList<CPlusPlus::Usage> usages = findUsages(manager, sym, src);
    foreach(const CPlusPlus::Usage& usage, usages) {
        added = false;
        CPlusPlus::Document::Ptr doc = manager->document(usage.path);
        if (doc) {
            CPlusPlus::Symbol* refsym = doc->lastVisibleSymbolAt(usage.line, usage.col + 1);
            if (refsym) {
                if (refsym->line() == static_cast<unsigned>(usage.line) &&
                    refsym->column() == static_cast<unsigned>(usage.col + 1)) {
                    // this is a symbol, definition?
                    // replace our target if we're a reference
                    if (cursor.kind == Cursor::Reference) {
                        const Location defloc = makeLocation(refsym);
                        // if we assumed we were the reference but we really are the
                        // definition then change our kind
                        if (cursor.target == defloc)
                            cursor.kind = symbolKind(refsym);
                        cursor.target = defloc;
                        added = true;
                    }
                }
            }
        }
        if (!added) {
            //error() << "adding ref" << fromQString(usage.path) << usage.line << usage.col;
            cursor.references.insert(Location(fromQString(usage.path),
                                              usage.line, usage.col + 1));
        }
    }

    error() << "got a symbol, tried" << location << "ended up with target" << cursor.target;
    return cursor;
}

DatabaseRParser::DatabaseRParser()
    : state(Starting), parser(0)
{
    start();
    moveToThread(this);
}

DatabaseRParser::~DatabaseRParser()
{
    Map<Path, RParserUnit*>::const_iterator unit = units.begin();
    const Map<Path, RParserUnit*>::const_iterator end = units.end();
    while (unit != end) {
        delete unit->second;
        ++unit;
    }
    delete parser;
}

void DatabaseRParser::run()
{
    manager = new CppModelManager;
    parser = new DocumentParser(manager);
    QObject::connect(manager.data(), SIGNAL(documentUpdated(CPlusPlus::Document::Ptr)),
                     parser, SLOT(onDocumentUpdated(CPlusPlus::Document::Ptr)));

    QMutexLocker locker(&mutex);
    if (state == Starting)
        changeState(Idle);
    locker.unlock();

    for (;;) {
        locker.relock();
        while (jobs.isEmpty()) {
            assert(state == Idle);
            waitForState(Equal, Processing);
        }

        assert(!jobs.isEmpty());
        changeState(Indexing);
        while (!jobs.isEmpty()) {
            RParserJob* job = jobs.dequeue();
            locker.unlock();
            processJob(job);
            locker.relock();
        }

        changeState(CollectingNames);
        locker.unlock();
        collectNames();
        locker.relock();

        changeState(Idle);
        locker.unlock();
    }
}

static inline const char* stateName(DatabaseRParser::State st)
{
    struct states {
        DatabaseRParser::State state;
        const char* name;
    } static s[] = { { DatabaseRParser::Starting, "starting" },
                     { DatabaseRParser::Processing, "processing" },
                     { DatabaseRParser::Indexing, "indexing" },
                     { DatabaseRParser::CollectingNames, "collectingnames" },
                     { DatabaseRParser::Idle, "idle" } };
    for (unsigned int i = 0; i < sizeof(s); ++i) {
        if (s[i].state == st)
            return s[i].name;
    }
    return 0;
}

// needs to be called with mutex locked
void DatabaseRParser::changeState(State st)
{
    error() << "rparser thread state changed from " << stateName(state) << " to " << stateName(st);
    state = st;
    wait.wakeAll();
}

// needs to be called with mutex locked
void DatabaseRParser::waitForState(WaitMode m, State st) const
{
    for (;;) {
        if (m == GreaterOrEqual && state >= st)
            break;
        if (m == Equal && state == st)
            break;
        wait.wait(&mutex);
    }
}

void DatabaseRParser::status(const String &query, Connection *conn) const
{
}

void DatabaseRParser::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
}

class RParserJob
{
public:
    RParserJob(const SourceInformation& i)
        : info(i)
    {
    }

    SourceInformation info;
};

void DatabaseRParser::processJob(RParserJob* job)
{
    const Path& fileName = job->info.sourceFile;
    RParserUnit* unit = findUnit(fileName);
    if (!unit) {
        unit = new RParserUnit;
        unit->info = job->info;
        units[fileName] = unit;
    }
    unit->reindex(manager);
}

void DatabaseRParser::collectNames()
{
    const CPlusPlus::Snapshot& snapshot = manager->snapshot();

    names.clear();

    CPlusPlus::Snapshot::const_iterator snap = snapshot.begin();
    const CPlusPlus::Snapshot::const_iterator end = snapshot.end();
    while (snap != end) {
        CPlusPlus::Document::Ptr doc = snap.value();

        CPlusPlus::Namespace* globalNamespace = doc->globalNamespace();
        if (globalNamespace) {
            FindSymbols find(FindSymbols::ListSymbols);
            find(globalNamespace);
            names += find.symbolNames();
        }
        ++snap;
    }
}

// needs to be called with mutex locked
void DatabaseRParser::startJob(RParserJob* job)
{
    jobs.enqueue(job);
    if (state == Idle || state == Starting) {
        changeState(Processing);
    }
}

int DatabaseRParser::index(const SourceInformation &sourceInformation)
{
    QMutexLocker locker(&mutex);
    startJob(new RParserJob(sourceInformation));
    waitForState(GreaterOrEqual, CollectingNames);

    return -1;
}

Set<Path> DatabaseRParser::dependencies(const Path &path, DependencyMode mode) const
{
    Set<Path> result;

    QMutexLocker locker(&mutex);
    waitForState(GreaterOrEqual, CollectingNames);

    // ### perhaps keep this around
    CPlusPlus::DependencyTable table;
    table.build(manager->snapshot());

    const QStringList deps = table.filesDependingOn(QString::fromStdString(path));
    foreach(const QString dep, deps) {
        result.insert(fromQString(dep));
    }

    return result;
}

Set<String> DatabaseRParser::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    QMutexLocker locker(&mutex);
    waitForState(GreaterOrEqual, Idle);

    Set<String> ret;
    Set<Path> paths = pathFilter.toSet();
    const bool pass = paths.isEmpty();

    Map<String, RParserName>::const_iterator name = names.lower_bound(string);
    const Map<String, RParserName>::const_iterator end = names.end();
    while (name != end && name->first.startsWith(string)) {
        if (pass || paths.intersects(name->second.paths))
            ret += name->second.names;
        ++name;
    }
    return ret;
}

Set<Database::Cursor> DatabaseRParser::findCursors(const String &string, const List<Path> &pathFilter) const
{
    QMutexLocker locker(&mutex);
    waitForState(GreaterOrEqual, Idle);

    Set<Path> cand, paths = pathFilter.toSet();
    {
        const bool pass = paths.isEmpty();
        Map<String, RParserName>::const_iterator name = names.lower_bound(string);
        const Map<String, RParserName>::const_iterator end = names.end();
        while (name != end && name->first.startsWith(string)) {
            if (pass)
                cand += name->second.paths;
            else
                cand += paths.intersected(name->second.paths);
            ++name;
        }
    }

    Set<Cursor> cursors;
    {
        CPlusPlus::Overview overview;

        Set<Path>::const_iterator path = cand.begin();
        const Set<Path>::const_iterator end = cand.end();
        while (path != end) {
            CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(*path));
            if (!doc) {
                error() << "No document for" << *path << "in findCursors";
                ++path;
                continue;
            }

            CPlusPlus::TranslationUnit* unit = doc->translationUnit();
            CPlusPlus::Namespace* globalNamespace = doc->globalNamespace();
            if (globalNamespace) {
                FindSymbols find(FindSymbols::Cursors);
                find(globalNamespace);
                Set<CPlusPlus::Symbol*> syms = find.symbols();
                foreach(CPlusPlus::Symbol* sym, syms) {
                    if (nameMatch(sym, string, overview))
                        cursors.insert(makeCursor(sym, unit));
                }
            }

            ++path;
        }
    }
    return cursors;
}

Set<Database::Cursor> DatabaseRParser::cursors(const Path &path) const
{
    QMutexLocker locker(&mutex);
    waitForState(GreaterOrEqual, CollectingNames);

    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(path));
    if (!doc)
        return Set<Cursor>();
    Set<Cursor> cursors;

    CPlusPlus::TranslationUnit* unit = doc->translationUnit();
    CPlusPlus::Namespace* globalNamespace = doc->globalNamespace();
    if (globalNamespace) {
        FindSymbols find(FindSymbols::Cursors);
        find(globalNamespace);
        Set<CPlusPlus::Symbol*> syms = find.symbols();
        foreach(const CPlusPlus::Symbol* sym, syms) {
            cursors.insert(makeCursor(sym, unit));
        }
    }

    return cursors;
}

bool DatabaseRParser::codeCompleteAt(const Location &location, const String &source,
                                     Connection *conn)
{
    error() << "Got code complete" << location << source;
    return false;
}

class DatabaseRParserPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<Database> createDatabase()
    {
        return shared_ptr<Database>(new DatabaseRParser);
    }
};

extern "C" RTagsPlugin* createInstance()
{
    return new DatabaseRParserPlugin;
}
