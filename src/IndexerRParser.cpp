#include "IndexerRParser.h"
#include "QueryMessage.h"
#include "RTagsPlugin.h"
#include "Server.h"
#include "SourceInformation.h"
#include <rct/Connection.h>
#include <rct/Log.h>
#include <rct/StopWatch.h>

#include <QMutexLocker>

#include <cpppreprocessor.h>
#include <searchsymbols.h>
#include <symbolfinder.h>
#include <ASTPath.h>
#include <DependencyTable.h>
#include <cplusplus/SymbolVisitor.h>
#include <typeinfo>
#include <cxxabi.h>

using namespace CppTools;
using namespace CppTools::Internal;

class RParserJob;
class RParserUnit;

static inline String fromQString(const QString& str)
{
    const QByteArray& utf8 = str.toUtf8();
    return String(utf8.constData(), utf8.size());
}

static inline String contextForLine(const char* filename, unsigned line)
{
    const Path p = Path::resolved(filename);
    if (!p.isFile())
        return String();
    const String contents = p.readAll();
    const char* start = contents.constData();
    const char* end = contents.constData() + contents.size();
    const char* cur = start;
    unsigned lineno = 1;
    while (cur < end) {
        switch (*cur++) {
        case '\n':
            if (line == lineno) {
                return String(start, cur - start - 1).trimmed();
            }
            start = cur;
            ++lineno;
            break;
        case '\0':
            return String();
        }
    }
    return String();
}

class RParserThread : public QThread
{
public:
    RParserThread();
    ~RParserThread();

    static RParserThread* instance();

    void init(const shared_ptr<Project>& project);
    void setProject(const shared_ptr<Project>& project);
    void enqueue(RParserJob* job);

    enum State { Starting,
                 Indexing,
                 CollectingNames,
                 Idle };

    enum WaitMode { GreaterOrEqual, Equal };
    class WaitForState
    {
    public:
        WaitForState(WaitMode mode, State state);
        ~WaitForState();

    private:
        QMutexLocker locker;
    };

    enum FindSymbolMode { Swap, Declaration, Definition };
    CPlusPlus::Symbol* findSymbol(CPlusPlus::Document::Ptr doc, const Location& srcLoc,
                                  FindSymbolMode mode, const QByteArray& src,
                                  CPlusPlus::LookupContext& ctx, Location& loc) const;

    // IndexerRParser APIs
    void dump(const SourceInformation& sourceInformation, Connection* conn) const;
    void dirty(const Set<Path>& files);
    Project::Cursor cursor(const Location& location) const;
    void references(const Location& location, unsigned flags, const List<Path>& pathFilters, Connection* conn) const;
    Set<Path> files(int mode) const;
    Set<Path> dependencies(const Path& path, Indexer::DependencyMode mode) const;
    Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const;
    Set<Project::Cursor> findCursors(const String &string, const List<Path> &pathFilter) const;
    Set<Project::Cursor> cursors(const Path &path) const;
    bool isIndexing() const;
    void remove(const Path& sourceFile);

protected:
    void run();

private:
    void changeState(State st);
    void waitForState(WaitMode m, State st) const;
    void processJob(RParserJob* job);
    RParserUnit* findUnit(const Path& path);
    void collectNames(const Set<Path>& files);
    void dirtyFiles(const Set<Path>& files);
    struct RParserName
    {
        Set<Path> paths;
        Set<String> names;
        void merge(const RParserName& other);
    };
    void mergeNames(const Map<String, RParserName>& lnames);

private:
    QQueue<RParserJob*> jobs;
    mutable QMutex mutex;
    mutable QWaitCondition wait;
    QWaitCondition jobsAvailable;
    State state;
    QPointer<CppModelManager> manager;
    DocumentParser* parser;
    Map<QString, QString> headerToSource;
    Map<Path, Map<Path, RParserUnit*> > units;
    shared_ptr<Project> project;
    Map<String, RParserName> names;
    int appargc;
    QApplication* app;

    static RParserThread* inst;

    friend class WaitForState;
    friend class DocumentParser;
    friend class FindSymbols;
};

static CPlusPlus::Overview overview;

static inline String symbolName(const CPlusPlus::Symbol* symbol)
{
    String symbolName = fromQString(overview.prettyName(symbol->name()));
    // if (symbolName.isEmpty()) {
    //     String type;
    //     if (symbol->isNamespace()) {
    //         type = "namespace";
    //     } else if (symbol->isEnum()) {
    //         type = "enum";
    //     } else if (const CPlusPlus::Class *c = symbol->asClass())  {
    //         if (c->isUnion())
    //             type = "union";
    //         else if (c->isStruct())
    //             type = "struct";
    //         else
    //             type = "class";
    //     } else {
    //         type = "symbol";
    //     }
    //     symbolName = "<anonymous ";
    //     symbolName += type;
    //     symbolName += '>';
    // }
    return symbolName;
}

static inline void debugSymbol(const CPlusPlus::Symbol* sym)
{
    if (!sym) {
        error("no symbol");
        return;
    }
    error("symbol %s:%u:%u", sym->fileName(), sym->line(), sym->column());
}

class RParserJob
{
public:
    RParserJob(const SourceInformation& i, Project::Type t)
        : info(i), type(t)
    {
    }

    Path fileName() const { return info.sourceFile; }

    SourceInformation info;
    Project::Type type;
};

class FindScopeCandidates : protected CPlusPlus::SymbolVisitor
{
public:
    FindScopeCandidates(CPlusPlus::Document::Ptr d, CPlusPlus::Symbol* f)
        : doc(d), find(f)
    {
    }

    QList<QList<const CPlusPlus::Name*> > operator()(CPlusPlus::Symbol* symbol)
    {
        accept(symbol);
        return candidates;
    }

protected:
    bool after(CPlusPlus::Symbol* symbol) const
    {
        return (symbol->line() > find->line() || (symbol->line() == find->line() && symbol->column() > find->column()));
    }

    bool contains(CPlusPlus::Scope* scope) const
    {
        unsigned line, column;
        CPlusPlus::TranslationUnit* unit = doc->translationUnit();
        unit->getPosition(scope->startOffset(), &line, &column);
        if (find->line() < line || (find->line() == line && find->column() < column))
            return false;
        unit->getPosition(scope->endOffset(), &line, &column);
        if (find->line() > line || (find->line() == line && find->column() > column))
            return false;
        return true;
    }

    virtual bool visit(CPlusPlus::UsingNamespaceDirective *nsd)
    {
        if (after(nsd))
            return false;
        candidates.append(CPlusPlus::LookupContext::fullyQualifiedName(nsd));
        return true;
    }

    virtual bool visit(CPlusPlus::Namespace *ns)
    {
        const CPlusPlus::Namespace* global = doc->globalNamespace();
        if (ns == global)
            return true;
        // check if the symbol we're looking for is inside this namespace scope
        if (!contains(ns))
            return false;

        QList<const CPlusPlus::Name*> current;
        do {
            current.prepend(ns->name());
            ns = ns->enclosingNamespace();
            if (ns == global)
                break;
        } while (ns);
        candidates.append(current);
        return true;
    }

private:
    CPlusPlus::Document::Ptr doc;
    CPlusPlus::Symbol* find;

    QList<QList<const CPlusPlus::Name*> > candidates;
};

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

class FindBaseVirtuals : protected CPlusPlus::SymbolVisitor
{
public:
    FindBaseVirtuals(CPlusPlus::Function* f)
        : find(f)
    {
    }

    QList<CPlusPlus::Function*> operator()(CPlusPlus::Symbol *symbol)
    {
        funcs.clear();
        accept(symbol);
        return funcs;
    }

protected:
    void check(CPlusPlus::Function* func)
    {
        if (func->isVirtual() || func->isPureVirtual()) {
            // possible, check if it's the same function as the one we want
            if (!(func->unqualifiedName() && func->unqualifiedName()->isEqualTo(find->unqualifiedName())))
                return;
            else if (func->argumentCount() == find->argumentCount()) {
                const unsigned argc = find->argumentCount();
                unsigned argIt = 0;
                for (; argIt < argc; ++argIt) {
                    CPlusPlus::Symbol *arg = func->argumentAt(argIt);
                    CPlusPlus::Symbol *otherArg = find->argumentAt(argIt);
                    if (!arg->type().isEqualTo(otherArg->type()))
                        return;
                }

                if (argIt == argc
                    && func->isConst() == find->type().isConst()
                    && func->isVolatile() == find->type().isVolatile()) {
                    funcs.append(func);
                }
            }
        }
    }

    using CPlusPlus::SymbolVisitor::visit;

    virtual bool visit(CPlusPlus::UsingNamespaceDirective *) { return false; }
    virtual bool visit(CPlusPlus::UsingDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::NamespaceAlias *) { return false; }
    virtual bool visit(CPlusPlus::Argument *) { return false; }
    virtual bool visit(CPlusPlus::TypenameArgument *) { return false; }
    virtual bool visit(CPlusPlus::ForwardClassDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::Enum *symbol) { return false; }
    virtual bool visit(CPlusPlus::BaseClass *) { return true; }
    virtual bool visit(CPlusPlus::Class *symbol) { return true; }
    virtual bool visit(CPlusPlus::Declaration *decl)
    {
        CPlusPlus::Function* func = decl->type()->asFunctionType();
        if (func)
            check(func);
        return false;
    }
    virtual bool visit(CPlusPlus::Function *symbol)
    {
        check(symbol);
        return false;
    }

    virtual bool visit(CPlusPlus::Namespace *symbol) { return false; }
    virtual bool visit(CPlusPlus::Block *symbol) { return false; }

    // Objective-C
    virtual bool visit(CPlusPlus::ObjCBaseClass *) { return false; }
    virtual bool visit(CPlusPlus::ObjCBaseProtocol *) { return false; }
    virtual bool visit(CPlusPlus::ObjCForwardClassDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::ObjCForwardProtocolDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::ObjCPropertyDeclaration *) { return false; }
    virtual bool visit(CPlusPlus::ObjCClass *symbol) { return true; }
    virtual bool visit(CPlusPlus::ObjCProtocol *symbol) { return false; }
    virtual bool visit(CPlusPlus::ObjCMethod *symbol)
    {
        // ### ????
        return false;
    }

private:
    CPlusPlus::Function* find;
    QList<CPlusPlus::Function*> funcs;
};

class FindSymbols : public CPlusPlus::SymbolVisitor
{
public:
    enum Mode { Cursors, ListSymbols };

    FindSymbols(Mode m) : mode(m) { }

    bool preVisit(CPlusPlus::Symbol* symbol);

    void operator()(CPlusPlus::Symbol* symbol);

    Set<CPlusPlus::Symbol*> symbols() { return syms; }
    Map<String, RParserThread::RParserName> symbolNames() { return names; };

private:
    Mode mode;
    Set<CPlusPlus::Symbol*> syms;
    Map<String, RParserThread::RParserName> names;
};

void RParserThread::RParserName::merge(const RParserName& other)
{
    paths += other.paths;
    names += other.names;
}

enum QualifiedMode { All, Smart };
static inline QList<const CPlusPlus::Name*> rtagsQualified(const CPlusPlus::Symbol* symbol, QualifiedMode mode)
{
    if (mode == Smart && symbol->isNamespace())
        mode = All;
    QList<const CPlusPlus::Name*> name;
    do {
        name.prepend(symbol->name());
        symbol = symbol->enclosingScope();
    } while (symbol && (symbol->isClass() || (mode == All && symbol->isNamespace())));
    return name;
}

static String rtagsQualifiedName(const CPlusPlus::Symbol* symbol, QualifiedMode mode)
{
    return fromQString(overview.prettyName(rtagsQualified(symbol, mode)));
}

bool FindSymbols::preVisit(CPlusPlus::Symbol* symbol)
{
    if (mode == Cursors) {
        syms.insert(symbol);
    } else {
        RParserThread::RParserName cur;
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

static inline bool nameMatch(CPlusPlus::Symbol* symbol, const String& name)
{
    String full;
    QList<const CPlusPlus::Name*> fullName = CPlusPlus::LookupContext::fullyQualifiedName(symbol);
    while (!fullName.isEmpty()) {
        const CPlusPlus::Name* n = fullName.takeLast();
        if (!full.isEmpty())
            full.prepend("::");
        full.prepend(fromQString(overview.prettyName(n)));
        if (full == name)
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

DocumentParser::DocumentParser(QPointer<CppModelManager> mgr,
                               RParserThread* parser,
                               QObject* parent)
    : QObject(parent), symbolCount(0), manager(mgr), rparser(parser)
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

static inline Project::Cursor::Kind symbolKind(const CPlusPlus::Symbol* sym)
{
    if (sym->asEnum()) {
        //error("enum");
        return Project::Cursor::Enum;
    } else if (sym->asFunction()) {
        //error("function");
        return Project::Cursor::MemberFunctionDeclaration;
    } else if (sym->asNamespace()) {
        //error("namespace");
        return Project::Cursor::Namespace;
    } else if (sym->asTemplate()) {
        //error("template");
    } else if (sym->asNamespaceAlias()) {
        //error("namespaceAlias");
    } else if (sym->asForwardClassDeclaration()) {
        //error("forward class");
        return Project::Cursor::Class;
    } else if (sym->asClass()) {
        //error("class");
        return Project::Cursor::Class;
    } else if (sym->asUsingNamespaceDirective()) {
        //error("using 1");
    } else if (sym->asUsingDeclaration()) {
        //error("using 2");
    } else if (sym->asDeclaration()) {
        //error("decl");
        return Project::Cursor::Variable; // ### ???
    } else if (sym->asArgument()) {
        //error("arg");
        return Project::Cursor::Variable;
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
    return Project::Cursor::Invalid;
}

static inline CPlusPlus::Symbol* findSymbolReferenced(QPointer<CppModelManager> manager,
                                                      CPlusPlus::Document::Ptr doc,
                                                      CPlusPlus::Symbol* symbol)
{
    const CPlusPlus::Identifier *symbolId = symbol->identifier();
    if (!symbolId) {
        error("no symbol id in findSymbolReferenced");
        return 0;
    }

    debug("looking for symbol %s (%s)", symbolId->chars(), rtagsQualifiedName(symbol, All).constData());
    QList<const CPlusPlus::Name*> qname = CPlusPlus::LookupContext::fullyQualifiedName(symbol);
    if (qname.isEmpty())
        return 0;

    const CPlusPlus::Snapshot& snapshot = manager->snapshot();

    QList<const CPlusPlus::Name*> scope = qname;
    scope.removeLast();

    // find candidate scopes
    FindScopeCandidates scopes(doc, symbol);
    QList<QList<const CPlusPlus::Name*> > candidates = scopes(doc->globalNamespace());
    debug("candidates size %d", candidates.size());

    // ### Use QFuture for this?

    CPlusPlus::Snapshot::const_iterator snap = snapshot.begin();
    const CPlusPlus::Snapshot::const_iterator end = snapshot.end();
    while (snap != end) {
        CPlusPlus::Document::Ptr doc = snap.value();
        debug("looking in %s", qPrintable(doc->fileName()));
        const CPlusPlus::Control* control = doc->control();
        if (control->findIdentifier(symbolId->chars(), symbolId->size())) {
            debug("found?");
            CPlusPlus::LookupContext lookup(doc, snapshot);
            CPlusPlus::ClassOrNamespace* ns = lookup.globalNamespace();
            foreach (const CPlusPlus::Name* name, scope) {
                ns = ns->lookupType(name);
                if (!ns && candidates.isEmpty())
                    return 0;
            }
            if (ns) {
                CPlusPlus::Symbol* newsym = ns->lookupInScope(qname);
                if (newsym && !newsym->isForwardClassDeclaration())
                    return newsym;
            }

            if (!candidates.isEmpty()) {
                // try the candidates
                foreach(const QList<const CPlusPlus::Name*>& candidate, candidates) {
                    QList<const CPlusPlus::Name*> candidateScope = candidate + qname;
                    candidateScope.removeLast();

                    debug("checking candidate %s", qPrintable(overview.prettyName(candidateScope)));
                    ns = lookup.globalNamespace();
                    foreach (const CPlusPlus::Name* name, candidateScope) {
                        ns = ns->lookupType(name);
                        if (!ns)
                            break;
                    }
                    if (ns) {
                        debug("candidate ok, checking if symbol exists");
                        CPlusPlus::Symbol* newsym = ns->lookupInScope(candidate + qname);
                        if (newsym && !newsym->isForwardClassDeclaration())
                            return newsym;
                    }
                }
            }
        }
        ++snap;
    }

    return 0;
}

static inline void writeBaseUsages(const QPointer<CppModelManager>& manager, const CPlusPlus::Document::Ptr& doc,
                                   CPlusPlus::Function* fun, const Set<Path>& pathFilter, bool wantContext, Connection* conn)
{
    CPlusPlus::Class* cls = fun->enclosingClass();
    if (!cls)
        return;

    FindBaseVirtuals find(fun);
    const unsigned baseCount = cls->baseClassCount();
    for (unsigned i = 0; i < baseCount; ++i) {
        CPlusPlus::BaseClass* base = cls->baseClassAt(i);
        CPlusPlus::Symbol* real = findSymbolReferenced(manager, doc, base);
        if (!real) {
            // boo
            continue;
        }

        const bool filtered = pathFilter.contains(Path::resolved(real->fileName()));
        QList<CPlusPlus::Function*> funcs = find(real);
        foreach(CPlusPlus::Function* newfun, funcs) {
            if (!filtered) {
                if (wantContext) {
                    conn->write<256>("%s:%d:%d %c\t%s", newfun->fileName(), newfun->line(), newfun->column(),
                                     Project::Cursor::kindToChar(Project::Cursor::MemberFunctionDeclaration),
                                     contextForLine(real->fileName(), newfun->line()).constData());
                } else {
                    conn->write<256>("%s:%d:%d", newfun->fileName(), newfun->line(), newfun->column());
                }
            }
            writeBaseUsages(manager, doc, newfun, pathFilter, wantContext, conn);
        }
    }
}

static inline void writeUsage(const QPointer<CppModelManager>& manager, const CPlusPlus::Usage& usage,
                              const CPlusPlus::Document::Ptr& doc, unsigned flags,
                              const Set<Path>& pathFilter, Connection* conn)
{
    const bool wantContext = !(flags & QueryMessage::NoContext);
    const bool wantVirtuals = flags & QueryMessage::FindVirtuals;
    const bool wantAll = flags & QueryMessage::AllReferences;

    Project::Cursor::Kind kind = Project::Cursor::Reference;
    if (doc) {
        CPlusPlus::Symbol* refsym = doc->lastVisibleSymbolAt(usage.line, usage.col + 1);
        if (refsym
            && refsym->line() == static_cast<unsigned>(usage.line)
            && refsym->column() == static_cast<unsigned>(usage.col + 1)) {
            if (wantVirtuals || wantAll) {
                if (CPlusPlus::Function *funTy = refsym->type()->asFunctionType()) {
                    if (funTy->isVirtual() || funTy->isPureVirtual()) {
                        kind = Project::Cursor::MemberFunctionDeclaration;
                        // we'll need to see in our base classes since we won't get a Usage for those
                        writeBaseUsages(manager, doc, funTy, pathFilter, wantContext, conn);
                    } else if (!wantAll) {
                        return;
                    }
                }
                if (wantAll) {
                    if (kind == Project::Cursor::Reference)
                        kind = symbolKind(refsym);
                } else if (kind == Project::Cursor::Reference) {
                    return;
                }
            } else {
                return;
            }
        }
    }
    if (kind == Project::Cursor::Reference && (wantVirtuals && !wantAll))
        return;
    //error() << "adding ref" << fromQString(usage.path) << usage.line << usage.col;
    if (wantContext) {
        conn->write<256>("%s:%d:%d %c\t%s", qPrintable(usage.path), usage.line, usage.col + 1,
                         Project::Cursor::kindToChar(kind), contextForLine(qPrintable(usage.path), usage.line).constData());
    } else {
        conn->write<256>("%s:%d:%d", qPrintable(usage.path), usage.line, usage.col + 1);
    }
}

static inline void writeUsages(const QPointer<CppModelManager>& manager, CPlusPlus::Symbol* symbol,
                               unsigned flags, const List<Path>& pathFilter, Connection* conn)
{
    const CPlusPlus::Identifier *symbolId = symbol->identifier();
    if (!symbolId) {
        error("no symbol id in findUsages");
        return;
    }

    const CPlusPlus::Snapshot& snapshot = manager->snapshot();
    const Set<Path> paths = pathFilter.toSet();
    const bool pass = paths.isEmpty();

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
            foreach(const CPlusPlus::Usage& usage, find.usages()) {
                if (!pass && !paths.contains(fromQString(usage.path)))
                    continue;
                const CPlusPlus::Document::Ptr doc = manager->document(usage.path);
                debug("usage at %s:%u:%u", qPrintable(usage.path), usage.line, usage.col + 1);
                writeUsage(manager, usage, doc, flags, paths, conn);
            }
        }
        ++snap;
    }
}

void DocumentParser::onDocumentUpdated(CPlusPlus::Document::Ptr doc)
{
    // seems I need to keep this around
    doc->keepSourceAndAST();

    // message out any diagnostics
    const QList<CPlusPlus::Document::DiagnosticMessage> diags = doc->diagnosticMessages();
    foreach(const CPlusPlus::Document::DiagnosticMessage& msg, diags) {
        warning("%s:%d:%d: %s", msg.fileName().toUtf8().constData(), msg.line(), msg.column(), msg.text().toUtf8().constData());
    }

    {
        QList<CPlusPlus::Document::Include> includes = doc->includes();
        if (!includes.isEmpty()) {
            QString srcFile = doc->fileName();
            QMutexLocker locker(&rparser->mutex);
            foreach(CPlusPlus::Document::Include include, includes) {
                rparser->headerToSource[include.fileName()] = srcFile;
            }
        }
    }

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

class DumpAST : public CPlusPlus::ASTVisitor
{
public:
    DumpAST(CPlusPlus::TranslationUnit *unit, Connection *conn)
        : CPlusPlus::ASTVisitor(unit), mDepth(0), mConn(conn)
    {}

protected:
    virtual bool preVisit(CPlusPlus::AST *ast)
    {
        const char *id = typeid(*ast).name();
        char *cppId = abi::__cxa_demangle(id, 0, 0, 0);
        id = cppId;
        String fill(mDepth * 2, ' ');
        String context;
        for (unsigned idx = ast->firstToken(); idx<ast->lastToken(); ++idx) {
            const char *str = spell(idx);
            if (!context.isEmpty()) {
                char last = context.last();
                if (last == ',') {
                    context += ' ';
                } else if (isalnum(last) && isalnum(*str)) {
                    context += ' ';
                } else if (*str == '{' || *str == '}') {
                    context += ' ';
                }
            }
            context.append(str);
        }

        mConn->write<128>("%s%s: %s", fill.constData(), id, context.constData());
        free(cppId);
        ++mDepth;
        return true;
    }

    void postVisit(CPlusPlus::AST *)
    {
        --mDepth;
    }
private:
    int mDepth;
    Connection *mConn;
};

class RParserUnit
{
public:
    SourceInformation info;

    static QHash<QString, CPlusPlus::Document::Ptr> defineDocs;
    static CPlusPlus::Document::Ptr defineDocument(QPointer<CppModelManager> manager, const QString& name, const QStringList& defines);

    void reindex(QPointer<CppModelManager> manager);
};

QHash<QString, CPlusPlus::Document::Ptr> RParserUnit::defineDocs;

CPlusPlus::Document::Ptr RParserUnit::defineDocument(QPointer<CppModelManager> manager, const QString& name, const QStringList& defines)
{
    QHash<QString, CPlusPlus::Document::Ptr>::const_iterator defs = defineDocs.find(defines.join(QLatin1String(":")));
    if (defs != defineDocs.end())
        return defs.value();
    QString defsrc;
    foreach(const QString& def, defines) {
        const int eq = def.indexOf(QLatin1Char('='));
        if (eq == -1)
            defsrc += QLatin1String("#define ") + def;
        else
            defsrc += QLatin1String("#define ") + def.left(eq) + QLatin1Char(' ') + def.mid(eq + 1);
        defsrc += QLatin1Char('\n');
    }
    const CPlusPlus::Snapshot& snapshot = manager->snapshot();
    CPlusPlus::Document::Ptr doc = snapshot.preprocessedDocument(defsrc, QString("<rparserdefines_%1>").arg(name));
    assert(doc);
    defineDocs[defines.join(QLatin1String(":"))] = doc;
    return doc;
}

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

    const QString srcFile = QString::fromStdString(info.sourceFile);
    const QString srcPath = QString::fromStdString(info.sourceFile.parentDir());
    QList<CPlusPlus::Document::Include> includes;
    {
        CPlusPlus::Document::Ptr doc = manager->document(srcFile);
        if (doc)
            includes = doc->includes();
    }
    const bool hasIncludes = !includes.isEmpty();

#warning grab the include paths and defines from the system compiler here
    static QStringList globalDefines;
    if (globalDefines.isEmpty())
        globalDefines << QLatin1String("__GNUC__=4");

    static QStringList incs = QStringList()
        << QLatin1String("/usr/include")
        << QLatin1String("/usr/include/c++/4.6")
        << QLatin1String("/usr/include/c++/4.7")
        << QLatin1String("/usr/include/c++/4.8")
        << QLatin1String("/usr/lib/gcc/i686-linux-gnu/4.6/include")
        << QLatin1String("/usr/lib/gcc/i686-linux-gnu/4.7/include")
        << QLatin1String("/usr/lib/gcc/i686-linux-gnu/4.8/include")
        << QLatin1String("/usr/include/i386-linux-gnu")
        << srcPath;
    List<SourceInformation::Build>::const_iterator build = info.builds.begin();
    const List<SourceInformation::Build>::const_iterator end = info.builds.end();
    while (build != end) {
        //error() << "reindexing" << info.sourceFile << build->includePaths << build->defines;
        preprocessor.removeFromCache(srcFile);
        if (hasIncludes) {
            foreach(const CPlusPlus::Document::Include& include, includes) {
                preprocessor.removeFromCache(include.fileName());
            }
        }

        preprocessor.mergeEnvironment(defineDocument(manager, srcFile, toQStringList(build->defines) + globalDefines));
        preprocessor.setIncludePaths(toQStringList(build->includePaths) + incs);
        preprocessor.run(srcFile);
        preprocessor.resetEnvironment();
        ++build;
    }
}

static inline Location makeLocation(CPlusPlus::Symbol* sym)
{
    const uint32_t fileId = Location::insertFile(Path::resolved(sym->fileName()));
    return Location(fileId, sym->line(), sym->column());
}

static inline Project::Cursor makeCursor(const CPlusPlus::Symbol* sym,
                                          const CPlusPlus::TranslationUnit* unit)
{
    Project::Cursor cursor;
    const uint32_t fileId = Location::insertFile(Path::resolved(sym->fileName()));
    cursor.location = Location(fileId, sym->line(), sym->column());
    const CPlusPlus::Token& token = unit->tokenAt(sym->sourceLocation());
    cursor.start = token.begin();
    cursor.end = token.end();
    cursor.kind = symbolKind(sym);
    cursor.symbolName = rtagsQualifiedName(sym, Smart);
    return cursor;
}

CPlusPlus::Symbol* RParserThread::findSymbol(CPlusPlus::Document::Ptr doc,
                                             const Location& srcLoc,
                                             FindSymbolMode mode,
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
                    debug("found outright");
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

        debug("looking in %s", qPrintable(doc->fileName()));
        CPlusPlus::ASTPath path(doc);
        QList<CPlusPlus::AST*> asts = path(line, column);
        debug("ast path size %d", asts.size());
        while (!asts.isEmpty()) {
            CPlusPlus::AST* ast = asts.takeLast();

            const int startIndex = ast->firstToken();
            int endIndex = ast->lastToken() - 1;
            while (endIndex >= 0) {
                unsigned el, ec;
                unit->getTokenStartPosition(endIndex, &el, &ec, 0);
                if (el < line || (el == line && ec <= column))
                    break;
                --endIndex;
            }

            assert(startIndex <= endIndex && endIndex >= 0);

            if (startIndex > 0) {
                // check if our previous token is an accessor token
                bool ok = true;
                const CPlusPlus::Token& prev = unit->tokenAt(startIndex - 1);
                switch (prev.kind()) {
                case CPlusPlus::T_COLON_COLON:
                case CPlusPlus::T_DOT:
                case CPlusPlus::T_ARROW:
                case CPlusPlus::T_DOT_STAR:
                case CPlusPlus::T_ARROW_STAR:
                    // yes, we need to look at our next AST
                    ok = false;
                    break;
                default:
                    break;
                }
                if (!ok)
                    continue;
            }
            const CPlusPlus::Token& start = unit->tokenAt(startIndex);
            const CPlusPlus::Token& last = unit->tokenAt(endIndex);
            const QByteArray expression = src.mid(start.begin(), last.end() - start.begin());

            debug("trying expr '%.40s' in scope %p", qPrintable(expression), scope);

            sym = canonicalSymbol(scope, expression, typeofExpression);
            if (sym) {
                unsigned startLine, startColumn;
                const CPlusPlus::StringLiteral* file;
                unit->getTokenStartPosition(startIndex, &startLine, &startColumn, &file);

                //unsigned endLine, endColumn;
                //unit->getTokenEndPosition(ast->lastToken() - 1, &endLine, &endColumn, 0);
                const uint32_t fileId = Location::fileId(Path::resolved(file->chars()));
                loc = Location(fileId, startLine, startColumn);

                warning() << "got it at" << loc;
                break;
            }
        }
    }

    if (!sym)
        return 0;

    if (CPlusPlus::Function* func = sym->type()->asFunctionType()) {
        debug("function type");
        // if we find a definition that's different from the declaration then replace
        CppTools::SymbolFinder finder;
        CPlusPlus::Symbol* definition = finder.findMatchingDefinition(sym, manager->snapshot(), true);
        if (!definition) {
            definition = finder.findMatchingDefinition(sym, manager->snapshot(), false);
        }
        if (definition) {
            if (sym != definition) {
                if (mode == Definition || mode == Swap) {
                    debug("swapping, taking the def");
                    sym = definition;
                }
            } else if (mode != Definition) {
                QList<CPlusPlus::Declaration*> decls = finder.findMatchingDeclaration(lookup, func);
                // take the first non-forward decl
                foreach(CPlusPlus::Declaration* decl, decls) {
                    if (decl->isForwardClassDeclaration())
                        continue;
                    debug("swapping, taking the decl");
                    sym = decl;
                    break;
                }
            }
        }
    } else {
        if (sym->type()->asClassType())
            debug("class type");
        if (sym->type()->asForwardClassDeclarationType())
            debug("fwd class type");
        bool wants = false;
        if (sym->asBaseClass())
            wants = true;
        else if (sym->asClass())
            wants = true;
        else if (sym->asForwardClassDeclaration())
            wants = true;
        if (wants) {
            debug("wants");
            CppTools::SymbolFinder finder;
            CPlusPlus::Class* cls = finder.findMatchingClassDeclaration(sym, manager->snapshot());
            if (cls) {
                debug("swapping");
                sym = cls;
            } else {
                CPlusPlus::Symbol* newsym = findSymbolReferenced(manager, doc, sym);
                if (newsym) {
                    debug("swapping with ref");
                    sym = newsym;
                }
            }
        }
    }

    return sym;
}

RParserThread::WaitForState::WaitForState(WaitMode mode, State state)
    : locker(&RParserThread::instance()->mutex)
{
    RParserThread::instance()->waitForState(mode, state);
}

RParserThread::WaitForState::~WaitForState()
{
}

RParserThread* RParserThread::inst = 0;

RParserThread::RParserThread()
    : QThread(), state(Starting), parser(0), appargc(0), app(new QApplication(appargc, 0))
{
    start();
    moveToThread(this);
}

RParserThread::~RParserThread()
{
    Map<Path, Map<Path, RParserUnit*> >::const_iterator unitsMap = units.begin();
    const Map<Path, Map<Path, RParserUnit*> >::const_iterator unitsEnd = units.begin();
    while (unitsMap != unitsEnd) {
        Map<Path, RParserUnit*>::const_iterator unit = unitsMap->second.begin();
        const Map<Path, RParserUnit*>::const_iterator end = unitsMap->second.end();
        while (unit != end) {
            delete unit->second;
            ++unit;
        }
        ++unitsMap;
    }
    delete parser;
    delete app;
}

static inline const char* stateName(RParserThread::State st)
{
    struct states {
        RParserThread::State state;
        const char* name;
    } static s[] = { { RParserThread::Starting, "starting" },
                     { RParserThread::Indexing, "indexing" },
                     { RParserThread::CollectingNames, "collectingnames" },
                     { RParserThread::Idle, "idle" } };
    for (unsigned int i = 0; i < sizeof(s); ++i) {
        if (s[i].state == st)
            return s[i].name;
    }
    return 0;
}

// needs to be called with mutex locked
void RParserThread::changeState(State st)
{
    if (state == st)
        return;
    warning() << "rparser thread state changed from " << stateName(state) << " to " << stateName(st);
    state = st;
    wait.wakeAll();
}

// needs to be called with mutex locked
void RParserThread::waitForState(WaitMode m, State st) const
{
    for (;;) {
        if (m == GreaterOrEqual && state >= st)
            break;
        if (m == Equal && state == st)
            break;
        wait.wait(&mutex);
    }
}

void RParserThread::init(const shared_ptr<Project>& proj)
{
    QMutexLocker locker(&mutex);
    if (!project)
        project = proj;
}

void RParserThread::setProject(const shared_ptr<Project>& proj)
{
    if (proj == project)
        return;

    WaitForState wait(GreaterOrEqual, Idle);
    project = proj;

    qDeleteAll(jobs);
    jobs.clear();

    names.clear();

    // delete the manager and recreate with new units
    delete parser;
    delete manager.data();
    manager = new CppModelManager;
    parser = new DocumentParser(manager, this);
    manager.data()->moveToThread(this);
    parser->moveToThread(this);
    QObject::connect(manager.data(), SIGNAL(documentUpdated(CPlusPlus::Document::Ptr)),
                     parser, SLOT(onDocumentUpdated(CPlusPlus::Document::Ptr)), Qt::DirectConnection);

    // create and enqueue jobs
    const Map<Path, RParserUnit*>& unitsMap = units[project->path()];
    Map<Path, RParserUnit*>::const_iterator it = unitsMap.begin();
    const Map<Path, RParserUnit*>::const_iterator end = unitsMap.end();
    while (it != end) {
        jobs.enqueue(new RParserJob(it->second->info, Project::Restore));
        ++it;
    }
    jobsAvailable.wakeOne();
}

void RParserThread::enqueue(RParserJob* job)
{
    QMutexLocker locker(&mutex);
    jobs.enqueue(job);
    state = Indexing; // ### a bit of a hack
    jobsAvailable.wakeOne();
}

RParserThread* RParserThread::instance()
{
    if (!inst)
        inst = new RParserThread;
    return inst;
}

void RParserThread::dump(const SourceInformation& sourceInformation, Connection* conn) const
{
    WaitForState wait(GreaterOrEqual, CollectingNames);

    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(sourceInformation.sourceFile));
    if (!doc) {
        conn->write<64>("Don't seem to have %s indexed", sourceInformation.sourceFile.constData());
        return;
    }
    DumpAST dump(doc->translationUnit(), conn);
    dump.accept(doc->translationUnit()->ast());
}

void RParserThread::dirty(const Set<Path>& files)
{
    QMutexLocker locker(&mutex);
    dirtyFiles(files);
}

Project::Cursor RParserThread::cursor(const Location& location) const
{
    WaitForState wait(GreaterOrEqual, CollectingNames);

    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(location.path()));
    if (!doc)
        return Project::Cursor();
    const QByteArray& src = doc->utf8Source();

    Project::Cursor cursor;

    CPlusPlus::Document::Ptr altDoc;
    {
        Map<QString, QString>::const_iterator src = headerToSource.find(QString::fromStdString(location.path()));
        if (src != headerToSource.end()) {
            altDoc = manager->document(src->second);
        }
    }

    CPlusPlus::LookupContext lookup(altDoc ? altDoc : doc, manager->snapshot());
    CPlusPlus::Symbol* sym = findSymbol(doc, location, Swap, src, lookup, cursor.location);
    if (!sym) {
        // look for includes
        QList<CPlusPlus::Document::Include> includes = doc->includes();
        foreach(const CPlusPlus::Document::Include& include, includes) {
            if (include.line() == static_cast<unsigned int>(location.line())) {
                // yes
                const uint32_t fileId = Location::insertFile(Path::resolved(fromQString(include.fileName())));
                cursor.target = cursor.location = Location(fileId, 1, 1);
                cursor.kind = Project::Cursor::File;
                return cursor;
            }
        }
        error() << "no symbol whatsoever for" << location;
        return Project::Cursor();
    }

    cursor.target = makeLocation(sym);
    if (cursor.location == cursor.target) {
        // declaration
        cursor.kind = symbolKind(sym);
    } else {
        // possible reference
        cursor.kind = Project::Cursor::Reference;
    }
    cursor.symbolName = symbolName(sym);

    warning() << "got a symbol, tried" << location << "ended up with target" << cursor.target;
    return cursor;
}

void RParserThread::references(const Location& location, unsigned flags, const List<Path>& pathFilters, Connection* conn) const
{
    WaitForState wait(GreaterOrEqual, CollectingNames);

    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(location.path()));
    if (!doc)
        return;
    const QByteArray& src = doc->utf8Source();

    Project::Cursor cursor;

    CPlusPlus::Document::Ptr altDoc;
    {
        Map<QString, QString>::const_iterator src = headerToSource.find(QString::fromStdString(location.path()));
        if (src != headerToSource.end()) {
            altDoc = manager->document(src->second);
        }
    }

    CPlusPlus::LookupContext lookup(altDoc ? altDoc : doc, manager->snapshot());
    CPlusPlus::Symbol* sym = findSymbol(doc, location, Declaration, src, lookup, cursor.location);
    if (!sym)
        return;

    writeUsages(manager, sym, flags, pathFilters, conn);
}

Set<Path> RParserThread::files(int mode) const
{
    Set<Path> result;

    const bool wantHeaders = (mode & Indexer::HeaderFiles);
    const bool wantSources = (mode & Indexer::SourceFiles);
    if (wantSources && !wantHeaders) {
        const SourceInformationMap& sources = project->sourceInfos();
        SourceInformationMap::const_iterator source = sources.begin();
        const SourceInformationMap::const_iterator end = sources.end();
        while (source != end) {
            result.insert(source->first);
            ++source;
        }
        return result;
    }

    WaitForState wait(GreaterOrEqual, CollectingNames);

    const CPlusPlus::Snapshot& snapshot = manager->snapshot();
    CPlusPlus::Snapshot::const_iterator snap = snapshot.begin();
    const CPlusPlus::Snapshot::const_iterator end = snapshot.end();
    while (snap != end) {
        CPlusPlus::Document::Ptr doc = snap.value();
        assert(doc);
        if (wantSources)
            result.insert(fromQString(doc->fileName()));
        if (wantHeaders) {
            QList<CPlusPlus::Document::Include> includes = doc->includes();
            foreach(const CPlusPlus::Document::Include& include, includes) {
                result.insert(fromQString(include.fileName()));
            }
        }

        ++snap;
    }

    return result;
}

Set<Path> RParserThread::dependencies(const Path& path, Indexer::DependencyMode mode) const
{
    Set<Path> result;

    WaitForState wait(GreaterOrEqual, CollectingNames);

    // ### perhaps keep this around
    CPlusPlus::DependencyTable table;
    table.build(manager->snapshot());

    if (mode == Indexer::DependsOnArg) {
        const QStringList deps = table.filesDependingOn(QString::fromStdString(path));
        foreach(const QString dep, deps) {
            result.insert(fromQString(dep));
        }
    } else {
        assert(mode == Indexer::ArgDependsOn);
        const QString qpath = QString::fromStdString(path);

        const QHash<QString, QStringList>& t = table.dependencyTable();
        QHash<QString, QStringList>::const_iterator it = t.begin();
        const QHash<QString, QStringList>::const_iterator end = t.end();
        while (it != end) {
            if (it.value().contains(qpath))
                result.insert(fromQString(it.key()));
            ++it;
        }
    }

    return result;
}

Set<String> RParserThread::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    WaitForState wait(GreaterOrEqual, Idle);

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

Set<Project::Cursor> RParserThread::findCursors(const String &string, const List<Path> &pathFilter) const
{
    WaitForState wait(GreaterOrEqual, Idle);

    Set<Path> cand, paths = pathFilter.toSet();
    {
        const bool pass = paths.isEmpty();
        Map<String, RParserName>::const_iterator name = names.find(string);
        const Map<String, RParserName>::const_iterator end = names.end();
        if (name != end) {
            if (pass)
                cand += name->second.paths;
            else
                cand += paths.intersected(name->second.paths);
            ++name;
        }
    }

    Set<Project::Cursor> cursors;
    {
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
                    if (nameMatch(sym, string))
                        cursors.insert(makeCursor(sym, unit));
                }
            }

            if (path->endsWith(string)) { // file name, add custom target for the file
                const uint32_t fileId = Location::insertFile(*path);
                Project::Cursor fileCursor;
                fileCursor.kind = Project::Cursor::File;
                fileCursor.location = fileCursor.target = Location(fileId, 1, 1);
                fileCursor.symbolName = *path;
                cursors.insert(fileCursor);
            }

            ++path;
        }
    }
    return cursors;
}

Set<Project::Cursor> RParserThread::cursors(const Path &path) const
{
    WaitForState wait(GreaterOrEqual, CollectingNames);

    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(path));
    if (!doc)
        return Set<Project::Cursor>();
    Set<Project::Cursor> cursors;

    CPlusPlus::TranslationUnit* unit = doc->translationUnit();
    CPlusPlus::Namespace* globalNamespace = doc->globalNamespace();
    if (globalNamespace) {
        FindSymbols find(FindSymbols::Cursors);
        find(globalNamespace);
        Set<CPlusPlus::Symbol*> syms = find.symbols();
        foreach(const CPlusPlus::Symbol* sym, syms) {
            if (!sym->line())
                continue;
            cursors.insert(makeCursor(sym, unit));
        }
    }

    return cursors;
}

bool RParserThread::isIndexing() const
{
    QMutexLocker locker(&mutex);
    return state == Indexing;
}

void RParserThread::remove(const Path& sourceFile)
{
    WaitForState wait(GreaterOrEqual, Idle);
    const QString qfile = QString::fromStdString(sourceFile);
    {
        CPlusPlus::Document::Ptr doc = manager->document(qfile);
        if (doc)
            doc->releaseSourceAndAST();
    }
    manager->removeFromSnapshot(qfile);
}

RParserUnit* RParserThread::findUnit(const Path& path)
{
    const Map<Path, RParserUnit*>& unitsMap = units[project->path()];
    Map<Path, RParserUnit*>::const_iterator unit = unitsMap.find(path);
    if (unit == unitsMap.end())
        return 0;
    return unit->second;
}

void RParserThread::processJob(RParserJob* job)
{
    const Path& fileName = job->info.sourceFile;
    //error() << "  indexing" << fileName;
    RParserUnit* unit = findUnit(fileName);
    if (!unit) {
        unit = new RParserUnit;
        unit->info = job->info;
        units[project->path()][fileName] = unit;
    }
    unit->reindex(manager);
}

inline void RParserThread::dirtyFiles(const Set<Path>& files)
{
    Map<String, RParserName>::iterator name = names.begin();
    while (name != names.end()) {
        if ((name->second.paths - files).isEmpty())
            names.erase(name++);
        else
            ++name;
    }
}

void RParserThread::mergeNames(const Map<String, RParserName>& lnames)
{
    Map<String, RParserName>::const_iterator name = lnames.begin();
    const Map<String, RParserName>::const_iterator end = lnames.end();
    while (name != end) {
        names[name->first].merge(name->second);
        ++name;
    }
}

void RParserThread::collectNames(const Set<Path>& files)
{
    dirtyFiles(files);

    Set<Path>::const_iterator file = files.begin();
    const Set<Path>::const_iterator end = files.end();
    while (file != end) {
        CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(*file));
        if (!doc) {
            error() << "No document for" << *file << "in collectNames";
            ++file;
            continue;
        }

        CPlusPlus::Namespace* globalNamespace = doc->globalNamespace();
        if (globalNamespace) {
            FindSymbols find(FindSymbols::ListSymbols);
            find(globalNamespace);
            mergeNames(find.symbolNames());
        }

        const String fileName(file->fileName());
        RParserName rname;
        rname.names.insert(fileName);
        rname.paths.insert(*file);
        names[fileName].merge(rname);

        ++file;
    }
}

void RParserThread::run()
{
    manager = new CppModelManager;
    parser = new DocumentParser(manager, this);
    QObject::connect(manager.data(), SIGNAL(documentUpdated(CPlusPlus::Document::Ptr)),
                     parser, SLOT(onDocumentUpdated(CPlusPlus::Document::Ptr)));

    QMutexLocker locker(&mutex);
    assert(state == Starting || state == Indexing);
    if (jobs.isEmpty())
        changeState(Idle);
    locker.unlock();

    for (;;) {
        locker.relock();
        while (jobs.isEmpty()) {
            assert(state == Idle);
            jobsAvailable.wait(&mutex);
        }

        Set<Path> indexed;
        int taken = 0;
        int localFiles;

        StopWatch timer;

        assert(!jobs.isEmpty());
        changeState(Indexing);
        while (!jobs.isEmpty()) {
            RParserJob* job = jobs.dequeue();
            ++taken;
            locker.unlock();
            processJob(job);
            locker.relock();

            CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(job->fileName()));
            assert(doc);
            assert(!job->fileName().isEmpty());
            indexed.insert(job->fileName());
            localFiles = 1;
            QList<CPlusPlus::Document::Include> includes = doc->includes();
            foreach(const CPlusPlus::Document::Include& include, includes) {
                // ### this really shouldn't happen but it does
                if (include.fileName().isEmpty())
                    continue;
                ++localFiles;
                indexed.insert(fromQString(include.fileName()));
            }

            error("[%3d%%] %d/%d %s %s, Files: %d",
                  static_cast<int>(round(taken / static_cast<double>(jobs.size() + taken) * 100.0)),
                  taken, jobs.size() + taken, String::formatTime(time(0), String::Time).constData(),
                  job->fileName().toTilde().constData(), localFiles);
            if (jobs.isEmpty()) {
                error() << "Parsed" << taken << "files in" << timer.elapsed() << "ms";
                if (job->type != Project::Restore)
                    project->startSaveTimer();
            }
            delete job;
        }

        changeState(CollectingNames);
        assert(jobs.isEmpty());
        locker.unlock();
        collectNames(indexed);
        locker.relock();

        if (!jobs.isEmpty()) {
            locker.unlock();
            continue;
        }

        changeState(Idle);
        locker.unlock();
    }
}

IndexerRParser::IndexerRParser(shared_ptr<Project> project)
    : Indexer(project)
{
    RParserThread::instance()->init(project);
}

IndexerRParser::~IndexerRParser()
{
}

void IndexerRParser::status(const String &query, Connection *conn, unsigned queryFlags) const
{
}

void IndexerRParser::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
    RParserThread::instance()->dump(sourceInformation, conn);
}

void IndexerRParser::dirty(const Set<Path>& files)
{
    RParserThread::instance()->dirty(files);
}

void IndexerRParser::index(const SourceInformation &sourceInformation, Project::Type type)
{
    RParserThread* thread = RParserThread::instance();
    thread->enqueue(new RParserJob(sourceInformation, type));
}

void IndexerRParser::activate()
{
    RParserThread::instance()->setProject(mProject);
}

Project::Cursor IndexerRParser::cursor(const Location &location) const
{
    return RParserThread::instance()->cursor(location);
}

void IndexerRParser::references(const Location& location, unsigned flags,
                                 const List<Path> &pathFilters, Connection *conn) const
{
    RParserThread::instance()->references(location, flags, pathFilters, conn);
}

Set<Path> IndexerRParser::files(int mode) const
{
    return RParserThread::instance()->files(mode);
}

Set<Path> IndexerRParser::dependencies(const Path &path, DependencyMode mode) const
{
    return RParserThread::instance()->dependencies(path, mode);
}

Set<String> IndexerRParser::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    return RParserThread::instance()->listSymbols(string, pathFilter);
}

Set<Project::Cursor> IndexerRParser::findCursors(const String &string, const List<Path> &pathFilter) const
{
    return RParserThread::instance()->findCursors(string, pathFilter);
}

Set<Project::Cursor> IndexerRParser::cursors(const Path &path) const
{
    return RParserThread::instance()->cursors(path);
}

bool IndexerRParser::codeCompleteAt(const Location &location, const String &source,
                                     Connection *conn)
{
    error() << "Got code complete" << location << source;
    return false;
}

String IndexerRParser::fixits(const Path &/*path*/) const
{
    return String();
}

bool IndexerRParser::isIndexing() const
{
    return RParserThread::instance()->isIndexing();
}

void IndexerRParser::remove(const Path &sourceFile)
{
    RParserThread::instance()->remove(sourceFile);
}

bool IndexerRParser::save(Serializer &serializer)
{
    if (!Server::saveFileIds())
        return false;
    serializer << mProject->sourceInfos();
    return true;
}

bool IndexerRParser::restore(Deserializer &deserializer)
{
    if (!Server::loadFileIds())
        return false;

    SourceInformationMap sources;
    deserializer >> sources;
    mProject->setSourceInfos(sources);

    return true;
}

class IndexerRParserPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<Indexer> init(shared_ptr<Project> project)
    {
        shared_ptr<Indexer> indexer(new IndexerRParser(project));
        mIndexer = indexer;
        return indexer;
    }
    virtual shared_ptr<Indexer> indexer()
    {
        return mIndexer.lock();
    }
    virtual String name() const { return "rparser"; }

private:
    weak_ptr<Indexer> mIndexer;
};

extern "C" RTagsPlugin* createInstance()
{
    return new IndexerRParserPlugin;
}
