#include "DatabaseRParser.h"
#include "SourceInformation.h"
#include <searchsymbols.h>
#include <LookupContext.h>
#include <ASTPath.h>

using namespace CppTools;
using namespace CppTools::Internal;

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

static inline QByteArray tokenForAst(CPlusPlus::AST* ast, CPlusPlus::TranslationUnit* unit,
                                     const QByteArray& src)
{
    const CPlusPlus::Token& start = unit->tokenAt(ast->firstToken());
    const CPlusPlus::Token& last = unit->tokenAt(ast->lastToken() - 1);
    return src.mid(start.begin(), last.end() - start.begin());
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
    const CPlusPlus::Snapshot& snapshot = manager->snapshot();

    CPlusPlus::Document::Ptr doc = snapshot.preprocessedDocument(unpreprocessedSource,
                                                                 symbol->fileName());
    doc->tokenize();

    QList<CPlusPlus::Usage> usages;

    CPlusPlus::Control *control = doc->control();
    if (control->findIdentifier(symbolId->chars(), symbolId->size()) != 0) {
        //doc->setGlobalNamespace(0);
        //doc->check();
        doc->check();

        CPlusPlus::FindUsages process(unpreprocessedSource, doc, snapshot);
        process(symbol);

        usages = process.usages();
    }

    return usages;
}

/*
void processInput(char* line)
{
    char* save;
    char* ret = strtok_r(line, ":", &save);

    QString fn;
    unsigned l, c, cnt = 0;

    while (ret) {
        // process token
        switch (cnt++) {
        case 0:
            fn = QString::fromUtf8(ret);
            break;
        case 1:
        case 2: {
            unsigned* x = (cnt == 2) ? &l : &c;
            *x = atoi(ret);
            break; }
        }
        ret = strtok_r(0, ":", &save);
    }

    Document::Ptr doc = manager->document(fn);
    if (!doc) {
        qWarning("No document for %s", qPrintable(fn));
        return;
    }

    if (cnt == 1) {
        qDebug("looking up sym %s", qPrintable(fn));
        SearchSymbols search;
        search.setSymbolsToSearchFor(SearchSymbols::AllTypes);
        search.setSeparateScope(true);
        QList<ModelItemInfo> items = search(doc);
        foreach(const ModelItemInfo& item, items) {
            qDebug("  got %s (%s)", qPrintable(item.fullyQualifiedName.join("::")), qPrintable(item.symbolType));
        }
        return;
    }

    if (cnt != 3) {
        qWarning("Invalid input %s", line);
        return;
    }

    qDebug("processing %s:%d:%d", qPrintable(fn), l, c);
    TranslationUnit *translationUnit = doc->translationUnit();
    printf("men... %p\n", doc->globalNamespace());
    LookupContext lookup(doc, manager->snapshot());

    TypeOfExpression typeofExpression;
    typeofExpression.init(doc, manager->snapshot(), lookup.bindings());

    printf("men... 2 %p\n", doc->globalNamespace());
    ReallyFindScopeAt really(translationUnit, l, c);
    Scope* scope = really(doc->globalNamespace());
    if (!scope) {
        qWarning("no scope at %d:%d", l, c);
        return;
    }
    //Scope* scope = doc->scopeAt(l, c);

    printf("men... 3 %p\n", doc->globalNamespace());
    QByteArray src = doc->utf8Source();

    ASTPath path(doc);
    QList<AST*> asts = path(l, c);
    qDebug("asts cnt %d", asts.size());
    if (asts.isEmpty()) {
        qWarning("no ast at %d:%d", l, c);
        return;
    }

    Symbol* decl = 0;
    for (int i = asts.size() - 1; i >= 0; --i) {
        typeofExpression.setExpandTemplates(true);
        QByteArray expression = tokenForAst(asts.at(i), translationUnit, src);
        decl = canonicalSymbol(scope, expression, typeofExpression);
        if (decl)
            break;
    }
    if (decl) {
        qDebug() << "finding refs";

        printf("men... 4 %p\n", doc->globalNamespace());
        const QList<Usage> usages = findUsages(decl, src);
        qDebug() << "found refs" << usages.size();
        foreach(const Usage& usage, usages) {
            qDebug("usage %s:%d:%d (%s)", qPrintable(usage.path), usage.line, usage.col, qPrintable(usage.lineText));

            Document::Ptr doc = manager->document(usage.path);
            if (doc) {
                Symbol* refsym = doc->lastVisibleSymbolAt(usage.line, usage.col + 1);
                if (refsym) {
                    if (refsym->line() == static_cast<unsigned>(usage.line) &&
                        refsym->column() == static_cast<unsigned>(usage.col + 1)) {
                        qDebug() << "it's a symbol!";
                    }
                }
            }
        }
    } else {
        // try to find the symbol outright
        Symbol* sym = doc->lastVisibleSymbolAt(l, c);
        if (sym) {
            const Identifier* id = sym->identifier();
            if (id) {
                // ### fryktelig
                if (sym->line() == l && sym->column() <= c &&
                    sym->column() + id->size() >= c) {
                    // yes
                    qDebug("found symbol outright %s at %s:%d:%d", id->chars(), sym->fileName(), sym->line(), sym->column());;

                    qDebug() << "finding refs";

                    const QList<Usage> usages = findUsages(sym, src);
                    foreach(const Usage& usage, usages) {
                        qDebug("usage %s:%d:%d (%s)", qPrintable(usage.path), usage.line, usage.col, qPrintable(usage.lineText));
                    }

                    return;
                }
            }
            // no
        }
    }
}
*/

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
    qDebug("bound %s", qPrintable(doc->fileName()));
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
    QStringList incs, defs;
    List<SourceInformation::Build>::const_iterator build = info.builds.begin();
    const List<SourceInformation::Build>::const_iterator end = info.builds.end();
    while (build != end) {
        preprocessor.setIncludePaths(toQStringList(build->includePaths));
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

CPlusPlus::Symbol* DatabaseRParser::findSymbol(CPlusPlus::Document::Ptr doc,
                                               const Location& loc,
                                               const QByteArray& src) const
{
    const unsigned line = loc.line();
    const unsigned column = loc.column();

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
                }
            }
        }
        // no
    }

    if (!sym) {
        // See if we can parse it:
        CPlusPlus::ASTPath path(doc);
        QList<CPlusPlus::AST*> asts = path(line, column);
        if (!asts.isEmpty()) {
            CPlusPlus::AST* ast = asts.last(); // just try the last one

            CPlusPlus::LookupContext lookup(doc, manager->snapshot());
            CPlusPlus::TypeOfExpression typeofExpression;
            typeofExpression.init(doc, manager->snapshot(), lookup.bindings());
            typeofExpression.setExpandTemplates(true);

            CPlusPlus::TranslationUnit* translationUnit = doc->translationUnit();
            ReallyFindScopeAt really(translationUnit, line, column);
            CPlusPlus::Scope* scope = really(doc->globalNamespace());

            const QByteArray expression = tokenForAst(ast, translationUnit, src);
            sym = canonicalSymbol(scope, expression, typeofExpression);
        }
    }

    return sym;
}

Database::Cursor DatabaseRParser::cursor(const Location &location) const
{
    CPlusPlus::Document::Ptr doc = manager->document(QString::fromStdString(location.path()));
    assert(doc != 0);

    const QByteArray& src = doc->utf8Source();

    CPlusPlus::Symbol* sym = findSymbol(doc, location, src);
    if (!sym)
        return Cursor();

    Cursor cursor;

    const QList<CPlusPlus::Usage> usages = findUsages(manager, sym, src);
    foreach(const CPlusPlus::Usage& usage, usages) {
        CPlusPlus::Document::Ptr doc = manager->document(usage.path);
        if (doc) {
            CPlusPlus::Symbol* refsym = doc->lastVisibleSymbolAt(usage.line, usage.col + 1);
            if (refsym) {
                if (refsym->line() == static_cast<unsigned>(usage.line) &&
                    refsym->column() == static_cast<unsigned>(usage.col + 1)) {
                }
            }
        }
    }

    return Cursor();
}

DatabaseRParser::DatabaseRParser()
{
    manager = new CppModelManager;
    parser = new DocumentParser(manager);
    QObject::connect(manager.data(), SIGNAL(documentUpdated(CPlusPlus::Document::Ptr)),
                     parser, SLOT(onDocumentUpdated(CPlusPlus::Document::Ptr)));
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

void DatabaseRParser::status(const String &query, Connection *conn) const
{
}

void DatabaseRParser::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
}

int DatabaseRParser::index(const SourceInformation &sourceInformation)
{
    RParserUnit* unit = findUnit(sourceInformation.sourceFile);
    if (!unit) {
        unit = new RParserUnit;
        unit->info = sourceInformation;
        units[sourceInformation.sourceFile] = unit;
    }
    unit->reindex(manager);
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
