#include "DatabaseRParser.h"
#include "SourceInformation.h"
#include <searchsymbols.h>
#include <LookupContext.h>
#include <ASTPath.h>

using namespace CppTools;
using namespace CppTools::Internal;

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

QByteArray DocumentParser::tokenForAst(CPlusPlus::AST* ast, CPlusPlus::TranslationUnit* unit, const QByteArray& src)
{
    const CPlusPlus::Token& start = unit->tokenAt(ast->firstToken());
    const CPlusPlus::Token& last = unit->tokenAt(ast->lastToken() - 1);
    return src.mid(start.begin(), last.end() - start.begin());
}

QByteArray DocumentParser::debugScope(CPlusPlus::Scope* scope, const QByteArray& src)
{
    return src.mid(scope->startOffset(), scope->endOffset() - scope->startOffset());
}

QList<CPlusPlus::Usage> DocumentParser::findUsages(CPlusPlus::Symbol* symbol, const QByteArray& unpreprocessedSource)
{
    const CPlusPlus::Identifier *symbolId = symbol->identifier();
    const CPlusPlus::Snapshot& snapshot = manager->snapshot();

    CPlusPlus::Document::Ptr doc = snapshot.preprocessedDocument(unpreprocessedSource, symbol->fileName());
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

    // if (seen.contains(canonical))
    //     return;
    // seen.insert(canonical);

    //QElapsedTimer timer;
    //timer.start();
    //LookupContext lookup(doc, manager->snapshot());
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
    //bind(ast, globalNamespace);
    //doc->check();
    //Visitor visitor(manager, lookup);
    //visitor.accept(globalNamespace);
    //qDebug("accepted %s %d", qPrintable(doc->fileName()), visitor.symbolCount);
    qDebug("bound %s", qPrintable(doc->fileName()));
    //symbolCount += visitor.symbolCount;
}

class RParserUnit
{
public:
    SourceInformation info;
    CPlusPlus::Document::Ptr doc;

    void reindex(QPointer<CppModelManager> manager);
};

void RParserUnit::reindex(QPointer<CppModelManager> manager)
{
    CppPreprocessor preprocessor(manager);

}

RParserUnit* DatabaseRParser::findUnit(const Path& path)
{
    Map<Path, RParserUnit*>::const_iterator it = units.find(path);
    if (it == units.end())
        return 0;
    return it->second;
}

Database::Cursor DatabaseRParser::cursor(const Location &location) const
{
    return Cursor();
}

DatabaseRParser::DatabaseRParser()
{
    manager = new CppModelManager;
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
