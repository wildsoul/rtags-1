#include "DatabaseClang.h"
#include "RTagsPlugin.h"
#include "SourceInformation.h"
#include <rct/LinkedList.h>
#include <rct/MutexLocker.h>
#include <rct/WaitCondition.h>

class ClangParseJob;

class TUWrapper
{
public:
    TUWrapper(CXTranslationUnit u)
        : unit(u)
    {
    }
    ~TUWrapper()
    {
        clang_disposeTranslationUnit(unit);
    }

    CXTranslationUnit unit;
};

class ClangUnit
{
public:
    ClangUnit(DatabaseClang* db);

    void reindex(const SourceInformation& info);
    bool isIndexing() const;
    bool ensureIndexed();

    CXIndex index() { return database->cidx; }
    CXIndexAction action() { return database->caction; }

    DatabaseClang* database;
    mutable Mutex mutex;
    SourceInformation sourceInformation;
    weak_ptr<TUWrapper> unit;
    bool indexed;
    shared_ptr<ClangParseJob> job;

    friend class ClangParseJob;
};

class ClangParseJob : public ThreadPool::Job
{
public:
    ClangParseJob(ClangUnit* unit, bool reparse);
    ~ClangParseJob();

    void wait();

protected:
    virtual void run();

private:
    static int abortQuery(CXClientData client_data, void* /*reserved*/);
    static void diagnostic(CXClientData client_data, CXDiagnosticSet diags, void* /*reserved*/);
    static CXIdxClientFile includedFile(CXClientData client_data, const CXIdxIncludedFileInfo* incl);
    static void indexDeclaration(CXClientData client_data, const CXIdxDeclInfo* decl);
    static void indexEntityReference(CXClientData client_data, const CXIdxEntityRefInfo* ref);

private:
    ClangUnit* mUnit;
    bool mReparse;
    WaitCondition mWait;

private:
    static void addCached(const Path& path, shared_ptr<TUWrapper> unit);
    static void disposeUnit(TUWrapper* unit);

    static Mutex cachedMutex;
    static int maxCached;
    static LinkedList<std::pair<Path, shared_ptr<TUWrapper> > > cached;
};

int ClangParseJob::maxCached = std::max(ThreadPool::idealThreadCount(), 5);
LinkedList<std::pair<Path, shared_ptr<TUWrapper> > > ClangParseJob::cached;
Mutex ClangParseJob::cachedMutex;

ClangUnit::ClangUnit(DatabaseClang* db)
    : database(db), indexed(false)
{
}

ClangParseJob::ClangParseJob(ClangUnit* unit, bool reparse)
    : mUnit(unit), mReparse(reparse)
{
}

ClangParseJob::~ClangParseJob()
{
}

int ClangParseJob::abortQuery(CXClientData client_data, void* /*reserved*/)
{
    return 0;
}

void ClangParseJob::diagnostic(CXClientData client_data, CXDiagnosticSet diags, void* /*reserved*/)
{
}

CXIdxClientFile ClangParseJob::includedFile(CXClientData client_data, const CXIdxIncludedFileInfo* incl)
{
    return 0;
}

void ClangParseJob::indexDeclaration(CXClientData client_data, const CXIdxDeclInfo* decl)
{
}

void ClangParseJob::indexEntityReference(CXClientData client_data, const CXIdxEntityRefInfo* ref)
{
}

void ClangParseJob::disposeUnit(TUWrapper* unit)
{
    MutexLocker locker(&cachedMutex);
    LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::iterator c = cached.begin();
    const LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::const_iterator end = cached.end();
    while (c != end) {
        if (c->second.get() == unit) {
            cached.erase(c);
            return;
        }
        ++c;
    }
}

void ClangParseJob::addCached(const Path& path, shared_ptr<TUWrapper> unit)
{
    MutexLocker locker(&cachedMutex);
    LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::const_iterator c = cached.begin();
    const LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::const_iterator end = cached.end();
    while (c != end) {
        if (c->first == path) {
            assert(c->second.get() == unit.get());
            return;
        }
        ++c;
    }
    cached.push_back(std::make_pair(path, unit));
    if (cached.size() > maxCached) {
        cached.pop_front();
        assert(cached.size() == maxCached);
    }
}

void ClangParseJob::wait()
{
    mWait.wait(&mUnit->mutex);
}

void ClangParseJob::run()
{
    // clang parse
    CXTranslationUnit unit;
    bool done = false;
    if (mReparse) {
        shared_ptr<TUWrapper> unitwrapper = mUnit->unit.lock();
        if (unitwrapper) {
            unit = unitwrapper->unit;
            if (clang_reparseTranslationUnit(unit, 0, 0, clang_defaultReparseOptions(unit)) != 0) {
                // bad
                disposeUnit(unitwrapper.get());
            } else {
                done = true;
            }
        }
    }
    if (!done) {
        const List<SourceInformation::Build>& builds = mUnit->sourceInformation.builds;
        List<SourceInformation::Build>::const_iterator build = builds.begin();
        const List<SourceInformation::Build>::const_iterator end = builds.end();
        while (build != end) {
            List<String> args;
#ifdef CLANG_INCLUDEPATH
            args.append(String("-I") + CLANG_INCLUDEPATH);
#endif

            List<String>::const_iterator define = build->defines.begin();
            const List<String>::const_iterator defineEnd = build->defines.end();
            while (define != defineEnd) {
                args.append("-D" + *define);
                ++define;
            }

            List<Path>::const_iterator include = build->includePaths.begin();
            List<Path>::const_iterator includeEnd = build->includePaths.end();
            while (include != includeEnd) {
                args.append("-I" + *include);
                ++include;
            }

            include = build->includes.begin();
            includeEnd = build->includes.end();
            while (include != includeEnd) {
                args.append("-include " + *include);
                ++include;
            }

            const char* clangArgs[args.size()];
            int clangOffset = 0;
            List<String>::const_iterator arg = args.begin();
            const List<String>::const_iterator argEnd = args.end();
            while (arg != argEnd) {
                clangArgs[clangOffset++] = arg->nullTerminated();
                ++arg;
            }

            //unit = clang_parseTranslationUnit(mUnit->index(), mUnit->sourceInformation.sourceFile.nullTerminated(),
            //                                  clangArgs, args.size(), 0, 0, CXTranslationUnit_DetailedPreprocessingRecord |
            //                                  CXTranslationUnit_PrecompiledPreamble | CXTranslationUnit_CacheCompletionResults);
            IndexerCallbacks callbacks;
            memset(&callbacks, 0, sizeof(IndexerCallbacks));
            callbacks.abortQuery = abortQuery;
            callbacks.diagnostic = diagnostic;
            callbacks.ppIncludedFile = includedFile;
            callbacks.indexDeclaration = indexDeclaration;
            callbacks.indexEntityReference = indexEntityReference;
            const unsigned opts = CXIndexOpt_IndexFunctionLocalSymbols;
            const unsigned tuOpts =
                CXTranslationUnit_DetailedPreprocessingRecord | CXTranslationUnit_PrecompiledPreamble | CXTranslationUnit_CacheCompletionResults;
            clang_indexSourceFile(mUnit->action(), this, &callbacks, sizeof(IndexerCallbacks), opts,
                                  mUnit->sourceInformation.sourceFile.nullTerminated(),
                                  clangArgs, args.size(), 0, 0, &unit, tuOpts);
            ++build;
        }
    }

    error() << "done parsing" << mUnit->sourceInformation.sourceFile << unit << "reparse" << mReparse;

    shared_ptr<TUWrapper> wrapper;
    if (unit) {
        wrapper.reset(new TUWrapper(unit));
        addCached(mUnit->sourceInformation.sourceFile, wrapper);
    }

    MutexLocker locker(&mUnit->mutex);
    mUnit->unit = wrapper;
    mUnit->indexed = (unit != 0);
    mWait.wakeOne();
}

void ClangUnit::reindex(const SourceInformation& info)
{
    MutexLocker locker(&mutex);
    if (job) {
        while (job->state() != ClangParseJob::Finished) {
            job->wait();
        }
    }
    const bool reparse = (sourceInformation == info);
    if (!reparse)
        sourceInformation = info;
    job.reset(new ClangParseJob(this, reparse));
    database->pool.start(job);
}

bool ClangUnit::isIndexing() const
{
    MutexLocker locker(&mutex);
    if (job) {
        if (job->state() != ClangParseJob::Finished)
            return true;
    }
    return false;
}

bool ClangUnit::ensureIndexed()
{
    MutexLocker locker(&mutex);
    if (job) {
        while (job->state() != ClangParseJob::Finished) {
            job->wait();
        }
        job.reset();
    }
    return indexed;
}

DatabaseClang::DatabaseClang()
    : pool(ThreadPool::idealThreadCount())
{
    cidx = clang_createIndex(1, 1);
    caction = clang_IndexAction_create(cidx);
}

DatabaseClang::~DatabaseClang()
{
    clang_IndexAction_dispose(caction);
    clang_disposeIndex(cidx);
}

static inline CXSourceLocation makeCXLocation(CXTranslationUnit unit, const Location& location)
{
    const CXFile file = clang_getFile(unit, location.path().nullTerminated());
    if (!file)
        return clang_getNullLocation();
    return clang_getLocation(unit, file, location.line(), location.column());
}

static inline Location makeLocation(CXCursor cursor)
{
    const CXSourceLocation cxloc = clang_getCursorLocation(cursor);
    if (clang_equalLocations(cxloc, clang_getNullLocation()))
        return Location();
    CXFile file;
    unsigned line, column;
    clang_getSpellingLocation(cxloc, &file, &line, &column, 0);
    CXString fileName = clang_getFileName(file);
    const Location loc(clang_getCString(fileName), line, column);
    clang_disposeString(fileName);
    return loc;
}

static CXVisitorResult referenceCursors(void* ctx, CXCursor cursor, CXSourceRange /*range*/)
{
    List<CXCursor>* cursors = reinterpret_cast<List<CXCursor>*>(ctx);
    cursors->append(cursor);
    return CXVisit_Continue;
}

Database::Cursor DatabaseClang::cursor(const Location &location) const
{
    const Map<Path, ClangUnit*>::const_iterator it = units.find(location.path());
    if (it == units.end())
        return Database::Cursor();
    ClangUnit* unit = it->second;
    if (!unit->ensureIndexed())
        return Database::Cursor();
    shared_ptr<TUWrapper> wrapper = unit->unit.lock();
    if (!wrapper)
        return Database::Cursor();
    const CXCursor cx = clang_getCursor(wrapper->unit, makeCXLocation(wrapper->unit, location));
    const CXCursor cxnull = clang_getNullCursor();
    if (clang_equalCursors(cx, cxnull)) {
        error() << "no cursor for location" << location;
        return Database::Cursor();
    }

    Database::Cursor c;
    c.location = makeLocation(cx);

    if (clang_isCursorDefinition(cx)) {
        error() << "cursor is def, trying to find decl" << location;
        // determine if we can find the declaration
        CXCursor parent = clang_getCursorSemanticParent(cx);
        if (!clang_equalCursors(parent, cxnull)) {
            // see if we can find a declaration cursor that's not also a definition
            CXFile parentFile;
            const CXSourceLocation cxloc = clang_getCursorLocation(parent);
            assert(!clang_equalLocations(cxloc, clang_getNullLocation()));
            clang_getSpellingLocation(cxloc, &parentFile, 0, 0, 0);
            CXCursorAndRangeVisitor referenceVisitor;
            List<CXCursor> refs;
            referenceVisitor.context = &refs;
            referenceVisitor.visit = referenceCursors;
            clang_findReferencesInFile(cx, parentFile, referenceVisitor);
            List<CXCursor>::const_iterator ref = refs.begin();
            const List<CXCursor>::const_iterator end = refs.end();
            while (ref != end) {
                if (clang_isDeclaration(clang_getCursorKind(*ref))
                    && !clang_isCursorDefinition(*ref)) {
                    // yay
                    error() << "cursor is def, found decl" << makeLocation(*ref);
                    c.target = makeLocation(*ref);
                    break;
                }
                ++ref;
            }
        }
    } else {
        error() << "cursor is not def, trying to find def" << location;
        CXCursor def = clang_getCursorDefinition(cx);
        if (clang_equalCursors(def, cxnull)) {
            // need to find it somewhere else

            // if that fails, find the reference
            error() << "def not found, trying to find canonical";
            def = clang_getCanonicalCursor(cx);
            if (!clang_equalCursors(def, cxnull)) {
                error() << "found canonical" << makeLocation(def);
                c.target = makeLocation(def);
            }
        } else {
            c.target = makeLocation(def);
        }
    }

    return c;
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
    ClangUnit* unit;
    const Map<Path, ClangUnit*>::const_iterator it = units.find(sourceInformation.sourceFile);
    if (it == units.end()) {
        unit = new ClangUnit(this);
        units[sourceInformation.sourceFile] = unit;
    } else {
        unit = it->second;
    }
    assert(unit);
    unit->reindex(sourceInformation);
    return -1;
}

void DatabaseClang::remove(const Path &sourceFile)
{
}

bool DatabaseClang::isIndexing() const
{
    /*
    const Map<Path, ClangUnit*>::const_iterator it = units.find(location.path());
    if (it == units.end())
        return false;
    return it->second->isIndexing();
    */

    // ### weird API
    return false;
}

Set<Path> DatabaseClang::dependencies(const Path &path, DependencyMode mode) const
{
    return Set<Path>();
}

Set<Path> DatabaseClang::files(int mode) const
{
    return Set<Path>();
}

Set<String> DatabaseClang::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    return Set<String>();
}

Set<Database::Cursor> DatabaseClang::findCursors(const String &string, const List<Path> &pathFilter) const
{
    return Set<Cursor>();
}

Set<Database::Cursor> DatabaseClang::cursors(const Path &path) const
{
    return Set<Cursor>();
}

bool DatabaseClang::codeCompleteAt(const Location &location, const String &source, Connection *conn)
{
    return false;
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
