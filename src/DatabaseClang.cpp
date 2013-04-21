#include "DatabaseClang.h"
#include "RTagsPlugin.h"
#include "SourceInformation.h"
#include <rct/MutexLocker.h>
#include <rct/WaitCondition.h>

class ClangJob;

class ClangUnit
{
public:
    ClangUnit(DatabaseClang* db);

    void reindex(const SourceInformation& info);
    bool isIndexing() const;
    bool ensureIndexed();

    CXIndex index() { return database->cidx; }

    DatabaseClang* database;
    mutable Mutex mutex;
    SourceInformation sourceInformation;
    CXTranslationUnit unit;
    shared_ptr<ClangJob> job;

    friend class ClangJob;
};

class ClangJob : public ThreadPool::Job
{
public:
    ClangJob(ClangUnit* unit, bool reparse);
    ~ClangJob();

    void wait();

protected:
    virtual void run();

private:
    ClangUnit* mUnit;
    bool mReparse;
    WaitCondition mWait;
};

ClangUnit::ClangUnit(DatabaseClang* db)
    : database(db), job(0)
{
}

ClangJob::ClangJob(ClangUnit* unit, bool reparse)
    : mUnit(unit), mReparse(reparse)
{
}

ClangJob::~ClangJob()
{
}

void ClangJob::wait()
{
    mWait.wait(&mUnit->mutex);
}

void ClangJob::run()
{
    // clang parse
    CXTranslationUnit unit;
    if (mReparse) {
        unit = mUnit->unit;
        assert(unit != 0);
        if (clang_reparseTranslationUnit(unit, 0, 0, clang_defaultReparseOptions(unit)) != 0) {
            // bad
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
    } else {
        const List<SourceInformation::Build>& builds = mUnit->sourceInformation.builds;
        List<SourceInformation::Build>::const_iterator build = builds.begin();
        const List<SourceInformation::Build>::const_iterator end = builds.end();
        while (build != end) {
            List<String> args;

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
            unit = clang_parseTranslationUnit(mUnit->index(), mUnit->sourceInformation.sourceFile.nullTerminated(),
                                              clangArgs, args.size(), 0, 0, CXTranslationUnit_DetailedPreprocessingRecord |
                                              CXTranslationUnit_PrecompiledPreamble | CXTranslationUnit_CacheCompletionResults);
            ++build;
        }
    }

    MutexLocker locker(&mUnit->mutex);
    mUnit->unit = unit;
    mWait.wakeOne();
}

void ClangUnit::reindex(const SourceInformation& info)
{
    MutexLocker locker(&mutex);
    if (job) {
        while (job->state() != ClangJob::Finished) {
            job->wait();
        }
    }
    const bool reparse = (sourceInformation == info);
    if (!reparse)
        sourceInformation = info;
    job.reset(new ClangJob(this, reparse));
    database->pool.start(job);
}

bool ClangUnit::isIndexing() const
{
    MutexLocker locker(&mutex);
    if (job) {
        if (job->state() != ClangJob::Finished)
            return true;
    }
    return false;
}

bool ClangUnit::ensureIndexed()
{
    MutexLocker locker(&mutex);
    if (job) {
        while (job->state() != ClangJob::Finished) {
            job->wait();
        }
        job.reset();
    }
    return (unit != 0);
}

DatabaseClang::DatabaseClang()
    : pool(ThreadPool::idealThreadCount())
{
    cidx = clang_createIndex(1, 1);
}

DatabaseClang::~DatabaseClang()
{
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
    const CXCursor cx = clang_getCursor(unit->unit, makeCXLocation(unit->unit, location));
    const CXCursor cxnull = clang_getNullCursor();
    if (clang_equalCursors(cx, cxnull))
        return Database::Cursor();

    Database::Cursor c;
    c.location = makeLocation(cx);

    if (clang_isCursorDefinition(cx)) {
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
                    c.target = makeLocation(*ref);
                    break;
                }
                ++ref;
            }
        }
    } else {
        CXCursor def = clang_getCursorDefinition(cx);
        if (clang_equalCursors(def, cxnull)) {
            // need to find it somewhere else
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
