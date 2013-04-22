#include "DatabaseClang.h"
#include "RTagsPlugin.h"
#include "SourceInformation.h"
#include "QueryMessage.h"
#include <rct/Connection.h>
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

struct ClangIndexInfo
{
    LinkedList<Path> files;

    Map<Location, Path> incs;
    DependSet depends, reverseDepends;
    Map<String, Set<String> > names;  // name->usr
    Map<Location, CursorInfo> usrs; // location->usr
    UsrSet decls, defs, refs;   // usr->locations
    VirtualSet virtuals; // usr->usrs

    void clear()
    {
        incs.clear();
        depends.clear();
        reverseDepends.clear();
        names.clear();
        usrs.clear();
        decls.clear();
        defs.clear();
        refs.clear();
        virtuals.clear();
    }

    Mutex mutex;
    bool stopped;
};

class ClangUnit
{
public:
    ClangUnit(DatabaseClang* db);

    void reindex(const SourceInformation& info);
    bool ensureIndexed();

    CXIndex index() { return database->cidx; }
    CXIndexAction action() { return database->caction; }

    enum MergeMode { Dirty, Add };
    void merge(const ClangIndexInfo& info, MergeMode mode);
    void dirty(const Path& path);

    DatabaseClang* database;
    mutable Mutex mutex;
    SourceInformation sourceInformation;
    weak_ptr<TUWrapper> unit;
    bool indexed;
    shared_ptr<ClangParseJob> job;
};

class ClangParseJob : public ThreadPool::Job
{
public:
    ClangParseJob(ClangUnit* unit, bool reparse);
    ~ClangParseJob();

    void wait();
    void stop();

protected:
    virtual void run();

private:
    static int abortQuery(CXClientData client_data, void* /*reserved*/);
    static void diagnostic(CXClientData client_data, CXDiagnosticSet diags, void* /*reserved*/);
    static CXIdxClientFile enteredMainFile(CXClientData client_data, CXFile mainFile, void* /*reserved*/);
    static CXIdxClientFile includedFile(CXClientData client_data, const CXIdxIncludedFileInfo* incl);
    static void indexDeclaration(CXClientData client_data, const CXIdxDeclInfo* decl);
    static void indexEntityReference(CXClientData client_data, const CXIdxEntityRefInfo* ref);
    static void indexArguments(ClangIndexInfo* info, const CXCursor& cursor);

private:
    ClangUnit* mUnit;
    bool mReparse;
    WaitCondition mWait;
    ClangIndexInfo mInfo;

private:
    static shared_ptr<TUWrapper> addCached(const Path& path, CXTranslationUnit unit, bool reparsed);
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

static inline void dirtyUsr(const Location& start, const String& usr, UsrSet& usrs)
{
    UsrSet::iterator entry = usrs.find(usr);
    if (entry == usrs.end())
        return;
    const Path& startPath = start.path();
    Set<Location>::iterator loc = entry->second.lower_bound(start);
    while (loc != entry->second.end() && loc->path() == startPath) {
        entry->second.erase(loc++);
    }
}

// should only be called with database->mutex locked
void ClangUnit::dirty(const Path& path)
{
    const Location start(path, 1, 1);
    {
        Map<Location, Path>::iterator inc = database->incs.lower_bound(start);
        const Map<Location, Path>::const_iterator end = database->incs.end();
        while (inc != end && inc->first.path() == path) {
            database->incs.erase(inc++);
        }
    }
    {
        Map<Location, CursorInfo>::iterator usr = database->usrs.lower_bound(start);
        while (usr != database->usrs.end() && usr->first.path() == path) {
            dirtyUsr(start, usr->second.usr, database->decls);
            dirtyUsr(start, usr->second.usr, database->defs);
            dirtyUsr(start, usr->second.usr, database->refs);
            database->usrs.erase(usr++);
        }
    }
    {
        // remove headers?
        DependSet::iterator dep = database->depends.find(path);
        if (dep != database->depends.end())
            database->depends.erase(dep);
    }
    {
        DependSet::iterator dep = database->reverseDepends.begin();
        while (dep != database->reverseDepends.end()) {
            Set<Path>& set = dep->second;
            if (set.remove(path)) {
                if (set.isEmpty())
                    database->reverseDepends.erase(dep++);
                else
                    ++dep;
            } else {
                ++dep;
            }
        }
    }
}

void ClangUnit::merge(const ClangIndexInfo& info, MergeMode mode)
{
    MutexLocker locker(&database->mutex);

    --database->pendingJobs;

    if (mode == Dirty)
        dirty(sourceInformation.sourceFile);

    database->incs.unite(info.incs);
    database->usrs.unite(info.usrs);

    {
        Map<String, Set<String> >::const_iterator name = info.names.begin();
        const Map<String, Set<String> >::const_iterator end = info.names.end();
        while (name != end) {
            database->names[name->first].unite(name->second);
            ++name;
        }
    }
    {
        const UsrSet* src[] = { &info.decls, &info.defs, &info.refs, 0 };
        UsrSet* dst[] = { &database->decls, &database->defs, &database->refs, 0 };
        for (unsigned i = 0; src[i]; ++i) {
            UsrSet::const_iterator usr = src[i]->begin();
            const UsrSet::const_iterator end = src[i]->end();
            while (usr != end) {
                (*dst[i])[usr->first].unite(usr->second);
                ++usr;
            }
        }
    }
    {
        const DependSet* src[] = { &info.depends, &info.reverseDepends, 0 };
        DependSet* dst[] = { &database->depends, &database->reverseDepends, 0 };
        for (unsigned i = 0; src[i]; ++i) {
            DependSet::const_iterator usr = src[i]->begin();
            const DependSet::const_iterator end = src[i]->end();
            while (usr != end) {
                (*dst[i])[usr->first].unite(usr->second);
                ++usr;
            }
        }
    }

    VirtualSet::const_iterator virt = info.virtuals.begin();
    const VirtualSet::const_iterator end = info.virtuals.end();
    while (virt != end) {
        database->virtuals[virt->first].unite(virt->second);
        ++virt;
    }
}

ClangParseJob::ClangParseJob(ClangUnit* unit, bool reparse)
    : mUnit(unit), mReparse(reparse)
{
    mInfo.stopped = false;
}

ClangParseJob::~ClangParseJob()
{
}

static inline Location makeLocation(const CXIdxLoc& cxloc)
{
    CXIdxClientFile file = 0;
    unsigned line, column;
    clang_indexLoc_getFileLocation(cxloc, &file, 0, &line, &column, 0);
    assert(file != 0);
    const Path* path = static_cast<Path*>(file);
    return Location(*path, line, column);
}

static inline Location makeLocation(const CXCursor& cursor)
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

static inline Database::Cursor::Kind makeKind(CXIdxEntityKind cxkind, bool def)
{
    switch (cxkind) {
    case CXIdxEntity_CXXClass:
        if (def)
            return Database::Cursor::Class;
        return Database::Cursor::ClassForwardDeclaration;
    case CXIdxEntity_CXXNamespace:
        return Database::Cursor::Namespace;
    case CXIdxEntity_CXXInstanceMethod:
    case CXIdxEntity_CXXConstructor:
    case CXIdxEntity_CXXDestructor:
    case CXIdxEntity_CXXStaticMethod:
        if (def)
            return Database::Cursor::MemberFunctionDefinition;
        return Database::Cursor::MemberFunctionDeclaration;
    case CXIdxEntity_Function:
        if (def)
            return Database::Cursor::MethodDefinition;
        return Database::Cursor::MethodDeclaration;
    case CXIdxEntity_Struct:
        if (def)
            return Database::Cursor::Struct;
        return Database::Cursor::StructForwardDeclaration;
    case CXIdxEntity_Enum:
        return Database::Cursor::Enum;
    case CXIdxEntity_EnumConstant:
        return Database::Cursor::EnumValue;
    case CXIdxEntity_Variable:
    case CXIdxEntity_CXXStaticVariable:
        return Database::Cursor::Variable;
    case CXIdxEntity_Field:
        return Database::Cursor::Field;
    case CXIdxEntity_Union:
        return Database::Cursor::Union;
    case CXIdxEntity_Unexposed:
    case CXIdxEntity_Typedef:
    case CXIdxEntity_ObjCClass:
    case CXIdxEntity_ObjCProtocol:
    case CXIdxEntity_ObjCCategory:
    case CXIdxEntity_ObjCInstanceMethod:
    case CXIdxEntity_ObjCClassMethod:
    case CXIdxEntity_ObjCProperty:
    case CXIdxEntity_ObjCIvar:
    case CXIdxEntity_CXXConversionFunction:
    case CXIdxEntity_CXXNamespaceAlias:
    case CXIdxEntity_CXXTypeAlias:
    case CXIdxEntity_CXXInterface:
        break;
    }
    return Database::Cursor::Invalid;
}

int ClangParseJob::abortQuery(CXClientData client_data, void* /*reserved*/)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    MutexLocker locker(&info->mutex);
    return info->stopped ? 1 : 0;
}

void ClangParseJob::diagnostic(CXClientData client_data, CXDiagnosticSet diags, void* /*reserved*/)
{
}

CXIdxClientFile ClangParseJob::enteredMainFile(CXClientData client_data, CXFile mainFile, void* /*reserved*/)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    CXString str = clang_getFileName(mainFile);
    info->files.push_back(clang_getCString(str));
    clang_disposeString(str);
    return &info->files.back();
}

CXIdxClientFile ClangParseJob::includedFile(CXClientData client_data, const CXIdxIncludedFileInfo* incl)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    CXString str = clang_getFileName(incl->file);
    const Path path(clang_getCString(str));
    clang_disposeString(str);
    info->files.push_back(path);
    const Location loc = makeLocation(incl->hashLoc);
    info->depends[loc.path()].insert(path);
    info->reverseDepends[path].insert(loc.path());
    info->incs[loc] = path;
    return &info->files.back();
}

static inline String makeUsr(const CXCursor& cursor)
{
    CXString str = clang_getCursorUSR(cursor);
    const String usr(clang_getCString(str));
    clang_disposeString(str);
    return usr;
}

static CXChildVisitResult argumentVisistor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    switch (clang_getCursorKind(parent)) {
    case CXCursor_ParmDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
        break;
    default:
        return CXChildVisit_Break;
    }
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_TypeRef: {
        // do stuff
        ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
        const Location refLoc = makeLocation(cursor);
        const String usr = makeUsr(clang_getCursorReferenced(cursor));

        CursorInfo cursorInfo;
        cursorInfo.usr = usr;
        cursorInfo.kind = Database::Cursor::Reference;
        info->usrs[refLoc] = cursorInfo;

        //error() << "indexing ref" << usr << refLoc;

        info->refs[usr].insert(refLoc);
        break; }
    case CXCursor_ParmDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
        return CXChildVisit_Recurse;
    default:
        break;
    }
    return CXChildVisit_Continue;
}

void ClangParseJob::indexArguments(ClangIndexInfo* info, const CXCursor& cursor)
{
    clang_visitChildren(cursor, argumentVisistor, info);
}

static inline void addNamePermutations(CXCursor cursor, const String& usr, Map<String, Set<String> >& names)
{
    List<String> subnames;
    unsigned res = 0;
    for (;;) {
        if (clang_isDeclaration(clang_getCursorKind(cursor))) {
            CXString cxname = clang_getCursorSpelling(cursor);
            String name(clang_getCString(cxname));
            clang_disposeString(cxname);
            if (!name.isEmpty()) {
                subnames.append(name);
                res += name.size();
            }
        }
        cursor = clang_getCursorSemanticParent(cursor);
        if (clang_equalCursors(cursor, clang_getNullCursor()))
            break;
    }

    if (subnames.isEmpty())
        return;

    String current;
    current.reserve(res + ((subnames.size() - 1) * 2));
    List<String>::const_iterator n = subnames.begin();
    const List<String>::const_iterator end = subnames.end();
    while (n != end) {
        if (!current.isEmpty())
            current.prepend("::");
        current.prepend(*n);
        names[current].insert(usr);
        ++n;
    }
}

void ClangParseJob::indexDeclaration(CXClientData client_data, const CXIdxDeclInfo* decl)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    const bool def = decl->isDefinition;
    const Location declLoc = makeLocation(decl->loc);
    const String usr(decl->entityInfo->USR);

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = makeKind(decl->entityInfo->kind, def);
    info->usrs[declLoc] = cursorInfo;

    //error() << "indexing" << (def ? "def" : "decl") << decl->entityInfo->kind << usr << declLoc;
    switch (decl->entityInfo->kind) {
    case CXIdxEntity_CXXInstanceMethod:
        if (clang_CXXMethod_isVirtual(decl->cursor)) {
            //error() << "virtual at" << makeLocation(decl->cursor);
            CXCursor* overridden;
            unsigned num;
            clang_getOverriddenCursors(decl->cursor, &overridden, &num);
            if (num) {
                String virtusr;
                for (unsigned i = 0; i < num; ++i) {
                    virtusr = makeUsr(overridden[i]);
                    //error() << "overridden at" << makeLocation(overridden[i]) << virtusr;
                    info->virtuals[usr].insert(virtusr);
                    info->virtuals[virtusr].insert(usr);
                }
                clang_disposeOverriddenCursors(overridden);
            }
        }
        // fall through
    case CXIdxEntity_CXXStaticMethod:
    case CXIdxEntity_CXXConstructor:
    case CXIdxEntity_Function:
        indexArguments(info, decl->cursor);
    default:
        break;
    }

    addNamePermutations(decl->cursor, usr, info->names);

    if (def)
        info->defs[usr].insert(declLoc);
    else
        info->decls[usr].insert(declLoc);
}

void ClangParseJob::indexEntityReference(CXClientData client_data, const CXIdxEntityRefInfo* ref)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    const Location refLoc = makeLocation(ref->loc);
    const String usr(ref->referencedEntity->USR);

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = Database::Cursor::Reference;
    info->usrs[refLoc] = cursorInfo;

    //error() << "indexing ref" << usr << refLoc;

    info->refs[usr].insert(refLoc);
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

shared_ptr<TUWrapper> ClangParseJob::addCached(const Path& path, CXTranslationUnit unit, bool reparsed)
{
    MutexLocker locker(&cachedMutex);
    LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::iterator c = cached.begin();
    const LinkedList<std::pair<Path, shared_ptr<TUWrapper> > >::const_iterator end = cached.end();
    while (c != end) {
        if (c->first == path) {
            shared_ptr<TUWrapper> result = c->second;
            if (!reparsed) {
                if (c->second->unit != unit) {
                    cached.erase(c);
                    result.reset(new TUWrapper(unit));
                    cached.push_back(std::make_pair(path, result));
                }
                return result;
            }
            assert(c->second->unit == unit);
            return result;
        }
        ++c;
    }
    shared_ptr<TUWrapper> result(new TUWrapper(unit));
    cached.push_back(std::make_pair(path, result));
    if (cached.size() > maxCached) {
        cached.pop_front();
        assert(cached.size() == maxCached);
    }
    return result;
}

void ClangParseJob::stop()
{
    MutexLocker locker(&mInfo.mutex);
    mInfo.stopped = true;
}

void ClangParseJob::wait()
{
    mWait.wait(&mUnit->mutex);
}

void ClangParseJob::run()
{
    {
        MutexLocker locker(&mInfo.mutex);
        if (mInfo.stopped) {
            MutexLocker locker(&mUnit->mutex);
            mWait.wakeOne();
            return;
        }
    }

    // clang parse
    CXTranslationUnit unit;
    if (mReparse) {
        // ### should handle multiple builds here
        shared_ptr<TUWrapper> unitwrapper = mUnit->unit.lock();
        if (unitwrapper) {
            unit = unitwrapper->unit;
            if (clang_reparseTranslationUnit(unit, 0, 0, clang_defaultReparseOptions(unit)) != 0) {
                // bad
                disposeUnit(unitwrapper.get());
                mReparse = false;
            } else {
                IndexerCallbacks callbacks;
                memset(&callbacks, 0, sizeof(IndexerCallbacks));
                callbacks.abortQuery = abortQuery;
                callbacks.diagnostic = diagnostic;
                callbacks.enteredMainFile = enteredMainFile;
                callbacks.ppIncludedFile = includedFile;
                callbacks.indexDeclaration = indexDeclaration;
                callbacks.indexEntityReference = indexEntityReference;
                const unsigned opts = CXIndexOpt_IndexFunctionLocalSymbols;

                if (clang_indexTranslationUnit(mUnit->action(), &mInfo, &callbacks, sizeof(IndexerCallbacks), opts, unit)) {
                    mInfo.clear();
                }
                mUnit->merge(mInfo, ClangUnit::Dirty);
            }
        } else {
            mReparse = false;
        }
    }
    if (!mReparse) {
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

            IndexerCallbacks callbacks;
            memset(&callbacks, 0, sizeof(IndexerCallbacks));
            callbacks.abortQuery = abortQuery;
            callbacks.diagnostic = diagnostic;
            callbacks.enteredMainFile = enteredMainFile;
            callbacks.ppIncludedFile = includedFile;
            callbacks.indexDeclaration = indexDeclaration;
            callbacks.indexEntityReference = indexEntityReference;
            const unsigned opts = CXIndexOpt_IndexFunctionLocalSymbols;
            const unsigned tuOpts =
                CXTranslationUnit_DetailedPreprocessingRecord | CXTranslationUnit_PrecompiledPreamble | CXTranslationUnit_CacheCompletionResults;

            if (clang_indexSourceFile(mUnit->action(), &mInfo, &callbacks, sizeof(IndexerCallbacks), opts,
                                      mUnit->sourceInformation.sourceFile.nullTerminated(),
                                      clangArgs, args.size(), 0, 0, &unit, tuOpts)) {
                mInfo.clear();
            }

            mUnit->merge(mInfo, build == builds.begin() ? ClangUnit::Dirty : ClangUnit::Add);

            ++build;
        }
    }

    error() << "done parsing" << mUnit->sourceInformation.sourceFile << unit << "reparse" << mReparse;

    shared_ptr<TUWrapper> wrapper;
    if (unit) {
        wrapper = addCached(mUnit->sourceInformation.sourceFile, unit, mReparse);
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
            if (!database->pool.remove(job)) {
                job->stop();
                job->wait();
            }
        }
    }
    const bool reparse = (sourceInformation == info);
    if (!reparse)
        sourceInformation = info;
    job.reset(new ClangParseJob(this, reparse));
    database->pool.start(job);
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
    : pool(std::max(ThreadPool::idealThreadCount(), 3)), pendingJobs(0)
{
    cidx = clang_createIndex(1, 1);
    caction = clang_IndexAction_create(cidx);
}

DatabaseClang::~DatabaseClang()
{
    clang_IndexAction_dispose(caction);
    clang_disposeIndex(cidx);
}

Database::Cursor DatabaseClang::cursor(const Location &location) const
{
    MutexLocker locker(&mutex);
    Map<Location, CursorInfo>::const_iterator usr = usrs.lower_bound(location);
    if (usr == usrs.end())
        return Database::Cursor();
    if (usr->first > location) { // we're looking for the previous one
        if (usr == usrs.begin())
            return Database::Cursor();
        --usr;
        if (usr->first.path() != location.path()) {
            // we've iterated past the beginning of the file
            return Database::Cursor();
        }
    }
    assert(!(usr->first > location));

    const String usrstr = usr->second.usr;

    Database::Cursor cursor;
    cursor.location = usr->first;
    cursor.kind = usr->second.kind;

    if (cursor.kind == Database::Cursor::Reference) {
        // reference, target should be definition (if possible)
        UsrSet::const_iterator target = defs.find(usrstr);
        if (target == defs.end()) {
            // try declaration
            target = decls.find(usrstr);
            if (target != decls.end()) {
                if (!target->second.isEmpty())
                    cursor.target = *target->second.begin();
            }
        } else {
            if (!target->second.isEmpty())
                cursor.target = *target->second.begin();
        }
    } else if (cursor.isDefinition()) {
        // definition, target should be declaration
        const UsrSet::const_iterator target = decls.find(usrstr);
        if (target != decls.end()) {
            if (!target->second.isEmpty())
                cursor.target = *target->second.begin();
        }
    } else {
        // declaration, taget should be definition
        const UsrSet::const_iterator target = defs.find(usrstr);
        if (target != defs.end()) {
            if (!target->second.isEmpty())
                cursor.target = *target->second.begin();
        }
    }
    return cursor;
}

void DatabaseClang::writeReferences(const String& usr, Connection* conn) const
{
    const UsrSet::const_iterator ref = refs.find(usr);
    if (ref != refs.end()) {
        Set<Location>::const_iterator loc = ref->second.begin();
        const Set<Location>::const_iterator end = ref->second.end();
        while (loc != end) {
            conn->write<256>("%s:%d:%d %c\t", loc->path().nullTerminated(), loc->line(), loc->column(), 'r');
            ++loc;
        }
    }
}

void DatabaseClang::writeDeclarations(const String& usr, Connection* conn) const
{
    const UsrSet* usrs[] = { &decls, &defs, 0 };
    for (int i = 0; usrs[i]; ++i) {
        const UsrSet::const_iterator decl = usrs[i]->find(usr);
        if (decl != usrs[i]->end()) {
            Set<Location>::const_iterator loc = decl->second.begin();
            const Set<Location>::const_iterator end = decl->second.end();
            while (loc != end) {
                conn->write<256>("%s:%d:%d %c\t", loc->path().nullTerminated(), loc->line(), loc->column(), 'r');
                ++loc;
            }
        }
    }
}

void DatabaseClang::references(const Location& location, unsigned queryFlags,
                               const List<Path> &pathFilter, Connection *conn) const
{
#warning need to respect pathFilter
    const bool wantVirtuals = queryFlags & QueryMessage::FindVirtuals;
    const bool wantAll = queryFlags & QueryMessage::AllReferences;

    MutexLocker locker(&mutex);
    Map<Location, CursorInfo>::const_iterator usr = usrs.lower_bound(location);
    if (usr == usrs.end()) {
        conn->write("`");
        return;
    }
    if (usr->first > location) { // we're looking for the previous one
        if (usr == usrs.begin()) {
            conn->write("`");
            return;
        }
        --usr;
        if (usr->first.path() != location.path()) {
            // we've iterated past the beginning of the file
            conn->write("`");
            return;
        }
    }
    assert(!(usr->first > location));

    const String usrstr = usr->second.usr;

    if (wantAll || !wantVirtuals) {
        writeReferences(usrstr, conn);
        if (wantAll)
            writeDeclarations(usrstr, conn);
    }
    if (wantVirtuals) {
        if (wantAll)
            writeReferences(usrstr, conn);
        writeDeclarations(usrstr, conn);

        const VirtualSet::const_iterator virt = virtuals.find(usrstr);
        Set<String>::const_iterator vusr = virt->second.begin();
        const Set<String>::const_iterator vend = virt->second.end();
        while (vusr != vend) {
            if (wantAll)
                writeReferences(*vusr, conn);
            writeDeclarations(*vusr, conn);
            ++vusr;
        }
    }
    conn->write("`");
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
    {
        MutexLocker locker(&mutex);
        ++pendingJobs;
    }
    unit->reindex(sourceInformation);
    return -1;
}

void DatabaseClang::remove(const Path &sourceFile)
{
}

bool DatabaseClang::isIndexing() const
{
    MutexLocker locker(&mutex);
    return (pendingJobs > 0);
}

static inline void addDeps(const Path& path, const DependSet& deps, Set<Path>& result)
{
    DependSet::const_iterator dep = deps.find(path);
    if (dep != deps.end()) {
        Set<Path>::const_iterator path = dep->second.begin();
        const Set<Path>::const_iterator end = dep->second.end();
        while (path != end) {
            if (!result.contains(*path)) {
                result.insert(*path);
                addDeps(*path, deps, result);
            }
            ++path;
        }
    }
}

Set<Path> DatabaseClang::dependencies(const Path &path, DependencyMode mode) const
{
    Set<Path> result;
    result.insert(path); // all files depend on themselves

    MutexLocker locker(&mutex);

    if (mode == ArgDependsOn) {
        addDeps(path, depends, result);
    } else {
        addDeps(path, reverseDepends, result);
    }
    return result;
}

Set<Path> DatabaseClang::files(int mode) const
{
    return Set<Path>();
}

Set<String> DatabaseClang::listSymbols(const String &string, const List<Path> &pathFilter) const
{
#warning need to respect pathFilter
    Set<String> result;

    MutexLocker locker(&mutex);
    Map<String, Set<String> >::const_iterator name = names.lower_bound(string);
    const Map<String, Set<String> >::const_iterator end = names.end();
    while (name != end && name->first.startsWith(string)) {
        result.insert(name->first);
        ++name;
    }

    return result;
}

static inline Location firstLocation(const String& usr, const UsrSet& set)
{
    const UsrSet::const_iterator it = set.find(usr);
    if (it == set.end())
        return Location();
    const Set<Location>& locs = it->second;
    if (locs.isEmpty())
        return Location();
    return *locs.begin();
}

Set<Database::Cursor> DatabaseClang::findCursors(const String &string, const List<Path> &pathFilter) const
{
#warning need to respect pathFilter
    MutexLocker locker(&mutex);
    Map<String, Set<String> >::const_iterator name = names.find(string);
    if (name == names.end())
        return Set<Cursor>();

    Set<Cursor> cursors;

    Set<String>::const_iterator usr = name->second.begin();
    const Set<String>::const_iterator end = name->second.end();
    while (usr != end) {
        const UsrSet* usrs[] = { &decls, &defs, 0 };
        for (int i = 0; usrs[i]; ++i) {
            const UsrSet::const_iterator decl = usrs[i]->find(*usr);
            if (decl != usrs[i]->end()) {
                Set<Location>::const_iterator loc = decl->second.begin();
                const Set<Location>::const_iterator end = decl->second.end();
                while (loc != end) {
                    Map<Location, CursorInfo>::const_iterator info = DatabaseClang::usrs.find(*loc);
                    if (info != DatabaseClang::usrs.end()) {
                        Cursor cursor;
                        cursor.symbolName = name->first;
                        cursor.location = *loc;
                        cursor.target = firstLocation(*usr, usrs[i] == &decls ? defs : decls);
                        cursor.kind = info->second.kind;
                        cursors.insert(cursor);
                    }
                    ++loc;
                }
            }
        }
        ++usr;
    }

    return cursors;
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
