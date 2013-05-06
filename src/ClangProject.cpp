#include "ClangProject.h"
#include "ClangCompletionJob.h"
#include "RTagsPlugin.h"
#include "SourceInformation.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "Server.h"
#include <rct/Connection.h>
#include <rct/MutexLocker.h>
#include <rct/RegExp.h>
#include <rct/ThreadPool.h>
#include <rct/WaitCondition.h>

class ClangParseJob;

struct ClangIndexInfo
{
    ClangProject* project;
    uint32_t fileId;

    Map<Location, uint32_t> incs;
    DependSet depends, reverseDepends;
    Map<String, Set<uint32_t> > names;  // name->usr
    Map<Location, CursorInfo> usrs; // location->usr
    UsrSet decls, defs, refs; // usr->locations
    VirtualSet virtuals; // usr->usrs
    Map<Path, Set<FixIt> > fixIts;
    bool hasDiags;

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
        fixIts.clear();
    }

    Mutex mutex;
    bool stopped;

    Map<uint32_t, bool> localSeen;
    int indexed;
    bool hasInclusions;

    static Mutex seenMutex;
    static Set<uint32_t> globalSeen;
    static Set<uint32_t> seenDecls, seenDefs;
};

Mutex ClangIndexInfo::seenMutex;
Set<uint32_t> ClangIndexInfo::globalSeen;
Set<uint32_t> ClangIndexInfo::seenDecls;
Set<uint32_t> ClangIndexInfo::seenDefs;

#ifdef CLANG_CAN_REPARSE
Mutex UnitCache::mutex;
LinkedList<std::pair<Path, shared_ptr<UnitCache::Unit> > > UnitCache::units;
#endif

class ClangUnit
{
public:
    ClangUnit(ClangProject* project);
    ~ClangUnit();

    void reindex(const SourceInformation& info);

    CXIndex index() { return project->cidx; }
    CXIndexAction action() { return project->caction; }

    ClangProject* project;
    mutable Mutex mutex;
    WaitCondition condition;
    SourceInformation sourceInformation;
    uint64_t indexed;
    shared_ptr<ClangParseJob> job;
};

class ClangParseJob : public ThreadPool::Job
{
public:
    ClangParseJob(ClangUnit* unit, bool reparse);
    ~ClangParseJob();

    void restart(bool reparse);
    void stop();

    // needs to be called with mUnit->mutex locked
    bool done() { return mDone; }

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
    static void indexMembers(ClangIndexInfo* info, const CXCursor& cursor);
    static void indexTranslationUnit(ClangIndexInfo* info, const CXTranslationUnit& unit);

    static void sendEmptyDiags(ClangIndexInfo* info);

private:
    ClangUnit* mUnit;
    bool mReparse;
    bool mDone;
    bool mRestarted;
    ClangIndexInfo mInfo;

    friend class ClangProject;
};

ClangUnit::ClangUnit(ClangProject* p)
    : project(p), indexed(0)
{
}

ClangUnit::~ClangUnit()
{
    MutexLocker lock(&mutex);
    if (job) {
        job->stop();
        while (!job->done())
            condition.wait(&mutex);
    }
}

ClangParseJob::ClangParseJob(ClangUnit* unit, bool reparse)
    : mUnit(unit), mReparse(reparse), mDone(false), mRestarted(false)
{
    mInfo.stopped = false;
    mInfo.hasInclusions = false;
    mInfo.project = mUnit->project;
    mInfo.fileId = mUnit->sourceInformation.sourceFileId();
    mInfo.indexed = 0;
    mInfo.hasDiags = false;
}

ClangParseJob::~ClangParseJob()
{
}

static inline Location makeLocation(const CXIdxLoc& cxloc, unsigned* offset = 0)
{
    CXIdxClientFile file = 0;
    CXFile cxfile = 0;
    unsigned line, column;
    clang_indexLoc_getFileLocation(cxloc, &file, &cxfile, &line, &column, offset);

    uint32_t fileId;
    if (file) {
        fileId = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(file));
    } else {
        // fall back to CXFile
        if (!cxfile)
            return Location();
        CXString fn = clang_getFileName(cxfile);
        const char* fileName = clang_getCString(fn);
        if (fileName) {
            fileId = Location::insertFile(Path::resolved(clang_getCString(fn)));
        } else {
            return Location();
        }
        clang_disposeString(fn);
    }
    return Location(fileId, line, column);
}

static inline Location makeLocation(const CXCursor& cursor, unsigned* offset = 0)
{
    const CXSourceLocation cxloc = clang_getCursorLocation(cursor);
    if (clang_equalLocations(cxloc, clang_getNullLocation()))
        return Location();
    CXFile file;
    unsigned line, column;
    clang_getSpellingLocation(cxloc, &file, &line, &column, offset);
    if (!file)
        return Location();
    CXString fileName = clang_getFileName(file);
    const uint32_t fileId = Location::insertFile(Path::resolved(clang_getCString(fileName)));
    const Location loc(fileId, line, column);
    clang_disposeString(fileName);
    return loc;
}

static inline Project::Cursor::Kind makeCursorKind(CXCursorKind cxkind, bool def)
{
    switch (cxkind) {
    case CXCursor_ClassDecl:
        if (def)
            return Project::Cursor::Class;
        return Project::Cursor::ClassForwardDeclaration;
    case CXCursor_Namespace:
        return Project::Cursor::Namespace;
    case CXCursor_CXXMethod:
        if (def)
            return Project::Cursor::MemberFunctionDefinition;
        return Project::Cursor::MemberFunctionDeclaration;
    case CXCursor_FunctionDecl:
        if (def)
            return Project::Cursor::MethodDefinition;
        return Project::Cursor::MethodDeclaration;
    case CXCursor_StructDecl:
        if (def)
            return Project::Cursor::Struct;
        return Project::Cursor::StructForwardDeclaration;
    case CXCursor_EnumDecl:
        return Project::Cursor::Enum;
    case CXCursor_EnumConstantDecl:
        return Project::Cursor::EnumValue;
    case CXCursor_VarDecl:
    case CXCursor_VariableRef:
        return Project::Cursor::Variable;
    case CXCursor_FieldDecl:
        return Project::Cursor::Field;
    case CXCursor_UnionDecl:
        return Project::Cursor::Union;
    default:
        break;
    }
    return Project::Cursor::Invalid;
}

static inline Project::Cursor::Kind makeIdxKind(CXIdxEntityKind cxkind, bool def)
{
    switch (cxkind) {
    case CXIdxEntity_CXXClass:
        if (def)
            return Project::Cursor::Class;
        return Project::Cursor::ClassForwardDeclaration;
    case CXIdxEntity_CXXNamespace:
        return Project::Cursor::Namespace;
    case CXIdxEntity_CXXInstanceMethod:
    case CXIdxEntity_CXXConstructor:
    case CXIdxEntity_CXXDestructor:
    case CXIdxEntity_CXXStaticMethod:
        if (def)
            return Project::Cursor::MemberFunctionDefinition;
        return Project::Cursor::MemberFunctionDeclaration;
    case CXIdxEntity_Function:
        if (def)
            return Project::Cursor::MethodDefinition;
        return Project::Cursor::MethodDeclaration;
    case CXIdxEntity_Struct:
        if (def)
            return Project::Cursor::Struct;
        return Project::Cursor::StructForwardDeclaration;
    case CXIdxEntity_Enum:
        return Project::Cursor::Enum;
    case CXIdxEntity_EnumConstant:
        return Project::Cursor::EnumValue;
    case CXIdxEntity_Variable:
    case CXIdxEntity_CXXStaticVariable:
        return Project::Cursor::Variable;
    case CXIdxEntity_Field:
        return Project::Cursor::Field;
    case CXIdxEntity_Union:
        return Project::Cursor::Union;
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
    return Project::Cursor::Invalid;
}

int ClangParseJob::abortQuery(CXClientData client_data, void* /*reserved*/)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    MutexLocker locker(&info->mutex);
    return info->stopped ? 1 : 0;
}

static inline void addDeps(uint32_t fileId, const DependSet& deps, Set<uint32_t>& result)
{
    DependSet::const_iterator dep = deps.find(fileId);
    if (dep != deps.end()) {
        Set<uint32_t>::const_iterator path = dep->second.begin();
        const Set<uint32_t>::const_iterator end = dep->second.end();
        while (path != end) {
            if (!result.contains(*path)) {
                result.insert(*path);
                addDeps(*path, deps, result);
            }
            ++path;
        }
    }
}

struct XmlEntry
{
    enum Type { None, Warning, Error, Fixit };

    XmlEntry(Type t = None, const String& m = String(), int l = 0, int c = 0, int eo = -1)
        : type(t), message(m), line(l), column(c), endOffset(eo)
    {
    }

    Type type;
    String message;
    int line, column, endOffset;
};

static inline String xmlEscape(const String& xml)
{
    if (xml.isEmpty())
        return xml;

    std::ostringstream strm;
    const char* ch = xml.constData();
    bool done = false;
    for (;;) {
        switch (*ch) {
        case '\0':
            done = true;
            break;
        case '"':
            strm << "\\\"";
            break;
        case '<':
            strm << "&lt;";
            break;
        case '>':
            strm << "&gt;";
            break;
        case '&':
            strm << "&amp;";
            break;
        default:
            strm << *ch;
            break;
        }
        if (done)
            break;
        ++ch;
    }
    return strm.str();
}

static inline Path path(const CXFile &file)
{
    const CXString fn = clang_getFileName(file);
    const Path path = Path::resolved(clang_getCString(fn));
    clang_disposeString(fn);
    return path;
}

void ClangParseJob::diagnostic(CXClientData client_data, CXDiagnosticSet diags, void* /*reserved*/)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);

    const unsigned diagnosticCount = clang_getNumDiagnosticsInSet(diags);
    const unsigned options = Server::options().options;

    info->hasDiags = info->hasDiags || diagnosticCount;

    Map<Path, Map<unsigned, XmlEntry> > xmlEntries;
    const bool xmlEnabled = testLog(RTags::CompilationErrorXml);

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnosticInSet(diags, i);
        int logLevel = INT_MAX;
        const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
        switch (severity) {
        case CXDiagnostic_Fatal:
        case CXDiagnostic_Error:
            logLevel = Error;
            break;
        case CXDiagnostic_Warning:
            logLevel = Warning;
            break;
        case CXDiagnostic_Note:
            logLevel = Debug;
            break;
        case CXDiagnostic_Ignored:
            break;
        }

        const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
        CXString cxstr = clang_getDiagnosticSpelling(diagnostic);
        const String msg(clang_getCString(cxstr));
        clang_disposeString(cxstr);
        if (xmlEnabled) {
            const CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diagnostic);
            XmlEntry::Type type = XmlEntry::None;
            switch (sev) {
            case CXDiagnostic_Warning:
                type = XmlEntry::Warning;
                break;
            case CXDiagnostic_Error:
            case CXDiagnostic_Fatal:
                type = XmlEntry::Error;
                break;
            default:
                break;
            }
            if (type != XmlEntry::None) {
                const unsigned rangeCount = clang_getDiagnosticNumRanges(diagnostic);
                bool rangeOk = rangeCount;
                for (unsigned rangePos = 0; rangePos < rangeCount; ++rangePos) {
                    const CXSourceRange range = clang_getDiagnosticRange(diagnostic, rangePos);
                    const CXSourceLocation start = clang_getRangeStart(range);
                    const CXSourceLocation end = clang_getRangeEnd(range);

                    unsigned line, column, startOffset, endOffset;
                    CXFile file;
                    clang_getSpellingLocation(start, &file, &line, &column, &startOffset);
                    Path p = ::path(file);
                    clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                    if (!rangePos && !startOffset && !endOffset) {
                        rangeOk = false;
                        // huh, range invalid? fall back to diag location
                        break;
                    } else {
                        xmlEntries[p][startOffset] = XmlEntry(type, msg, line, column, endOffset);
                    }
                }
                if (!rangeOk) {
                    unsigned line, column, offset;
                    CXFile file;
                    clang_getSpellingLocation(diagLoc, &file, &line, &column, &offset);
                    xmlEntries[::path(file)][offset] = XmlEntry(type, msg, line, column);
                }
            }
            if (testLog(logLevel) || testLog(RTags::CompilationError)) {
                if (testLog(logLevel))
                    logDirect(logLevel, msg.constData());
                if (testLog(RTags::CompilationError))
                    logDirect(RTags::CompilationError, msg.constData());
            }

            const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
            RegExp rx;
            if (options & Server::IgnorePrintfFixits) {
                rx = "^%[A-Za-z0-9]\\+$";
            }
            for (unsigned f=0; f<fixItCount; ++f) {
                CXSourceRange range;
                const CXString diagnosticString = clang_getDiagnosticFixIt(diagnostic, f, &range);
                unsigned startOffset, line, column, endOffset;
                CXFile file;
                clang_getSpellingLocation(clang_getRangeStart(range), &file, &line, &column, &startOffset);
                clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);

                const Path p = ::path(file);
                const char *string = clang_getCString(diagnosticString);
                if (options & Server::IgnorePrintfFixits && rx.indexIn(string) == 0) {
                    error("Ignored fixit for %s: Replace %d-%d with [%s]", p.constData(),
                          startOffset, endOffset, string);
                    continue;
                }

                // error("Fixit for %s: Replace %d-%d with [%s]", p.constData(), startOffset, endOffset, string);

                if (xmlEnabled) {
                    XmlEntry& entry = xmlEntries[p][startOffset];
                    entry.type = XmlEntry::Fixit;
                    if (entry.message.isEmpty()) {
                        entry.message = String::format<64>("did you mean '%s'?", string);
                        entry.line = line;
                        entry.column = column;
                    }
                    entry.endOffset = endOffset;
                }
                if (testLog(logLevel) || testLog(RTags::CompilationError)) {
                    const String msg = String::format<128>("Fixit for %s: Replace %d-%d with [%s]", p.constData(),
                                                           startOffset, endOffset, string);
                    if (testLog(logLevel))
                        logDirect(logLevel, msg.constData());
                    if (testLog(RTags::CompilationError))
                        logDirect(RTags::CompilationError, msg.constData());
                }
                info->fixIts[p].insert(FixIt(startOffset, endOffset, string));
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }
    if (xmlEnabled) {
        logDirect(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>");
        if (!xmlEntries.isEmpty()) {
            Map<Path, Map<unsigned, XmlEntry> >::const_iterator entry = xmlEntries.begin();
            const Map<Path, Map<unsigned, XmlEntry> >::const_iterator end = xmlEntries.end();

            const char* severities[] = { "none", "warning", "error", "fixit" };

            while (entry != end) {
                log(RTags::CompilationErrorXml, "<file name=\"%s\">", entry->first.constData());
                const Map<unsigned, XmlEntry>& map = entry->second;
                Map<unsigned, XmlEntry>::const_iterator it = map.begin();
                const Map<unsigned, XmlEntry>::const_iterator end = map.end();
                while (it != end) {
                    const XmlEntry& entry = it->second;
                    log(RTags::CompilationErrorXml, "<error line=\"%d\" column=\"%d\" startOffset=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                        entry.line, entry.column, it->first,
                        (entry.endOffset == -1 ? "" : String::format<32>("endOffset=\"%d\" ", entry.endOffset).constData()),
                        severities[entry.type], xmlEscape(entry.message).constData());
                    ++it;
                }
                logDirect(RTags::CompilationErrorXml, "</file>");
                ++entry;
            }
        }

        Set<Path> files;
        {
            MutexLocker locker(&info->project->mutex);
            Set<uint32_t> deps;

            addDeps(info->fileId, info->project->depends, deps);
            Set<uint32_t>::const_iterator it = deps.begin();
            const Set<uint32_t>::const_iterator end = deps.end();
            while (it != end) {
                files.insert(Location::path(*it));
                ++it;
            }
        }

        for (Set<Path>::const_iterator it = files.begin(); it != files.end(); ++it) {
            if (!xmlEntries.contains(*it)) {
                log(RTags::CompilationErrorXml, "<file name=\"%s\"/>", it->constData());
            }
        }

        logDirect(RTags::CompilationErrorXml, "</checkstyle>");
    }
}

void ClangParseJob::sendEmptyDiags(ClangIndexInfo* info)
{
    const bool xmlEnabled = testLog(RTags::CompilationErrorXml);
    if (!xmlEnabled)
        return;

    Set<Path> files;
    {
        MutexLocker locker(&info->project->mutex);
        Set<uint32_t> deps;

        deps.insert(info->fileId);

        addDeps(info->fileId, info->project->depends, deps);
        Set<uint32_t>::const_iterator it = deps.begin();
        const Set<uint32_t>::const_iterator end = deps.end();
        while (it != end) {
            files.insert(Location::path(*it));
            ++it;
        }
    }

    logDirect(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>");
    for (Set<Path>::const_iterator it = files.begin(); it != files.end(); ++it) {
        log(RTags::CompilationErrorXml, "<file name=\"%s\"/>", it->constData());
    }
    logDirect(RTags::CompilationErrorXml, "</checkstyle>");
}

CXIdxClientFile ClangParseJob::enteredMainFile(CXClientData client_data, CXFile mainFile, void* /*reserved*/)
{
    CXString str = clang_getFileName(mainFile);
    const uint32_t fileId = Location::insertFile(Path::resolved(clang_getCString(str)));
    clang_disposeString(str);
    return reinterpret_cast<CXIdxClientFile>(fileId);
}

CXIdxClientFile ClangParseJob::includedFile(CXClientData client_data, const CXIdxIncludedFileInfo* incl)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    CXString str = clang_getFileName(incl->file);
    const Path path = Path::resolved(clang_getCString(str));
    clang_disposeString(str);
    const Location loc = makeLocation(incl->hashLoc);
    if (loc.isEmpty())
        return 0;
    const uint32_t fileId = Location::insertFile(path);

    info->depends[loc.fileId()].insert(fileId);
    info->reverseDepends[fileId].insert(loc.fileId());
    info->incs[loc] = fileId;

    return reinterpret_cast<CXIdxClientFile>(fileId);
}

static inline uint32_t makeUsr(const CXCursor& cursor)
{
    CXString str = clang_getCursorUSR(cursor);
    const uint32_t usr = ClangProject::usrMap().insert(clang_getCString(str));
    clang_disposeString(str);
    return usr;
}

static inline unsigned cursorLength(const CXCursor& cursor)
{
    CXString cxname = clang_getCursorSpelling(cursor);
    const char* cstr = clang_getCString(cxname);
    const unsigned len = cstr ? strlen(cstr) : 0;
    clang_disposeString(cxname);
    return len;
}

static inline void addNamePermutations(CXCursor cursor, const uint32_t usr, Map<String, Set<uint32_t> >& names)
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
            } else if (subnames.isEmpty()) {
                break;
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

static inline bool allowFile(uint32_t fileId, ClangIndexInfo* info)
{
    const Map<uint32_t, bool>::const_iterator seen = info->localSeen.find(fileId);
    if (seen != info->localSeen.end()) {
        if (!seen->second)
            return false;
    } else {
        MutexLocker locker(&ClangIndexInfo::seenMutex);
        if (!ClangIndexInfo::globalSeen.insert(fileId)) {
            info->localSeen[fileId] = false;
            return false;
        }
        info->localSeen[fileId] = true;
        ++info->indexed;
    }
    return true;
}

static inline void addDeclaration(CXClientData client_data, CXCursor cursor)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);

    unsigned offset;
    const Location declLoc = makeLocation(cursor, &offset);
    if (declLoc.isEmpty())
        return;
    if (!allowFile(declLoc.fileId(), info))
        return;

    const bool def = clang_isCursorDefinition(cursor);
    const uint32_t usr = makeUsr(cursor);
    if (def && info->defs.contains(usr))
        return;

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = makeCursorKind(clang_getCursorKind(cursor), def);
    cursorInfo.start = offset;
    cursorInfo.end = offset + cursorLength(cursor);

    info->usrs[declLoc] = cursorInfo;

    addNamePermutations(cursor, usr, info->names);

    if (def)
        info->defs[usr].insert(declLoc);
    else
        info->decls[usr].insert(declLoc);
}

static inline void addReference(CXClientData client_data, CXCursor cursor)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);

    unsigned offset;
    const Location refLoc = makeLocation(cursor, &offset);
    if (refLoc.isEmpty())
        return;

    if (!allowFile(refLoc.fileId(), info))
        return;

    const uint32_t usr = makeUsr(clang_getCursorReferenced(cursor));

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = Project::Cursor::Reference;
    cursorInfo.start = offset;
    cursorInfo.end = offset + cursorLength(cursor);
    info->usrs[refLoc] = cursorInfo;

    //error() << "indexing ref" << usr << refLoc;

    info->refs[usr].insert(refLoc);
}

static CXChildVisitResult argumentVisistor(CXCursor cursor, CXCursor /*parent*/, CXClientData client_data)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_ParmDecl:
        return CXChildVisit_Recurse;
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        addReference(client_data, cursor);
        return CXChildVisit_Continue;
    default:
        break;
    }
    return CXChildVisit_Break;
}

static CXChildVisitResult memberVisistor(CXCursor cursor, CXCursor /*parent*/, CXClientData client_data)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_FieldDecl:
    case CXCursor_CXXBaseSpecifier:
        return CXChildVisit_Recurse;
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        addReference(client_data, cursor);
        break;
    case CXCursor_CXXMethod:
        addDeclaration(client_data, cursor);
        break;
    default:
        break;
    }
    return CXChildVisit_Continue;
}

static CXChildVisitResult unitVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    //const bool isTranslationUnit = clang_isTranslationUnit(clang_getCursorKind(parent));

    switch (clang_getCursorKind(cursor)) {
    case CXCursor_VarDecl:
        addDeclaration(client_data, cursor);
        return CXChildVisit_Recurse;
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        addReference(client_data, cursor);
        break;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_UnionDecl:
        addDeclaration(client_data, cursor);
        clang_visitChildren(cursor, memberVisistor, client_data);
        break;
    case CXCursor_MacroExpansion:
        addReference(client_data, cursor);
        break;
    case CXCursor_MacroDefinition:
        addDeclaration(client_data, cursor);
        break;
    default:
        break;
    }
    return CXChildVisit_Continue;
}

void ClangParseJob::indexArguments(ClangIndexInfo* info, const CXCursor& cursor)
{
    clang_visitChildren(cursor, argumentVisistor, info);
}

void ClangParseJob::indexMembers(ClangIndexInfo* info, const CXCursor& cursor)
{
    clang_visitChildren(cursor, memberVisistor, info);
}

void ClangParseJob::indexTranslationUnit(ClangIndexInfo* info, const CXTranslationUnit& unit)
{
    clang_visitChildren(clang_getTranslationUnitCursor(unit), unitVisitor, info);
}

void ClangParseJob::indexDeclaration(CXClientData client_data, const CXIdxDeclInfo* decl)
{
    ClangIndexInfo* info = static_cast<ClangIndexInfo*>(client_data);
    unsigned offset;
    const Location declLoc = makeLocation(decl->loc, &offset);
    if (!decl->entityInfo->USR || declLoc.isEmpty())
        return;

    const bool allowed = allowFile(declLoc.fileId(), info);

    switch (decl->entityInfo->templateKind) {
    case CXIdxEntity_NonTemplate:
        // Hack, typedefs for templates are not actually template entities. Allow them all for now.
        // ### better/possible to get the referenced symbol here?
        if (decl->entityInfo->kind == CXIdxEntity_Typedef)
            break;
        if (!allowed)
            return;
        break;
    default:
        break;
    }

    const uint32_t usr = ClangProject::usrMap().insert(decl->entityInfo->USR);
    const bool def = decl->isDefinition;

    if (!allowed) {
        // we're let through, check if we've seen our definition or declaration before
        MutexLocker locker(&ClangIndexInfo::seenMutex);
        if (def && !ClangIndexInfo::seenDefs.insert(usr))
            return;
        else if (decl && !ClangIndexInfo::seenDecls.insert(usr))
            return;
        // no? continue.
    }

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = makeIdxKind(decl->entityInfo->kind, def);
    cursorInfo.start = offset;
    cursorInfo.end = offset + cursorLength(decl->cursor);

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
                uint32_t virtUsr;
                for (unsigned i = 0; i < num; ++i) {
                    virtUsr = makeUsr(overridden[i]);
                    //error() << "overridden at" << makeLocation(overridden[i]) << virtusr;
                    info->virtuals[usr].insert(virtUsr);
                    info->virtuals[virtUsr].insert(usr);
                }
                clang_disposeOverriddenCursors(overridden);
            }
        }
        // fall through
    case CXIdxEntity_CXXStaticMethod:
    case CXIdxEntity_CXXConstructor:
    case CXIdxEntity_Function:
        indexArguments(info, decl->cursor);
        break;
    case CXIdxEntity_CXXClass:
    case CXIdxEntity_Struct:
    case CXIdxEntity_Union:
        indexMembers(info, decl->cursor);
        break;
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
    unsigned offset;
    const Location refLoc = makeLocation(ref->loc, &offset);
    if (!ref->referencedEntity->USR || refLoc.isEmpty())
        return;

    if (!allowFile(refLoc.fileId(), info))
        return;

    const uint32_t usr = ClangProject::usrMap().insert(ref->referencedEntity->USR);

    CursorInfo cursorInfo;
    cursorInfo.usr = usr;
    cursorInfo.kind = Project::Cursor::Reference;
    cursorInfo.start = offset;
    cursorInfo.end = offset + cursorLength(ref->cursor);
    info->usrs[refLoc] = cursorInfo;

    //error() << "indexing ref" << usr << refLoc;

    info->refs[usr].insert(refLoc);
}

void ClangParseJob::stop()
{
    MutexLocker locker(&mInfo.mutex);
    mInfo.stopped = true;
}

// needs to be called with mUnit->mutex locked
void ClangParseJob::restart(bool reparse)
{
    mRestarted = true;
    mReparse = reparse;
    stop();
}

static CXChildVisitResult hasInclusionsVisitor(CXCursor cursor, CXCursor /*parent*/, CXClientData client_data)
{
    if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
        *static_cast<bool*>(client_data) = true;
        return CXChildVisit_Break;
    }
    return CXChildVisit_Continue;
}

static inline bool hasInclusions(CXTranslationUnit unit)
{
    CXCursor top = clang_getTranslationUnitCursor(unit);
    bool has = false;
    clang_visitChildren(top, hasInclusionsVisitor, &has);
    return has;
}

void ClangParseJob::run()
{
    const Path sourceFile = mUnit->sourceInformation.sourceFile;

    {
        MutexLocker locker(&mInfo.mutex);
        if (mInfo.stopped) {
            if (mRestarted) {
                mInfo.stopped = false;
                mRestarted = false;
            } else {
                MutexLocker locker(&mUnit->mutex);
                mDone = true;
                mUnit->condition.wakeAll();
                return;
            }
        }
    }

    // loop until we're done or stopped and not restarted
    for (;;) {
        {
            MutexLocker locker(&ClangIndexInfo::seenMutex);
            const uint32_t fileId = Location::fileId(sourceFile);
            if (fileId && ClangIndexInfo::globalSeen.contains(fileId)) {
                // the file has already been indexed, we need to take out the fileid from the seen list
                ClangIndexInfo::globalSeen.remove(fileId);

                // ### do we need to take out all the deps as well?
                Set<uint32_t> deps;
                {
                    MutexLocker locker(&mUnit->project->mutex);
                    addDeps(fileId, mUnit->project->depends, deps);
                }
                ClangIndexInfo::globalSeen.subtract(deps);
            }
        }

        // clang parse
        uint64_t parseTime = 0;
#ifdef CLANG_CAN_REPARSE
        if (mReparse) {
            // ### should handle multiple builds here
            shared_ptr<UnitCache::Unit> unitptr = UnitCache::get(sourceFile);
            if (unitptr) {
                CXTranslationUnit unit = unitptr->unit;

                String fileData;
                parseTime = Rct::currenTimeMs();
                if (!Rct::readFile(sourceFile, fileData)) {
                    error() << "unable to read file, fall back to indexSourceFile";
                    mInfo.clear();
                    mReparse = false;
                } else {
                    CXUnsavedFile unsaved;
                    unsaved.Filename = sourceFile.nullTerminated();
                    unsaved.Contents = fileData.constData();
                    unsaved.Length = fileData.size();

                    if (clang_reparseTranslationUnit(unit, 1, &unsaved, clang_defaultReparseOptions(unit)) != 0) {
                        // bad
                        mInfo.clear();
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
                        const unsigned opts = CXIndexOpt_IndexFunctionLocalSymbols | CXIndexOpt_IndexImplicitTemplateInstantiations;

                        const int indexFailed = clang_indexTranslationUnit(mUnit->action(), &mInfo, &callbacks, sizeof(IndexerCallbacks), opts, unit);

                        {
                            MutexLocker locker(&mInfo.mutex);
                            if (mInfo.stopped) {
                                MutexLocker locker(&mUnit->mutex);
                                mInfo.clear();
                                // unit is bad, we need to throw it away
                                unitptr.reset();
                                if (mRestarted) {
                                    mRestarted = false;
                                    mInfo.stopped = false;
                                    continue;
                                }
                                mDone = true;
                                mUnit->condition.wakeAll();
                                return;
                            }
                        }

                        if (indexFailed) {
                            mInfo.clear();
                            mReparse = false;
                        } else {
                            // need to index the global members of the TU
                            indexTranslationUnit(&mInfo, unit);

                            mInfo.hasInclusions = hasInclusions(unit);
                        }

                        if (mReparse) {
                            if (!mInfo.hasDiags)
                                sendEmptyDiags(&mInfo);
                        }
                    }
                }
            } else {
                mReparse = false;
            }

            if (mReparse) {
                // all ok
                assert(unitptr != 0);
                assert(parseTime);
                UnitCache::put(sourceFile, unitptr);
            }
        }
#endif
        if (!mReparse) {
            const List<String> defaultArgs = Server::options().defaultArguments;

            const List<SourceInformation::Build>& builds = mUnit->sourceInformation.builds;
            List<SourceInformation::Build>::const_iterator build = builds.begin();
            const List<SourceInformation::Build>::const_iterator end = builds.end();
            while (build != end) {
                List<String> args = build->args;
                args.append(defaultArgs);
#ifdef CLANG_INCLUDEPATH
                args.append("-I" CLANG_INCLUDEPATH);
#endif
                // don't really need to copy all of these
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
                const unsigned opts = CXIndexOpt_IndexFunctionLocalSymbols | CXIndexOpt_IndexImplicitTemplateInstantiations;
                const unsigned tuOpts =
                    CXTranslationUnit_DetailedPreprocessingRecord | CXTranslationUnit_PrecompiledPreamble | CXTranslationUnit_CacheCompletionResults;

                CXTranslationUnit unit = 0;
                parseTime = Rct::currenTimeMs();
                const int indexFailed = clang_indexSourceFile(mUnit->action(), &mInfo, &callbacks, sizeof(IndexerCallbacks), opts,
                                                              sourceFile.nullTerminated(), clangArgs, args.size(), 0, 0, &unit, tuOpts);

                {
                    MutexLocker locker(&mInfo.mutex);
                    if (mInfo.stopped) {
                        MutexLocker locker(&mUnit->mutex);
                        mInfo.clear();
                        // unit is bad, we need to throw it away
                        if (unit)
                            clang_disposeTranslationUnit(unit);
                        if (mRestarted) {
                            mInfo.stopped = false;
                            break;
                        }
                        mDone = true;
                        mUnit->condition.wakeAll();
                        return;
                    }
                }

                if (indexFailed) {
                    if (unit) {
                        clang_disposeTranslationUnit(unit);
                        unit = 0;
                    }
                    mInfo.clear();
                } else {
                    // need to index the global members of the TU
                    indexTranslationUnit(&mInfo, unit);
                }

                if (unit) {
                    assert(parseTime);
#ifdef CLANG_CAN_REPARSE
                    UnitCache::add(sourceFile, unit);
#else
                    clang_disposeTranslationUnit(unit);
#endif

                    if (!mInfo.hasDiags)
                        sendEmptyDiags(&mInfo);
                }

                ++build;
            }
            MutexLocker locker(&mUnit->mutex);
            if (mRestarted) {
                MutexLocker locker(&mInfo.mutex);
                mInfo.clear();
                mInfo.stopped = false;
                mRestarted = false;
                continue;
            }
        }

        // error() << "done parsing" << mUnit->sourceInformation.sourceFile << "reparse" << mReparse;

        MutexLocker locker(&mUnit->mutex);
        if (mRestarted) {
            MutexLocker locker(&mInfo.mutex);
            assert(mInfo.stopped);
            mInfo.clear();
            mInfo.stopped = false;
            mRestarted = false;
            continue;
        }

        mUnit->indexed = parseTime;
        mDone = true;
        mUnit->project->jobFinished(mUnit->job);
        mUnit->job.reset(); // remove myself
        mUnit->condition.wakeAll();
        break;
    }
}

void ClangUnit::reindex(const SourceInformation& info)
{
    MutexLocker locker(&mutex);

#ifdef CLANG_CAN_REPARSE
    const bool reparse = (sourceInformation == info);
#else
    const bool reparse = false;
#endif

    if (job && !job->done()) {
        if (!reparse)
            sourceInformation = info;
        job->restart(reparse);
        return;
    }

    if (!reparse)
        sourceInformation = info;
    job.reset(new ClangParseJob(this, reparse));
    ThreadPool::instance()->start(job);
}

LockingStringMap ClangProject::umap;

ClangProject::ClangProject(const Path &path)
    : Project(path), pendingJobs(0), jobsProcessed(0)
{
    cidx = clang_createIndex(0, 1);
    caction = clang_IndexAction_create(cidx);
}

ClangProject::~ClangProject()
{
    clang_IndexAction_dispose(caction);
    clang_disposeIndex(cidx);
    for (Map<uint32_t, ClangUnit*>::const_iterator it = units.begin(); it != units.end(); ++it) {
        delete it->second;
    }
}

bool ClangProject::save(Serializer &serializer)
{
    if (!Server::saveFileIds())
        return false;
    MutexLocker lock(&mutex);

    serializer << sourceInfos() << incs << depends << reverseDepends << names
               << usrs << decls << defs << refs << virtuals << umap
               << static_cast<uint32_t>(units.size());
    for (Map<uint32_t, ClangUnit*>::const_iterator it = units.begin(); it != units.end(); ++it) {
        serializer << it->first << it->second->indexed;
    }

    return true;
}

bool ClangProject::restore(Deserializer &deserializer)
{
    if (!Server::loadFileIds())
        return false;

    uint32_t unitCount;
    SourceInformationMap sources;
    deserializer >> sources >> incs >> depends >> reverseDepends >> names
                 >> usrs >> decls >> defs >> refs >> virtuals >> umap >> unitCount;
    setSourceInfos(sources);
    for (uint32_t i=0; i<unitCount; ++i) {
        uint32_t fileId;
        time_t parsed;
        deserializer >> fileId >> parsed;
        const Path source = Location::path(fileId);
        const Set<Path> deps = dependencies(source, ArgDependsOn);
        assert(deps.contains(source));
        for (Set<Path>::const_iterator it = deps.begin(); it != deps.end(); ++it) {
            // error() << "Checking" << *it << "for" << source
            //         << static_cast<uint32_t>(it->lastModified()) << "vs" << static_cast<uint32_t>(parsed);
            if (it->lastModified() > parsed) { // should this be >= ???
                // error() << "reparsing" << source << "because" << it->lastModified() << ">" << parsed;
                index(sources.value(source), Restore);
                break;
            }
        }
    }

    return true;
}

static inline Project::Cursor cursorForInclude(const Location& location, const Map<Location, uint32_t>& incs)
{
    Map<Location, uint32_t>::const_iterator inc = incs.lower_bound(location);
    if (inc == incs.end())
        return Project::Cursor();
    if (inc->first > location) {
        if (inc == incs.begin())
            return Project::Cursor();
        --inc;
        if (inc->first.path() != location.path()
            || inc->first.line() != location.line())
            return Project::Cursor();
    }
    assert(inc->first.line() == location.line());

    Project::Cursor cursor;
    cursor.location = inc->first;
    cursor.target = Location(inc->second, 1, 1);
    cursor.kind = Project::Cursor::File;

    return cursor;
}

Project::Cursor ClangProject::cursor(const Location &location) const
{
    MutexLocker locker(&mutex);
    Map<Location, CursorInfo>::const_iterator usr = usrs.lower_bound(location);
    if (usr == usrs.end())
        return cursorForInclude(location, incs);
    if (usr->first > location) { // we're looking for the previous one
        if (usr == usrs.begin())
            return cursorForInclude(location, incs);
        --usr;
        if (usr->first.path() != location.path()) {
            // we've iterated past the beginning of the file
            return cursorForInclude(location, incs);
        }
        if ((usr->first.line() < location.line())
            || (usr->first.column() + usr->second.length() <= location.column())) {
            // our location is after the end of the the previous location
            return cursorForInclude(location, incs);
        }
        assert(usr->first.line() == location.line());
    }
    assert(!(usr->first > location));

    //error() << "found loc, asked for" << location << "resolved to" << usr->first
    //        << "refers to" << usr->second.loc << "and kind" << usr->second.kind;

    Cursor cursor;
    cursor.location = usr->first;
    cursor.kind = usr->second.kind;

    const uint32_t targetUsr = usr->second.usr;

    if (cursor.kind == Cursor::Reference) {
        // reference, target should be definition (if possible)
        UsrSet::const_iterator target = defs.find(targetUsr);
        if (target == defs.end()) {
            // try declaration
            target = decls.find(targetUsr);
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
        const UsrSet::const_iterator target = decls.find(targetUsr);
        if (target != decls.end()) {
            if (!target->second.isEmpty())
                cursor.target = *target->second.begin();
        }
    } else {
        // declaration, taget should be definition
        const UsrSet::const_iterator target = defs.find(targetUsr);
        if (target != defs.end()) {
            if (!target->second.isEmpty())
                cursor.target = *target->second.begin();
        }
    }

    return cursor;
}

inline char ClangProject::locationType(const Location& location) const
{
    //Map<Location, CursorInfo> usrs;    // location->usr
    const Map<Location, CursorInfo>::const_iterator info = usrs.find(location);
    if (info == usrs.end())
        return '\0';
    return Cursor::kindToChar(info->second.kind);
}

void ClangProject::writeReferences(const uint32_t usr, const Set<uint32_t>& pathSet, Connection* conn, unsigned int keyFlags) const
{
    const bool pass = pathSet.isEmpty();
    const UsrSet::const_iterator ref = refs.find(usr);
    if (ref != refs.end()) {
        Set<Location>::const_iterator loc = ref->second.begin();
        const Set<Location>::const_iterator end = ref->second.end();
        while (loc != end) {
            if (pass || pathSet.contains(loc->fileId()))
                conn->write(loc->toString(keyFlags, locationType(*loc)));
            ++loc;
        }
    }
}

void ClangProject::writeDeclarations(const uint32_t usr, const Set<uint32_t>& pathSet, Connection* conn, unsigned int keyFlags) const
{
    const bool pass = pathSet.isEmpty();
    const UsrSet* usrs[] = { &decls, &defs, 0 };
    for (int i = 0; usrs[i]; ++i) {
        const UsrSet::const_iterator decl = usrs[i]->find(usr);
        if (decl != usrs[i]->end()) {
            Set<Location>::const_iterator loc = decl->second.begin();
            const Set<Location>::const_iterator end = decl->second.end();
            while (loc != end) {
                if (pass || pathSet.contains(loc->fileId()))
                    conn->write(loc->toString(keyFlags, locationType(*loc)));
                ++loc;
            }
        }
    }
}

static inline void makePathSet(const List<Path>& pathFilter, Set<uint32_t>& result)
{
    List<Path>::const_iterator path = pathFilter.begin();
    const List<Path>::const_iterator end = pathFilter.end();
    while (path != end) {
        const uint32_t fileId = Location::fileId(*path);
        if (fileId)
            result.insert(fileId);
        ++path;
    }
}

void ClangProject::references(const Location& location, unsigned queryFlags,
                               const List<Path> &pathFilter, Connection *conn) const
{
    Set<uint32_t> pathSet;
    if (!pathFilter.isEmpty())
        makePathSet(pathFilter, pathSet);

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
        if ((usr->first.line() < location.line())
            || (usr->first.column() + usr->second.length() <= location.column())) {
            // our location is after the end of the the previous location
            conn->write("`");
            return;
        }
        assert(usr->first.line() == location.line());
    }
    assert(!(usr->first > location));

    const uint32_t targetUsr = usr->second.usr;

    if (wantAll || !wantVirtuals) {
        writeReferences(targetUsr, pathSet, conn, queryFlags);
        if (wantAll)
            writeDeclarations(targetUsr, pathSet, conn, queryFlags);
    }
    if (wantVirtuals) {
        if (wantAll)
            writeReferences(targetUsr, pathSet, conn, queryFlags);
        writeDeclarations(targetUsr, pathSet, conn, queryFlags);

        const VirtualSet::const_iterator virt = virtuals.find(targetUsr);
        Set<uint32_t>::const_iterator vusr = virt->second.begin();
        const Set<uint32_t>::const_iterator vend = virt->second.end();
        while (vusr != vend) {
            if (wantAll)
                writeReferences(*vusr, pathSet, conn, queryFlags);
            writeDeclarations(*vusr, pathSet, conn, queryFlags);
            ++vusr;
        }
    }
    conn->write("`");
}

void ClangProject::status(const String &query, Connection *conn, unsigned queryFlags) const
{
    MutexLocker lock(&mutex);
    if (query.isEmpty() || query.contains("symbolnames", String::CaseInsensitive)) {
        conn->write("SymbolNames:");
        for (Map<String, Set<uint32_t> >::const_iterator it = names.begin(); it != names.end(); ++it) {
            conn->write("  " + it->first);
            for (Set<uint32_t>::const_iterator usrIt = it->second.begin(); usrIt != it->second.end(); ++usrIt) {
                if (usrIt != it->second.begin())
                    conn->write(String());
                const Set<Location> locations = decls.value(*usrIt) + defs.value(*usrIt);
                for (Set<Location>::const_iterator lit = locations.begin(); lit != locations.end(); ++lit) {
                    conn->write("    " + lit->toString(queryFlags));
                }
            }
        }
    }

    if (query.isEmpty() || query.contains("symbols", String::CaseInsensitive)) {
        const UsrSet *sets[] = { &defs, &decls };
        for (int i=0; i<2; ++i) {
            const UsrSet &set = *sets[i];
            for (UsrSet::const_iterator it = set.begin(); it != set.end(); ++it) {

            }
        }
    }
#ifdef CLANG_CAN_REPARSE
    if (query.isEmpty() || query.contains("unitcache")) {
        const List<Path> paths = UnitCache::paths();
        conn->write("UnitCache:");
        for (int i=0; i<paths.size(); ++i) {
            conn->write("  " + paths.at(i));
        }
    }
#endif
    // DependSet depends, reverseDepends;
    // Map<Location, CursorInfo> usrs;    // location->usr
    // UsrSet decls, defs, refs;          // usr->locations
    // VirtualSet virtuals;               // usr->usrs
    // Map<Path, Set<FixIt> > fixIts;

}

void ClangProject::dump(const SourceInformation &sourceInformation, Connection *conn) const
{
}

void ClangProject::index(const SourceInformation &sourceInformation, Type type)
{
    const uint32_t fileId = Location::insertFile(sourceInformation.sourceFile);
    ClangUnit *&unit = units[fileId];
    if (!unit) {
        unit = new ClangUnit(this);
    } else if (type != Dirty && unit->indexed < sourceInformation.sourceFile.lastModifiedMs()) {
        return;
    }
    {
        MutexLocker locker(&mutex);
        if (!pendingJobs++)
            timer.restart();
    }
    unit->reindex(sourceInformation);
}

void ClangProject::remove(const Path &sourceFile)
{
    const uint32_t fileId = Location::fileId(sourceFile);

    MutexLocker locker(&mutex);
    {
        // remove headers?
        DependSet::iterator dep = depends.find(fileId);
        if (dep != depends.end())
            depends.erase(dep);
    }
    {
        DependSet::iterator dep = reverseDepends.begin();
        while (dep != reverseDepends.end()) {
            Set<uint32_t>& set = dep->second;
            if (set.remove(fileId)) {
                if (set.isEmpty())
                    reverseDepends.erase(dep++);
                else
                    ++dep;
            } else {
                ++dep;
            }
        }
    }
}

bool ClangProject::isIndexing() const
{
    MutexLocker locker(&mutex);
    return (pendingJobs > 0);
}

Set<Path> ClangProject::dependencies(const Path &path, DependencyMode mode) const
{
    MutexLocker locker(&mutex);

    Set<uint32_t> deps;
    const uint32_t fileId = Location::fileId(path);
    if (mode == ArgDependsOn) {
        addDeps(fileId, depends, deps);
    } else {
        addDeps(fileId, reverseDepends, deps);
    }

    Set<Path> result;
    result.insert(path); // all files depend on themselves

    Set<uint32_t>::const_iterator dep = deps.begin();
    const Set<uint32_t>::const_iterator end = deps.end();
    while (dep != end) {
        result.insert(Location::path(*dep));
        ++dep;
    }

    return result;
}

Set<Path> ClangProject::files(int mode) const
{
    return Set<Path>();
}

Set<String> ClangProject::listSymbols(const String &string, const List<Path> &pathFilter) const
{
    // this is pretty awful
    Set<uint32_t> allUsrs;
    const bool pass = pathFilter.isEmpty();

    if (!pass) {
        List<Path>::const_iterator path = pathFilter.begin();
        const List<Path>::const_iterator end = pathFilter.end();
        while (path != end) {
            const uint32_t fileId = Location::fileId(*path);
            if (fileId) {
                const Location start(fileId, 1, 1);
                Map<Location, CursorInfo>::const_iterator usr = usrs.lower_bound(start);
                const Map<Location, CursorInfo>::const_iterator usrEnd = usrs.end();
                while (usr != usrEnd && usr->first.fileId() == fileId) {
                    allUsrs.insert(usr->second.usr);
                    ++usr;
                }
            }
            ++path;
        }
    }


    Set<String> result;

    MutexLocker locker(&mutex);
    Map<String, Set<uint32_t> >::const_iterator name = names.lower_bound(string);
    const Map<String, Set<uint32_t> >::const_iterator end = names.end();
    while (name != end && name->first.startsWith(string)) {
        if (pass || allUsrs.intersects(name->second))
            result.insert(name->first);
        ++name;
    }

    return result;
}

static inline Location firstLocation(const uint32_t usr, const UsrSet& set)
{
    const UsrSet::const_iterator it = set.find(usr);
    if (it == set.end())
        return Location();
    const Set<Location>& locs = it->second;
    if (locs.isEmpty())
        return Location();
    return *locs.begin();
}

Set<Project::Cursor> ClangProject::findCursors(const String &string, const List<Path> &pathFilter) const
{
    Set<uint32_t> pathSet;
    const bool pass = pathFilter.isEmpty();
    if (!pass)
        makePathSet(pathFilter, pathSet);

    MutexLocker locker(&mutex);
    Map<String, Set<uint32_t> >::const_iterator name = names.find(string);
    if (name == names.end())
        return Set<Cursor>();

    Set<Cursor> cursors;

    Set<uint32_t>::const_iterator usr = name->second.begin();
    const Set<uint32_t>::const_iterator end = name->second.end();
    while (usr != end) {
        const UsrSet* usrs[] = { &decls, &defs, 0 };
        for (int i = 0; usrs[i]; ++i) {
            const UsrSet::const_iterator decl = usrs[i]->find(*usr);
            if (decl != usrs[i]->end()) {
                Set<Location>::const_iterator loc = decl->second.begin();
                const Set<Location>::const_iterator end = decl->second.end();
                while (loc != end) {
                    Map<Location, CursorInfo>::const_iterator info = ClangProject::usrs.find(*loc);
                    if (info != ClangProject::usrs.end() && (pass || pathSet.contains(info->first.fileId()))) {
                        Cursor cursor;
                        cursor.symbolName = name->first;
                        cursor.location = *loc;
                        cursor.target = firstLocation(*usr, usrs[i] == &decls ? defs : decls);
                        cursor.kind = info->second.kind;
                        cursor.start = info->second.start;
                        cursor.end = info->second.end;
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

String ClangProject::fixits(const Path &path) const
{
    MutexLocker lock(&mutex);
    const Map<Path, Set<FixIt> >::const_iterator it = fixIts.find(path);
    String out;
    if (it != fixIts.end()) {
        const Set<FixIt> &fixIts = it->second;
        if (!fixIts.isEmpty()) {
            Set<FixIt>::const_iterator f = fixIts.end();
            do {
                --f;
                if (!out.isEmpty())
                    out.append('\n');
                out.append(String::format<32>("%d-%d %s", f->start, f->end, f->text.constData()));
            } while (f != fixIts.begin());
        }
    }
    return out;
}

Set<Project::Cursor> ClangProject::cursors(const Path &path) const
{
    Set<Cursor> cursors;

    MutexLocker lock(&mutex);
    const uint32_t fileId = Location::fileId(path);
    if (fileId) {
        const Location start(fileId, 1, 1);
        Map<Location, CursorInfo>::const_iterator usr = usrs.lower_bound(start);
        const Map<Location, CursorInfo>::const_iterator usrEnd = usrs.end();
        while (usr != usrEnd && usr->first.fileId() == fileId) {
            if (usr->second.kind != Cursor::Reference) {
                Cursor cursor;
                cursor.start = usr->second.start;
                cursor.end = usr->second.end;
                cursor.location = usr->first;
                // this will be wrong if the file has changed since the CursorInfo was created
                cursor.symbolName = cursor.location.read(cursor.start, cursor.end);
                cursor.kind = usr->second.kind;
                if (cursor.isDefinition()) {
                    // target is decl if it exists
                    cursor.target = firstLocation(usr->second.usr, decls);
                } else {
                    // target is def if it exists
                    cursor.target = firstLocation(usr->second.usr, defs);
                }
                cursors.insert(cursor);
            }
            ++usr;
        }
    }

    return cursors;
}

void ClangProject::jobFinished(const shared_ptr<ClangParseJob> &job)
{
    const ClangIndexInfo& info = job->mInfo;

    MutexLocker lock(&mutex);
    ++jobsProcessed;
    --pendingJobs;
    error("[%3d%%] %d/%d %s %s, Symbols: %d References: %d Files: %d",
          static_cast<int>(round(jobsProcessed / static_cast<double>(pendingJobs + jobsProcessed) * 100.0)),
          jobsProcessed, pendingJobs + jobsProcessed, String::formatTime(time(0), String::Time).constData(),
          Location::path(info.fileId).toTilde().constData(),
          info.decls.size() + info.defs.size(), info.refs.size(),
          info.indexed);
    if (!pendingJobs) {
        error() << "Parsed" << jobsProcessed << "files in" << timer.elapsed() << "ms";
        sync(job);
        jobsProcessed = 0;

        MutexLocker locker(&ClangIndexInfo::seenMutex);
        ClangIndexInfo::seenDecls.clear();
        ClangIndexInfo::seenDefs.clear();
    } else {
        syncJobs.append(job);
    }
}

void ClangProject::dirty(const Set<Path>& files)
{
    dirtyFiles = files;
}

static inline void dirtyUsr(const Location& start, uint32_t usr, UsrSet& usrs)
{
    UsrSet::iterator entry = usrs.find(usr);
    if (entry == usrs.end())
        return;
    const uint32_t startFileId = start.fileId();
    Set<Location>::iterator locs = entry->second.lower_bound(start);
    while (locs != entry->second.end() && locs->fileId() == startFileId) {
        entry->second.erase(locs++);
    }
}

// should only be called with project->mutex locked
void ClangProject::dirtyUsrs()
{
    if (dirtyFiles.isEmpty())
        return;

    Set<uint32_t> filesToDirty;
    {
        // collect all fileids to dirty
        Set<Path>::const_iterator file = dirtyFiles.begin();
        const Set<Path>::const_iterator end = dirtyFiles.end();
        while (file != end) {
            const uint32_t fileId = Location::fileId(*file);
            if (fileId) {
                // add all the files that depend on me and then myself
                addDeps(fileId, reverseDepends, filesToDirty);
                filesToDirty.insert(fileId);
            }
            ++file;
        }
    }

    dirtyFiles.clear();

    Set<uint32_t>::const_iterator dirty = filesToDirty.begin();
    const Set<uint32_t>::const_iterator dirtyEnd = filesToDirty.end();
    while (dirty != dirtyEnd) {
        const uint32_t fileId = *dirty;
        const Location start(fileId, 1, 1);
        {
            Map<Location, CursorInfo>::iterator usr = usrs.lower_bound(start);
            while (usr != usrs.end() && usr->first.fileId() == fileId) {
                dirtyUsr(start, usr->second.usr, decls);
                dirtyUsr(start, usr->second.usr, defs);
                dirtyUsr(start, usr->second.usr, refs);
                usrs.erase(usr++);
            }
        }
        ++dirty;
    }
}

void ClangProject::dirtyDeps(uint32_t fileId)
{
    {
        const Location start(fileId, 1, 1);
        Map<Location, uint32_t>::iterator inc = incs.lower_bound(start);
        const Map<Location, uint32_t>::const_iterator end = incs.end();
        while (inc != end && inc->first.fileId() == fileId) {
            incs.erase(inc++);
        }
    }
    {
        // remove headers?
        DependSet::iterator dep = depends.find(fileId);
        if (dep != depends.end()) {
            depends.erase(dep);
        }
    }
    {
        DependSet::iterator dep = reverseDepends.begin();
        while (dep != reverseDepends.end()) {
            Set<uint32_t>& set = dep->second;
            if (set.remove(fileId)) {
                if (set.isEmpty()) {
                    reverseDepends.erase(dep++);
                } else
                    ++dep;
            } else {
                ++dep;
            }
        }
    }
}

void ClangProject::syncJob(const shared_ptr<ClangParseJob>& job)
{
    const ClangIndexInfo& info = job->mInfo;

    if (!info.hasInclusions || !info.depends.isEmpty())
        dirtyDeps(info.fileId);

    incs.unite(info.incs);
    usrs.unite(info.usrs);
    fixIts.unite(info.fixIts);

    {
        Map<String, Set<uint32_t> >::const_iterator name = info.names.begin();
        const Map<String, Set<uint32_t> >::const_iterator end = info.names.end();
        while (name != end) {
            names[name->first].unite(name->second);
            ++name;
        }
    }
    {
        const UsrSet* src[] = { &info.decls, &info.defs, &info.refs, 0 };
        UsrSet* dst[] = { &decls, &defs, &refs, 0 };
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
        DependSet* dst[] = { &depends, &reverseDepends, 0 };
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
        virtuals[virt->first].unite(virt->second);
        ++virt;
    }
}

void ClangProject::sync(const shared_ptr<ClangParseJob>& currentJob)
{
    dirtyUsrs();
    syncJob(currentJob);

    // this is intentional, pop the synced job for
    // each iteration to free the synced data
    while (!syncJobs.isEmpty()) {
        syncJob(syncJobs.back());
        syncJobs.pop_back();
    }

    startSaveTimer();
}

bool ClangProject::codeCompleteAt(const Location &location, const String &source, Connection *conn)
{
#ifdef CLANG_CAN_REPARSE
    MutexLocker lock(&mutex);
    shared_ptr<UnitCache::Unit> unit = UnitCache::get(location.path());
    if (!unit || !unit->unit) {
        error() << "No unit for" << location;
        return false;
    }

    shared_ptr<ClangCompletionJob> job(new ClangCompletionJob(unit, location, source));
    job->finished().connectAsync(this, &ClangProject::onCompletionFinished);
    job->completion().connectAsync(this, &ClangProject::onCompletion);
    conn->destroyed().connect(this, &ClangProject::onConnectionDestroyed);
    mCompletions[job.get()] = conn;
    ThreadPool::instance()->start(job);

    return true;
#else
    error() << "clang is too old, can't safely do reparsing";
    return false;
#endif
}

void ClangProject::onConnectionDestroyed(Connection *conn)
{
    for (Map<ClangCompletionJob*, Connection*>::iterator it = mCompletions.begin(); it != mCompletions.end(); ++it) {
        if (it->second == conn) {
            mCompletions.erase(it);
            // could abort completion job
            break;
        }
    }
}

void ClangProject::onCompletionFinished(ClangCompletionJob *job)
{
    if (Connection *conn = mCompletions.take(job)) {
        conn->destroyed().disconnect(this, &ClangProject::onConnectionDestroyed);
        conn->finish();
    }
}

void ClangProject::onCompletion(ClangCompletionJob *job, String completion, String signature)
{
    if (Connection *conn = mCompletions.value(job))
        conn->write(completion + ' ' + signature);
}

class ClangProjectPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<Project> createProject(const Path &path)
    {
        return shared_ptr<Project>(new ClangProject(path));
    }
};

extern "C" RTagsPlugin* createInstance()
{
    return new ClangProjectPlugin;
}

