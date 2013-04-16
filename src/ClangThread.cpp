#include "ClangThread.h"
#include "Server.h"
#include "RTags.h"
#include <rct/RegExp.h>

static inline String eatString(const CXString &str)
{
    const String ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

static inline Path path(const CXFile &file)
{
    const CXString fn = clang_getFileName(file);
    const Path path = Path::resolved(clang_getCString(fn));
    clang_disposeString(fn);
    return path;
}

static void inclusionVisitor(CXFile includedFile,
                             CXSourceLocation */*includeStack*/,
                             unsigned /*includeLen*/,
                             CXClientData userData)
{
    const Path path = ::path(includedFile);
    reinterpret_cast<Set<Path>*>(userData)->insert(path);
}

ClangThread::ClangThread()
    : mState(Idle)
{

}

void ClangThread::run()
{
    CXIndex index = clang_createIndex(0, 1);
    const List<String> &defaultArguments = Server::instance()->options().defaultArguments;

    while (true) {
        SourceInformation source;
        {
            MutexLocker lock(&mMutex);
            mState = Idle;
            while (mPending.isNull() && mState != Done) {
                mCondition.wait(&mMutex);
            }
            if (mState == Done)
                break;
            mState = Parsing;
            std::swap(source, mPending);
        }

        for (int i=0; i<source.builds.size(); ++i) {
            const List<String> args = source.builds.at(i).args;
            int idx = 0;
            List<const char*> clangArgs(args.size() + defaultArguments.size() + 1, 0);

            const List<String> *lists[] = { &args, &defaultArguments };
            for (int i=0; i<2; ++i) {
                const int count = lists[i]->size();
                for (int j=0; j<count; ++j) {
                    clangArgs[idx++] = lists[i]->at(j).constData();
                }
            }
            clangArgs[idx++] = "-I" CLANG_INCLUDEPATH;

            CXTranslationUnit unit = clang_parseTranslationUnit(index, source.sourceFile.constData(),
                                                                clangArgs.data(), idx, 0, 0, 0);
            if (!unit) {
                error() << "Failed to parse" << source;
                continue;
            }
            Set<Path> included;
            clang_getInclusions(unit, inclusionVisitor, &included);
            diagnose(source, unit, included);

            clang_disposeTranslationUnit(unit);
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

void ClangThread::diagnose(const SourceInformation &source, CXTranslationUnit unit, const Set<Path> &files)
{
    const unsigned diagnosticCount = clang_getNumDiagnostics(unit);
    const unsigned options = Server::instance()->options().options;

    Map<Path, Map<unsigned, XmlEntry> > xmlEntries;
    const bool xmlEnabled = testLog(RTags::CompilationErrorXml);

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(unit, i);
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
        const String msg = eatString(clang_getDiagnosticSpelling(diagnostic));
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
                mFixIts[p].insert(FixIt(startOffset, endOffset, string));
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

        for (Set<Path>::const_iterator it = files.begin(); it != files.end(); ++it) {
            if (!xmlEntries.contains(*it)) {
                log(RTags::CompilationErrorXml, "<file name=\"%s\"/>", it->constData());
            }
        }

        logDirect(RTags::CompilationErrorXml, "</checkstyle>");
    }
}

String ClangThread::fixIts(const Path &path) const
{
    MutexLocker lock(&mMutex);
    const Map<Path, Set<FixIt> >::const_iterator it = mFixIts.find(path);
    String out;
    if (it != mFixIts.end()) {
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

void ClangThread::index(const SourceInformation &sourceInfo)
{
    MutexLocker lock(&mMutex);
    mPending = sourceInfo;
    mCondition.wakeOne();
}

void ClangThread::stop()
{
    MutexLocker lock(&mMutex);
    mState = Done;
    mCondition.wakeOne();
}
