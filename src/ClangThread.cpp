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
ClangThread::ClangThread()
    : mDone(false)
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
            while (mPending.isNull() && !mDone) {
                mCondition.wait(&mMutex);
            }
            if (mDone)
                break;
            std::swap(source, mPending);
        }

        for (int i=0; i<source.builds.size(); ++i) {
            const List<String> args = source.builds.at(i).args;
            int idx = 0;
            List<const char*> clangArgs(args.size() + defaultArguments.size(), 0);

            const List<String> *lists[] = { &args, &defaultArguments };
            for (int i=0; i<2; ++i) {
                const int count = lists[i]->size();
                for (int j=0; j<count; ++j) {
                    clangArgs[idx++] = lists[i]->at(j).constData();
                }
            }


            CXTranslationUnit unit = clang_parseTranslationUnit(index, source.sourceFile.constData(),
                                                                clangArgs.data(), idx, 0, 0, 0);
            if (!unit) {
                error() << "Failed to parse" << source;
                continue;
            }
            diagnose(source, unit);

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


void ClangThread::diagnose(const SourceInformation &source, CXTranslationUnit unit)
{
}
//     List<String> compilationErrors;
//     const unsigned diagnosticCount = clang_getNumDiagnostics(unit);
//     const unsigned options = Server::instance()->options().options;

//     Map<Path, Map<unsigned, XmlEntry> > xmlEntries;
//     const bool xmlEnabled = testLog(RTags::CompilationErrorXml);

//     for (unsigned i=0; i<diagnosticCount; ++i) {
//         CXDiagnostic diagnostic = clang_getDiagnostic(unit, i);
//         int logLevel = INT_MAX;
//         const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
//         switch (severity) {
//         case CXDiagnostic_Fatal:
//         case CXDiagnostic_Error:
//             logLevel = Error;
//             break;
//         case CXDiagnostic_Warning:
//             logLevel = Warning;
//             break;
//         case CXDiagnostic_Note:
//             logLevel = Debug;
//             break;
//         case CXDiagnostic_Ignored:
//             break;
//         }

//         const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
//         const String msg = eatString(clang_getDiagnosticSpelling(diagnostic));
//         if (xmlEnabled) {
//             const CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diagnostic);
//             XmlEntry::Type type = XmlEntry::None;
//             switch (sev) {
//             case CXDiagnostic_Warning:
//                 type = XmlEntry::Warning;
//                 break;
//             case CXDiagnostic_Error:
//             case CXDiagnostic_Fatal:
//                 type = XmlEntry::Error;
//                 break;
//             default:
//                 break;
//             }
//             if (type != XmlEntry::None) {
//                 const unsigned rangeCount = clang_getDiagnosticNumRanges(diagnostic);
//                 if (!rangeCount) {
//                     unsigned line, column, offset;
//                     CXFile file;
//                     clang_getSpellingLocation(diagLoc, &file, &line, &column, &offset);
//                     Path path = eatString(clang_getFileName(file));
//                     path.resolve(); // ### ???
//                     xmlEntries[path][offset] = XmlEntry(type, msg, line, column);
//                 } else {
//                     for (unsigned rangePos = 0; rangePos < rangeCount; ++rangePos) {
//                         const CXSourceRange range = clang_getDiagnosticRange(diagnostic, rangePos);
//                         const CXSourceLocation start = clang_getRangeStart(range);
//                         const CXSourceLocation end = clang_getRangeEnd(range);

//                         unsigned line, column, startOffset, endOffset;
//                         CXFile file;
//                         clang_getSpellingLocation(start, &file, &line, &column, &startOffset);
//                         clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
//                         Path path = eatString(clang_getFileName(file));
//                         path.resolve(); // ### ???
//                         if (!rangePos && !startOffset && !endOffset) {
//                             // huh, range invalid? fall back to diag location
//                             unsigned offset;
//                             clang_getSpellingLocation(diagLoc, &file, &line, &column, &offset);
//                             path = eatString(clang_getFileName(file));
//                             path.resolve(); // ### ???
//                             xmlEntries[path][offset] = XmlEntry(type, msg, line, column);
//                             break;
//                         } else {
//                             xmlEntries[path][startOffset] = XmlEntry(type, msg, line, column, endOffset);
//                         }
//                     }
//                 }
//             }
//             if (testLog(logLevel) || testLog(RTags::CompilationError)) {
//                 if (testLog(logLevel))
//                     logDirect(logLevel, msg.constData());
//                 if (testLog(RTags::CompilationError))
//                     logDirect(RTags::CompilationError, msg.constData());
//             }

//             const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
//             RegExp rx;
//             if (options & Server::IgnorePrintfFixits) {
//                 rx = "^%[A-Za-z0-9]\\+$";
//             }
//             for (unsigned f=0; f<fixItCount; ++f) {
//                 unsigned startOffset, line, column;
//                 CXFile file;
//                 CXSourceRange range;
//                 const CXString string = clang_getDiagnosticFixIt(diagnostic, f, &range);
//                 clang_getSpellingLocation(clang_getRangeStart(range), &file, &line, &column, &startOffset);

//                 const char *cstr = clang_getCString(string);
//                 unsigned endOffset;
//                 clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);
//                 if (options & Server::IgnorePrintfFixits && rx.indexIn(cstr) == 0) {
//                     error("Ignored fixit for %s: Replace %d-%d with [%s]", path().constData(),
//                           startOffset, endOffset, string);
//                 } else {
//                     error("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
//                           startOffset, endOffset, cstr);
//                     if (xmlEnabled) {
//                         XmlEntry& entry = xmlEntries[loc.fileId()][startOffset];
//                         entry.type = XmlEntry::Fixit;
//                         if (entry.message.isEmpty()) {
//                             entry.message = String::format<64>("did you mean '%s'?", cstr);
//                             entry.line = line;
//                             entry.column = column;
//                         }
//                         entry.endOffset = endOffset;
//                     }
//                     if (testLog(logLevel) || testLog(RTags::CompilationError)) {
//                         const String msg = String::format<128>("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
//                                                                startOffset, endOffset, cstr);
//                         if (testLog(logLevel))
//                             logDirect(logLevel, msg.constData());
//                         if (testLog(RTags::CompilationError))
//                             logDirect(RTags::CompilationError, msg.constData());
//                     }
//                     mData->fixIts[loc.fileId()].insert(FixIt(startOffset, endOffset, string));
//                 }
//                 clang_disposeString(string);
//             }
//         }

//         clang_disposeDiagnostic(diagnostic);
//     }
//     if (xmlEnabled) {
//         logDirect(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>");
//         if (!xmlEntries.isEmpty()) {
//             Map<uint32_t, Map<int, XmlEntry> >::const_iterator entry = xmlEntries.begin();
//             const Map<uint32_t, Map<int, XmlEntry> >::const_iterator end = xmlEntries.end();

//             const char* severities[] = { "none", "warning", "error", "fixit" };

//             while (entry != end) {
//                 log(RTags::CompilationErrorXml, "<file name=\"%s\">", Location::path(entry->first).constData());
//                 const Map<int, XmlEntry>& map = entry->second;
//                 Map<int, XmlEntry>::const_iterator it = map.begin();
//                 const Map<int, XmlEntry>::const_iterator end = map.end();
//                 while (it != end) {
//                     const XmlEntry& entry = it->second;
//                     log(RTags::CompilationErrorXml, "<error line=\"%d\" column=\"%d\" startOffset=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
//                         entry.line, entry.column, it->first,
//                         (entry.endOffset == -1 ? "" : String::format<32>("endOffset=\"%d\" ", entry.endOffset).constData()),
//                         severities[entry.type], xmlEscape(entry.message).constData());
//                     ++it;
//                 }
//                 logDirect(RTags::CompilationErrorXml, "</file>");
//                 ++entry;
//             }
//         }

//         for (Set<uint32_t>::const_iterator it = mVisitedFiles.begin(); it != mVisitedFiles.end(); ++it) {
//             if (!xmlEntries.contains(*it)) {
//                 const String fn = Location::path(*it);
//                 log(RTags::CompilationErrorXml, "<file name=\"%s\"/>", fn.constData());
//             }
//         }

//         logDirect(RTags::CompilationErrorXml, "</checkstyle>");
//     }
//     return !isAborted();
// }

String ClangThread::fixIts(const Path &path) const
{
    MutexLocker lock(&mMutex);
    return mFixIts.value(path);
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
    mDone = true;
    mCondition.wakeOne();
}
