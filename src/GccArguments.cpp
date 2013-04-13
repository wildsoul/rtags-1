#include "GccArguments.h"
#include "RTags.h"
#include "Server.h"
#include <rct/Log.h>
#include <rct/Process.h>

GccArguments::GccArguments()
    : mLanguage(NoLang)
{
}

void GccArguments::clear()
{
    mArgs.clear();
    mDefines.clear();
    mIncludePaths.clear();
    mInputFiles.clear();
    mUnresolvedInputFiles.clear();
    mBase.clear();
    mCompiler.clear();
    mLanguage = NoLang;
}

static inline GccArguments::Lang guessLang(const Path &fullPath)
{
    String compiler = fullPath.fileName();
    String c;
    int dash = compiler.lastIndexOf('-');
    if (dash >= 0) {
        c = String(compiler.constData() + dash + 1, compiler.size() - dash - 1);
    } else {
        c = String(compiler.constData(), compiler.size());
    }

    if (c.size() != compiler.size()) {
        bool isVersion = true;
        for (int i=0; i<c.size(); ++i) {
            if ((c.at(i) < '0' || c.at(i) > '9') && c.at(i) != '.') {
                isVersion = false;
                break;
            }
        }
        if (isVersion) {
            dash = compiler.lastIndexOf('-', dash - 1);
            if (dash >= 0) {
                c = compiler.mid(dash + 1, compiler.size() - c.size() - 2 - dash);
            } else {
                c = compiler.left(dash);
            }
        }
    }

    GccArguments::Lang lang = GccArguments::NoLang;
    if (c.startsWith("g++") || c.startsWith("c++")) {
        lang = GccArguments::CPlusPlus;
    } else if (c.startsWith("gcc") || c.startsWith("cc")) {
        lang = GccArguments::C;
    }
    return lang;
}

static inline void eatAutoTools(List<String> &args)
{
    List<String> copy = args;
    for (int i=0; i<args.size(); ++i) {
        const String &arg = args.at(i);
        if (arg.endsWith("cc") || arg.endsWith("g++") || arg.endsWith("c++") || arg == "cd") {
            if (i) {
                args.erase(args.begin(), args.begin() + i);
                if (testLog(Debug)) {
                    debug() << "ate something " << copy;
                    debug() << "now we have " << args;
                }
            }
            break;
        }
    }
}

static inline String trim(const char *start, int size)
{
    while (size && isspace(*start)) {
        ++start;
        --size;
    }
    while (size && isspace(start[size - 1])) {
        --size;
    }
    return String(start, size);
}

bool GccArguments::parse(String args, const Path &base)
{
    clear();
    mInputFiles.clear();
    mBase = base;

    char quote = '\0';
    List<String> split;
    String old2 = args;
    {
        char *cur = args.data();
        char *prev = cur;
        // ### handle escaped quotes?
        int size = args.size();
        while (size > 0) {
            switch (*cur) {
            case '"':
            case '\'':
                if (quote == '\0')
                    quote = *cur;
                else if (*cur == quote)
                    quote = '\0';
                break;
            case ' ':
                if (quote == '\0') {
                    if (cur > prev)
                        split.append(trim(prev, cur - prev));
                    prev = cur + 1;
                }
                break;
            default:
                break;
            }
            --size;
            ++cur;
        }
        if (cur > prev)
            split.append(trim(prev, cur - prev));
    }
    eatAutoTools(split);

    if (split.isEmpty()) {
        clear();
        return false;
    }
    debug() << "GccArguments::parse (" << args << ") => " << split;

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), Path::MakeAbsolute, base);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = base;
    }
    if (split.isEmpty())
        return false;

    if (split.first().endsWith("rtags-gcc-prefix.sh")) {
        if (split.size() == 1)
            return false;
        split.removeAt(0);
    }

    mLanguage = guessLang(split.front());
    if (mLanguage == NoLang) {
        clear();
        return false;
    }

    const int s = split.size();
    bool seenCompiler = false;
    String arg;
    for (int i=0; i<s; ++i) {
        arg = split.at(i);
        if (arg.isEmpty())
            continue;
        if ((arg.startsWith('\'') && arg.endsWith('\'')) ||
            (arg.startsWith('"') && arg.endsWith('"')))
            arg = arg.mid(1, arg.size() - 2);
        if (arg.startsWith('-')) {
            if (arg.startsWith("-x")) {
                String a;
                if (arg.size() == 2 && i + 1 < s) {
                    a = split.at(++i);
                } else {
                    a = arg.mid(2);
                }
                if (a == "c-header" || a == "c++-header")
                    return false;
                mArgs.append("-x");
                mArgs.append(a);
            } else if (arg.startsWith("-D")) {
                if (arg.size() == 2 && i + 1 < s) {
                    mDefines.append(split.at(++i));
                    mArgs.append("-D" + split.at(i));
                } else {
                    mDefines.append(arg.mid(2));
                    mArgs.append(arg);
                }
            } else if (arg.startsWith("-I")) {
                Path inc;
                bool ok = false;
                if (arg.size() > 2) {
                    inc = Path::resolved(arg.mid(2), Path::RealPath, path, &ok);
                } else if (i + 1 < s) {
                    inc = Path::resolved(split.at(++i), Path::RealPath, path, &ok);
                }
                if (ok) {
                    mIncludePaths.append(inc);
                    mArgs.append("-I" + inc);
                }
            } else if (arg == "-include") {
                if (i + 1 < s) {
                    bool ok;
                    Path inc = Path::resolved(split.at(++i), Path::RealPath, path, &ok);
                    if (ok) {
                        mIncludes.append(inc);
                        mArgs.append(arg);
                        mArgs.append(inc);
                    }
                }
            }
        } else {
            if (!seenCompiler) {
                seenCompiler = true;
            } else {
                Path input = Path::resolved(arg, Path::MakeAbsolute, path);
                if (input.isSource()) {
                    mUnresolvedInputFiles.append(input);
                    input.resolve(Path::RealPath);
                    mInputFiles.append(input);
                }
            }
        }
    }

    if (mUnresolvedInputFiles.isEmpty()) {
        clear();
        return false;
    }

    if (mInputFiles.isEmpty()) {
        error("Unable to find or resolve input files");
        const int c = mUnresolvedInputFiles.size();
        for (int i=0; i<c; ++i) {
            const String &input = mUnresolvedInputFiles.at(i);
            error("  %s", input.constData());
        }
        clear();
        return false;
    }

    static Map<Path, Path> resolvedFromPath;
    Path &compiler = resolvedFromPath[split.front()];
    if (compiler.isEmpty()) {
        compiler = Process::findCommand(split.front());
        if (compiler.isEmpty()) {
            compiler = split.front();
        }
    }
    mCompiler = compiler;
    return true;
}

Path GccArguments::projectRoot() const
{
    const List<Path> *files[] = { &mUnresolvedInputFiles, &mInputFiles };
    for (int i=0; i<2; ++i) {
        const List<Path> &list = *files[i];
        for (int j=0; j<list.size(); ++j) {
            Path src = list.at(j);
            if (!src.isAbsolute())
                src.prepend(mBase);
            Path srcRoot = RTags::findProjectRoot(src);
            if (!srcRoot.isEmpty()) {
                return srcRoot;
            }
        }
    }
    return Path();
}
