#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/String.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();

    bool parse(String args, const Path &base);
    void clear();

    Lang language() const { return mLanguage; }
    List<String> arguments() const { return mArgs; }
    List<String> defines() const { return mDefines; }
    List<Path> includePaths() const { return mIncludePaths; }
    List<Path> includes() const { return mIncludes; }
    List<Path> inputFiles() const { return mInputFiles; }
    List<Path> unresolvedInputFiles() const { return mUnresolvedInputFiles; }
    Path baseDirectory() const { return mBase; }
    Path compiler() const { return mCompiler; }
    Path projectRoot() const;
private:
    List<String> mArgs;
    List<String> mDefines;
    List<Path> mIncludePaths, mIncludes;
    List<Path> mInputFiles, mUnresolvedInputFiles;
    Path mBase, mCompiler;
    GccArguments::Lang mLanguage;
};

#endif
