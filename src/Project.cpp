#include "Project.h"
#include "FileManager.h"
#include "Server.h"
#include "Database.h"
#include <math.h>
#include <rct/Log.h>
#include <rct/MemoryMonitor.h>
#include <rct/Path.h>
#include <rct/Rct.h>
#include <rct/SHA256.h>
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include <rct/WriteLocker.h>

static void *ModifiedFiles = &ModifiedFiles;
static void *Save = &Save;
enum { ModifiedFilesTimeout = 50, SaveTimeout = 1000 };

Project::Project(const Path &path)
    : mPath(path)
{
    mWatcher.modified().connect(this, &Project::onFileModified);
    mWatcher.removed().connect(this, &Project::onFileRemoved);
    mDatabase = Server::instance()->factory().createDatabase();
}

void Project::init()
{
    assert(!isValid());
    mFileManager.reset(new FileManager);
    mFileManager->init(static_pointer_cast<Project>(shared_from_this()));
}

bool Project::save()
{
    Path srcPath = mPath;
    Server::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    Path p = options.dataDir;
    if (!p.exists()) {
        if (!Path::mkdir(p)) {
            error("Unable to make path %s", p.constData());
            return false;
        }
    } else if (!p.isDir()) {
        error("Path is not a directory %s", p.constData());
        return false;
    }
    p += srcPath;
    FILE *f = fopen(p.constData(), "w");
    if (!f) {
        error("Can't open file %s", p.constData());
        return false;
    }
    String out;
    {
        Serializer o(out);
        o << mSources;
    }
    {
        Serializer o(f);
        o << static_cast<int>(Server::DatabaseVersion) << SHA256::hash(out) << out;
    }
    fclose(f);
    return true;
}

bool Project::restore()
{
    StopWatch timer;
    Path path = mPath;
    Server::encodePath(path);
    const Path p = Server::instance()->options().dataDir + path;
    bool restoreError = false;
    FILE *f = fopen(p.constData(), "r");
    if (!f)
        return false;

    Deserializer in(f);
    int version;
    in >> version;
    if (version != Server::DatabaseVersion) {
        error("Wrong database version. Expected %d, got %d for %s. Removing.", Server::DatabaseVersion, version, p.constData());
        restoreError = true;
    } else {
        String sha, contents;
        in >> sha >> contents;
        if (sha != SHA256::hash(contents)) {
            error("%s seems to be corrupted, refusing to restore %s",
                  p.constData(), mPath.constData());
            restoreError = true;
        } else {
            Deserializer s(contents);
            s >> mSources;
            for (SourceInformationMap::const_iterator it = mSources.begin(); it != mSources.end(); ++it) {
                index(it->second, Restore);
            }
            mSaveTimer.stop();
        }
    }
    fclose(f);
    if (restoreError) {
        Path::rm(p);
        return false;
    }

    return true;
}

bool Project::isValid() const
{
    return mFileManager.get();
}

void Project::unload()
{
    mDatabase.reset();
    mFileManager.reset();
}

bool Project::match(const Match &p, bool *indexed) const
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1].compare(paths[0]) ? 2 : 1;
    bool ret = false;
    for (int i=0; i<count; ++i) {
        const Path &path = paths[i];
        if (isIndexed(path)) {
            if (indexed)
                *indexed = true;
            return true;
        } else if (mFiles.contains(path) || p.match(mPath)) {
            if (!indexed)
                return true;
            ret = true;
        }
    }
    if (indexed)
        *indexed = false;
    return ret;
}

void Project::index(const SourceInformation &sourceInformation, Type type)
{
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(sourceInformation.sourceFile.constData(), fileFilter))
        return;

    shared_ptr<Project> project = static_pointer_cast<Project>(shared_from_this());
    if (type != Restore) {
        mSources[sourceInformation.sourceFile] = sourceInformation;
        mSaveTimer.start(shared_from_this(), SaveTimeout, SingleShot, Save);
    }
    const Path dir = sourceInformation.sourceFile.parentDir();
    if (mWatchedPaths.insert(dir))
        mWatcher.watch(dir);

    mDatabase->index(sourceInformation);
    mSourceIndexed(static_pointer_cast<Project>(shared_from_this()), sourceInformation);

    static const char *names[] = { "index", "dirty", "dump", "restore" };
    error("Indexed %s (%s)", sourceInformation.sourceFile.constData(), names[type]);
}

static inline Path resolveCompiler(const Path &compiler)
{
    Path resolved;
    const char *linkFn;
    const char *fn;
    int fnLen;
    if (compiler.isSymLink()) {
        resolved = compiler.resolved();
        linkFn = resolved.fileName();
        fn = compiler.fileName(&fnLen);
    } else {
        linkFn = fn = compiler.fileName(&fnLen);
    }
    if (!strcmp(linkFn, "gcc-rtags-wrapper.sh") || !strcmp(linkFn, "icecc")) {
        const char *path = getenv("PATH");
        const char *last = path;
        bool done = false;
        bool found = false;
        char buf[PATH_MAX];
        while (!done) {
            switch (*path) {
            case '\0':
                done = true;
            case ':': {
                int len = (path - last);
                if (len > 0 && len + 2 + fnLen < static_cast<int>(sizeof(buf))) {
                    memcpy(buf, last, len);
                    buf[len] = '\0';
                    if (buf[len - 1] != '/')
                        buf[len++] = '/';
                    strcpy(buf + len, fn);
                    if (!access(buf, F_OK|X_OK)) {
                        if (buf == compiler) {
                            found = true;
                        } else if (found) {
                            char res[PATH_MAX];
                            buf[len + fnLen] = '\0';
                            if (realpath(buf, res)) {
                                len = strlen(res);
                                if ((len < 21 || strcmp(res + len - 21, "/gcc-rtags-wrapper.sh"))
                                    && (len < 6 || strcmp(res + len - 6, "/icecc"))) {
                                    return Path(res, len);
                                }
                                // ignore if it there's another wrapper thing in the path
                            } else {
                                return Path(buf, len + fnLen);
                            }
                        }
                    }
                }
                last = path + 1;
                break; }
            default:
                break;
            }
            ++path;
        }
    }
    if (resolved.isEmpty())
        return compiler.resolved();
    return resolved;
}

bool Project::index(const Path &sourceFile, const GccArguments &args)
{
    const Path compiler = resolveCompiler(args.compiler().canonicalized());
    SourceInformation sourceInformation = sourceInfo(sourceFile);
    bool added = false;
    const SourceInformation::Build build(compiler,
                                         args.arguments(),
                                         args.defines(),
                                         args.includePaths(),
                                         args.includes());
    if (sourceInformation.isNull()) {
        sourceInformation.sourceFile = sourceFile;
    } else {
        List<SourceInformation::Build> &builds = sourceInformation.builds;
        const bool allowMultiple = Server::instance()->options().options & Server::AllowMultipleBuildsForSameCompiler;
        for (int j=0; j<builds.size(); ++j) {
            if (builds.at(j).compiler == compiler) {
                if (builds.at(j) == build) {
                    debug() << sourceFile << " is not dirty. ignoring";
                    return false;
                } else if (!allowMultiple) {
                    builds[j] = build;
                    added = true;
                    break;
                }
            }
        }
    }
    if (!added)
        sourceInformation.builds.append(build);
    index(sourceInformation, Index);
    return true;
}

void Project::onFileModified(const Path &file)
{
    debug() << file << "was modified" << mModifiedFiles.contains(file);
    if (!file.isFile() || !mModifiedFiles.insert(file)) {
        return;
    }

    if (mModifiedFiles.size() == 1 && file.isSource()) {
        dirty(mModifiedFiles);
        mModifiedFiles.clear();
    } else {
        mModifiedFilesTimer.start(shared_from_this(), ModifiedFilesTimeout,
                                  SingleShot, ModifiedFiles);
    }
}

void Project::onFileRemoved(const Path &path)
{
    if (mSources.remove(path))
        mDatabase->remove(path);
}

SourceInformationMap Project::sourceInfos() const
{
    return mSources;
}

SourceInformation Project::sourceInfo(const Path &path) const
{
    return mSources.value(path);
}

int Project::reindex(const Match &match)
{
    int ret = 0;

    const Set<Path> files = mDatabase->files();
    Set<Path>::const_iterator file = files.begin();
    const Set<Path>::const_iterator end = files.end();
    while (file != end) {
        if (match.match(*file)) {
            SourceInformationMap::const_iterator info = mSources.find(*file);
            if (info == mSources.end()) {
                ++file;
                continue;
            }

            index(info->second, Dirty);
            ++ret;

            const Set<Path> subfiles = mDatabase->dependencies(*file, Database::DependsOnArg);
            Set<Path>::const_iterator subfile = subfiles.begin();
            const Set<Path>::const_iterator subend = subfiles.end();
            while (subfile != subend) {
                info = mSources.find(*subfile);
                if (info == mSources.end()) {
                    ++subfile;
                    continue;
                }
                index(info->second, Dirty);
                ++ret;
                ++subfile;
            }
        }
        ++file;
    }
    return ret;
}

int Project::remove(const Match &match)
{
    int count = 0;
    SourceInformationMap::iterator it = mSources.begin();
    while (it != mSources.end()) {
        if (match.match(it->second.sourceFile)) {
            mSources.erase(it++);
            // ### gotta remove from database
            ++count;
        } else {
            ++it;
        }
    }
    return count;
}


int Project::dirty(const Set<Path> &dirty)
{
    int ret = 0;
    Set<Path> dirtyFiles = dirty;
    Map<Path, List<String> > toIndex;
    for (Set<Path>::const_iterator it = dirty.begin(); it != dirty.end(); ++it)
        dirtyFiles += mDatabase->dependencies(*it, Database::DependsOnArg);
    for (Set<Path>::const_iterator it = dirtyFiles.begin(); it != dirtyFiles.end(); ++it) {
        const SourceInformationMap::const_iterator found = mSources.find(*it);
        if (found != mSources.end()) {
            index(found->second, Dirty);
            ++ret;
        }
    }
    return ret;
}

void Project::timerEvent(TimerEvent *e)
{
    if (e->userData() == ModifiedFiles) {
        dirty(mModifiedFiles);
        mModifiedFiles.clear();
    } else if (e->userData() == Save) {
        save();
    }
}


bool Project::isIndexed(const Path &file) const
{
    return mSources.contains(file) || !mDatabase->dependencies(file, Database::DependsOnArg).isEmpty();
}

SourceInformationMap Project::sources() const
{
    return mSources;
}

bool Project::isIndexing() const
{
    return mDatabase->isIndexing();
}
