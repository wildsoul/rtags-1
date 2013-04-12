#include "Project.h"
#include "FileManager.h"
#include "IndexerJob.h"
#include "RTags.h"
#include "Server.h"
#include "ValidateDBJob.h"
#include <math.h>
#include <rct/Log.h>
#include <rct/MemoryMonitor.h>
#include <rct/Path.h>
#include <rct/Rct.h>
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include <rct/WriteLocker.h>

static void *ModifiedFiles = &ModifiedFiles;
enum { ModifiedFilesTimeout = 50 };

Project::Project(const Path &path)
    : mPath(path), mJobCounter(0)
{
    mWatcher.modified().connect(this, &Project::onFileModified);
    mWatcher.removed().connect(this, &Project::onFileModified);
    mDatabase = Server::instance()->factory().createDatabase();
}

void Project::init()
{
    assert(!isValid());
    mFileManager.reset(new FileManager);
    mFileManager->init(static_pointer_cast<Project>(shared_from_this()));
}

bool Project::restore()
{
    StopWatch timer;
    Path path = mPath;
    RTags::encodePath(path);
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
        goto end;
    }
    {
        int fs;
        in >> fs;
        if (fs != Rct::fileSize(f)) {
            error("%s seems to be corrupted, refusing to restore %s",
                  p.constData(), mPath.constData());
            restoreError = true;
            goto end;
        }
    }
    {

        SourceInformationMap sources;
        in >> sources;
        for (SourceInformationMap::const_iterator it = sources.begin(); it != sources.end(); ++it) {
            index(it->second, IndexerJob::Makefile);
        }
    }
end:
    fclose(f);
    if (restoreError) {
        Path::rm(p);
        return false;
    } else {
        error() << "Restored project" << mPath << "in" << timer.elapsed() << "ms";
    }

    return true;
}

bool Project::isValid() const
{
    return mFileManager;
}

void Project::unload()
{
    mJobs.clear();
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
        const uint32_t id = Location::fileId(path);
        if (isIndexed(id)) {
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

void Project::onJobFinished(shared_ptr<IndexerJob> job)
{
    const uint32_t fileId = job->fileId();
    if (mJobs.value(fileId) != job)
        return;
    mJobs.remove(fileId);

    const int idx = mJobCounter - mJobs.size();

    error("[%3d%%] %d/%d %s Parsed %s in %dms: %s.",
          static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
          String::formatTime(time(0), String::Time).constData(),
          job->path().constData(), job->elapsed(),
          job->symbolCount() == -1 ? "Error" : String::format<32>("%d symbols", job->symbolCount()).constData());
    bool ok;
    PendingJob pending = mPendingJobs.take(fileId, &ok);
    if (ok)
        index(pending.source, pending.type);
}

bool Project::save()
{
    StopWatch timer;
    Path srcPath = mPath;
    RTags::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    const Path p = options.dataDir + srcPath;
    FILE *f = fopen(p.constData(), "w");
    if (!f) {
        error("Can't open file %s", p.constData());
        return false;
    }
    Serializer out(f);
    out << static_cast<int>(Server::DatabaseVersion);
    const int pos = ftell(f);
    out << mSources;
    const int size = ftell(f);
    fseek(f, pos, SEEK_SET);
    out << size;

    error() << "saved project" << path() << "in" << String::format<12>("%dms", timer.elapsed()).constData();
    fclose(f);
    return true;
}

void Project::index(const SourceInformation &sourceInformation, IndexerJob::Type type)
{
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(sourceInformation.sourceFile.constData(), fileFilter))
        return;
    const uint32_t fileId = Location::insertFile(sourceInformation.sourceFile);
    shared_ptr<IndexerJob> &job = mJobs[fileId];
    if (job) {
        const PendingJob pending = { sourceInformation, type };
        mPendingJobs[fileId] = pending;
        return;
    }
    shared_ptr<Project> project = static_pointer_cast<Project>(shared_from_this());

    mSources[fileId] = sourceInformation;
    save(); // do this in a timer?

    if (!mJobCounter++)
        mTimer.start();

    job.reset(new IndexerJob(project, type, sourceInformation));
    job->finished().connectAsync(this, &Project::onJobFinished);
    Server::instance()->startJob(job);
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
                                if (strcmp(res + len - 21, "/gcc-rtags-wrapper.sh") && strcmp(res + len - 6, "/icecc")) {
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

bool Project::index(const Path &sourceFile, const Path &cc, const List<String> &args)
{
    const Path compiler = resolveCompiler(cc.canonicalized());
    SourceInformation sourceInformation = sourceInfo(Location::insertFile(sourceFile));
    const bool js = args.isEmpty() && sourceFile.endsWith(".js");
    bool added = false;
    if (sourceInformation.isNull()) {
        sourceInformation.sourceFile = sourceFile;
    } else if (js) {
        debug() << sourceFile << " is not dirty. ignoring";
        return false;
    } else {
        List<SourceInformation::Build> &builds = sourceInformation.builds;
        const bool allowMultiple = Server::instance()->options().options & Server::AllowMultipleBuildsForSameCompiler;
        for (int j=0; j<builds.size(); ++j) {
            if (builds.at(j).compiler == compiler) {
                if (builds.at(j).args == args) {
                    debug() << sourceFile << " is not dirty. ignoring";
                    return false;
                } else if (!allowMultiple) {
                    builds[j].args = args;
                    added = true;
                    break;
                }
            }
        }
    }
    if (!added)
        sourceInformation.builds.append(SourceInformation::Build(compiler, args));
    index(sourceInformation, IndexerJob::Makefile);
    return true;
}

void Project::onFileModified(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    debug() << file << "was modified" << fileId << mModifiedFiles.contains(fileId);
    if (!fileId || !mModifiedFiles.insert(fileId)) {
        return;
    }
    if (mModifiedFiles.size() == 1 && file.isSource()) {
        startDirtyJobs();
    } else {
        mModifiedFilesTimer.start(shared_from_this(), ModifiedFilesTimeout,
                                  SingleShot, ModifiedFiles);
    }
}

SourceInformationMap Project::sourceInfos() const
{
    MutexLocker lock(&mMutex);
    return mSources;
}

SourceInformation Project::sourceInfo(uint32_t fileId) const
{
    if (fileId) {
        MutexLocker lock(&mMutex);
        return mSources.value(fileId);
    }
    return SourceInformation();
}

void Project::addDependencies(const DependencyMap &deps, Set<uint32_t> &newFiles)
{
    StopWatch timer;

    const DependencyMap::const_iterator end = deps.end();
    for (DependencyMap::const_iterator it = deps.begin(); it != end; ++it) {
        Set<uint32_t> &values = mDependencies[it->first];
        if (values.isEmpty()) {
            values = it->second;
        } else {
            values.unite(it->second);
        }
        if (newFiles.isEmpty()) {
            newFiles = it->second;
        } else {
            newFiles.unite(it->second);
        }
        newFiles.insert(it->first);
    }
}

Set<uint32_t> Project::dependencies(uint32_t fileId, DependencyMode mode) const
{
    MutexLocker lock(&mMutex);
    if (mode == DependsOnArg)
        return mDependencies.value(fileId);

    Set<uint32_t> ret;
    const DependencyMap::const_iterator end = mDependencies.end();
    for (DependencyMap::const_iterator it = mDependencies.begin(); it != end; ++it) {
        if (it->second.contains(fileId))
            ret.insert(it->first);
    }
    return ret;
}

int Project::reindex(const Match &match)
{
    Set<uint32_t> files;
    const DependencyMap::const_iterator end = mDependencies.end();
    for (DependencyMap::const_iterator it = mDependencies.begin(); it != end; ++it) {
        if (match.isEmpty() || match.match(Location::path(it->first))) {
            files += it->first;
        }
    }
    return dirty(files);
}

int Project::remove(const Match &match)
{
    int count = 0;
    {
        MutexLocker lock(&mMutex);
        SourceInformationMap::iterator it = mSources.begin();
        while (it != mSources.end()) {
            if (match.match(it->second.sourceFile)) {
                const uint32_t fileId = it->first;
                mSources.erase(it++);
                shared_ptr<IndexerJob> job = mJobs.value(fileId);
                if (job)
                    job->abort();
                ++count;
            } else {
                ++it;
            }
        }
    }
    return count;
}


int Project::dirty(const Set<uint32_t> &dirty)
{
    int ret = 0;
    Set<uint32_t> dirtyFiles;
    Map<Path, List<String> > toIndex;
    for (Set<uint32_t>::const_iterator it = dirty.begin(); it != dirty.end(); ++it)
        dirtyFiles += mDependencies.value(*it);
    bool indexed = false;
    for (Set<uint32_t>::const_iterator it = dirtyFiles.begin(); it != dirtyFiles.end(); ++it) {
        const SourceInformationMap::const_iterator found = mSources.find(*it);
        if (found != mSources.end()) {
            index(found->second, IndexerJob::Dirty);
            ++ret;
        }
    }
    return ret;
}


bool Project::isIndexed(uint32_t fileId) const
{
    return mDependencies.contains(fileId);
}

SourceInformationMap Project::sources() const
{
    return mSources;
}
DependencyMap Project::dependencies() const
{
    return mDependencies;
}
