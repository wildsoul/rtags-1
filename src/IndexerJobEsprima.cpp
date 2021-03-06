#include "IndexerJobEsprima.h"
#include "JSParser.h"
#include "Project.h"
#include "RTagsPlugin.h"

class EsprimaPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<IndexerJob> createJob(const shared_ptr<Project> &project,
                                             IndexerJob::Type type,
                                             const SourceInformation &sourceInformation)
    {
        if (sourceInformation.isJS())
            return shared_ptr<IndexerJob>(new IndexerJobEsprima(project, type, sourceInformation));
        return shared_ptr<IndexerJob>();
    }
    virtual shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                             const shared_ptr<Project> &project,
                                             const SourceInformation &sourceInformation)
    {
        if (sourceInformation.isJS())
            return shared_ptr<IndexerJob>(new IndexerJobEsprima(msg, project, sourceInformation));
        return shared_ptr<IndexerJob>();
    }
};

extern "C" {
RTagsPlugin *createInstance()
{
    return new EsprimaPlugin;
}
};

IndexerJobEsprima::IndexerJobEsprima(const shared_ptr<Project> &project,
                                     Type type,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(project, type, sourceInformation)
{}

IndexerJobEsprima::IndexerJobEsprima(const QueryMessage &msg,
                                     const shared_ptr<Project> &project,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation)
{
}

void IndexerJobEsprima::index()
{
    const String contents = mSourceInformation.sourceFile.readAll(1024 * 1024 * 100);
    if (contents.isEmpty()) {
        error() << "Can't open" << mSourceInformation.sourceFile << "for reading";
        return;
    }

    JSParser parser;
    if (!parser.init()) {
        error() << "Can't init JSParser for" << mSourceInformation.sourceFile;
        return;
    }
    if (isAborted())
        return;
    String errors; // ### what to do about this one?
    String dump;
    if (!parser.parse(mSourceInformation.sourceFile, contents, &mData->symbols, &mData->symbolNames,
                      &errors, mType == Dump ? &dump : 0)) {
        error() << "Can't parse" << mSourceInformation.sourceFile;
    }
    mParseTime = time(0);

    if (mType == Dump) {
        dump += "\n";
        {
            Log stream(&dump);
            stream << "symbols:\n";
            for (Map<Location, CursorInfo>::const_iterator it = mData->symbols.begin(); it != mData->symbols.end(); ++it) {
                stream << it->first << it->second.toString(0) << '\n';
            }

            stream << "symbolnames:\n";
            for (Map<String, Set<Location> >::const_iterator it = mData->symbolNames.begin(); it != mData->symbolNames.end(); ++it) {
                stream << it->first << it->second << '\n';
            }

            assert(id() != -1);
        }
        write(dump);

        mData->symbols.clear();
        mData->symbolNames.clear();
    } else {
        mData->dependencies[mFileId].insert(mFileId);
        mData->message = String::format<128>("%s in %dms. (%d syms, %d symNames, %d refs)",
                                             mSourceInformation.sourceFile.toTilde().constData(),
                                             static_cast<int>(mTimer.elapsed()) / 1000, mData->symbols.size(), mData->symbolNames.size(), mData->references.size());
    }
}

