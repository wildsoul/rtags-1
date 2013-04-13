#include "Preprocessor.h"
#include <rct/Connection.h>
#include <rct/Process.h>
#include <rct/Log.h>

static inline String filterPreprocessor(const Path &path)
{
    String ret;
    FILE *f = fopen(path.constData(), "r");
    if (f) {
        char line[1026];
        int r;
        while ((r = Rct::readLine(f, line, sizeof(line) - 1)) != -1) {
            int start = 0;
            while (start < r && isspace(line[start]))
                ++start;
            if (start == r || line[start] != '#')
                continue;
            line[r] = '\n';
            ret.append(line, r + 1);

            int end = r - 1;
            while (end >= start && isspace(line[end]))
                --end;
            while ((r = Rct::readLine(f, line, sizeof(line) - 1)) != -1) {
                line[r] = '\n';
                ret.append(line, r + 1);
                end = r - 1;
                while (end >= 0 && isspace(line[end]))
                    --end;
                if (end < 0 || line[end] != '\\') {
                    break;
                }
            }
        }

        fclose(f);
    }

    return ret;
}


Preprocessor::Preprocessor(const SourceInformation &args, uint8_t buildIndex, Connection *connection)
    : mArgs(args), mBuildIndex(buildIndex), mConnection(connection), mProc(0)
{
    mProc = new Process;
    mProc->finished().connect(this, &Preprocessor::onProcessFinished);
}

Preprocessor::~Preprocessor()
{
    delete mProc;
}

void Preprocessor::preprocess()
{
    List<String> args = mArgs.builds.at(mBuildIndex).args;
    args.append("-E");
    args.append(mArgs.sourceFile);
    List<String> environ;
    environ << "PATH=/usr/local/bin:/usr/bin";
    mProc->start(mArgs.builds.at(mBuildIndex).compiler, args, environ);
}

void Preprocessor::onProcessFinished(Process *)
{
    mConnection->write<256>("// %s %s", mArgs.builds.at(mBuildIndex).compiler.constData(),
                            String::join(mArgs.builds.at(mBuildIndex).args, ' ').constData());
    mConnection->write(mProc->readAllStdOut());
    const String err = mProc->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    deleteLater();
}
