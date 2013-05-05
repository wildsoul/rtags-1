#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

#include <rct/List.h>
#include <rct/Path.h>
#include <rct/Plugin.h>
#include <rct/Tr1.h>
#include "RTagsPlugin.h"

class RTagsPluginFactory
{
public:
    ~RTagsPluginFactory()
    {
        cleanup();
    }
    void cleanup()
    {
        for (int i=0; i<mPlugins.size(); ++i) {
            delete mPlugins.at(i);
        }
        mPlugins.clear();
    }
    bool addPlugin(const Path &plugin)
    {
        mError.clear();
        Plugin<RTagsPlugin> *p = new Plugin<RTagsPlugin>(plugin);
        if (!p->instance()) {
            mError = p->error();
            delete p;
            return false;
        }

        mPlugins.append(p);
        return true;
    }

    String error() const { return mError; }

    shared_ptr<Project> createProject(const Path &path)
    {
        shared_ptr<Project> ret;
        for (int i=0; i<mPlugins.size(); ++i) {
            assert(mPlugins.at(i)->instance());
            ret = mPlugins.at(i)->instance()->createProject(path);
            if (ret)
                break;
        }
        return ret;
    }

private:
    List<Plugin<RTagsPlugin> *> mPlugins;
    String mError;
};

#endif
