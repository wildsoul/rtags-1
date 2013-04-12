#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

#include <rct/Memory.h>
#include <rct/Path.h>
#include <rct/List.h>
#include <rct/Plugin.h>
#include "RTagsPlugin.h"

class RTagsPluginFactory
{
public:
    ~RTagsPluginFactory()
    {
        for (int i=0; i<mPlugins.size(); ++i) {
            delete mPlugins.at(i);
        }
    }
    bool addPlugin(const Path &plugin)
    {
        Plugin<RTagsPlugin> *p = new Plugin<RTagsPlugin>(plugin);
        if (!p->instance()) {
            delete p;
            return false;
        }

        mPlugins.append(p);
        return true;
    }

    shared_ptr<Database> createDatabase()
    {
        shared_ptr<Database> ret;
        for (int i=0; i<mPlugins.size(); ++i) {
            assert(mPlugins.at(i)->instance());
            ret = mPlugins.at(i)->instance()->createDatabase();
            if (ret)
                break;
        }
        return ret;
    }
private:
    List<Plugin<RTagsPlugin> *> mPlugins;
};

#endif
