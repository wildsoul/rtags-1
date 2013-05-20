#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

#include <rct/Map.h>
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
        Map<String, Plugin<RTagsPlugin> *>::const_iterator plugin = mPlugins.begin();
        const Map<String, Plugin<RTagsPlugin> *>::const_iterator end = mPlugins.end();
        while (plugin != end) {
            delete plugin->second;
            ++plugin;
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

        const String name = p->instance()->name();
        if (mPlugins.contains(name)) {
            ::error() << "Already got a plugin by name" << name;
            return false;
        }
        mPlugins[name] = p;
        return true;
    }

    String error() const { return mError; }

    shared_ptr<Project> createProject(const Path &path, const String& pluginName)
    {
        Map<String, Plugin<RTagsPlugin> *>::const_iterator plugin = mPlugins.find(pluginName);
        if (plugin == mPlugins.end())
            return shared_ptr<Project>();
        assert(plugin->second->instance());
        return plugin->second->instance()->createProject(path);
    }

    RTagsPlugin* plugin(const String& pluginName) const
    {
        Map<String, Plugin<RTagsPlugin> *>::const_iterator plugin = mPlugins.find(pluginName);
        if (plugin == mPlugins.end())
            return 0;
        assert(plugin->second->instance());
        return plugin->second->instance();
    }

private:
    Map<String, Plugin<RTagsPlugin> *> mPlugins;
    String mError;
};

#endif
