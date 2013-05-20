#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/List.h>
#include <rct/Path.h>
#include <rct/Tr1.h>

class Project;
class Indexer;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual shared_ptr<Indexer> init(shared_ptr<Project> project) = 0;
    virtual shared_ptr<Indexer> indexer() = 0;
    virtual String name() const = 0;
};

#endif
