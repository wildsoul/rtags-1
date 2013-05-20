#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/List.h>
#include <rct/Path.h>
#include <rct/Tr1.h>
#include "SourceInformation.h"

class Project;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual String name() const = 0;
    virtual shared_ptr<Project> createProject(const Path &) = 0;
};

#endif
