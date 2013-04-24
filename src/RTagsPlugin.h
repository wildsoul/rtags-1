#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/List.h>
#include <rct/Path.h>
#include <rct/Tr1.h>

class Database;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual shared_ptr<Database> createDatabase(const Path &) = 0;
};

#endif
