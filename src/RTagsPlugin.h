#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/Memory.h>
#include <rct/Path.h>
#include <rct/List.h>

class Database;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual shared_ptr<Database> createDatabase() = 0;
};

#endif
