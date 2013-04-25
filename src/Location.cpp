#include "Location.h"
#include "Server.h"
#include "QueryMessage.h"
#include <rct/Serializer.h>

Map<Path, uint32_t> Location::sPathsToIds;
Map<uint32_t, Path> Location::sIdsToPaths;
Mutex Location::sMutex;

String Location::context() const
{
    const Path p = path();
    FILE *f = fopen(p.constData(), "r");
    String ret;
    if (f) {
        const int l = line();
        for (int i=1; i<l; ++i)
            Rct::readLine(f);
        char buf[1024];
        const int r = Rct::readLine(f, buf, sizeof(buf) - 1);
        ret.append(buf, r);
    }
    return ret;
}
String Location::toString(unsigned flags, const char type) const
{
    String ret = encode();
    if (type != '\0') {
        ret += ' ';
        ret += type;
    }
    if (!(flags & QueryMessage::NoContext))
        ret += '\t' + context();
    return ret;
}
