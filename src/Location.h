#ifndef Location_h
#define Location_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/Rct.h>
#include <assert.h>
#include <stdio.h>

class Location
{
public:
    Location(const Path &path = Path(), int line = 0, int column = 0)
        : mPath(path), mLine(line), mColumn(column)
    {
        assert(line >= 0);
        assert(column >= 0);
        assert(path.isEmpty() == !line);
        assert(path.isEmpty() == !column);
    }
    inline Path path() const { return mPath; }
    int line() const { return mLine; }
    int column() const { return mColumn; }
    inline bool isNull() const { return !mLine; }
    inline bool isValid() const { return mLine; }
    inline bool isEmpty() const { return !mLine; }
    inline void clear() { mPath.clear(); mLine = mColumn = 0; }
    inline int compare(const Location &other) const
    {
        int diff = mPath.compare(other.mPath);
        if (diff)
            return diff;
        diff = other.mLine - mLine;
        if (diff)
            return diff;
        diff = other.mColumn - mColumn;
        return diff;
    }
    inline bool operator==(const Location &other) const { return !compare(other); }
    inline bool operator!=(const Location &other) const { return !operator==(other); }
    inline bool operator<(const Location &other) const { return compare(other) < 0; }
    inline bool operator>(const Location &other) const { return compare(other) > 0; }
    inline bool operator==(const String &str) const { return !compare(Location::decode(str)); }

    enum KeyFlag {
        NoFlag = 0x0,
        ShowContext = 0x2
    };

    String key(unsigned flags = NoFlag) const
    {
        if (isNull())
            return String();
        if (flags & ShowContext)
            return encode() + '\t' + context();
        return encode();
    }

    String context() const
    {
        if (!isEmpty()) {
            Path p = path();
            FILE *f = fopen(p.constData(), "r");
            char buf[1024];
            if (f) {
                int len = 0;
                for (int i=1; i<mLine; ++i)
                    len = Rct::readLine(f, buf, sizeof(buf) - 1);
                fclose(f);
                return String(buf, len);
            }
        }
        return String();
    }

    String encode() const
    {
        int size = mPath.size() + 3;
        int ints[] = { mLine, mColumn };
        for (int i=0; i<2; ++i) {
            while (ints[i] >= 10) {
                ints[i] /= 10;
                ++size;
            }
            ++size;
        }
        
        String ret(size, ' ');
        snprintf(ret.data(), size, "%s:%d:%d", mPath.constData(), mLine, mColumn);
        return ret;
    }

    static Location decode(const String &string)
    {
        const int colon2 = string.lastIndexOf(':');

        if (colon2 == -1) {
            error("Can't create location from this: %s", string.constData());
            return Location();
        }
        const int colon1 = string.lastIndexOf(':', colon2 - 1);
        if (colon1 <= 0) {
            error("Can't create location from this: %s", string.constData());
            return Location();
        }
        const int column = string.mid(colon2 + 1).toLongLong();
        const int line = string.mid(colon1 + 1, colon2 - colon1).toLongLong();
        if (colon1 <= 0 || line <= 0) {
            error("Can't create location from this: %s", string.constData());
            return Location();
        }
        return Location(string.left(colon1), line, column);
    }
private:
    static inline int digits(int len)
    {
        int ret = 1;
        while (len >= 10) {
            len /= 10;
            ++ret;
        }
        return ret;
    }
    
    Path mPath;
    int mLine, mColumn;
};

template <> inline Serializer &operator<<(Serializer &s, const Location &t)
{
    s << t.path() << t.line() << t.column();
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Location &t)
{
    Path p;
    int l, c;
    s >> p >> l >> c;
    t = Location(p, l, c);
    return s;
}

static inline Log operator<<(Log dbg, const Location &loc)
{
    const String out = "Location(" + loc.key() + ")";
    return (dbg << out);
}

#endif
