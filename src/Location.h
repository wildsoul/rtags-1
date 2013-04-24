#ifndef Location_h
#define Location_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>
#include <assert.h>
#include <stdio.h>

class Location
{
public:
    uint64_t mData;

    Location()
        : mData(0)
    {}
    Location(uint64_t data)
        : mData(data)
    {}
    Location(uint32_t fileId, uint32_t line, uint32_t column)
        : mData((uint64_t(column & 0xFFF) << 52) | (uint64_t(line & 0xFFFFF) << 32) | fileId)
    {}

    bool isEmpty() const
    {
        return !mData;
    }

    inline int compare(const Location &other) const
    {
        int diff = fileId() - other.fileId();
        if (diff)
            return diff;
        diff = line() - other.line();
        if (diff)
            return diff;
        diff = column() - other.column();
        return diff;
    }
    inline bool operator==(const Location &other) const { return !compare(other); }
    inline bool operator!=(const Location &other) const { return !operator==(other); }
    inline bool operator<(const Location &other) const { return compare(other) < 0; }
    inline bool operator>(const Location &other) const { return compare(other) > 0; }
    inline bool operator==(const String &str) const { return !compare(Location::decode(str)); }

    static inline uint32_t fileId(const Path &path)
    {
        MutexLocker lock(&sMutex);
        return sPathsToIds.value(path);
    }
    static inline Path path(uint32_t id)
    {
        MutexLocker lock(&sMutex);
        return sIdsToPaths.value(id);
    }

    static inline uint32_t insertFile(const Path &path)
    {
        uint32_t ret;
        {
            MutexLocker lock(&sMutex);
            uint32_t &id = sPathsToIds[path];
            if (!id) {
                id = sIdsToPaths.size() + 1;
                sIdsToPaths[id] = path;
            }
            ret = id;
        }

        return ret;
    }

    inline uint32_t fileId() const { return uint32_t(mData); }
    inline int line() const { return int((mData >> 32) & 0xFFFFF); }
    inline int column() const { return int(mData >> 52); }

    inline Path path() const
    {
        if (mCachedPath.isEmpty()) {
            const uint32_t f = fileId();
            if (f) {
                MutexLocker lock(&sMutex);
                mCachedPath = sIdsToPaths.value(f);
            }
        }
        return mCachedPath;
    }
    inline bool isNull() const { return !mData; }
    inline bool isValid() const { return mData; }
    inline void clear() { mData = 0; mCachedPath.clear(); }

    enum KeyFlag {
        NoFlag = 0x0,
        ShowContext = 0x1
    };
    String toString(unsigned flags = NoFlag) const
    {
        String ret = encode();
        if (flags & ShowContext)
            ret += '\t' + context();
        return ret;
    }

    String context() const;

    static Location fromKey(const char *data)
    {
        Location ret;
        memcpy(&ret.mData, data, sizeof(ret.mData));
        return ret;
    }

    String encode() const
    {
        if (isNull())
            return String();

        const String p = path();
        const int l = line(), c = column();
        int size = p.size() + 3; // account for the '\0'
        int ints[] = { l, c };
        for (int i=0; i<2; ++i) {
            while (ints[i] >= 10) {
                ints[i] /= 10;
                ++size;
            }
            ++size;
        }

        String ret(size, ' ');
        snprintf(ret.data(), size, "%s:%d:%d", p.constData(), l, c);
        ret.resize(size - 1); // we don't want the '\0' inside our actual data
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
        const uint32_t fileId = insertFile(string.left(colon1));
        return Location(fileId, line, column);
    }

    static Map<uint32_t, Path> idsToPaths()
    {
        MutexLocker lock(&sMutex);
        return sIdsToPaths;
    }

    static Map<Path, uint32_t> pathsToIds()
    {
        MutexLocker lock(&sMutex);
        return sPathsToIds;
    }
private:
    friend class Server;
    static Map<Path, uint32_t> sPathsToIds;
    static Map<uint32_t, Path> sIdsToPaths;
    static Mutex sMutex;
    mutable Path mCachedPath;
};

template <> inline int fixedSize(const Location &)
{
    return sizeof(uint64_t);
}

template <> inline Serializer &operator<<(Serializer &s, const Location &t)
{
    s.write(reinterpret_cast<const char*>(&t.mData), sizeof(uint64_t));
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Location &t)
{
    s.read(reinterpret_cast<char*>(&t), sizeof(uint64_t));
    return s;
}

static inline Log operator<<(Log dbg, const Location &loc)
{
    const String out = "Location(" + loc.encode() + ")";
    return (dbg << out);
}

#endif
