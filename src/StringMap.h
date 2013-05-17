#ifndef STRINGMAP_H
#define STRINGMAP_H

//#define STRINGMAP_UNORDERED

#ifdef STRINGMAP_UNORDERED // using an unordered_map or a map?
#  include <rct/Tr1.h>
#else
#  include <map>
#endif
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>
#include <string.h>
#include <stdlib.h>

class StringMap
{
#ifdef STRINGMAP_UNORDERED
    struct Hasher
    {
        size_t operator()(const char* str) const
        {
            if (!str)
                return 0;

            // djb2 by Dan Bernstein
            size_t hash = 5381;
            int c;

            while ((c = *str++))
                hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

            return hash;
        }
    };
    struct Comparator
    {
        // equals
        bool operator()(const char* str1, const char* str2) const
        {
            if (!str1 || !str2)
                return str1 == str2;
            return !strcmp(str1, str2);
        }
    };
    typedef unordered_map<const char*, uint32_t, Hasher, Comparator> MapType;
#else
    struct Comparator
    {
        // less-than
        bool operator()(const char* str1, const char* str2) const
        {
            if (!str1 || !str2)
                return (!str1 && str2);
            return (strcmp(str1, str2) < 0);
        }
    };
    typedef std::map<const char*, uint32_t, Comparator> MapType;
#endif

public:
    StringMap()
        : nextId(0)
    {
    }

    ~StringMap()
    {
        MapType::const_iterator it = usrs.begin();
        MapType::const_iterator end = usrs.end();
        while (it != end) {
            free(const_cast<char*>(it->first));
            ++it;
        }
    }

    uint32_t insert(const char* str)
    {
        MapType::const_iterator it = usrs.find(str);
        if (it != usrs.end())
            return it->second;
        usrs.insert(std::make_pair(strdup(str), ++nextId));
        return nextId;
    }

    uint32_t value(const char* str) const
    {
        MapType::const_iterator it = usrs.find(str);
        if (it != usrs.end())
            return it->second;
        return 0;
    }

private:
    uint32_t nextId;
    MapType usrs;

private:
    // non-copyable
    StringMap(const StringMap& usr);
    StringMap& operator=(const StringMap& usr);

    friend class LockingStringMap;
};

class LockingStringMap
{
public:
    LockingStringMap() { }

    uint32_t insert(const char* str)
    {
        MutexLocker locker(&mutex);
        return map.insert(str);
    }

    uint32_t value(const char* str) const
    {
        MutexLocker locker(&mutex);
        return map.value(str);
    }

    void serialize(Serializer &s) const
    {
        s << map.nextId << static_cast<uint32_t>(map.usrs.size());
        for (StringMap::MapType::const_iterator it = map.usrs.begin(); it != map.usrs.end(); ++it) {
            const uint16_t len = strlen(it->first);
            s << len;
            if (len)
                s.write(it->first, len);
            s << it->second;
        }
    }

    void deserialize(Deserializer &s)
    {
        uint32_t size;
        s >> map.nextId >> size;
        while (size--) {
            uint16_t len;
            s >> len;
            char *string = reinterpret_cast<char*>(malloc(len + 1));
            if (len)
                s.read(string, len);
            string[len] = '\0';
            uint32_t val;
            s >> val;
            map.usrs[string] = val;
        }
    }
private:
    mutable Mutex mutex;
    StringMap map;

private:
    LockingStringMap(const LockingStringMap& usr);
    LockingStringMap& operator=(const LockingStringMap& usr);
};


template <> inline Serializer &operator<<(Serializer &s, const LockingStringMap &u)
{
    u.serialize(s);
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, LockingStringMap &u)
{
    u.deserialize(s);
    return s;
}

#endif
