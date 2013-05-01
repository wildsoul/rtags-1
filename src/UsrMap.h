#ifndef UsrMap_h
#define UsrMap_h

#include <rct/Tr1.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>
#include <string.h>
#include <stdlib.h>

class UsrMap
{
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
        bool operator()(const char* str1, const char* str2) const
        {
            if (!str1 || !str2)
                return str1 == str2;
            return !strcmp(str1, str2);
        }
    };
    typedef unordered_map<const char*, uint32_t, Hasher, Comparator> MapType;

public:
    UsrMap()
        : nextId(0)
    {
    }

    ~UsrMap()
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
    UsrMap(const UsrMap& usr);
    UsrMap& operator=(const UsrMap& usr);

    friend class LockingUsrMap;
};

class LockingUsrMap
{
public:
    LockingUsrMap() { }

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
        for (UsrMap::MapType::const_iterator it = map.usrs.begin(); it != map.usrs.end(); ++it) {
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
            char *string = new char[len + 1];
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
    UsrMap map;

private:
    LockingUsrMap(const LockingUsrMap& usr);
    LockingUsrMap& operator=(const LockingUsrMap& usr);
};


template <> inline Serializer &operator<<(Serializer &s, const LockingUsrMap &u)
{
    u.serialize(s);
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, LockingUsrMap &u)
{
    u.deserialize(s);
    return s;
}

#endif
