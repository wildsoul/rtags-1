#ifndef UsrMap_h
#define UsrMap_h

#include <rct/Tr1.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>
#include <string.h>
#include <stdlib.h>

class UsrMap
{
public:
    UsrMap()
        : nextId(0)
    {
    }

    ~UsrMap()
    {
        unordered_map<const char*, uint32_t>::const_iterator it = usrs.begin();
        unordered_map<const char*, uint32_t>::const_iterator end = usrs.end();
        while (it != end) {
            free(const_cast<char*>(it->first));
            ++it;
        }
    }

    uint32_t insert(const char* str)
    {
        unordered_map<const char*, uint32_t>::const_iterator it = usrs.find(str);
        if (it != usrs.end())
            return it->second;
        usrs.insert(std::make_pair(strdup(str), ++nextId));
        return nextId;
    }

    uint32_t value(const char* str) const
    {
        unordered_map<const char*, uint32_t>::const_iterator it = usrs.find(str);
        if (it != usrs.end())
            return it->second;
        return 0;
    }

private:
    struct Hasher
    {
        size_t operator()(const char* str) const
        {
            if (!str)
                return 0;
            // http://www.cse.yorku.ca/~oz/hash.html
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
                return (!str1 && !str2);
            return !strcmp(str1, str2);
        }
    };

private:
    uint32_t nextId;
    unordered_map<const char*, uint32_t, Hasher, Comparator> usrs;

private:
    // non-copyable
    UsrMap(const UsrMap& usr);
    UsrMap& operator=(const UsrMap& usr);
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

private:
    mutable Mutex mutex;
    UsrMap map;

private:
    LockingUsrMap(const LockingUsrMap& usr);
    LockingUsrMap& operator=(const LockingUsrMap& usr);
};

#endif
