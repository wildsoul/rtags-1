#ifndef SourceInformation_h
#define SourceInformation_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>
#include <rct/Serializer.h>
#include "Location.h"

class SourceInformation
{
public:
    SourceInformation()
        : fileId(0)
    {}

    Path sourceFile;
    mutable uint32_t fileId;
    uint32_t sourceFileId() const
    {
        if (!fileId)
            fileId = Location::insertFile(sourceFile);
        return fileId;
    }

    struct Build
    {
        Build(const Path &c = Path(),
              const List<String> &a = List<String>(),
              const List<String> &d = List<String>(),
              const List<Path> &i = List<Path>(),
              const List<Path> &inc = List<Path>())
            : compiler(c), defines(d), includePaths(i), includes(inc), args(a)
        {}

        inline bool operator==(const Build &other) const
        {
            return (compiler == other.compiler && defines == other.defines
                    && includePaths == other.includePaths && args == other.args
                    && includes == other.includes);
        }
        inline bool operator!=(const Build &other) const { return !operator==(other); }

        Path compiler;
        List<String> defines;
        List<Path> includePaths;
        List<Path> includes;
        List<String> args;
    };
    List<Build> builds;

    inline bool isJS() const
    {
        return builds.isEmpty() && sourceFile.endsWith(".js");
    }

    inline bool isNull() const
    {
        return sourceFile.isEmpty();
    }

    inline String toString() const
    {
        String out = sourceFile + '\n';
        for (int i=0; i<builds.size(); ++i) {
            out += String::format<256>("  %s -D: %s -I: %s -include: %s Args: %s\n",
                                       builds.at(i).compiler.constData(),
                                       String::join(builds.at(i).defines, ' ').constData(),
                                       String::join(builds.at(i).includePaths, ' ').constData(),
                                       String::join(builds.at(i).includes, ' ').constData(),
                                       String::join(builds.at(i).args, ' ').constData());
        }
        return out;
    }
    inline bool operator==(const SourceInformation &other) const
    {
        return sourceFile == other.sourceFile && builds == other.builds;
    }

    inline bool operator!=(const SourceInformation &other) const
    {
        return !operator==(other);
    }
};

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation::Build &b)
{
    s << b.compiler << b.defines << b.includePaths << b.includes << b.args;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation::Build &b)
{
    s >> b.compiler >> b.defines >> b.includePaths >> b.includes >> b.args;
    return s;
}

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.sourceFile << t.builds;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.sourceFile >> t.builds;
    return s;
}

#endif
