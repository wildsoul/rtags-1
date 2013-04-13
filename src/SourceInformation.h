#ifndef SourceInformation_h
#define SourceInformation_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>
#include <rct/Serializer.h>

class SourceInformation
{
public:
    SourceInformation()
    {}

    Path sourceFile;

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
        List<String> includes;
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

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.sourceFile << t.builds.size();
    for (int i=0; i<t.builds.size(); ++i) {
        s << t.builds.at(i).compiler << t.builds.at(i).defines << t.builds.at(i).includePaths
          << t.builds.at(i).includes << t.builds.at(i).args;
    }

    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.sourceFile;
    int size;
    s >> size;
    t.builds.resize(size);
    for (int i=0; i<size; ++i) {
        s >> t.builds[i].compiler >> t.builds[i].defines >> t.builds.at(i).includePaths
          >> t.builds[i].includes >> t.builds.at(i).args;
    }
    return s;
}

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

#endif
