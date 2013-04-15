#ifndef Database_h
#define Database_h

#include <Location.h>
#include <rct/Serializer.h>
#include <rct/String.h>
#include <rct/List.h>

class Connection;
class SourceInformation;
class Database
{
public:
    Database() {}
    virtual ~Database() {}

    virtual bool save(Serializer &) const { return false; }
    virtual bool load(Deserializer &) const { return false; }

    class Cursor
    {
    public:
        Cursor() : kind(Invalid), start(-1), end(-1) {}
        enum Kind {
            Invalid,
            MemberFunctionDefinition,
            MemberFunctionDeclaration,
            MethodDefinition,
            MethodDeclaration,
            Class,
            Namespace,
            Struct,
            Variable,
            Parameter,
            Field,
            Enum,
            EnumValue,
            Macro,
            Reference
        };

        static const char *kindToString(Kind kind);
        enum CursorInfoFlag { // these are combined with the keyflags from Location
            IncludeTarget = 0x10,
            IncludeReferences = 0x20
        };
        String toString(unsigned flags) const;
        bool isDefinition() const;
        bool isValid() const { return kind != Invalid; }
        bool isInvalid() const { return kind == Invalid; }
        bool isNull() const { return kind == Invalid; }
        bool isEmpty() const { return kind == Invalid; }

        Location location;
        String symbolName;
        Location target;
        Set<Location> references;
        Kind kind;
        int start, end;

        bool operator==(const Cursor &other) const { return !compare(other); }
        bool operator<(const Cursor &other) const { return compare(other) < 0; }
        bool operator>(const Cursor &other) const { return !compare(other) > 0; }
        int compare(const Cursor &other) const
        {
            if (isDefinition() != other.isDefinition())
                return -1;
            return location.compare(other.location);
        }

    };
    virtual Cursor cursor(const Location &location) const = 0;
    virtual void status(const String &query, Connection *conn) const = 0;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const = 0;
    virtual int index(const SourceInformation &sourceInformation) = 0;
    virtual void remove(const SourceInformation &sourceInformation) = 0;
    virtual bool isIndexing() const = 0;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };

    virtual Set<Path> dependencies(const Path &path, DependencyMode mode) const = 0;
    virtual Set<String> listSymbols(const String &string, const List<Path> &pathFilter) const = 0;
    virtual Set<Cursor> findCursors(const String &string, const List<Path> &pathFilter) const = 0;
    virtual Set<Cursor> cursors(const Path &path) const = 0;
    virtual bool codeCompleteAt(const Location &location, const String &source, Connection *conn) = 0;
};
#endif
