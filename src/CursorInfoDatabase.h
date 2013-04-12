#ifndef ClangDatabase_h
#define ClangDatabase_h

#include <Database.h>
class ClangDatabase : public Database
{
public:
    virtual bool save(Serializer &serializer) const;
    virtual bool load(Deserializer &deserializer) const;

    virtual shared_ptr<Cursor> cursor(const Location &location) const
    {
        Cursors::const_iterator it = mCursors.lower_bound(location);
        if (it != mCursors.end() && it->first == location) {
            return it;
        } else if (it != map.begin()) {
            --it;
            if (it->first.fileId() == location.fileId()) {
                const int off = location.offset() - it->first.offset();
                if (it->second.symbolLength > off)
                    return it->second;
            }
        }
        return shared_ptr<Cursor>();
    }
    virtual String toString(const Cursor &cursor) const { return String(); }
    virtual void status(const String &query, Connection *conn) const;
    virtual void dump(const SourceInformation &sourceInformation, Connection *conn) const = 0;
    virtual int index(const SourceInformation &sourceInformation) = 0;
    virtual void abort(int)
    virtual void sync() = 0;
    virtual List<String> matchSymbolNames(const String &string) const = 0;
    virtual Set<Cursor> findCursors(const String &string) const = 0;

    class ClangCursor : public Cursor
    {
    public:
        ClangCursor() : enumValue(-1), symbolLength(-1) {}
        int enumValue, symbolLength;
    };
    typedef Map<String, Location> SymbolNames;
    typedef Map<Location, shared_ptr<Cursor> > Cursors;
    SymbolNames mSymbolNames;
    Cursors mCursors;
};

#endif
