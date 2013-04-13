#ifndef CompletionMessage_h
#define CompletionMessage_h

#include "ClientMessage.h"
#include "Location.h"
#include <rct/Path.h>
#include <rct/String.h>

class CompletionMessage : public ClientMessage
{
public:
    enum { MessageId = CompletionId };

    CompletionMessage(const Location &location);

    Location location() const { return mLocation; }

    void setContents(const String &contents) { mContents = contents; }
    String contents() const { return mContents; }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);

    void setProjects(const List<String> &projects) { mProjects = projects; }
    List<String> projects() const { return mProjects; }
private:
    Location mLocation;
    String mContents;
    List<String> mProjects;
};

#endif
