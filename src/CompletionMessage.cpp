#include "CompletionMessage.h"
#include <rct/Serializer.h>


CompletionMessage::CompletionMessage(const Location &loc)
    : ClientMessage(MessageId), mLocation(loc)
{
}

void CompletionMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mLocation << mContents << projects();
}

void CompletionMessage::decode(Deserializer &deserializer)
{
    List<String> projects;
    deserializer >> mRaw >> mLocation >> mContents >> projects;
    setProjects(projects);
}
