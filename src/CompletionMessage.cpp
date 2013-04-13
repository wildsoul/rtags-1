#include "CompletionMessage.h"
#include <rct/Serializer.h>


CompletionMessage::CompletionMessage(const Location &loc)
    : ClientMessage(MessageId), mLocation(loc)
{
}

void CompletionMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mLocation << mContents << mProjects;
}

void CompletionMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mLocation >> mContents >> mProjects;
}
