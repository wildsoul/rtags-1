#include "QueryMessage.h"
#include "Database.h"
#include <rct/Serializer.h>

QueryMessage::QueryMessage(Type type)
    : ClientMessage(MessageId), mType(type), mFlags(0), mMax(-1), mMinLine(-1), mMaxLine(-1), mBuildIndex(0)
{
}

void QueryMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mQuery << mContext << mType << mFlags << mMax
               << mMinLine << mMaxLine << mBuildIndex << mPathFilters << mProjects;
}

void QueryMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mQuery >> mContext >> mType >> mFlags >> mMax
                 >> mMinLine >> mMaxLine >> mBuildIndex >> mPathFilters >> mProjects;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = Location::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= Location::ShowContext;
    if (queryFlags & QueryMessage::CursorInfoIncludeReferences)
        ret |= Database::Cursor::IncludeReferences;
    if (queryFlags & QueryMessage::CursorInfoIncludeTarget)
        ret |= Database::Cursor::IncludeTarget;
    return ret;
}

Match QueryMessage::match() const
{
    unsigned flags = Match::Flag_StringMatch;
    if (mFlags & MatchRegexp)
        flags |= Match::Flag_RegExp;

    return Match(mQuery, flags);
}
