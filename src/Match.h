#ifndef Match_h
#define Match_h

#include <rct/String.h>
#include <rct/RegExp.h>
#include <rct/Log.h>

class Match
{
public:
    enum Flag {
        Flag_None = 0x0,
        Flag_StringMatch = 0x1,
        Flag_RegExp = 0x2,
        Flag_CaseInsensitive = 0x4
    };

    inline Match(const String &pattern = String(), unsigned flags = Flag_StringMatch)
        : mFlags(flags)
    {
        if (flags & Flag_RegExp)
            mRegExp = pattern;
        mPattern = pattern;
    }

    unsigned flags() const { return mFlags; }

    inline Match(const RegExp &regExp)
        : mRegExp(regExp), mPattern(regExp.pattern()), mFlags(Flag_RegExp)
    {}

    inline bool match(const String &text) const
    {
        return indexIn(text) != -1 || indexIn(text, mPattern, mRegExp, mFlags) != -1;
    }

    inline int indexIn(const String &text) const
    {
        return indexIn(mPattern, text, mRegExp, mFlags);
    }
    inline bool isEmpty() const
    {
        return !mFlags || mPattern.isEmpty();
    }

    inline RegExp regExp() const
    {
        return mRegExp;
    }

    inline String pattern() const
    {
        return mPattern;
    }
private:
    static inline int indexIn(const String &pattern, const String &text, const RegExp &regExp, unsigned flags)
    {
        int index = -1;
        if (flags & Flag_StringMatch) {
            index = text.indexOf(pattern, 0, flags & Flag_CaseInsensitive ? String::CaseInsensitive : String::CaseSensitive);
            if (index == -1) {
                const Path p = Path::resolved(pattern);
                const Path p2 = Path::resolved(text);
                if (p != pattern || p2 != text)
                    index = indexIn(p, p2, regExp, flags);
            }
        }
        if (index == -1 && flags & Flag_RegExp)
            index = regExp.indexIn(text);
        return index;
    }
    
    RegExp mRegExp;
    String mPattern;
    unsigned mFlags;
};

inline Log operator<<(Log log, const Match &match)
{
    String ret = "Match(flags: ";
    ret += String::number(match.flags(), 16);
    if (match.regExp().isValid())
        ret += " rx: " + match.regExp().pattern();
    if (!match.pattern().isEmpty())
        ret += " pattern: " + match.pattern();
    ret += ")";
    log << ret;
    return log;
}



#endif
