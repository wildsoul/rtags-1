#include "Database.h"

const char * Database::Cursor::kindToString(Kind kind)
{
    const char *names[] = {
        "Invalid",
        "File",
        "MemberFunctionDefinition",
        "MemberFunctionDeclaration",
        "MethodDefinition",
        "MethodDeclaration",
        "Class",
        "ClassForwardDeclaration",
        "Namespace",
        "Struct",
        "StructForwardDeclaration",
        "Variable",
        "Argument",
        "Field",
        "Enum",
        "EnumValue",
        "Union",
        "Macro",
        "Reference",
        0
    };
    return names[kind];
}

char Database::Cursor::kindToChar(Kind kind)
{
    const char chars[] = {
        '0', // Invalid
        'p', // File
        'F', // MemberFunctionDefinition
        'f', // MemberFunctionDeclaration
        'M', // MethodDefinition
        'm', // MethodDeclaration
        'C', // Class
        'c', // ClassForwardDeclaration
        'n', // Namespace
        'S', // Struct
        's', // StructForwardDeclaration,
        'v', // Variable
        'a', // Argument
        'l', // Field
        'E', // Enum
        'e', // EnumValue
        'u', // Union
        'D', // Macro
        'r' // Reference
    };
    return chars[kind];
}

String Database::Cursor::toString(unsigned flags) const
{
    String ret = String::format<1024>("SymbolName: %s\n"
                                      "Kind: %s\n"
                                      "%s" // range
                                      "%s", // definition
                                      symbolName.constData(),
                                      kindToString(kind),
                                      (start != -1 && end != -1 ? String::format<32>("Range: %d-%d\n", start, end).constData() : ""),
                                      isDefinition() ? "Definition\n" : "");

    if (!target.isEmpty() && flags & IncludeTarget) {
        ret.append("Target: ");
        ret.append(target.key(flags));
    }
#warning need to do this, will need the Database pointer to do it though
    /*
    if (!references.isEmpty() && !(flags & IncludeReferences)) {
        ret.append("References:");
        for (Set<Location>::const_iterator rit = references.begin(); rit != references.end(); ++rit) {
            const Location &l = *rit;
            ret.append("\n    ");
            ret.append(l.key(flags));
        }
        ret.append('\n');
    }
    */
    return ret;
}

bool Database::Cursor::isDefinition() const
{
    switch (kind) {
    case Invalid:
    case MemberFunctionDeclaration:
    case MethodDeclaration:
    case Reference:
    case ClassForwardDeclaration:
    case StructForwardDeclaration:
        return false;
    case File:
    case Macro:
    case MemberFunctionDefinition:
    case MethodDefinition:
    case Class:
    case Namespace:
    case Struct:
    case Variable:
    case Argument:
    case Field:
    case Enum:
    case EnumValue:
    case Union:
        return true;
    }
    return false;
}
