#include "Database.h"

const char * Database::Cursor::kindToString(Kind kind)
{
    const char *names[] = {
        "Invalid",
        "MemberFunctionDefinition",
        "MemberFunctionDeclaration",
        "MethodDefinition",
        "MethodDeclaration",
        "Class",
        "Namespace",
        "Struct",
        "Variable",
        "Parameter",
        "Field",
        "Enum",
        "EnumValue",
        "Macro",
        "Reference",
        0
    };
    return names[kind];
}

String Database::Cursor::toString(unsigned flags) const
{
    
}

bool Database::Cursor::isDefinition() const
{
    switch (kind) {
    case Invalid:
    case MemberFunctionDeclaration:
    case MethodDeclaration:
    case Reference:
        return false;
    case Macro:
    case MemberFunctionDefinition:
    case MethodDefinition:
    case Class:
    case Namespace:
    case Struct:
    case Variable:
    case Parameter:
    case Field:
    case Enum:
    case EnumValue:
        return true;
    }
    return false;
}
