#include "FollowLocationJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "Project.h"

FollowLocationJob::FollowLocationJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, 0, project), location(loc)
{
}

void foobar()
{

}

void FollowLocationJob::execute()
{
    foobar(true);
    Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
    if (scope.isNull())
        return;
    Scope<const ErrorSymbolMap&> errorScope = project()->lockErrorSymbolsForRead();
    if (errorScope.isNull())
        return;

    const ErrorSymbolMap::const_iterator e = errorScope.data().find(location.fileId());

    bool foundInError = false;
    SymbolMap::const_iterator it = RTags::findCursorInfo(scope.data(), location, context(),
                                                         e == errorScope.data().end() ? 0 : &e->second, &foundInError);
    const SymbolMap &map = foundInError ? scope.data() : e->second;
    if (it == map.end())
        return;

    if (foundInError) {
        error() << "Found it in errors" << location << it->second;
    }

    const CursorInfo &cursorInfo = it->second;
    if (cursorInfo.isClass() && cursorInfo.isDefinition())
        return;

    Location loc;
    CursorInfo target = cursorInfo.bestTarget(map, &loc);
    if (!loc.isNull()) {
        // ### not respecting DeclarationOnly
        if (cursorInfo.kind != target.kind) {
            if (!target.isDefinition() && !target.targets.isEmpty()) {
                switch (target.kind) {
                case CXCursor_ClassDecl:
                case CXCursor_ClassTemplate:
                case CXCursor_StructDecl:
                case CXCursor_FunctionDecl:
                case CXCursor_CXXMethod:
                case CXCursor_Destructor:
                case CXCursor_Constructor:
                case CXCursor_FunctionTemplate:
                    target = target.bestTarget(map, &loc);
                    break;
                default:
                    break;
                }
            }
        }
        if (!loc.isNull()) {
            if (queryFlags() & QueryMessage::DeclarationOnly && target.isDefinition()) {
                Location declLoc;
                const CursorInfo decl = target.bestTarget(map, &declLoc);
                if (!declLoc.isNull()) {
                    write(declLoc);
                    return;;
                }
            }
            write(loc);
        }
    }
}
