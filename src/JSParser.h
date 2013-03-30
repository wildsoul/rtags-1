#ifndef JSParser_h
#define JSParser_h

#include <RTags.h>
#include <Location.h>
#include <CursorInfo.h>
#include <rct/Log.h>
#include <rct/Map.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <v8.h>

class JSParser
{
public:
    JSParser();
    ~JSParser();
    bool init();

    bool parse(const Path &path, const String &contents,
               SymbolMap *cursors,
               SymbolNameMap *symbolNames,
               String *errors,
               String *json = 0);
private:
    enum Flag {
        None = 0x0,
        Scope = 0x1
    };
    enum HandlerType {
        NoHandlerType,
        Body,
        Array,


        ArrayExpression,
        AssignmentExpression,
        BinaryExpression,
        BlockStatement,
        BreakStatement,
        CallExpression,
        CatchClause,
        ConditionalExpression,
        ContinueStatement,
        DoWhileStatement,
        EmptyStatement,
        ExpressionStatement,
        ForInStatement,
        ForStatement,
        FunctionDeclaration,
        FunctionExpression,
        Identifier,
        IfStatement,
        Literal,
        LogicalExpression,
        MemberExpression,
        NewExpression,
        ObjectExpression,
        Program,
        Property,
        ReturnStatement,
        ThisExpression,
        ThrowStatement,
        TryStatement,
        UnaryExpression,
        UpdateExpression,
        VariableDeclaration,
        VariableDeclarator,
        WhileStatement
    };
    struct State {
        State(HandlerType t, const String &n, State *p = 0)
            : type(t), prev(p), name(n), scope(0)
        {}
        ~State()
        {
            delete scope;
        }
        String indentString() const;

        HandlerType type;
        State *prev;
        String name;
        List<String> parents;
        Map<String, uint32_t> *scope;
    };
    bool recurse(v8::Handle<v8::Object> object, State *state, const String &name = String());
    void handleIdentifier(v8::Handle<v8::Object> object, State *state);
    void handleArrayExpression(v8::Handle<v8::Object> object, State *state);
    void handleAssignmentExpression(v8::Handle<v8::Object> object, State *state);
    void handleBinaryExpression(v8::Handle<v8::Object> object, State *state);
    void handleBlockStatement(v8::Handle<v8::Object> object, State *state);
    void handleBreakStatement(v8::Handle<v8::Object> object, State *state);
    void handleCallExpression(v8::Handle<v8::Object> object, State *state);
    void handleCatchClause(v8::Handle<v8::Object> object, State *state);
    void handleConditionalExpression(v8::Handle<v8::Object> object, State *state);
    void handleContinueStatement(v8::Handle<v8::Object> object, State *state);
    void handleDoWhileStatement(v8::Handle<v8::Object> object, State *state);
    void handleEmptyStatement(v8::Handle<v8::Object> object, State *state);
    void handleExpressionStatement(v8::Handle<v8::Object> object, State *state);
    void handleForInStatement(v8::Handle<v8::Object> object, State *state);
    void handleForStatement(v8::Handle<v8::Object> object, State *state);
    void handleFunctionDeclaration(v8::Handle<v8::Object> object, State *state);
    void handleFunctionExpression(v8::Handle<v8::Object> object, State *state);
    void handleIfStatement(v8::Handle<v8::Object> object, State *state);
    void handleLiteral(v8::Handle<v8::Object> object, State *state);
    void handleLogicalExpression(v8::Handle<v8::Object> object, State *state);
    void handleMemberExpression(v8::Handle<v8::Object> object, State *state);
    void handleNewExpression(v8::Handle<v8::Object> object, State *state);
    void handleObjectExpression(v8::Handle<v8::Object> object, State *state);
    void handleProgram(v8::Handle<v8::Object> object, State *state);
    void handleProperty(v8::Handle<v8::Object> object, State *state);
    void handleReturnStatement(v8::Handle<v8::Object> object, State *state);
    void handleThisExpression(v8::Handle<v8::Object> object, State *state);
    void handleThrowStatement(v8::Handle<v8::Object> object, State *state);
    void handleTryStatement(v8::Handle<v8::Object> object, State *state);
    void handleUnaryExpression(v8::Handle<v8::Object> object, State *state);
    void handleUpdateExpression(v8::Handle<v8::Object> object, State *state);
    void handleVariableDeclaration(v8::Handle<v8::Object> object, State *state);
    void handleVariableDeclarator(v8::Handle<v8::Object> object, State *state);
    void handleWhileStatement(v8::Handle<v8::Object> object, State *state);

    CursorInfo createSymbol(v8::Handle<v8::Object> object, State *state, CursorInfo::JSCursorKind kind);
    void handleProperties(v8::Handle<v8::Object> object, State *state);

    typedef void (JSParser::*Handler)(v8::Handle<v8::Object>, State *state);

    struct HandlerNode
    {
        const char *name;
        const HandlerType type;
        const Handler handler;
    };

    static int compareHandler(const void *l, const void *r);

    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Object> mEsprima;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
    uint32_t mFileId;
    SymbolMap *mSymbols;
    SymbolNameMap *mSymbolNames;
    String *mErrors;
};


#endif
