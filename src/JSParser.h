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
        State(State *p = 0)
            : prev(p), scope(0)
        {}
        ~State()
        {
            delete scope;
        }
        String indentString() const;

        State *prev;
        String parent;
        Map<String, uint32_t> *scope;
    };
    bool recurse(v8::Handle<v8::Object> object, State *state, const String &name = String());
    void handleIdentifier(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleArrayExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleAssignmentExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleBinaryExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleBlockStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleBreakStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleCallExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleCatchClause(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleConditionalExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleContinueStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleDoWhileStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleEmptyStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleExpressionStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleForInStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleForStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleFunctionDeclaration(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleFunctionExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleIfStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleLiteral(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleLogicalExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleMemberExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleNewExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleObjectExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleProgram(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleProperty(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleReturnStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleThisExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleThrowStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleTryStatement(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleUnaryExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleUpdateExpression(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleVariableDeclaration(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleVariableDeclarator(v8::Handle<v8::Object> object, State *state, const String &name);
    void handleWhileStatement(v8::Handle<v8::Object> object, State *state, const String &name);

    // CursorInfo handleKeyType(v8::Handle<v8::Object> object, State *state, const String &name, CursorInfo::JSCursorKind kind);
    CursorInfo createSymbol(v8::Handle<v8::Object> object, State *state, CursorInfo::JSCursorKind kind);
    void handleValueType(v8::Handle<v8::Object> object, State *state, const String &name, const String &key);
    void handleProperties(v8::Handle<v8::Object> object, State *state);

    typedef void (JSParser::*Handler)(v8::Handle<v8::Object>, State *state, const String &name);

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
