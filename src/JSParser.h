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
#include <deque>

struct JSScope;

struct Declaration
{
    JSScope* scope;
    CursorInfo::JSCursorKind kind;
    std::string name;
    int start;
    int end;

    std::deque<std::pair<int, int> > refs;
};

struct JSScope
{
    enum NodeType { None, FunctionDeclaration, BlockStatement, VariableDeclaration, VariableDeclarator, Identifier, Literal,
                    ReturnStatement, ExpressionStatement, AssignmentExpression, CallExpression, Program, MemberExpression,
                    ObjectExpression };

    JSScope(NodeType type);
    ~JSScope();

    void addDeclaration(CursorInfo::JSCursorKind kind, const std::string& name, int start, int end);

    std::deque<Declaration> mDeclarations;
    NodeType mType;
    int mObjectsAdded;
};

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
    void syncScope(const JSScope& scope);
    void recurse(const v8::Handle<v8::Value>& node);
    void recurse(const v8::Handle<v8::Array>& node);
    void recurse(const v8::Handle<v8::Object>& node);
    bool parentScopeIs(JSScope::NodeType type, int level = 1) const;

    enum VarType { UnknownVar, Var, Let };
    void addDeclarations(VarType type, const v8::Handle<v8::Array>& decls);
    void addDeclaration(JSScope* scope, CursorInfo::JSCursorKind kind, const v8::Handle<v8::Object>& obj);
    void parseIdentifiers(const v8::Handle<v8::Object>& obj);

    JSScope* findDeclarationScope(VarType type, JSScope* start = 0);
    JSScope* findScope(JSScope::NodeType type);
    Declaration* findDeclaration(const std::string& name);

    std::deque<JSScope> mScopes;
    std::deque<std::string> mObjects;
    JSScope* mObjectScope;

    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Object> mEsprima;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
    uint32_t mFileId;

    SymbolMap* mSymbols;
    SymbolNameMap* mSymbolNames;
    String* mErrors;
};


#endif
