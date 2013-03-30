#include "JSParser.h"
#include <v8.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>

#define toCString(str) *v8::String::Utf8Value(str)

static v8::Handle<v8::String> toJSON(v8::Handle<v8::Value> obj, bool pretty)
{
    v8::HandleScope scope;

    v8::Handle<v8::Context> context = v8::Context::GetCurrent();
    v8::Handle<v8::Object> global = context->Global();

    v8::Handle<v8::Object> JSON = global->Get(v8::String::New("JSON"))->ToObject();
    v8::Handle<v8::Function> JSON_stringify = v8::Handle<v8::Function>::Cast(JSON->Get(v8::String::New("stringify")));

    v8::Handle<v8::Value> args[3];
    args[0] = obj;
    args[1] = v8::Null();
    args[2] = v8::String::New("    ");

    return scope.Close(JSON_stringify->Call(JSON, pretty ? 3 : 2, args)->ToString());
}

Log operator<<(Log log, v8::Handle<v8::String> string)
{
    log << toCString(string);
    return log;
}

Log operator<<(Log log, v8::Handle<v8::Value> value)
{
    log << toCString(toJSON(value, true));
    return log;
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Object> object, v8::Handle<v8::String> property)
{
    if (object.IsEmpty() || !object->IsObject())
        return v8::Handle<T>();
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop(object->Get(property));
    if (prop.IsEmpty() || prop->IsNull() || prop->IsUndefined()) {
        return scope.Close(v8::Handle<T>());
    } else {
        return scope.Close(v8::Handle<T>::Cast(prop));
    }
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Object> object, const char *property)
{
    return get<T>(object, v8::String::New(property));
}

template <typename T>
static v8::Persistent<T> getPersistent(v8::Handle<v8::Object> object, v8::Handle<v8::String> property)
{
    v8::Persistent<T> prop(get<T>(object, property));
    return prop;
}

template <typename T>
static v8::Persistent<T> getPersistent(v8::Handle<v8::Object> object, const char *property)
{
    return getPersistent<T>(object, v8::String::New(property));
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Array> object, int index)
{
    if (object.IsEmpty() || !object->IsArray())
        return v8::Handle<T>();
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop = object->Get(index);
    if (prop.IsEmpty() || prop->IsNull() || prop->IsUndefined()) {
        return scope.Close(v8::Handle<T>());
    } else {
        return scope.Close(v8::Handle<T>::Cast(prop));
    }
}

static String listProperties(v8::Handle<v8::Object> obj)
{
    List<String> ret;
    if (!obj.IsEmpty() && obj->IsObject()) {
        v8::HandleScope scope;
        v8::Handle<v8::Array> props = obj->GetOwnPropertyNames();
        for (unsigned i=0; i<props->Length(); ++i) {
            ret += toCString(get<v8::String>(props, i));
        }
    }
    return String::join(ret, ", ");
}

static inline bool operator==(v8::Handle<v8::String> l, const char *r)
{
    // error() << "comparing" << (l.IsEmpty() ? "empty" : toCString(l)) << r;
    return l.IsEmpty() ? (!r || !strlen(r)) : !strcmp(toCString(l), r);
}

static inline bool operator==(const char *l, v8::Handle<v8::String> r)
{
    return operator==(r, l);
}

static inline bool operator!=(const char *l, v8::Handle<v8::String> r)
{
    return !operator==(r, l);
}

static inline bool operator!=(v8::Handle<v8::String> l, const char *r)
{
    return !operator==(l, r);
}

String JSParser::State::indentString() const
{
    int indent = 0;
    const State *s = prev;
    while (s) {
        ++indent;
        s = s->prev;
    }
    return String(indent * 2, ' ');
}

JSParser::JSParser()
    : mIsolate(0), mFileId(0), mSymbols(0), mSymbolNames(0), mErrors(0)
{}

JSParser::~JSParser()
{
    {
        const v8::Isolate::Scope isolateScope(mIsolate);
#ifdef V8_DISPOSE_REQUIRES_ARG
        if (!mParse.IsEmpty())
            mParse.Dispose(mIsolate);
        if (!mEsprima.IsEmpty())
            mEsprima.Dispose(mIsolate);
        if (!mContext.IsEmpty())
            mContext.Dispose(mIsolate);
#else
        if (!mParse.IsEmpty())
            mParse.Dispose();
        if (!mEsprima.IsEmpty())
            mEsprima.Dispose();
        if (!mContext.IsEmpty())
            mContext.Dispose();
#endif
    }
    mIsolate->Dispose();
}

bool JSParser::init()
{
    mIsolate = v8::Isolate::New();
    const v8::Isolate::Scope isolateScope(mIsolate);
    v8::HandleScope handleScope;
    mContext = v8::Context::New();
    v8::Context::Scope scope(mContext);
    assert(!mContext.IsEmpty());

    const String esprimaSrcString = Path(ESPRIMA_JS).readAll();
    v8::Handle<v8::String> esprimaSrc = v8::String::New(esprimaSrcString.constData(), esprimaSrcString.size());

    v8::TryCatch tryCatch;
    v8::Handle<v8::Script> script = v8::Script::Compile(esprimaSrc);
    if (tryCatch.HasCaught() || script.IsEmpty() || !tryCatch.Message().IsEmpty()) {
        v8::Handle<v8::Message> message = tryCatch.Message();
        v8::String::Utf8Value msg(message->Get());
        printf("%s:%d:%d: esprima error: %s {%d-%d}\n", ESPRIMA_JS, message->GetLineNumber(),
               message->GetStartColumn(), *msg, message->GetStartPosition(), message->GetEndPosition());
        return false;
    }
    script->Run();

    v8::Handle<v8::Object> global = mContext->Global();
    assert(!global.IsEmpty());
    mEsprima = getPersistent<v8::Object>(global, "esprima");
    if (mEsprima.IsEmpty() || !mEsprima->IsObject()) {
        return false;
    }
    mParse = getPersistent<v8::Function>(mEsprima, "parse");

    return !mParse.IsEmpty() && mParse->IsFunction();
}

bool JSParser::parse(const Path &path, const String &contents, SymbolMap *symbols, SymbolNameMap *symbolNames,
                     String *errors, String *json)
{
    const v8::Isolate::Scope isolateScope(mIsolate);
    mFileId = Location::insertFile(path);
    v8::HandleScope handleScope;
    v8::Context::Scope scope(mContext);
    String tmp;
    if (contents.isEmpty())
        tmp = path.readAll();
    const String &c = contents.isEmpty() ? tmp : contents;
    if (c.isEmpty()) {
        printf("[%s] %s:%d: if (c.isEmpty()) { [after]\n", __func__, __FILE__, __LINE__);
        return false;
    }
    v8::Handle<v8::Value> args[2];
    // args[0] = v8::String::New(file.constData(), file.size());
    args[0] = v8::String::New(c.constData(), c.size());
    v8::Handle<v8::Object> options = v8::Object::New();
    options->Set(v8::String::New("range"), v8::Boolean::New(true));
    options->Set(v8::String::New("tolerant"), v8::Boolean::New(true));
    args[1] = options;
    assert(!mEsprima.IsEmpty() && mEsprima->IsObject());
    assert(!mParse.IsEmpty() && mParse->IsFunction());
    assert(!args[0].IsEmpty() && args[0]->IsString());
    assert(!args[1].IsEmpty() && args[1]->IsObject());
    v8::Handle<v8::Value> result = mParse->Call(mEsprima, 2, args);

    mSymbols = symbols;
    mSymbolNames = symbolNames;
    mErrors = errors;
    if (!result.IsEmpty() && result->IsObject()) {
        if (json)
            *json = toCString(toJSON(result, true));
        State state;
        state.scope = new Map<String, uint32_t>;
        recurse(get<v8::Object>(result->ToObject(), "body"), &state);
    } else if (errors) {
        *errors = "Failed to parse";
    }
    // for (Map<int, CursorInfo>::const_iterator it = symbols.begin(); it != symbols.end(); ++it) {
    //     error() << String::format<64>("%s,%d", path.constData(), it->first) << it->second;
    // }
    mSymbols = 0;
    mSymbolNames = 0;
    mErrors = 0;
    return true;
}

CursorInfo JSParser::handleKeyType(v8::Handle<v8::Object> object, State *state, const String &name, CursorInfo::JSCursorKind kind)
{
    v8::Handle<v8::String> type = get<v8::String>(object, "type");
    if (type == "Identifier") {
        return createSymbol(object, state, kind);
    } else if (type == "MemberExpression") {

    }

}

CursorInfo JSParser::createSymbol(v8::Handle<v8::Object> object, State *state, CursorInfo::JSCursorKind kind)
{
    v8::HandleScope handleScope;
    v8::Context::Scope scope(mContext);
    v8::Handle<v8::String> name;
    v8::Handle<v8::Array> range;
    v8::Handle<v8::String> type = get<v8::String>(object, "type");
    if (type == "Identifier") {
        name = get<v8::String>(object, "name");
        range = get<v8::Array>(object, "range");
    } else if (type == "MemberExpression") {
        v8::Handle<v8::Object> obj = (get<v8::Object>(object, "object"), state);
        v8::Handle<v8::Object> property = (get<v8::Object>(object, "property"), state);

    }
    if (name.IsEmpty()) {
        error() << "Bullshit object for createSymbol" << toJSON(object, true);
        return CursorInfo();
    }
    assert(!range.IsEmpty() && range->Length() == 2);
    const uint32_t offset = get<v8::Integer>(range, 0)->Value();
    const uint32_t length = get<v8::Integer>(range, 1)->Value() - offset;
    CursorInfo c;
    const Location loc(mFileId, offset);
    const String symbolName(toCString(name), name->Length());

    if (state->parent.isEmpty()) {
        c.symbolName = symbolName;
    } else {
        c.symbolName = state->parent + '.' + symbolName;
    }
    c.symbolLength = length;
    c.kind = kind;
    if (c.kind == CursorInfo::JSReference || c.kind == CursorInfo::JSWeakVariable) {
        State *s = state;
        while (s) {
            if (s->scope) {
                uint32_t targetOffset = s->scope->value(c.symbolName, UINT_MAX);
                error() << "looking for" << c.symbolName << "in" << s->scope->keys() << (targetOffset != UINT_MAX);
                if (targetOffset != UINT_MAX) {
                    const Location target(mFileId, targetOffset);
                    c.targets.insert(target);
                    if (mSymbols) {
                        assert(mSymbols->contains(target));
                        (*mSymbols)[target].references.insert(loc);
                    }
                    c.kind = CursorInfo::JSReference;
                    break;
                }
            }
            s = s->prev;
        }
    }
    if (mSymbols)
        (*mSymbols)[loc] = c;
    if (c.kind != CursorInfo::JSReference) {
        if (mSymbolNames) {
            (*mSymbolNames)[c.symbolName].insert(loc);
            if (symbolName != c.symbolName)
                (*mSymbolNames)[symbolName].insert(loc);
        }
        Map<String, uint32_t> *scope = 0;
        State *s = state;
        while (s) {
            if (s->scope) {
                scope = s->scope;
                break;
            }
            s = s->prev;
        }
        assert(scope);

        (*scope)[c.symbolName] = offset;
    }
    error() << state->indentString() << "+++ adding" << c.kindSpelling() << c.symbolName << "at" << loc << "scope";
    return c;
}

int JSParser::compareHandler(const void *l, const void *r)
{
    const HandlerNode *left = reinterpret_cast<const HandlerNode*>(l);
    const HandlerNode *right = reinterpret_cast<const HandlerNode*>(r);
    assert(left->name);
    assert(right->name);
    return strcmp(left->name, right->name);
}

bool JSParser::recurse(v8::Handle<v8::Object> object, State *state, const String &name)
{
    if (object.IsEmpty())
        return false;

    if (object->IsArray()) {
        v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(object);
        if (name == "body") {
            assert(!state->scope);
            state->scope = new Map<String, uint32_t>();
        }
        for (unsigned i=0; i<array->Length(); ++i) {
            recurse(get<v8::Object>(array, i), state);
        }
    } else if (object->IsObject()) {
        v8::Handle<v8::String> objectType = get<v8::String>(object, "type");
        if (!objectType.IsEmpty()) {
            assert(objectType->IsString());
            static const HandlerNode handlers[] = {
                { "ArrayExpression", ArrayExpression, &JSParser::handleArrayExpression },
                { "AssignmentExpression", AssignmentExpression, &JSParser::handleAssignmentExpression },
                { "BinaryExpression", BinaryExpression, &JSParser::handleBinaryExpression },
                { "BlockStatement", BlockStatement, &JSParser::handleBlockStatement },
                { "BreakStatement", BreakStatement, &JSParser::handleBreakStatement },
                { "CallExpression", CallExpression, &JSParser::handleCallExpression },
                { "CatchClause", CatchClause, &JSParser::handleCatchClause },
                { "ConditionalExpression", ConditionalExpression, &JSParser::handleConditionalExpression },
                { "ContinueStatement", ContinueStatement, &JSParser::handleContinueStatement },
                { "DoWhileStatement", DoWhileStatement, &JSParser::handleDoWhileStatement },
                { "EmptyStatement", EmptyStatement, &JSParser::handleEmptyStatement },
                { "ExpressionStatement", ExpressionStatement, &JSParser::handleExpressionStatement },
                { "ForInStatement", ForInStatement, &JSParser::handleForInStatement },
                { "ForStatement", ForStatement, &JSParser::handleForStatement },
                { "FunctionDeclaration", FunctionDeclaration, &JSParser::handleFunctionDeclaration },
                { "FunctionExpression", FunctionExpression, &JSParser::handleFunctionExpression },
                { "Identifier", Identifier, &JSParser::handleIdentifier },
                { "IfStatement", IfStatement, &JSParser::handleIfStatement },
                { "Literal", Literal, &JSParser::handleLiteral },
                { "LogicalExpression", LogicalExpression, &JSParser::handleLogicalExpression },
                { "MemberExpression", MemberExpression, &JSParser::handleMemberExpression },
                { "NewExpression", NewExpression, &JSParser::handleNewExpression },
                { "ObjectExpression", ObjectExpression, &JSParser::handleObjectExpression },
                { "Program", Program, &JSParser::handleProgram },
                { "Property", Property, &JSParser::handleProperty },
                { "ReturnStatement", ReturnStatement, &JSParser::handleReturnStatement },
                { "ThisExpression", ThisExpression, &JSParser::handleThisExpression },
                { "ThrowStatement", ThrowStatement, &JSParser::handleThrowStatement },
                { "TryStatement", TryStatement, &JSParser::handleTryStatement },
                { "UnaryExpression", UnaryExpression, &JSParser::handleUnaryExpression },
                { "UpdateExpression", UpdateExpression, &JSParser::handleUpdateExpression },
                { "VariableDeclaration", VariableDeclaration, &JSParser::handleVariableDeclaration },
                { "VariableDeclarator", VariableDeclarator, &JSParser::handleVariableDeclarator },
                { "WhileStatement", WhileStatement, &JSParser::handleWhileStatement },
                { 0, NoHandlerType, 0 }
            };
            const v8::String::Utf8Value str(objectType);
            const HandlerNode node = { *str, NoHandlerType, 0 };
            enum { NodeSize = sizeof(HandlerNode) };
            const HandlerNode *res = reinterpret_cast<const HandlerNode*>(bsearch(&node, handlers,
                                                                                  (sizeof(handlers) - NodeSize) / NodeSize,
                                                                                  NodeSize, &JSParser::compareHandler));
            assert(res);
            assert(res->name);
            (this->*res->handler)(object, state, res->name);
        }
    }

    return true;
}

void JSParser::handleValueType(v8::Handle<v8::Object> object, State *state, const String &name, const String &key)
{
    v8::Handle<v8::String> objectType = get<v8::String>(object, "type");
    if (objectType == "Identifier") {
        State s(state);
        createSymbol(object, state, CursorInfo::JSReference);
    } else if (objectType == "ObjectExpression") {
        State s(state);
        s.parent = key;
        handleObjectExpression(object, &s, "init");
    } else {
        error() << "Unhandled" << name << objectType;
    }

}

void JSParser::handleProperties(v8::Handle<v8::Object> object, State *state)
{
    v8::Handle<v8::Array> properties = object->GetOwnPropertyNames();

    for (unsigned i=0; i<properties->Length(); ++i) {
        v8::Handle<v8::String> key = get<v8::String>(properties, i);
        v8::Handle<v8::Object> value = get<v8::Object>(object, key);
        if (!value.IsEmpty()) {
            const v8::String::Utf8Value utf8(key);
            recurse(value, state, *utf8);
        }
    }
}


void JSParser::handleIdentifier(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "Identifier" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleArrayExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ArrayExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleAssignmentExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    v8::HandleScope scope;
    error() << state->indentString() << "AssignmentExpression" << name << listProperties(object);
    v8::Handle<v8::Object> left = get<v8::Object>(object, "left");
    v8::Handle<v8::String> leftType = get<v8::String>(left, "type");
    CursorInfo info;
    if (leftType == "Identifier") {
        info = createSymbol(left, state, CursorInfo::JSVariable);
    } else if (type == "MemberExpression") {
        info = createSymbol(left, state, CursorInfo::JSWeakVariable);
    }
    
    v8::Handle<v8::Object> right = get<v8::Object>(object, "right");
    if (!right.IsEmpty()) {
        handleValueType(right, state, "right", info.symbolName);
    }
}

void JSParser::handleBinaryExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "BinaryExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleBlockStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "BlockStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleBreakStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "BreakStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleCallExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "CallExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleCatchClause(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "CatchClause" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleConditionalExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ConditionalExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleContinueStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ContinueStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleDoWhileStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "DoWhileStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleEmptyStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "EmptyStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleExpressionStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ExpressionStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleForInStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ForInStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleForStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ForStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleFunctionDeclaration(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "FunctionDeclaration" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleFunctionExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "FunctionExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleIfStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "IfStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleLiteral(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "Literal" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleLogicalExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "LogicalExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleMemberExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "MemberExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleNewExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "NewExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleObjectExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ObjectExpression" << name << listProperties(object);
    v8::HandleScope scope;
    v8::Handle<v8::Array> properties = get<v8::Array>(object, "properties");
    for (unsigned i=0; i<properties->Length(); ++i) {
        handleProperty(get<v8::Object>(properties, i), state, name);
    }
}

void JSParser::handleProgram(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "Program" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleProperty(v8::Handle<v8::Object> object, State *state, const String &name)
{
    v8::HandleScope scope;
    error() << state->indentString() << "Property" << name << listProperties(object);

    // error() << "getting property" << state->parent;
    const CursorInfo info = createSymbol(get<v8::Object>(object, "key"), state, CursorInfo::JSVariable);
    v8::Handle<v8::Object> value = get<v8::Object>(object, "value");
    if (!value.IsEmpty()) {
        handleValueType(value, state, "value", info.symbolName);
    }
}

void JSParser::handleReturnStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ReturnStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleThisExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ThisExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleThrowStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "ThrowStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleTryStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "TryStatement" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleUnaryExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "UnaryExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleUpdateExpression(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "UpdateExpression" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleVariableDeclaration(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "VariableDeclaration" << name << listProperties(object);
    handleProperties(object, state);
}

void JSParser::handleVariableDeclarator(v8::Handle<v8::Object> object, State *state, const String &name)
{
    v8::HandleScope scope;
    error() << state->indentString() << "VariableDeclarator" << name << listProperties(object);
    const CursorInfo info = createSymbol(get<v8::Object>(object, "id"), state, CursorInfo::JSVariable);
    v8::Handle<v8::Object> init = get<v8::Object>(object, "init");
    if (!init.IsEmpty()) {
        handleValueType(init, state, "init", info.symbolName);
    }
}

void JSParser::handleWhileStatement(v8::Handle<v8::Object> object, State *state, const String &name)
{
    error() << state->indentString() << "WhileStatement" << name << listProperties(object);
    handleProperties(object, state);
}
