#include "JSParser.h"
#include <v8.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>

#define toCString(str) *v8::String::Utf8Value(str)

static v8::Handle<v8::String> toJSON(v8::Handle<v8::Value> obj, bool pretty = true)
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
    log << toCString(toJSON(value));
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

static inline String listProperties(v8::Handle<v8::Object> obj)
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

JSParser::JSParser()
    : mIsolate(0), mFileId(0), mRoot(0), mSymbols(0), mSymbolNames(0), mErrors(0)
{}

JSParser::~JSParser()
{
    delete mRoot;
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
            *json = toCString(toJSON(result));
        mRoot = recurse(result, 0, String());
        if (mRoot) {
            error() << toJSON(result);
            // error() << mRoot->dump();
            visit(mRoot);
        }
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

struct ObjectTypeNode {
    const char *name;
    const Node::ObjectType type;
};

static int compareNode(const void *l, const void *r)
{
    const ObjectTypeNode *left = reinterpret_cast<const ObjectTypeNode*>(l);
    const ObjectTypeNode *right = reinterpret_cast<const ObjectTypeNode*>(r);
    assert(left->name);
    assert(right->name);
    return strcmp(left->name, right->name);
}

Node *JSParser::recurse(v8::Handle<v8::Value> value, Node *parent, const String &name)
{
    v8::HandleScope handleScope;
    Node *ret = 0;
    if (value.IsEmpty()) {
        ret = new Node(parent, Node::Type_Null, name);
    } else if (value->IsArray()) {
        v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(value);
        ArrayNode *node = new ArrayNode(parent, name);
        node->nodes.resize(array->Length());
        for (int i=0; i<node->nodes.size(); ++i) {
            node->nodes[i] = recurse(array->Get(i), node);
        }

        ret = node;
    } else if (value->IsObject()) {
        v8::Handle<v8::Object> object = v8::Handle<v8::Object>::Cast(value);
        v8::Handle<v8::Array> properties = object->GetOwnPropertyNames();

        ObjectNode *node = new ObjectNode(parent, name);

        static const ObjectTypeNode handlers[] = {
            { "ArrayExpression", Node::ArrayExpression },
            { "AssignmentExpression", Node::AssignmentExpression },
            { "BinaryExpression", Node::BinaryExpression },
            { "BlockStatement", Node::BlockStatement },
            { "BreakStatement", Node::BreakStatement },
            { "CallExpression", Node::CallExpression },
            { "CatchClause", Node::CatchClause },
            { "ConditionalExpression", Node::ConditionalExpression },
            { "ContinueStatement", Node::ContinueStatement },
            { "DoWhileStatement", Node::DoWhileStatement },
            { "EmptyStatement", Node::EmptyStatement },
            { "ExpressionStatement", Node::ExpressionStatement },
            { "ForInStatement", Node::ForInStatement },
            { "ForStatement", Node::ForStatement },
            { "FunctionDeclaration", Node::FunctionDeclaration },
            { "FunctionExpression", Node::FunctionExpression },
            { "Identifier", Node::Identifier },
            { "IfStatement", Node::IfStatement },
            { "Literal", Node::Literal },
            { "LogicalExpression", Node::LogicalExpression },
            { "MemberExpression", Node::MemberExpression },
            { "NewExpression", Node::NewExpression },
            { "ObjectExpression", Node::ObjectExpression },
            { "Program", Node::Program },
            { "Property", Node::Property },
            { "ReturnStatement", Node::ReturnStatement },
            { "ThisExpression", Node::ThisExpression },
            { "ThrowStatement", Node::ThrowStatement },
            { "TryStatement", Node::TryStatement },
            { "UnaryExpression", Node::UnaryExpression },
            { "UpdateExpression", Node::UpdateExpression },
            { "VariableDeclaration", Node::VariableDeclaration },
            { "VariableDeclarator", Node::VariableDeclarator },
            { "WhileStatement", Node::WhileStatement },
            { 0, Node::None }
        };
        const v8::Handle<v8::String> objectType = get<v8::String>(object, "type");
        const v8::String::Utf8Value str(objectType);
        const ObjectTypeNode n = { *str, Node::None };
        const void *result = bsearch(&n, handlers,
                                     (sizeof(handlers) - sizeof(ObjectTypeNode)) / sizeof(ObjectTypeNode),
                                     sizeof(ObjectTypeNode), &compareNode);
        const ObjectTypeNode *res = reinterpret_cast<const ObjectTypeNode*>(result);
        assert(res);
        node->objectType = res->type;
        for (unsigned i=0; i<properties->Length(); ++i) {
            v8::Handle<v8::String> key = get<v8::String>(properties, i);
            String k = toCString(key);
            node->nodes[k] = recurse(get<v8::Object>(object, key), node, k);
        }
        ret = node;
    } else if (value->IsInt32()) {
        ret = new IntegerNode(parent, v8::Handle<v8::Integer>::Cast(value)->Value(), name);
    } else if (value->IsString()) {
        ret = new StringNode(parent, *v8::String::Utf8Value(v8::Handle<v8::String>::Cast(value)), name);
    } else if (value->IsTrue()) {
        ret = new Node(parent, Node::Type_True, name);
    } else if (value->IsFalse()) {
        ret = new Node(parent, Node::Type_False, name);
    } else {
        error() << "Unknown object type" << toJSON(value);
        return 0;
    }
    if (!parent) {
        ret->scope = new Map<String, int>;
    }
    return ret;
}

void JSParser::createObject(ObjectNode *node)
{
    Node *nameNode = node->child("name");
    assert(nameNode && nameNode->type == Node::Type_String);
    const String name = nameNode->toString();
    Node *rangeNode = node->child("range");
    assert(rangeNode && rangeNode->type == Node::Type_Array && rangeNode->count() == 2);
    const int offset = rangeNode->child(0)->toInteger();
    const int length = rangeNode->child(1)->toInteger() - offset;
    Map<String, int> *scope = 0;
    for (Node *n = node; n; n = n->parent) {
        if (!scope && n->scope) {
            scope = n->scope;
            break;
        }
    }
    assert(scope);

    int kind = -1;
    bool weak = false;
    String symbolName;
    switch (node->parent->objectType) {
    case Node::VariableDeclarator:
        if (node->name == "id")
            kind = CursorInfo::JSVariable;
        break;
    case Node::FunctionDeclaration:
        kind = CursorInfo::JSFunction;
        break;
    case Node::CallExpression:
        kind = CursorInfo::JSReference;
        break;
    case Node::MemberExpression:
        if (node->name == "object") {
            kind = CursorInfo::JSReference;
        } else if (node->name == "property") {
            weak = true;
            kind = CursorInfo::JSVariable;
            // error() << "Fiske\n" << node->parent->dump();
            if (!node->parent->child("object")) {
                printf("[%s] %s:%d: if (!node->parent->child(\"object\")) { [after]\n", __func__, __FILE__, __LINE__);
            } else if (!node->parent->child("object")->child("name")) {
                error() << "tasken\n" << node->parent->child("object")->dump();
                printf("[%s] %s:%d: } else if (!node->parent->child(\"object\")->child(\"name\")) { [after]\n", __func__, __FILE__, __LINE__);
            } else {
                symbolName = node->parent->child("object")->child("name")->toString();
                symbolName += '.';
            }
        }
        break;
    case Node::AssignmentExpression:
        if (node->name == "left") {
            weak = true;
            kind = CursorInfo::JSVariable;
        } else if (node->name == "right") {
            kind = CursorInfo::JSReference;
        }
        break;
    case Node::Property:
        if (node->name == "key") {
            error() << "got property key" << node->parent->parent->parent->objectTypeString()
                    << node->parent->parent->parent->name;
            if (node->parent->parent
                && node->parent->parent->parent
                && node->parent->parent->parent->parent
                && node->parent->parent->parent->objectTypeString() == "ObjectExpression"
                && node->parent->parent->parent->name == "init"
                && node->parent->parent->parent->parent->objectTypeString() == "VariableDeclarator") {
                // fy faen!
                // error() << node->parent->parent->parent->dump();
                symbolName = node->parent->parent->parent->parent->child("id")->child("name")->toString();
                symbolName += ".";
            }
            weak = true;
            kind = CursorInfo::JSVariable;
        }
        break;
    default:
        break;
    }
    if (kind == -1) {
        error() << "Unhandled identifier:\n" << node->parent->dump(0);
        return;
    }
    symbolName += name;
    const Location loc(mFileId, offset);
    if (mSymbols) {
        CursorInfo info;
        if (kind == CursorInfo::JSReference || weak) {
            for (Map<String, int>::const_iterator it = scope->begin(); it != scope->end(); ++it) {
                if (it->first == symbolName) {
                    kind = CursorInfo::JSReference;
                    const Location target(mFileId, it->second);
                    (*mSymbols)[target].references.insert(loc);
                    info.targets.insert(target);
                    break;
                }
            }
        }
        info.kind = kind;
        info.symbolName = symbolName;
        info.symbolLength = length;
        (*mSymbols)[loc] = info;
    }
    if (kind != CursorInfo::JSReference)
        (*scope)[symbolName] = offset;
    if (mSymbolNames) {
        (*mSymbolNames)[symbolName].insert(loc);
        if (symbolName != name)
            (*mSymbolNames)[name].insert(loc);
    }
    
}

void JSParser::visit(Node *node)
{
    if (node) {
        switch (node->type) {
        case Node::Type_Object:
            // error() << "visiting" << node->objectType();
            if (node->objectType == Node::Identifier) {
                createObject(static_cast<ObjectNode*>(node));
            } else {
                const Map<String, Node*>::const_iterator end = node->end();
                Map<String, Node*>::const_iterator it = node->begin();
                while (it != end) {
                    visit(it->second);
                    ++it;
                }
            }
            break;
        case Node::Type_Array: {
            const int count = node->count();
            for (int i=0; i<count; ++i) {
                visit(node->child(i));
            }
            break; }
        default:
            break;
        }
    }
}
