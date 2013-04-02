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
    : mIsolate(0), mFileId(0)
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
    mSymbols = 0;
    mSymbolNames = 0;
    mErrors = 0;
    mObjectScope = 0;

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
    mSymbols = symbols;
    mSymbolNames = symbolNames;
    mErrors = errors;

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

        mSymbols = 0;
        mSymbolNames = 0;
        mErrors = 0;

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
    if (!result.IsEmpty() && result->IsObject()) {
        if (json)
            *json = toCString(toJSON(result));
        recurse(result);
        assert(mScopes.empty());
    }

    mSymbols = 0;
    mSymbolNames = 0;
    mErrors = 0;

    return true;
}

static void addNamePermutations(SymbolNameMap& symbolNames, std::string name, const Set<Location>& locs)
{
    assert(!name.empty());
    symbolNames[name] = locs;
    size_t dot = name.find('.');
    while (dot != std::string::npos) {
        name = name.substr(dot + 1);
        if (!name.empty()) {
            symbolNames[name] = locs;
            dot = name.find('.');
        } else {
            dot = std::string::npos;
        }
    }
}

static std::string stripDots(const std::string& name)
{
    const size_t lastDot = name.rfind('.');
    if (lastDot == std::string::npos)
        return name;
    if (lastDot + 1 >= name.size())
        return std::string();
    return name.substr(lastDot + 1);
}

void JSParser::syncScope(const JSScope& scope)
{
    //error() << "syncing scope" << scope.mType;
    std::deque<Declaration>::const_reverse_iterator decl = scope.mDeclarations.rbegin();
    const std::deque<Declaration>::const_reverse_iterator declEnd = scope.mDeclarations.rend();
    while (decl != declEnd) {
        const Declaration& declaration = *decl;
        CursorInfo info;
        info.kind = declaration.kind;
        info.symbolName = stripDots(declaration.name);
        info.start = declaration.start;
        info.end = declaration.end;
        info.symbolLength = info.symbolName.size(); // ### probably wrong
        Location loc(mFileId, info.start);
        info.targets.insert(loc);
        CursorInfo refInfo = info;
        refInfo.kind = CursorInfo::JSReference;
        std::deque<std::pair<int, int> >::const_iterator ref = declaration.refs.begin();
        const std::deque<std::pair<int, int> >::const_iterator refEnd = declaration.refs.end();
        while (ref != refEnd) {
            Location refLoc(mFileId, ref->first);
            info.references.insert(refLoc);
            (*mSymbols)[refLoc] = refInfo;
            ++ref;
        }
        (*mSymbols)[loc] = info;
        Set<Location> locs;
        locs.insert(loc);

        addNamePermutations(*mSymbolNames, info.symbolName, locs);

        ++decl;
    }
}

static inline JSScope::NodeType typeStringToType(const char* str)
{
    if (!strcmp(str, "FunctionDeclaration"))
        return JSScope::FunctionDeclaration;
    if (!strcmp(str, "FunctionExpression"))
        return JSScope::FunctionExpression;
    if (!strcmp(str, "BlockStatement"))
        return JSScope::BlockStatement;
    if (!strcmp(str, "VariableDeclaration"))
        return JSScope::VariableDeclaration;
    if (!strcmp(str, "VariableDeclarator"))
        return JSScope::VariableDeclarator;
    if (!strcmp(str, "Identifier"))
        return JSScope::Identifier;
    if (!strcmp(str, "Literal"))
        return JSScope::Literal;
    if (!strcmp(str, "ReturnStatement"))
        return JSScope::ReturnStatement;
    if (!strcmp(str, "ExpressionStatement"))
        return JSScope::ExpressionStatement;
    if (!strcmp(str, "AssignmentExpression"))
        return JSScope::AssignmentExpression;
    if (!strcmp(str, "CallExpression"))
        return JSScope::CallExpression;
    if (!strcmp(str, "Program"))
        return JSScope::Program;
    if (!strcmp(str, "MemberExpression"))
        return JSScope::MemberExpression;
    if (!strcmp(str, "ObjectExpression"))
        return JSScope::ObjectExpression;
    //error() << "Unrecognized scope type" << str;
    return JSScope::None;
}

JSScope::JSScope(NodeType type)
    : mType(type), mObjectsAdded(0)
{
}

JSScope::~JSScope()
{
}

Declaration* JSScope::findDeclaration(const std::string& name)
{
    std::deque<Declaration>::reverse_iterator decl = mDeclarations.rbegin();
    const std::deque<Declaration>::const_reverse_iterator declEnd = mDeclarations.rend();
    while (decl != declEnd) {
        if (decl->name == name)
            return &*decl;
        ++decl;
    }
    return 0;
}

void JSScope::addDeclaration(CursorInfo::JSCursorKind kind, const std::string& name, int start, int end)
{
    {
        Declaration* decl = findDeclaration(name);
        if (decl) {
            decl->start = start;
            decl->end = end;
            return;
        }
    }
    Declaration decl = { this, kind, name, start, end };
    mDeclarations.push_back(decl);
}

JSScope* JSParser::findDeclarationScope(VarType type, JSScope* start)
{
    bool foundStart = (start == 0);
    std::deque<JSScope>::reverse_iterator it = mScopes.rbegin();
    const std::deque<JSScope>::const_reverse_iterator end = mScopes.rend();
    while (it != end) {
        switch (it->mType) {
        case JSScope::BlockStatement:
            if (type == Var) {
                ++it;
                continue;
            }
            // fall through for Let
        case JSScope::Program:
        case JSScope::FunctionDeclaration:
        case JSScope::FunctionExpression:
            if (!foundStart) {
                if (&*it == start)
                    foundStart = true;
            } else {
                return &*it;
            }
        default:
            break;
        }
        ++it;
    }
    return 0;
}

JSScope* JSParser::findScope(JSScope::NodeType type)
{
    std::deque<JSScope>::reverse_iterator it = mScopes.rbegin();
    const std::deque<JSScope>::const_reverse_iterator end = mScopes.rend();
    while (it != end) {
        if (it->mType == type)
            return &*it;
        ++it;
    }
    return 0;
}

Declaration* JSParser::findDeclaration(const std::string& name)
{
    std::deque<JSScope>::reverse_iterator scope = mScopes.rbegin();
    const std::deque<JSScope>::const_reverse_iterator scopeEnd = mScopes.rend();
    while (scope != scopeEnd) {
        std::deque<Declaration>::reverse_iterator decl = scope->mDeclarations.rbegin();
        const std::deque<Declaration>::const_reverse_iterator declEnd = scope->mDeclarations.rend();
        while (decl != declEnd) {
            if (decl->name == name)
                return &*decl;
            ++decl;
        }
        ++scope;
    }
    return 0;
}

bool JSParser::parentScopeIs(JSScope::NodeType type, int level) const
{
    return mScopes.size() > static_cast<unsigned>(level) && mScopes[mScopes.size() - (level + 1)].mType == type;
}

void JSParser::recurse(const v8::Handle<v8::Value>& node)
{
    if (node.IsEmpty())
        return;
    if (node->IsArray())
        recurse(v8::Handle<v8::Array>::Cast(node));
    else if (node->IsObject())
        recurse(v8::Handle<v8::Object>::Cast(node));
}

void JSParser::recurse(const v8::Handle<v8::Array>& node)
{
    v8::HandleScope handleScope;
    const uint32_t len = node->Length();
    for (uint32_t i = 0; i < len; ++i) {
        recurse(node->Get(i));
    }
}

static bool parseIdentifier(const v8::Handle<v8::Object>& obj, std::string& name, int& start, int& end)
{
    assert(!obj.IsEmpty() && (obj->IsObject() || obj->IsNull()));
    if (obj->IsNull())
        return false;
    v8::Handle<v8::String> nameString = v8::String::New("name");
    if (obj->Has(nameString)) {
        v8::Handle<v8::String> rangeString = v8::String::New("range");
        if (obj->Has(rangeString)) {
            v8::Handle<v8::Array> range = v8::Handle<v8::Array>::Cast(obj->Get(rangeString));
            assert(!range.IsEmpty() && range->IsArray());
            assert(range->Length() == 2);
            start = v8::Handle<v8::Integer>::Cast(range->Get(0))->Value();
            end = v8::Handle<v8::Integer>::Cast(range->Get(1))->Value();
            name = *(v8::String::Utf8Value(obj->Get(nameString)));
            return true;
        }
    }
    return false;
}

static std::string generateName(const v8::Handle<v8::Object>& node, int* level, int* start, int* end)
{
    assert(!node.IsEmpty() && node->IsObject());

    v8::HandleScope handleScope;
    std::string result;

    bool computed = false;
    {
        v8::Handle<v8::String> compString = v8::String::New("computed");
        if (node->Has(compString)) {
            v8::Handle<v8::Boolean> comp = node->Get(compString)->ToBoolean();
            assert(!comp.IsEmpty() && comp->IsBoolean());
            computed = comp->Value();
        }
    }

    v8::Handle<v8::String> objString = v8::String::New("object");
    if (node->Has(objString)) {
        result = generateName(v8::Handle<v8::Object>::Cast(node->Get(objString)), level, start, end);
        if (!*level)
            return result;
    }

    v8::Handle<v8::String> propString = v8::String::New("property");
    if (node->Has(propString)) {
        if (!computed) {
            if (!result.empty())
                result += '.';
        } else {
            result.clear();
        }
        result += generateName(v8::Handle<v8::Object>::Cast(node->Get(propString)), level, start, end);
        if (!*level)
            return result;
    }

    std::string nodeName;
    int nodeStart, nodeEnd;
    if (parseIdentifier(node, nodeName, nodeStart, nodeEnd)) {
        --*level;

        *start = nodeStart;
        *end = nodeEnd;

        if (!result.empty())
            result += '.';
        result += nodeName;
    }
    return result;
}

static void addObject(JSScope* scope, const std::deque<std::string>& objects, int start, int end)
{
    std::string name;
    std::deque<std::string>::const_iterator obj = objects.begin();
    const std::deque<std::string>::const_iterator objEnd = objects.end();
    while (obj != objEnd) {
        if (!name.empty())
            name += '.';
        name += *obj;
        ++obj;
    }
    assert(!name.empty());
    scope->addDeclaration(CursorInfo::JSVariable, name, start, end);
}

static int addObjects(const std::string& name, std::deque<std::string>& objects)
{
    std::string n = name;
    size_t dot = n.find('.');
    int added = 0;
    while (dot != std::string::npos) {
        const std::string sub = n.substr(0, dot);
        if (!sub.empty()) {
            objects.push_back(sub);
            ++added;
        }
        if (dot + 1 < n.size()) {
            n = n.substr(dot + 1);
            dot = n.find('.');
        } else {
            dot = std::string::npos;
        }
    }
    if (!n.empty()) {
        objects.push_back(n);
        ++added;
    }
    return added;
}

void JSParser::recurse(const v8::Handle<v8::Object>& node)
{
    assert(!node.IsEmpty() && ((node->IsObject() && !node->IsArray()) || node->IsNull()));
    if (node->IsNull())
        return;

    v8::HandleScope handleScope;
    v8::Handle<v8::String> typeString = v8::String::New("type");
    bool typePushed = false;
    JSScope::NodeType nodeType = JSScope::None;
    if (node->Has(typeString)) {
        v8::Handle<v8::Value> type = node->Get(typeString);
        if (type->IsString()) {
            v8::String::Utf8Value str(type);
            nodeType = typeStringToType(*str);
            if (nodeType != JSScope::None) {
                //error() << "adding scope" << *str << nodeType;
                //error() << toCString(toJSON(node));
                mScopes.push_back(JSScope(nodeType));
                typePushed = true;
            }
        }
    }

    bool further = true;

    // stuff
    switch (nodeType) {
    case JSScope::MemberExpression: {
        int level = -1, start, end;
        std::string expr = generateName(node, &level, &start, &end);
        int cnt = abs(level) - 2;
        assert(cnt >= 0);
        JSScope* scope = 0;
        for (int i = 0; i < cnt; ++i) {
            level = i + 1;
            int substart, subend;
            std::string subexpr = generateName(node, &level, &substart, &subend);
            Declaration* decl = findDeclaration(subexpr);
            if (!scope) {
                if (!decl) {
                    // not declared, we need to bind this to the global scope
                    scope = &mScopes.front();
                    assert(scope->mType == JSScope::Program);
                } else {
                    scope = decl->scope;
                }
            }
            assert(scope);
            if (!decl) {
                scope->addDeclaration(CursorInfo::JSVariable, subexpr, substart, subend);
            } else {
                decl->refs.push_back(std::make_pair(substart, subend));
                // found reference!
            }
        }
        if (!scope) {
            Declaration* decl = findDeclaration(expr);
            if (!decl) {
                // not declared, we need to bind this to the global scope
                scope = &mScopes.front();
                assert(scope->mType == JSScope::Program);
            } else {
                scope = decl->scope;
            }
        }
        assert(scope);
        Declaration* decl = findDeclaration(expr);
        if (!decl) {
            scope->addDeclaration(CursorInfo::JSVariable, expr, start, end);
        } else {
            decl->refs.push_back(std::make_pair(start, end));
        }
        JSScope* objectScope = findScope(JSScope::ExpressionStatement);
        if (objectScope) {
            objectScope->mObjectsAdded += addObjects(expr, mObjects);
        }
        further = false;
        break; }
    case JSScope::ObjectExpression: {
        v8::Handle<v8::String> propStr = v8::String::New("properties");
        if (node->Has(propStr)) {
            v8::Handle<v8::String> keyStr = v8::String::New("key");
            v8::Handle<v8::String> valueStr = v8::String::New("value");
            v8::Handle<v8::Array> props = v8::Handle<v8::Array>::Cast(node->Get(propStr));
            assert(!props.IsEmpty() && props->IsArray());
            const uint32_t len = props->Length();
            for (uint32_t i = 0; i < len; ++i) {
                v8::Handle<v8::Object> sub = v8::Handle<v8::Object>::Cast(props->Get(i));
                assert(!sub.IsEmpty() && sub->IsObject());
                if (sub->Has(keyStr)) {
                    v8::Handle<v8::Object> key = v8::Handle<v8::Object>::Cast(sub->Get(keyStr));
                    assert(!key.IsEmpty() && key->IsObject());
                    std::string nodeName;
                    int nodeStart, nodeEnd;
                    if (parseIdentifier(key, nodeName, nodeStart, nodeEnd)) {
                        JSScope* scope = mObjectScope;
                        if (!scope) {
                            scope = &mScopes.front();
                            assert(scope->mType == JSScope::Program);
                        }
                        mObjects.push_back(nodeName);
                        addObject(scope, mObjects, nodeStart, nodeEnd);
                        if (sub->Has(valueStr)) {
                            recurse(v8::Handle<v8::Object>::Cast(sub->Get(valueStr)));
                        }
                        mObjects.pop_back();
                    }
                }
            }
        }
        break; }
    case JSScope::VariableDeclaration: {
        v8::Handle<v8::String> declStr = v8::String::New("declarations");
        v8::Handle<v8::String> kindStr = v8::String::New("kind");
        if (!node->Has(declStr) || !node->Has(kindStr))
            break;
        v8::Handle<v8::Array> decls = v8::Handle<v8::Array>::Cast(node->Get(declStr));
        assert(!decls.IsEmpty() && decls->IsArray());
        v8::Handle<v8::String> kind = v8::Handle<v8::String>::Cast(node->Get(kindStr));
        assert(!kind.IsEmpty() && kind->IsString());
        v8::String::Utf8Value k(kind);
        VarType vartype = UnknownVar;
        if (!strcmp(*k, "var"))
            vartype = Var;
        else if (!strcmp(*k, "let"))
            vartype = Let;
        assert(vartype != UnknownVar);
        addDeclarations(vartype, decls);
        further = false;
        break; }
    case JSScope::FunctionExpression:
    case JSScope::FunctionDeclaration: {
        v8::Handle<v8::String> idStr = v8::String::New("id");
        if (node->Has(idStr)) {
            // this is the function name, this should be declared in the parent scope of the function scope
            JSScope* scope = findDeclarationScope(Var);
            assert(scope);
            // now get the parent scope
            scope = findDeclarationScope(Var, scope);
            assert(scope);
            addDeclaration(scope, CursorInfo::JSFunction, v8::Handle<v8::Object>::Cast(node->Get(idStr)));
        }
        v8::Handle<v8::String> paramsStr = v8::String::New("params");
        if (node->Has(paramsStr)) {
            // function arguments should be bound to the function scope
            v8::Handle<v8::Array> params = v8::Handle<v8::Array>::Cast(node->Get(paramsStr));
            assert(!params.IsEmpty() && params->IsArray());
            const uint32_t len = params->Length();

            JSScope* scope = findDeclarationScope(Var);
            assert(scope);
            for (uint32_t i = 0; i < len; ++i) {
                addDeclaration(scope, CursorInfo::JSVariable, v8::Handle<v8::Object>::Cast(params->Get(i)));
            }
        }
        break; }
    case JSScope::Identifier: {
        std::string nodeName;
        int nodeStart, nodeEnd;
        if (parseIdentifier(node, nodeName, nodeStart, nodeEnd)) {
            Declaration* decl = findDeclaration(nodeName);
            if (!decl) { // add to global scope;
                JSScope* scope = &mScopes.front();
                assert(scope->mType == JSScope::Program);
                scope->addDeclaration(CursorInfo::JSVariable, nodeName, nodeStart, nodeEnd);
            } else {
                decl->refs.push_back(std::make_pair(nodeStart, nodeEnd));
            }
        }
        break; }
    default:
        break;
    }

    if (further) {
        v8::Handle<v8::Array> properties = node->GetOwnPropertyNames();
        if (!properties.IsEmpty()) {
            const uint32_t len = properties->Length();
            for (uint32_t i = 0; i < len; ++i) {
                v8::Handle<v8::Value> key = properties->Get(i);
                v8::Handle<v8::Value> prop = node->Get(key);
                if (!prop.IsEmpty() && prop->IsObject()) {
                    v8::String::Utf8Value keyStr(key);
                    recurse(prop);
                }
            }
        }
    }

    if (typePushed) {
        JSScope& current = mScopes.back();
        syncScope(current);
        for (int i = 0; i < current.mObjectsAdded; ++i) {
            mObjects.pop_back();
        }
        mScopes.pop_back();
    }
}

void JSParser::addDeclarations(VarType type, const v8::Handle<v8::Array>& decls)
{
    v8::HandleScope handleScope;
    JSScope* scope = findDeclarationScope(type);
    assert(scope);

    v8::Handle<v8::String> idStr = v8::String::New("id");
    v8::Handle<v8::String> initStr = v8::String::New("init");

    const uint32_t len = decls->Length();
    for (uint32_t i = 0; i < len; ++i) {
        v8::Handle<v8::Object> obj = v8::Handle<v8::Object>::Cast(decls->Get(i));
        assert(!obj.IsEmpty() && obj->IsObject());
        assert(obj->Has(idStr));
        v8::Handle<v8::Object> sub = v8::Handle<v8::Object>::Cast(obj->Get(idStr));
        addDeclaration(scope, CursorInfo::JSVariable, sub);
        if (obj->Has(initStr)) {
            bool setScope = false;
            if (!mObjectScope) {
                mObjectScope = scope;
                setScope = true;
            }
            mObjects.push_back(*(v8::String::Utf8Value(sub->Get(v8::String::New("name")))));
            parseIdentifiers(v8::Handle<v8::Object>::Cast(obj->Get(initStr)));
            mObjects.pop_back();
            if (setScope)
                mObjectScope = 0;
        }
    }
}

void JSParser::addDeclaration(JSScope* scope, CursorInfo::JSCursorKind kind, const v8::Handle<v8::Object>& obj)
{
    assert(scope);
    std::string nodeName;
    int nodeStart, nodeEnd;
    if (parseIdentifier(obj, nodeName, nodeStart, nodeEnd)) {
        scope->addDeclaration(kind, nodeName, nodeStart, nodeEnd);
    }
}

void JSParser::parseIdentifiers(const v8::Handle<v8::Object>& obj)
{
    assert(!obj.IsEmpty() && (obj->IsNull() || obj->IsObject()));
    if (obj->IsNull())
        return;
    recurse(obj);
}
