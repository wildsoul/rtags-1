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

struct ArrayNode;
struct ObjectNode;
struct Node
{
    enum Type {
        Type_Null,
        Type_False,
        Type_True,
        Type_Object,
        Type_Array,
        Type_Integer,
        Type_String
    };
    Node(Node *p, Type t, const String &n) : parent(p), type(t), objectType(None), name(n), scope(0) {}
    virtual ~Node() { delete scope; }

    virtual Node *child(int i) const { return 0; }
    virtual int count() const { return 0; }
    virtual Map<String, Node*>::const_iterator begin() const { assert(0); return Map<String, Node*>::const_iterator(); }
    virtual Map<String, Node*>::const_iterator end() const { assert(0); return Map<String, Node*>::const_iterator(); }
    virtual Node *child(const String &key) const { return 0; }
    virtual String toString() const { return String(); }
    virtual int toInteger() const { return -1; }

    virtual String dump(int indent, bool ignoreFirstIndent, const String &filter) const
    {
        String ret;
        if (!ignoreFirstIndent)
            ret = String(indent * 4, ' ');
        switch (type) {
        case Type_Null: ret += "null"; break;
        case Type_True: ret += "true"; break;
        case Type_False: ret += "false"; break;
        default: assert(0); break;
        }
        return ret;
    }

    String objectTypeString() const
    {
        if (Node *n = child("type"))
            return n->toString();
        return String();
    }
    
    enum ObjectType {
        None,
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

    Node *parent;
    const Type type;
    ObjectType objectType;
    const String name;
    Map<String, int> *scope;
};

struct ArrayNode : public Node
{
    ArrayNode(Node *parent, const String &name) : Node(parent, Type_Array, name) {}
    virtual ~ArrayNode()
    {
        for (int i=0; i<nodes.size(); ++i)
            delete nodes.at(i);
    }
    virtual String dump(int indent, bool ignoreFirstIndent, const String &filter) const
    {
        String ret;
        if (!ignoreFirstIndent)
            ret = String(indent * 4, ' ');
        if (nodes.isEmpty()) {
            ret += "[]";
        } else {
            if (name == "range") {
                ret += "[ ";
            } else {
                ret += "[\n";
            }
            for (int i=0; i<nodes.size(); ++i) {
                if (name == "range") {
                    ret += nodes.at(i)->dump(indent + 1, true, filter);
                    if (i + 1 < nodes.size())
                        ret += ", ";
                } else {
                    ret += nodes.at(i)->dump(indent + 1, false, filter);
                    if (i + 1 < nodes.size())
                        ret += ',';
                    ret += "\n";
                }
            }
            if (name == "range") {
                ret += " ]";
            } else {
                ret += String(indent * 4, ' ');
                ret += ']';
            }
        }
        return ret;
    }
    virtual int count() const { return nodes.size(); }
    virtual Node *child(int i) const { return nodes.value(i); }

    List<Node*> nodes;
};

struct ObjectNode : public Node
{
    ObjectNode(Node *parent, const String &name) : Node(parent, Type_Object, name) {}
    virtual ~ObjectNode()
    {
        for (Map<String, Node*>::const_iterator it = nodes.begin(); it != nodes.end(); ++it)
            delete it->second;
    }
    virtual int count() const { return nodes.size(); }
    virtual Map<String, Node*>::const_iterator begin() const { return nodes.begin(); }
    virtual Map<String, Node*>::const_iterator end() const { return nodes.end(); }
    virtual Node *child(const String &key) const { return nodes.value(key); }
    virtual String dump(int indent, bool ignoreFirstIndent, const String &filter) const
    {
        String ret;
        if (!ignoreFirstIndent)
            ret = String(indent * 4, ' ');
        if (nodes.isEmpty()) {
            ret += "{}";
        } else {
            ret += "{\n";
            Map<String, Node*>::const_iterator it = nodes.begin();
            while (it != nodes.end()) {
                if (!filter.contains(it->first)) {
                    ret += String((indent + 1) * 4, ' ') + "\"" + it->first + "\": " + (it->second ? it->second->dump(indent + 1, true, filter) : String());
                    if (it != nodes.end())
                        ret += ',';
                    ret += "\n";
                }
                ++it;
            }
            ret += String(std::max(0, indent) * 4, ' ');
            ret += '}';
        }
        return ret;
    }


    Map<String, Node*> nodes;
};

struct IntegerNode : public Node
{
    IntegerNode(Node *parent, int val, const String &name)
        : Node(parent, Type_Integer, name), value(val)
    {}

    virtual int toInteger() const { return value; }
    virtual String dump(int indent, bool ignoreFirstIndent, const String &) const
    {
        String ret;
        if (!ignoreFirstIndent)
            ret = String(indent * 4, ' ');
        ret += String::number(value);
        return ret;
    }

    const int value;
};


struct StringNode : public Node
{
    StringNode(Node *parent, const String &val, const String &name)
        : Node(parent, Type_String, name), value(val)
    {}

    virtual String toString() const { return value; }
    virtual String dump(int indent, bool ignoreFirstIndent, const String &) const
    {
        String ret;
        if (!ignoreFirstIndent)
            ret = String(indent * 4, ' ');
        ret += "\"" + value + "\""; // escape ###
        return ret;
    }

    const String value;
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
    Node *recurse(v8::Handle<v8::Value> value, Node *parent, const String &name = String());
    void visit(Node *node);
    void createObject(ObjectNode *node);

    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Object> mEsprima;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
    uint32_t mFileId;
    Node *mRoot;
    SymbolMap *mSymbols;
    SymbolNameMap *mSymbolNames;
    String *mErrors;
};


#endif
