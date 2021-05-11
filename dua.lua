-- MIT License

-- Copyright (c) 2021 tsukanov-as

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

local setmt, getmt = setmetatable, getmetatable
local format = string.format
local sub = string.sub
local rep = string.rep
local find = string.find
local byte = string.byte
local tostring = tostring
local concat = table.concat
local pairs = pairs
local ipairs = ipairs

local Type = {
    __call = function(self, t)
        return setmt(t or {}, self)
    end;
}
setmt(Type, Type)

local List = Type({
    __tostring = function(self)
        local res = {}
        for i, v in ipairs(self) do
            if type(v) == "string" then
                res[i] = format("%q", v)
            else
                res[i] = tostring(v)
            end
        end
        return "[" .. concat(res, ", ") .. "]"
    end;
    _pretty = function(self, level)
        level = (level or 0) + 1
        local res = {}
        for i, v in ipairs(self) do
            if type(v) == "string" then
                res[i] = rep("  ", level)..i..": "..format("%q", v)
            elseif type(v) == "table" and getmt(v)._pretty then
                res[i] = rep("  ", level)..i..": "..getmt(v)._pretty(v, level)
            else
                res[i] = rep("  ", level)..i..": "..tostring(v)
            end
        end
        return "[\n"..concat(res, "\n").."\n"..rep("  ", level-1).."]"
    end
})

local Dict = Type({
    __tostring = function(self)
        local res = {}
        local i = 0
        for k, v in pairs(self) do
            i = i + 1
            if type(v) == "string" then
                res[i] = format("%q: %s", k, tostring(v))
            else
                res[i] = format("%s: %s", tostring(k), tostring(v))
            end
        end
        return "{" .. concat(res, "; ") .. "}"
    end;
})

local Hex = Type({
    __tostring = function(t)
        return format("0x%02X", t[1])
    end;
})

local Raw = Type({
    __tostring = function(t)
        local s = t[1]
        local i = 0
        local x = nil
        while true do
            x = rep("=", i)
            if not find(s, "[" .. x .. "[", 1, true) and not find(s, "]" .. x .. "]", 1, true) then
                break
            end
            i = i + 1
        end
        return format(" [%s[%s]%s]", x, s, x)
    end;
})

local ast = {}

do
    local function __index(self, k)
        if type(k) == "string" then
            return self[getmt(self)[k]]
        end
        return nil
    end;

    local function __tostring(self)
        local res = {}
        for i, v in ipairs(self) do
            if type(v) == "string" then
                res[i] = format("%q", v)
            else
                res[i] = tostring(v)
            end
        end
        return getmt(self)._tag .. "(" .. concat(res, ", ") .. ")"
    end

    local function pretty(self, level)
        level = (level or 0) + 1
        local mt = getmt(self)
        local res = {}
        for i = 3, #self do
            local v = self[i]
            local prefix = rep("  ", level)..mt[i]..": "
            if type(v) == "string" then
                res[#res+1] = prefix..format("%q", v)
            elseif type(v) == "table" and getmt(v)._pretty then
                res[#res+1] = prefix..getmt(v)._pretty(v, level)
            else
                res[#res+1] = prefix..tostring(v)
            end
        end
        return mt._tag.." ("..self[1]..", "..self[2]..")\n"..concat(res, "\n")
    end

    local define = function(tag)
        return function(t)
            t._tag = tag
            t._pretty = pretty
            t.__tostring = __tostring
            t.__index = __index
            for i, v in ipairs(t) do
                t[v] = i
            end
            ast[tag] = Type(t)
        end
    end

    define "Symbol" {"Name", "Decl"}

    define "Module" {"Pos", "End", "Body", "Comments"}
    define "Value"  {"Pos", "End", "Value"}
    define "Ident"  {"Pos", "End", "Name", "Args", "Tail"}
    define "Field"  {"Pos", "End", "Name", "Args"}
    define "Index"  {"Pos", "End", "Expr"}

    define "Var"   {"Pos", "End", "Decl"}
    define "Type"  {"Pos", "End", "Name", "Decl"}
    define "Const" {"Pos", "End", "List"}

    define "VarDecl"   {"Pos", "End", "Name", "Type", "Expr"}
    define "ConstDecl" {"Pos", "End", "Name", "Type", "Value"}
    define "RecodDecl" {"Pos", "End", "List"}
    define "FieldDecl" {"Pos", "End", "Name", "Type"}
    define "ArrayDecl" {"Pos", "End", "Size", "Type"}

    define "SignDecl"   {"Pos", "End", "Receiver", "Name", "Parameters", "Type"}
    define "FuncDecl"   {"Pos", "End", "Sign", "Body"}
    define "Receiver"   {"Pos", "End", "Name", "Type"}
    define "Parameter"  {"Pos", "End", "Name", "Type", "Var"}
    define "Parameters" {"Pos", "End", "List", "Var"}

    define "Set"  {"Pos", "End", "Ident", "Expr"}
    define "Call" {"Pos", "End", "Ident"}

    define "Inc"  {"Pos", "End", "Ident", "Expr"}
    define "Dec"  {"Pos", "End", "Ident", "Expr"}

    define "If"     {"Pos", "End", "Expr", "Then", "Else"}
    define "Else"   {"Pos", "End", "Body"}
    define "For"    {"Pos", "End", "Ident", "From", "Limit", "Step", "Body"}
    define "While"  {"Pos", "End", "Expr", "Body"}
    define "Repeat" {"Pos", "End", "Body", "Expr", "Else"}
    define "Case"   {"Pos", "Len", "Expr", "List", "Else"}
    define "When"   {"Pos", "Len", "List", "Expr", "Body"}

    define "Body"  {"Pos", "Len", "List"}
    define "Local" {"Pos", "Len", "Expr", "List"}

    define "Return"   {"Pos", "End", "Expr"}
    define "Continue" {"Pos", "End"}
    define "Break"    {"Pos", "End"}

    define "Nop" {"Pos", "End"}

    define "Unop" {"Pos", "Len", "Op", "Rhs"}
    define "Binop" {"Pos", "Len", "Op", "Lhs", "Rhs"}
    define "Paren" {"Pos", "Len", "Expr"}

end

local function set(t)
    local s = {}
    for _, v in ipairs(t) do
        s[v] = v
    end
    return s
end

local KEYWORDS = set{
    "break", "continue", "else", "false", "for", "function", "if", "type", "var",
    "nil", "and", "or", "by", "to", "not", "return", "true", "when", "case", "record",
    "const", "while", "do", "end", "repeat", "until", "then", "elseif", "local"
}
local RESERVED = set{"in"}
local LITERALS = set{"str", "chr", "raw", "num", "true", "false", "nil"}
local REL_OPS = set{"==", "<>", "<", ">", "<=", ">="}
local MUL_OPS = set{"*", "/", "%"}
local ADD_OPS = set{"+", "-"}
local UNR_OPS = set{"+", "-", "~"}

local MAP = {
    [0x22] = "str";
    [0x27] = "chr";
    [0x60] = "raw";
}
local HEX = {}
local ALPHA_OR_DIGIT = {}
do
    local sym = "()[]{}*:;.,/+-=<>%"
    for i = 1, #sym do
        MAP[byte(sym, i)] = sub(sym, i, i)
    end
    for i = 0x01, 0x20 do
        MAP[i] = "space"
    end
    for i = 0x30, 0x39 do
        MAP[i] = "digit"
        ALPHA_OR_DIGIT[i] = true
    end
    local abc = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    for i = 1, #abc do
        MAP[byte(abc, i)] = "alpha"
        ALPHA_OR_DIGIT[byte(abc, i)] = true
    end
    local hex = "ABCDEFabcdef0123456789"
    for i = 1, #hex do
        HEX[byte(hex, i)] = true
    end
end

local p_path = nil;
local p_src = nil;
local p_line = 1;
local p_pos = 0;
local p_tokpos = 0;
local p_end = 0;
local p_chr = nil;
local p_tok = nil;
local p_lit = "";
local p_val = nil;
local p_comments = List{};
local p_symbols = Dict{};
local p_scope = List{};
local p_level = 0;
local p_continue = List{};
local p_looplevel = 0;

local function errorf(notef, ...)
    if p_path then
        error(p_path .. ":" .. p_line .. ": " .. format(notef, ...), 2)
    else
        error(format(notef, ...) .. " in line " .. p_line, 2)
    end
end

local function next()
    p_pos = p_pos + 1
    p_chr = byte(p_src, p_pos);
end

local function scan()
    repeat
        p_tok = MAP[p_chr]
        p_lit = ""
        p_val = nil
        p_end = p_pos
        while p_tok == "space" do
            if p_chr == 0x0A then
                p_line = p_line + 1
            end
            next()
            p_tok = MAP[p_chr]
        end
        p_tokpos = p_pos
        local case = p_tok
        if case == "alpha" then
            local beg = p_pos
            repeat
                next()
            until not ALPHA_OR_DIGIT[p_chr]
            p_lit = sub(p_src, beg, p_pos-1)
            p_tok = KEYWORDS[p_lit] or "ident"
            if p_tok == "true" then
                p_val = true
            elseif p_tok == "false" then
                p_val = false
            end
        elseif case == "digit" then
            local beg = p_pos
            local base = 10
            if p_chr == 0x30 then
                next()
                local case = p_chr
                if case == 0x58 or case == 0x78 then
                    next()
                    beg = p_pos
                    if not HEX[p_chr] then
                        errorf("expected hex digit, found '%c'", p_chr)
                    end
                    repeat
                        next()
                    until not HEX[p_chr]
                    base = 16
                    if p_pos - beg > 8 then
                        errorf("integer greater than 32 bits", p_tok)
                    end
                elseif case == 0x42 or case == 0x62 then
                    next()
                    beg = p_pos
                    if p_chr ~= 0x30 and p_chr ~= 0x31 then
                        errorf("expected bin digit, found '%c'", p_chr)
                    end
                    repeat
                        next()
                    until p_chr ~= 0x30 and p_chr ~= 0x31
                    base = 2
                    if p_pos - beg > 32 then
                        errorf("integer greater than 32 bits", p_tok)
                    end
                end
            end
            if base == 10 then
                while MAP[p_chr] == "digit" do
                    next()
                end
                if p_chr == 0x2E then
                    repeat
                        next()
                    until MAP[p_chr] ~= "digit"
                end
                if p_chr == 0x45 or p_chr == 0x65 then
                    next()
                    if p_chr == 0x2B or p_chr == 0x2D then
                        next()
                    end
                    if MAP[p_chr] ~= "digit" then
                        errorf("expected digit, found '%s'", p_tok)
                    end
                    repeat
                        next()
                    until MAP[p_chr] ~= "digit"
                end
            end
            p_tok = "num"
            p_lit = sub(p_src, beg, p_pos - 1)
            if base == 10 then
                p_val = tonumber(p_lit)
            else
                p_val = Hex({tonumber(p_lit, base)})
            end
            if p_val == nil then
                errorf("malformed number '%s'", p_lit)
            end
        elseif case == "str" then
            local beg = p_pos
            repeat
                next()
                if p_chr == 0x5C then
                    p_pos = p_pos + 2;
                    p_chr = byte(p_src, p_pos)
                end
            until p_chr == 0x22 or p_chr == 0x0A or p_chr == nil
            if p_chr ~= 0x22 then
                errorf("expected \", found EOL")
            end
            p_lit = sub(p_src, beg + 1, p_pos - 1)
            p_val = p_lit
            next()
        elseif case == "raw" then
            local beg = p_pos
            repeat
                next()
                if p_chr == 0x0A then
                    p_line = p_line + 1
                end
            until p_chr == 0x60 or p_chr == nil
            if p_chr ~= 0x60 then
                errorf("expected `, found '%c'", p_chr)
            end
            p_lit = sub(p_src, beg + 1, p_pos - 1)
            p_val = Raw({p_lit})
            next()
        elseif case == "chr" then
            local beg = p_pos
            next()
            if p_chr == 0x5C then
                beg = p_pos
                next()
            end
            next()
            if p_chr ~= 0x27 then
                errorf("expected ', found '%c'", p_chr)
            end
            p_lit = sub(p_src, beg + 1, p_pos - 1)
            p_val = Hex({byte(p_lit)})
            next()
        elseif (case == nil) and (p_chr ~= nil) then
            errorf("unknown symbol '%c'", p_chr)
        else
            local prev = p_chr
            next()
            if p_chr == 0x3D then
                if prev == 0x3D then
                    p_tok = "==";
                    next()
                elseif prev == 0x2B then
                    p_tok = "+=";
                    next()
                elseif prev == 0x2D then
                    p_tok = "-=";
                    next()
                elseif prev == 0x3C then
                    p_tok = "<=";
                    next()
                elseif prev == 0x3E then
                    p_tok = ">=";
                    next()
                end
            elseif p_chr == 0x3E then
                if prev == 0x3C then
                    p_tok = "<>";
                    next()
                elseif prev == 0x2D then
                    p_tok = "->";
                    next()
                end
            elseif p_chr == 0x2F then
                if prev == 0x2F then
                    local beg = p_pos
                    repeat
                        next()
                    until p_chr == 0x0A or p_chr == nil
                    p_lit = sub(p_src, beg + 1, p_pos - 1)
                    p_comments[p_line] = p_lit
                    p_tok = "//"
                end
            end
        end
        -- print(p_tok)
    until p_tok ~= "//"
end

local function expect(t, l)
    if p_tok ~= t then
        local str = nil
        if p_tok == "num" or p_tok == "str" or p_tok == "chr" or p_tok == "ident" then
            str = p_lit
        else
            str = tostring(p_tok)
        end
        if p_path then
            error(format("%s:%d: expected '%s', found '%s'", p_path, p_line, tostring(t), str), l or 2)
        else
            error(format("expected '%s', found '%s' in line %d", tostring(t), str, p_line), l or 2)
        end
    end
end

local function skip(t)
    expect(t, 3)
    return scan()
end

local function open_scope(symbols)
    p_symbols = symbols or Dict{}
    p_level = p_level + 1
    p_scope[p_level] = p_symbols
end

local function close_scope()
    p_level = p_level - 1
    p_symbols = p_scope[p_level]
end

local function find_symbol(name)
    if name == "_" then
        return nil
    end
    local v = p_symbols[name]
    local i = p_level
    while v == nil and i > 1 do
        i = i - 1
        v = p_scope[i][name]
    end
    return v, i
end

local parse_expr = nil

local function parse_tail(call)
    local tail = List{}
    while true do
        local pos = p_tokpos
        if p_tok == "." then
            scan()
            if KEYWORDS[p_lit] == nil then
                expect("ident")
            end
            local name = p_lit
            local args = false
            scan()
            local endpos = p_end
            if p_tok == "(" then
                scan()
                args = List({})
                while p_tok ~= ")" do
                    args[#args + 1] = parse_expr()
                    if p_tok ~= "," then
                        break
                    end
                    scan()
                end
                skip(")")
                endpos = p_end
                call = true
            else
                call = false
            end
            if args and (KEYWORDS[name] or RESERVED[name]) then
                errorf("name '%s' cannot be used in a method call", name)
            end
            local item = ast.Field{pos, endpos-1, name, args}
            tail[#tail+1] = item
        elseif p_tok == "[" then
            scan()
            if p_tok == "]" then
                errorf("expected expression, found ']'")
            end
            local expr = parse_expr()
            skip("]")
            tail[#tail+1] = ast.Index{pos, p_end, expr}
        else
            break
        end
    end
    return #tail > 0 and tail, call
end

local function parse_ident()
    local pos = p_tokpos
    local name = p_lit
    local sym = find_symbol(name)
    if not sym then
        errorf("undeclared variable '%s'", name)
    end
    local decl = sym.Decl
    if getmt(decl) == ast.ConstDecl then
        return decl.Value
    end
    local call = false
    local args, tail = false, false
    scan()
    if p_tok == "(" then
        scan()
        args = List({})
        while p_tok ~= ")" do
            args[#args + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        skip(")")
        call = true
    end
    tail, call = parse_tail(call)
    return ast.Ident{pos, p_end, name, args, tail}, call
end

local function parse_paren()
    local pos = p_tokpos
    skip("(")
    local expr = parse_expr()
    skip(")")
    return ast.Paren{pos, p_end, expr}
end

local function parse_operand()
    if p_tok == "ident" then
        return parse_ident()
    elseif p_tok == "(" then
        return parse_paren()
    elseif LITERALS[p_tok] then
        local pos, val = p_tokpos, p_val
        scan()
        return ast.Value{pos, p_end, val}
    else
        errorf("expected operand, found '%s'", p_tok)
    end
end

local function parse_unary()
    local pos = p_tokpos
    local expr = nil
    if UNR_OPS[p_tok] then
        local op = p_tok
        scan()
        local rhs = parse_operand()
        expr = ast.Unop{pos, p_end, op, rhs}
    else
        expr = parse_operand()
    end
    return expr
end

local function parse_mul()
    local pos = p_tokpos
    local lhs = parse_unary()
    while MUL_OPS[p_tok] do
        local op = p_tok
        scan()
        local rhs = parse_unary()
        lhs = ast.Binop{pos, p_end, op, lhs, rhs}
        pos = p_tokpos
    end
    return lhs
end

local function parse_add()
    local pos = p_tokpos
    local lhs = parse_mul()
    while ADD_OPS[p_tok] do
        local op = p_tok
        scan()
        local rhs = parse_mul()
        lhs = ast.Binop{pos, p_end, op, lhs, rhs}
        pos = p_tokpos
    end
    return lhs
end

local function parse_rel()
    local pos = p_tokpos
    local lhs = parse_add()
    while REL_OPS[p_tok] do
        local op = p_tok
        scan()
        local rhs = parse_add()
        lhs = ast.Binop{pos, p_end, op, lhs, rhs}
        pos = p_tokpos
    end
    return lhs
end

local function parse_not()
    local pos = p_tokpos
    local expr = nil
    if p_tok == "not" then
        local op = p_tok
        scan()
        local rhs = parse_rel()
        expr = ast.Not{pos, p_end, op, rhs}
    else
        expr = parse_rel()
    end
    return expr
end

local function parse_and()
    local pos = p_tokpos
    local lhs = parse_not()
    while p_tok == "and" do
        local op = p_tok
        scan()
        local rhs = parse_not()
        lhs = ast.Binop{pos, p_end, op, lhs, rhs}
        pos = p_tokpos
    end
    return lhs
end

parse_expr = function()
    local pos = p_tokpos
    local lhs = parse_and()
    while p_tok == "or" do
        local op = p_tok
        scan()
        local rhs = parse_and()
        lhs = ast.Binop{pos, p_end, op, lhs, rhs}
    end
    return lhs
end

local parse_body = nil

local function parse_set_or_call()
    local pos = p_tokpos
    local name = p_lit
    local id, call = parse_ident()
    if call then
        return ast.Call{pos, p_end, id}
    end
    if p_tok == "=" then
        scan()
        local expr = parse_expr()
        return ast.Set{pos, p_end, id, expr}
    end
    if p_tok == "+=" then
        scan()
        local expr = parse_add()
        return ast.Inc{pos, p_end, id, expr}
    end
    if p_tok == "-=" then
        scan()
        local expr = parse_add()
        return ast.Dec{pos, p_end, id, expr}
    end
    expect("=")
end

local function parse_if()
    local pos = p_tokpos
    scan() -- skip 'if' or 'elseif'
    local expr = parse_expr()
    skip("then")
    local body = parse_body()
    local else_body = false
    if p_tok == "elseif" then
        else_body = parse_if()
        return ast.If{pos, p_end, expr, body, else_body}
    elseif p_tok == "else" then
        scan()
        else_body = parse_body()
    end
    skip("end")
    return ast.If{pos, p_end, expr, body, else_body}
end

local function parse_case()
    local pos = p_tokpos
    skip("case")
    local case = false
    local list = List{}
    if p_tok == "when" then
        while p_tok == "when" do
            scan()
            local expr = parse_expr()
            skip("then")
            local body = parse_body()
            list[#list+1] = ast.When{pos, p_end, false, expr, body}
        end
    else
        case = parse_expr()
        expect("when")
        while p_tok == "when" do
            scan()
            local vals = List{}
            while true do
                vals[#vals+1] = parse_expr()
                if p_tok ~= "," then
                    break
                end
                scan()
            end
            local expr = false
            if p_tok == ";" then
                scan()
                expr = parse_expr()
            end
            skip("then")
            local body = parse_body()
            list[#list+1] = ast.When{pos, p_end, vals, expr, body}
        end
    end
    local else_body = false
    if p_tok == "else" then
        scan()
        else_body = parse_body()
    end
    skip("}")
    return ast.Case{pos, p_end, case, list, else_body}
end

local function parse_for()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("for")
    expect("ident")
    local id, call = parse_ident()
    if call then
        errorf("unexpected '(' after '%s'", id.Name)
    end
    expect("=")
    scan()
    local from = parse_expr()
    if p_tok == "," then
        scan()
    else
        skip("to")
    end
    local to = parse_expr()
    local by = false
    if p_tok == "," or p_tok == "by" then
        scan()
        by = parse_expr()
    end
    skip("do")
    local body = parse_body(nil, true)
    skip("end")
    return ast.For{pos, p_end, id, from, to, by, body}
end

local function parse_while()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("while")
    local expr = parse_expr()
    skip("do")
    local body = parse_body(nil, true)
    skip("end")
    return ast.While{pos, p_end, expr, body}
end

local function parse_repeat()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("repeat")
    local body = parse_body(nil, true)
    skip("until")
    local expr = parse_expr()
    return ast.Repeat{pos, p_end, body, expr}
end

local END = set{"end", "elseif", "when", "else"}

local function parse_return()
    local pos = p_tokpos
    skip("return")
    local expr = false
    if p_tok and not END[p_tok] then
        expr = parse_expr()
    end
    if p_tok and not END[p_tok] then
        expect("end")
    end
    return ast.Return{pos, p_end, expr}
end

local function parse_break()
    local pos = p_tokpos
    assert(p_looplevel > 0, "no loop to break")
    skip("break")
    if p_tok and not END[p_tok] then
        expect("end")
    end
    return ast.Break{pos, p_end}
end

local function parse_continue()
    local pos = p_tokpos
    assert(p_looplevel > 0, "no loop to continue")
    skip("continue")
    if p_tok and not END[p_tok] then
        expect("end")
    end
    p_continue[p_looplevel] = true
    return ast.Continue{pos, p_end}
end

local parse_type_decl

local function parse_const_decl()
    local pos = p_tokpos
    local name = p_lit
    if RESERVED[name] then
        errorf("name '%s' is reserved", name)
    end
    local sym = find_symbol(name)
    if sym then
        if getmt(sym.Decl) == ast.ConstDecl then
            errorf("shadowing of constant is prohibited, you need to change the name '%s'", name)
        else
            errorf("shadowing of variable is prohibited, you need to change the name '%s'", name)
        end
    end
    scan()
    sym = ast.Symbol({name, false})
    p_symbols[name] = sym
    skip(":")
    local type = parse_type_decl()
    skip("=")
    if not LITERALS[p_tok] then
        errorf("expected value")
    end
    local val = p_val
    scan()
    local decl = ast.ConstDecl({pos, p_end, type, val})
    sym.Init = decl
    return decl
end

local function parse_const()
    local pos = p_tokpos
    skip("const")
    if p_tok == "ident" then
        local list = List{parse_const_decl()}
        return ast.Const{pos, p_end, list}
    end
    if p_tok == "(" then
        scan()
        local list = List{}
        while p_tok == "ident" do
            list[#list+1] = parse_const_decl()
            if p_tok == ";" then
                scan()
            end
        end
        skip(")")
        return ast.Const{pos, p_end, list}
    end
    expect("ident")
end

local TYPEDECL = set{ast.Type, ast.RecodDecl, ast.ArrayDecl}

parse_type_decl = function()
    local pos = p_tokpos
    if p_tok == "ident" then
        local name = p_lit
        local sym = find_symbol(name)
        if not sym then
            errorf("undeclared type '%s'", name)
        end
        local decl = sym.Decl
        if not TYPEDECL[getmt(decl)] then
            errorf("not a type '%s'", name)
        end
        scan()
        return decl
    elseif p_tok == "[" then
        scan()
        local size = false
        if p_tok == "ident" then
            local name = p_lit
            local sym = find_symbol(name)
            if not sym then
                errorf("undeclared name '%s'", name)
            end
            local decl = sym.Decl
            if getmt(decl) ~= ast.ConstDecl then
                errorf("not a const '%s'", name)
            end
            size = sym
        elseif p_tok == "num" then
            local pos = p_tokpos
            local val = p_val
            scan()
            size = ast.Value{pos, p_end, val}
        end
        skip("]")
        local type = parse_type_decl()
        return ast.ArrayDecl{pos, p_end, size, type}
    elseif p_tok == "record" then
        scan()
        local list = List{}
        while p_tok ~= "end" do
            expect("ident")
            local pos = p_tokpos
            local name = p_lit
            scan()
            skip(":")
            local type = parse_type_decl()
            list[#list+1] = ast.FieldDecl{pos, p_end, name, type}
        end
        skip("end")
        return ast.RecodDecl{pos, p_end, list}
    end
    errorf("expected type declaration, found '%s'", p_tok)
end

local function parse_type()
    local pos = p_tokpos
    skip("type")
    expect("ident")
    local name = p_lit
    scan()
    skip("=")
    local decl = parse_type_decl()
    p_symbols[name] = ast.Symbol{name, decl}
    return ast.Type{pos, p_end, name, decl}
end

local function parse_var_decl()
    local pos = p_tokpos
    expect("ident")
    local name = p_lit
    scan()
    skip(":")
    local type = parse_type_decl()
    local expr = false
    if p_tok == "=" then
        scan()
        expr = parse_expr()
    end
    local decl = ast.VarDecl{pos, p_end, name, type, expr}
    p_symbols[name] = ast.Symbol{name, decl}
    return decl
end

local function parse_var()
    local pos = p_tokpos
    skip("var")
    local decl = parse_var_decl()
    return ast.Var{pos, p_end, decl}
end

local function parse_parameters()
    local pos = p_tokpos
    skip("(")
    local list = List{}
    while true do
        local parpos = p_tokpos
        local var = false
        if p_tok == "var" then
            var = true
            scan()
        end
        if p_tok ~= "ident" then
            break
        end
        local name = p_lit
        scan()
        skip(":")
        local type = parse_type_decl()
        local id = ast.Parameter{parpos, p_end, name, type, var}
        list[#list + 1] = id
        if p_symbols[name] then
            errorf("parameter '%s' is already declared", name)
        end
        p_symbols[name] = id
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip(")")
    return ast.Parameters{pos, p_end, list}
end

local function parse_function()
    local pos = p_tokpos
    local name = false
    local receiver = false
    local symbols = Dict{}
    skip("function")
    if p_tok == "(" then
        local receiver_pos = p_tokpos
        scan()
        expect("ident")
        local receiver_name = p_lit
        scan()
        expect("ident")
        local receiver_type = p_lit
        local sym = find_symbol(receiver_type)
        if not sym then
            errorf("undeclared variable '%s'", receiver_type)
        end
        if getmt(sym.Decl) == ast.ConstDecl then
            errorf("'%s' is a constant", receiver_type)
        end
        scan()
        skip(")")
        receiver = ast.Receiver({"receiver", receiver_pos, p_end, receiver_name, receiver_type})
        symbols[receiver_name] = ast.Symbol{"receiver_name", receiver}
    end
    expect("ident")
    name = p_lit
    if find_symbol(name) then
        errorf("shadowing of variable is prohibited, you need to change the name '%s'", name)
    end
    scan()
    local sym = ast.Symbol{name, false}
    p_symbols[name] = sym
    open_scope(symbols)
    local parameters = parse_parameters()
    local type = false -- TODO parse type
    if p_tok == "->" then
        scan()
        type = parse_type_decl()
    end
    local sign = ast.SignDecl{pos, p_end, receiver, name, parameters, type}
    sym[2] = sign
    local body = parse_body()
    close_scope()
    local func = ast.FuncDecl{pos, p_end, sign, body}
    p_symbols[name] = func
    scan()
    return func
end

local function parse_statement()
    if p_tok == "ident" then
        return parse_set_or_call()
    elseif p_tok == "var" then
        return parse_var()
    elseif p_tok == "type" then
        return parse_type()
    elseif p_tok == "function" then
        return parse_function()
    elseif p_tok == "if" then
        return parse_if()
    elseif p_tok == "{" then
        return parse_body()
    elseif p_tok == "for" then
        return parse_for()
    elseif p_tok == "while" then
        return parse_while()
    elseif p_tok == "repeat" then
        return parse_repeat()
    elseif p_tok == "return" then
        return parse_return()
    elseif p_tok == "break" then
        return parse_break()
    elseif p_tok == "continue" then
        return parse_continue()
    elseif p_tok == "case" then
        return parse_case()
    elseif p_tok == "const" then
        return parse_const()
    elseif p_tok == ";" then
        local pos = p_pos
        scan()
        return ast.Nop{pos, p_end}
    end
    return nil
end

parse_body = function(vars, loop)
    local pos = p_tokpos
    open_scope(vars)
    local body = List({})
    while true do
        local stmt = parse_statement()
        if stmt == nil then
            break
        end
        body[#body + 1] = stmt
    end
    close_scope()
    if loop and p_continue[p_looplevel] then
        body[#body+1] = ast.Label({"label", 0, 0, "continue"})
        p_continue[p_looplevel] = false
        p_looplevel = p_looplevel - 1
    end
    if pos > p_end then
        pos = p_end
    end
    return ast.Body{pos, p_end, body}
end

local function parse_module(src, path, vars)
    p_path = path
    p_src = src
    p_line = 1
    p_chr = nil
    p_pos = 0
    p_end = 0
    p_tokpos = 0
    p_tok = nil
    p_lit = ""
    p_val = nil
    p_comments = Dict{}
    p_symbols = Dict{
        int  = ast.Symbol{"int", ast.Type{0, 0, "int", false}};
        real = ast.Symbol{"real", ast.Type{0, 0, "real", false}};
        byte = ast.Symbol{"byte", ast.Type{0, 0, "byte", false}};
    }
    p_level = 1
    p_scope = List{p_symbols}
    p_continue = List{}
    p_looplevel = 0
    p_pos = p_pos + 1;
    p_chr = byte(p_src, p_pos)
    scan()
    local pos = p_tokpos
    local body = parse_body(vars)
    local module = ast.Module({pos, p_end, body, p_comments})
    if p_tok ~= nil then
        errorf("unexpected '%s'", p_tok)
    end
    return module
end

return {
    parse_module = parse_module
}