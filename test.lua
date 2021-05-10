local dua = require "dua"

local function test(name)
    local vars = {
        print = {"id", 0, 0, "print", false, false};
        pairs = {"id", 0, 0, "pairs", false, false};
        next =  {"id", 0, 0, "next", false, false};
        mock =  {"id", 0, 0, "mock", false, false};
    }
    return function(src)
        local r, m = pcall(dua.parse_module, src, nil, vars)
        if not r then
            error("failed: " .. name .. "\nerror: " .. m, 2)
        end
        return function(want)
            -- local res = dua.emit_module(m, 1)
            local res = getmetatable(m)._pretty(m).."\n"
            if res ~= want then
                print(res)
                -- error("failed: " .. name .. "\nres:\n" .. res .. "\nast:\n" .. tostring(m), 2)
            end
        end
    end
end

test "case_01"
[[
    var x: int = 1
    x = 2
]]
[[
Module (5, 29)
  Body: Body (5, 29)
    Expr: [
      1: Var (5, 19)
        Decl: VarDecl (9, 19)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: Value (18, 19)
            Value: 1
      2: Set (24, 29)
        Ident: Ident (24, 25)
          Name: "x"
          Args: false
          Tail: false
        Expr: Value (28, 29)
          Value: 2
    ]
  Comments: {}
]]

test "case_02"
[[
    var x: int
    x = 1
]]
[[
Module (5, 25)
  Body: Body (5, 25)
    Expr: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Set (20, 25)
        Ident: Ident (20, 21)
          Name: "x"
          Args: false
          Tail: false
        Expr: Value (24, 25)
          Value: 1
    ]
  Comments: {}
]]

test "case_03"
[[
    var x: int
    x = 1 * 2 + 3
]]
[[
Module (5, 33)
  Body: Body (5, 33)
    Expr: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Set (20, 33)
        Ident: Ident (20, 21)
          Name: "x"
          Args: false
          Tail: false
        Expr: Binop (24, 33)
          Op: "+"
          Lhs: Binop (24, 29)
            Op: "*"
            Lhs: Value (24, 25)
              Value: 1
            Rhs: Value (28, 29)
              Value: 2
          Rhs: Value (32, 33)
            Value: 3
    ]
  Comments: {}
]]

test "case_04"
[[
    var x: int
    x = 1 * 2 + 3
]]
[[
Module (5, 33)
  Body: Body (5, 33)
    Expr: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Set (20, 33)
        Ident: Ident (20, 21)
          Name: "x"
          Args: false
          Tail: false
        Expr: Binop (24, 33)
          Op: "+"
          Lhs: Binop (24, 29)
            Op: "*"
            Lhs: Value (24, 25)
              Value: 1
            Rhs: Value (28, 29)
              Value: 2
          Rhs: Value (32, 33)
            Value: 3
    ]
  Comments: {}
]]

test "case_05"
[[
    var x: int
    x = 1 * (2 + 3)
]]
[[
Module (5, 35)
  Body: Body (5, 35)
    Expr: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Set (20, 35)
        Ident: Ident (20, 21)
          Name: "x"
          Args: false
          Tail: false
        Expr: Binop (24, 35)
          Op: "*"
          Lhs: Value (24, 25)
            Value: 1
          Rhs: Paren (28, 35)
            Expr: Binop (29, 34)
              Op: "+"
              Lhs: Value (29, 30)
                Value: 2
              Rhs: Value (33, 34)
                Value: 3
    ]
  Comments: {}
]]