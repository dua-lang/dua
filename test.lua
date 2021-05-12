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
    List: [
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
    List: [
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
    List: [
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
    List: [
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
    List: [
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

test "case_06"
[[
    var x: int
    for x = 1 to 10 by 2 do

    end
]]
[[
Module (5, 52)
  Body: Body (5, 52)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: For (20, 52)
        Ident: Ident (24, 25)
          Name: "x"
          Args: false
          Tail: false
        From: Value (28, 29)
          Value: 1
        Limit: Value (33, 35)
          Value: 10
        Step: Value (39, 40)
          Value: 2
        Body: Body (43, 43)
          List: [

          ]
    ]
  Comments: {}
]]

test "case_07"
[[
    var x: int
    var y: int
    for x = 1 to 10 do
        y = x
    end
]]
[[
Module (5, 75)
  Body: Body (5, 75)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Var (20, 30)
        Decl: VarDecl (24, 30)
          Name: "y"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      3: For (35, 75)
        Ident: Ident (39, 40)
          Name: "x"
          Args: false
          Tail: false
        From: Value (43, 44)
          Value: 1
        Limit: Value (48, 50)
          Value: 10
        Step: false
        Body: Body (62, 67)
          List: [
            1: Set (62, 67)
              Ident: Ident (62, 63)
                Name: "y"
                Args: false
                Tail: false
              Expr: Ident (66, 67)
                Name: "x"
                Args: false
                Tail: false
          ]
    ]
  Comments: {}
]]

test "case_08"
[[
    var x: int
    while x < 10 do
        x += 1
    end
]]
[[
Module (5, 58)
  Body: Body (5, 58)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: While (20, 58)
        Expr: Binop (26, 32)
          Op: "<"
          Lhs: Ident (26, 27)
            Name: "x"
            Args: false
            Tail: false
          Rhs: Value (30, 32)
            Value: 10
        Body: Body (44, 50)
          List: [
            1: Inc (44, 50)
              Ident: Ident (44, 45)
                Name: "x"
                Args: false
                Tail: false
              Expr: Value (49, 50)
                Value: 1
          ]
    ]
  Comments: {}
]]

test "case_09"
[[
    var x: int
    repeat
        x += 1
    until x > 10
]]
[[
Module (5, 58)
  Body: Body (5, 58)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Repeat (20, 58)
        Body: Body (35, 41)
          List: [
            1: Inc (35, 41)
              Ident: Ident (35, 36)
                Name: "x"
                Args: false
                Tail: false
              Expr: Value (40, 41)
                Value: 1
          ]
        Expr: Binop (52, 58)
          Op: ">"
          Lhs: Ident (52, 53)
            Name: "x"
            Args: false
            Tail: false
          Rhs: Value (56, 58)
            Value: 10
    ]
  Comments: {}
]]

test "case_10"
[[
    var x: int
    if x < 10 then
        x += 1
    end
]]
[[
Module (5, 57)
  Body: Body (5, 57)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: If (20, 57)
        Expr: Binop (23, 29)
          Op: "<"
          Lhs: Ident (23, 24)
            Name: "x"
            Args: false
            Tail: false
          Rhs: Value (27, 29)
            Value: 10
        Then: Body (43, 49)
          List: [
            1: Inc (43, 49)
              Ident: Ident (43, 44)
                Name: "x"
                Args: false
                Tail: false
              Expr: Value (48, 49)
                Value: 1
          ]
        Else: false
    ]
  Comments: {}
]]

test "case_11"
[[
    var x: int
    if x < 10 then
        x += 1
    elseif x < 20 then
        x += 2
    elseif x < 30 then
        x += 3
    else
        x += 4
    end
]]
[[
Module (5, 157)
  Body: Body (5, 157)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: If (20, 157)
        Expr: Binop (23, 29)
          Op: "<"
          Lhs: Ident (23, 24)
            Name: "x"
            Args: false
            Tail: false
          Rhs: Value (27, 29)
            Value: 10
        Then: Body (43, 49)
          List: [
            1: Inc (43, 49)
              Ident: Ident (43, 44)
                Name: "x"
                Args: false
                Tail: false
              Expr: Value (48, 49)
                Value: 1
          ]
        Else: If (54, 157)
          Expr: Binop (61, 67)
            Op: "<"
            Lhs: Ident (61, 62)
              Name: "x"
              Args: false
              Tail: false
            Rhs: Value (65, 67)
              Value: 20
          Then: Body (81, 87)
            List: [
              1: Inc (81, 87)
                Ident: Ident (81, 82)
                  Name: "x"
                  Args: false
                  Tail: false
                Expr: Value (86, 87)
                  Value: 2
            ]
          Else: If (92, 157)
            Expr: Binop (99, 105)
              Op: "<"
              Lhs: Ident (99, 100)
                Name: "x"
                Args: false
                Tail: false
              Rhs: Value (103, 105)
                Value: 30
            Then: Body (119, 125)
              List: [
                1: Inc (119, 125)
                  Ident: Ident (119, 120)
                    Name: "x"
                    Args: false
                    Tail: false
                  Expr: Value (124, 125)
                    Value: 3
              ]
            Else: Body (143, 149)
              List: [
                1: Inc (143, 149)
                  Ident: Ident (143, 144)
                    Name: "x"
                    Args: false
                    Tail: false
                  Expr: Value (148, 149)
                    Value: 4
              ]
    ]
  Comments: {}
]]

test "case_12"
[[
    function foo(x: int, y: int) -> int
        return x * y
    end
]]
[[
Module (5, 69)
  Body: Body (5, 69)
    List: [
      1: FuncDecl (5, 61)
        Sign: SignDecl (5, 40)
          Receiver: false
          Name: "foo"
          Parameters: Parameters (17, 33)
            List: [
              1: Parameter (18, 24)
                Name: "x"
                Type: Type (0, 0)
                  Name: "int"
                  Decl: false
                Var: false
              2: Parameter (26, 32)
                Name: "y"
                Type: Type (0, 0)
                  Name: "int"
                  Decl: false
                Var: false
            ]
          Type: Type (0, 0)
            Name: "int"
            Decl: false
        Body: Body (49, 61)
          List: [
            1: Return (49, 61)
              Expr: Binop (56, 61)
                Op: "*"
                Lhs: Ident (56, 57)
                  Name: "x"
                  Args: false
                  Tail: false
                Rhs: Ident (60, 61)
                  Name: "y"
                  Args: false
                  Tail: false
          ]
    ]
  Comments: {}
]]

test "case_13"
[[
    type Record = record
        x: int
        y: int
    end
    var x: Record
]]
[[
Module (5, 81)
  Body: Body (5, 81)
    List: [
      1: Type (5, 63)
        Name: "Record"
        Decl: RecodDecl (19, 63)
          List: [
            1: FieldDecl (34, 40)
              Name: "x"
              Type: Type (0, 0)
                Name: "int"
                Decl: false
            2: FieldDecl (49, 55)
              Name: "y"
              Type: Type (0, 0)
                Name: "int"
                Decl: false
          ]
      2: Var (68, 81)
        Decl: VarDecl (72, 81)
          Name: "x"
          Type: RecodDecl (19, 63)
            List: [
              1: FieldDecl (34, 40)
                Name: "x"
                Type: Type (0, 0)
                  Name: "int"
                  Decl: false
              2: FieldDecl (49, 55)
                Name: "y"
                Type: Type (0, 0)
                  Name: "int"
                  Decl: false
            ]
          Expr: false
    ]
  Comments: {}
]]

test "case_14"
[[
    var x: [10]int
]]
[[
Module (5, 19)
  Body: Body (5, 19)
    List: [
      1: Var (5, 19)
        Decl: VarDecl (9, 19)
          Name: "x"
          Type: ArrayDecl (12, 19)
            Size: Value (13, 15)
              Value: 10
            Type: Type (0, 0)
              Name: "int"
              Decl: false
          Expr: false
    ]
  Comments: {}
]]

test "case_15"
[[
    var x: []int
]]
[[
Module (5, 17)
  Body: Body (5, 17)
    List: [
      1: Var (5, 17)
        Decl: VarDecl (9, 17)
          Name: "x"
          Type: ArrayDecl (12, 17)
            Size: false
            Type: Type (0, 0)
              Name: "int"
              Decl: false
          Expr: false
    ]
  Comments: {}
]]

test "case_16"
[[
    var x: int
    case x
    when 1, 2 then
        x += 1
    when 3 then
        x += 2
    else
        x += 3
    end
]]
[[
Module (5, 123)
  Body: Body (5, 123)
    List: [
      1: Var (5, 15)
        Decl: VarDecl (9, 15)
          Name: "x"
          Type: Type (0, 0)
            Name: "int"
            Decl: false
          Expr: false
      2: Case (20, 123)
        Expr: Ident (25, 26)
          Name: "x"
          Args: false
          Tail: false
        List: [
          1: When (31, 60)
            List: [
              1: Value (36, 37)
                Value: 1
              2: Value (39, 40)
                Value: 2
            ]
            Expr: false
            Body: Body (54, 60)
              List: [
                1: Inc (54, 60)
                  Ident: Ident (54, 55)
                    Name: "x"
                    Args: false
                    Tail: false
                  Expr: Value (59, 60)
                    Value: 1
              ]
          2: When (65, 91)
            List: [
              1: Value (70, 71)
                Value: 3
            ]
            Expr: false
            Body: Body (85, 91)
              List: [
                1: Inc (85, 91)
                  Ident: Ident (85, 86)
                    Name: "x"
                    Args: false
                    Tail: false
                  Expr: Value (90, 91)
                    Value: 2
              ]
        ]
        Else: Body (109, 115)
          List: [
            1: Inc (109, 115)
              Ident: Ident (109, 110)
                Name: "x"
                Args: false
                Tail: false
              Expr: Value (114, 115)
                Value: 3
          ]
    ]
  Comments: {}
]]