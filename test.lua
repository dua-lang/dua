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
            local res = "    "..tostring(m).."\n"
            if res ~= want then
                error("failed: " .. name .. "\nres:\n" .. res .. "\nast:\n" .. tostring(m), 2)
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
    Module(5, 29, Body(5, 29, [Var(5, 19, VarDecl(9, 19, "x", Type(0, 0, "int"), Value(18, 19, 1))), Set(24, 29, Ident(24, 25, "x", false, false), Value(28, 29, 2))]), {})
]]