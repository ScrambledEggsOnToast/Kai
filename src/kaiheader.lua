-- Kai header
do
    local refs = {}
    local Typingmt = {
        __index = {},
        __newindex = function(t,k,v)
            error("cannot modify protected Kai values")
        end,
        __metatable = true,
        __concat = function(a,b)
            for i = 1, #b[refs] do
                a[refs][#a[refs]+1] = b[refs][i]
            end
            return a
        end

    }
    local Kai = {
        newtyping = function(r,...)
            local t = {}
            t[refs] = {{r,args}}
            setmetatable(t, Typingmt)
            return t
        end
    }
    local Kaimt = {
        __index = Kai,
        __newindex = function(t,k,v)
            error("cannot modify protected Kai values")
        end,
        __metatable = true
    }
    __Kai = {}
    setmetatable(__Kai, Kaimt)

end

