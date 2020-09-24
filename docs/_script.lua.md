## Lua Support

You can also run Lua code through the _script method:

* _script.**lua** (or lua_v51)
* _script.**lua_v51**, **lua_v52**, **lua_v53** or **lua_v54** - runs the code using specific Lua version as external Lua processes
* _script.**luain** - runs the code within the same Lua thread and process
* _script.**luajit** - runs the code using LuaJIT 5.1 as an external Lua process

* _script32.**lua** (or lua_v51) - runs the code using Lua 5.1 32-bit version as external Lua process

### Example - Hello World

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.lua
 [[
  print(string.upper(s));
 ]]
end

say(" Hello World! ")
```