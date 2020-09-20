## Lua Support

You can also run Lua code through the _script method:

* **lua** (or lua_v51)
* **lua_v51**, **lua_v52**, **lua_v53** or **lua_v54** - runs the code using specific Lua version as external Lua processes
* **luain** - runs the code within the same Lua thread and process
* **luajit** - runs the code using LuaJIT 5.1 as an external Lua process

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