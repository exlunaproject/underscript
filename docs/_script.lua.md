## Lua Support

You can also run Lua code through the _script method:

* _script.**luascript** (or lua.v51) - runs the code using Lua 5.1 64-bit version as external Lua process
* _script.**luascript32** - runs the code using Lua 5.1 32-bit version as external Lua process

* _script.lua.**v51**, **v52**, **v53** or **v54** - runs the code using specific Lua version as external Lua processes
* _script.lua.**in** - runs the code within the same Lua thread and process
* _script.lua.**jit** - runs the code using LuaJIT 5.1 as an external Lua process



### Example - Hello World

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.luascript
 [[
  print(string.upper(s));
 ]]
end

say(" Hello World! ")
```