## VBScript Support

### Hello World Example

```lua
require "Underscript"

-- Prints "Hello World!"
function say(s) _script.vbscript
 [[
 Underscript.WriteLn(s & "!")
 ]]
end

say("Hello World")
```

**Note**: VBScript is case-insensitive - be careful to not use Lua variables with the same name and different case.
