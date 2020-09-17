## TCL Support

### Hello World

```lua
require "Underscript"

function test(s) _script.tcl
 [[
 set s "Hello World!";
 puts s;
 ]]
 return s
end

test("Change me!")
```