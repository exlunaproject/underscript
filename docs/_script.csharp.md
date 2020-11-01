## CSharp Support

### Hello World

```lua
require "Underscript"

function writeln(s) _script.csharp
 [[
 Console.WriteLine(s.ToUpper());
 ]]
end

writeln("Hello World!")
```