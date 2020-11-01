## JavaScript Supersets Support

You can also run JS code through the following methods:

* _script.**typescript** - TypeScript (Deno), same as _script.jspp.tsdeno
* _script.jspp.**script** - JS++ (by Onux)
* _script.jspp.**tsdeno** - TypeScript (Deno)
* _script.jspp.**tiscript** - TScript (by Terra Informatica)

### Hello World (Using TypeScript)

```lua
require "Underscript"

function say(s) _script.typescript
 [[
 console.log(s.toUpperCase()+'!');
 ]]
end

say('Hello World')
```

### Hello World (Using JS++)

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.jspp.script
 [[
 Console.log(s.toUpperCase()+'!');
 ]]
end

say('Hello World')
```

### Hello World (Using TIScript)

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.jscript
 [[
 stdout.println(s.toUpperCase()+"!");
 ]]
end

say('Hello World')
```

