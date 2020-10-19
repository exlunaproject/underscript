# Underscript Lua Extensions

Underscript is ninja Lua extension library that extends Lua to:

* **Run a wide variety of scripting languages from within Lua scripts** through the _script function. The script can use the variables from the Lua script as it was part of itself. All kinds of scripting languages are supported, including various versions of Lua itself - allowing to run for example a LuaJIT script from within a standard Lua script. This function is expanding and evolving.
* **Cross-language import** a library through requirex functions (still limited and evolving). This allows for example to require a JavaScript file like it was a Lua script and then use its functions.
* **Run 32-bit Lua on Lua 64-bit** through the require32 function. This LoL (Lua on Lua) system allows to load and use legacy 32-bit Lua DLLs and libraries from within a 64-bit Lua application (still beta and evolving). 

## Examples

### LoL (Lua 32-bit on Lua 64-bit)
```lua
require "luaonlua"
uuid = require32("uuid") -- imports the uuid library from Lua 32-bit
print(uuid.new())
m = require32("math") -- imports the math library from Lua 32-bit
print(m.sqrt(1234))
print(type(m.sqrt(1234)))
print(m.pi)
```

This is possible under Windows systems thanks to WoW64, the subsystem of Windows capable of running 32-bit applications on 64-bit Windows. 

LoL when compiled to 32-bit also allows you to run 64-bit Lua on Lua 32-bit through the require64 function, but it has not been tested yet.

### Cross-Language Import

```lua
require "Underscript"
test = requirex.js("test") -- Loads test.js
test.hello()
print(test.aloha("Lua"))
```

### Run Script

```lua
require "Underscript"

-- Function Example using PHP
function crc16(s) _script.php
 [[
   $crc = 0xFFFF; 
   for ($x = 0; $x < strlen ($s); $x++) { 
     $crc = $crc ^ ord($s[$x]); 
     for ($y = 0; $y < 8; $y++) { 
       if (($crc & 0x0001) == 0x0001) { 
         $crc = (($crc >> 1) ^ 0xA001); 
       } else { $crc = $crc >> 1; } 
     } 
   } 
   $s = $crc; 
 ]]
 return s
end

-- Simple Hello World Example using PHP
function echo(s) _script.php
 [[
 echo($s);
 ]]
end
```

All local Lua variables (string, number, boolean and nil) are
automatically made accessible from within the code just like if they were part of the Lua script that is running it, as shown in the examples above. If you prefer, connected variables can be disabled or enabled, including for global variables (see **/docs/options.md**)

## Available Languages and Engines

Currently JavaScript, Java, LuaJIT, Pascal, Perl, PHP, Python, Ruby, TCL, VBScript and various versions of Lua itself are supported.

function | language
--- | ---
**_script.pascal**|DWS Pascal - Standard Script, same as pascalx.script (see table below)
**_script.perl**|Perl (Strawberry Perl), same as perlx.script
_script.perlx.script|Perl (Strawberry Perl)
_script.perlx.active|Perl (ActivePerl)
**_script.python**|Python script|
**_script.php**|PHP script|
**_script.java**|BeanShell Java script, same as _script.javax.bsh|
_script.javax.bsh|BeanShell Java script|
_script.javax.bshcore|BeanShell Core Java script|
**_script.javascript**|JavaScript using default JS engine (QuickJS), same as _script.js.quick (see table below|
**_script.ruby**|Ruby script|
**_script.tcl**|TCL script|
**_script.vbscript**|VBScript (MS Engine)|

###JavaScript
function | language
--- | ---
**_script.javascript**|JavaScript using default JS engine (QuickJS), same as _script.js.quick|
_script.jscript|MS JavaScript engine, same as _script.js.jscript|
_script.js.jscript|MS JavaScript engine
_script.js.core|Apple's JavaScriptCore engine
_script.js.node|Node.JS engine
_script.js.nodestrict|Node.JS engine in strict mode
_script.js.quick|QuickJS engine
_script.js.spider|Mozilla's SpiderMonkey engine
_script.js.v8|Pure V8 engine (built-in extension)
_script.js.v8ext|Pure V8 engine (external process)
_script.jspp.tiscript|JavaScript++ (TIScript), a superset of JavaScript

###Pascal
function | language
--- | ---
**_script.pascal**|DWS Pascal - Standard Script, same as pascalx.script
_script.pas.script|DWS Pascal - Standard Script
_script.pas.webscript|DWS Pascal - Web Script
_script.pas.prog|RemObjects Pascal - Program Script
_script.pas.prog|RemObjects Pascal - Function Script
_script.pas.short|RemObjects Pascal - Short Script

###Lua itself
function | language
--- | ---
_script.lua.script|Lua 5.1 (64-bit), same as _script.lua.v51
_script.lua.script32|Lua 5.1 (32-bit)
_script.lua.in|Lua 5.1 (built-in 64-bit Lua, same Lua state)
_script.lua.jit|LuaJIT 5.1 (64-bit)
_script.lua.v51|LuaJIT 5.1 (64-bit)
_script.lua.v52|LuaJIT 5.2 (64-bit)
_script.lua.v53|LuaJIT 5.3 (64-bit)
_script.lua.v54|LuaJIT 5.4 (64-bit)

**Case Sensitiveness in Pascal and VBScript** - PascalScript and VBScript are case-insensitive. When using them avoid using Lua variables with the same name but different case.

## Directories

* `/docs` - Documentation about the library's functions and options
* `/src` - the main executable source and built-in resource files
 * `/luaonlua` - Lua on Lua (LoL) library
 * `/thirdparty` - Scripting-related third-party dependencies
 
## Download

Compiled binaries for Windows can be downloaded from the links below.

* [Underscript 64-bit](https://www.syhunt.com/pub/downloads/underscript-2.0.0.exe) (preview)

Third-party libraries already included:
* PHP, freely available from http://www.php.net/
* Ruby, freely available from http://www.ruby-lang.org
* PascalScript, freely available from http://www.RemObjects.com/

Not included - must be downloaded separately:
* ActivePerl (optional for running Perl scripts) available from http://www.activestate.com/activeperl
* Python (optional for running Python scripts) freely available from http://python.org/

## Compiling

For compiling Underscript, you will need [Catarinka](https://github.com/felipedaragon/catarinka) and [pLua](https://github.com/felipedaragon/pLua-XE).
 
There is no need to install third-party components in the IDE - you can just add the dependencies listed above to the library path and hit compile. It currently compiles under Delphi XE10 Tokyo or up.

We also would love to see a C port of this library.

## License & Credits

Underscript was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This code is licensed under a MIT license - see the LICENSE file for details.