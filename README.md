# Underscript

Underscript is a Lua extension library that allows you to **run a variety of scripting languages from within Lua scripts**. Underscript's scripted operations can be isolated or "connected" with the variables from the Lua state of the host. 

Currently JavaScript, Pascal, Perl, PHP, Python, Ruby, TCL and VBScript are supported.

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

All global and local Lua variables (string, number, boolean and nil) are
automatically made accessible from within the code just like if they were part of the Lua script that is running it, as shown in the examples above. If you prefer, connected global or local variables can be disabled (see **/docs/options.md**)

**Case Sensitiveness in Pascal and VBScript** - PascalScript and VBScript are case-insensitive. When using them avoid using Lua variables with the same name but different case.

## Directories

* `/docs` - Documentation about the library's functions and options
* `/src` - the main executable source and built-in resource files
 * `/thirdparty` - Scripting-related third-party dependencies
 
## Download

Compiled binaries for Windows can be downloaded from the links below.

* [Huntpad 32-bit](http://www.syhunt.com/en/index.php?n=Tools.DownloadHuntpad) (included with it)
* [Huntpad 64-bit](http://www.syhunt.com/en/index.php?n=Tools.DownloadHuntpad) (coming soon)

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