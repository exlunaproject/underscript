## Perl Support

### Hello World Example (using embedded Strawberry Perl)

```lua
require "Underscript"

-- Prints "Hello World!"
function say(s) _script.perl
 [[
 print($s.'!');
 ]]
end

say('Hello World')
```

### Hello World Example (using ActivePerl)

**Note**: The ActivePerl installer must be downloaded separately from the [official site](http://www.activestate.com/activeperl).

```lua
require "Underscript"

-- Prints "Hello World!"
function say(s) _script.perlactive
 [[
 $Underscript->WriteLn($s.'!');
 ]]
end

say('Hello World')
```

