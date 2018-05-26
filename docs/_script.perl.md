## Perl Support

## Hello World Example

```lua
require "Underscript"

-- Prints "Hello World!"
function say(s) _script.perl
 [[
 $Underscript->WriteLn($s.'!');
 ]]
end

say('Hello World')
```

**Note**: The ActivePerl installer must be downloaded separately from the [official site](http://www.activestate.com/activeperl).