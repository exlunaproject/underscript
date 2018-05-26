## Python Support

### Hello World Example

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.python
[[
print s.upper()
]]
end

say('Hello World!')
```

Note: The Python installer must be downloaded separately from the [official site](http://python.org/).