## Python Support

### Hello World Example (using embedded Python)

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

### Hello World Example (using environment's Python)

Note: The Python installer must be downloaded separately from the [official site](http://python.org/).

```lua
-- See above example 
function say(s) _script.alpha.pythonenv
-- ..
```