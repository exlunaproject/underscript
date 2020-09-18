## Options

The options table can be used to enable or disable library features. The following options are available:

name | return type | description
--- | --- | ---
**modulename** | string | Gets or sets the module name. Default: Underscript
**redirectio** | boolean | Enables or disables the IO redirection. Disabled by default. When enabled, Underscript_write(v), Underscript_writeln(v) and Underscript_logerror(line,msg) may be called during a script execution.
**usevars** | boolean | Enables or disables the use of Lua variables (local or global). Enabled by default.
**useglobals** | boolean | Enables or disables the use of global Lua variables. Disabled by default. Requires usevars enabled.
**uselocals** | boolean | 	Enables or disables the use of local Lua variables. Enabled by default. Requires usevars enabled.

### Example

```lua
r = require "Underscript.Runner"
r.options.modulename = 'demo'
r.options.redirectio = true
```

After this, the following functions from your Lua script may be called by Underscript if required:

```lua
function demo_writeln(text)
 -- (...)
end

function demo_write(text)
 -- (...)
end

function demo_logerror(line,msg)
 -- (...)
end
```