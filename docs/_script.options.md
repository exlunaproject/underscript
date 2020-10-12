## Options

The options table can be used to enable or disable library features. The following options are available:

name | return type | description
--- | --- | ---
**modulename** | string | Gets or sets the module name. Default: UConsole
**redirectio** | boolean | Enables or disables the IO redirection. Disabled by default. When enabled, IO module and functions may be called during a script execution.
**handleerrors** | boolean | Enables or disables the use of Lua error handling. Enabled by default. If disabled, luaL_error() will not be called if a script error occurs.
**usevars** | boolean | Enables or disables the use of Lua variables (local or global). Enabled by default.
**useglobals** | boolean | Enables or disables the use of global Lua variables. Disabled by default. Requires usevars enabled.
**uselocals** | boolean | 	Enables or disables the use of local Lua variables. Enabled by default. Requires usevars enabled.

### IO Redirection Example

```lua
require "Underscript"
_script.redirectio = true
```

After this, the following functions from your Lua script will be called by Underscript when required and if these have been declared:

```lua
function uconsole.debug(text)
 -- (...)
end

function uconsole.writeln(text)
 -- (...)
end

function uconsole.write(text)
 -- (...)
end

function uconsole.errorln(line,msg)
 -- (...)
end
```