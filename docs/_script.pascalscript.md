## Pascal Support

### Example 1 - Hello World

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.pascalscript
 [[
  UConsole.WriteLn(Uppercase(Trim(s)));
 ]]
end

say(" Hello World! ")
```

### Example 2 - Pascal Program

```lua
require "Underscript"

function printcrc16() _script.pascal.prog
 [[
const DivPol = $1021;
const Amount = 20000;

function CRC16(s:string):Word; 
var 
    CRC:Word; 
    n,Bit:Byte; 
begin 
      CRC:=0; 
      for n:=1 to Length(s) do 
      begin 
           CRC:=CRC xor (ord(s[n]) shl 8); 
           for bit:=0 to 7 do 
           begin 
                if CRC and $8000 <> 0 then 
                   CRC:=(CRC shl 1) xor DivPol 
                else 
                   CRC:=CRC shl 1; 
           end; 
      end; 
      result:=CRC; 
end;

begin
 UConsole.WriteLn(IntToStr(CRC16('astring')));
end.
]]
end
```

**Note**: Pascal is case-insensitive - be careful to not use Lua variables with the same name and different case.

### Credits

Pascal support made possible by the [RemObjects Pascal Script](http://www.remobjects.com/) and DWS projects.