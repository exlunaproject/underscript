## PHP Support

### Hello World Example

```lua
require "Underscript"

function echo(s) _script.php
 [[
 echo($s);
 ]]
end

echo("Hello World!")
```

### Function Example

```lua
require "Underscript"

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
```

### Include Example

myecho.php:
```php
<?
function myecho($s){
 echo $s;
}
?>
```

demo.lua:
```lua
require "Underscript"

function echo(s) _script.php
 [[
 include('myecho.php');
 myecho($s);
 ]]
end

echo("Hello World!")
```

### Custom PHP Functions
* **uconsole_write**(v)
Writes a variable to the console without creating a new line
* **uconsole_writeln**(v)
Writes a new line to the console