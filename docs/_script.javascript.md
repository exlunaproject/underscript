## JavaScript Support

You can run JS code through the following methods:

* _script.**javascript** - default JS engine (QuickJS engine)
* _script.**jscript** - MS JavaScript engine
* _script.js.**core** - Apple's JavaScriptCore engine
* _script.js.**node** or **nodestrict** - Node.JS
* _script.js.**quick** - QuickJS engine
* _script.js.**spider** - SpiderMonkey engine
* _script.js.**v8** - Pure V8 engine

### Hello World (Using NodeJS)

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.js.node
 [[
 console.log(s.toUpperCase()+'!');
 ]]
end

say('Hello World')
```

### Hello World (Using pure V8 engine)

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.js.v8
 [[
 print(s.toUpperCase()+'!');
 ]]
end

say('Hello World')
```

### Hello World (Using MS JScript)

```lua
require "Underscript"

-- Prints "Hello World!" in uppercase
function say(s) _script.jscript
 [[
 UConsole.WriteLn(s.toUpperCase()+'!');
 ]]
end

say('Hello World')
```

### Function Example (Using MS JScript)

```lua
require "Underscript"

function encode64(s) _script.jscript
 [[
 var keyStr = "ABCDEFGHIJKLMNOP" +
               "QRSTUVWXYZabcdef" +
               "ghijklmnopqrstuv" +
               "wxyz0123456789+/" +
               "=";
     var input = escape(s);
     var output = "";
     var chr1, chr2, chr3 = "";
     var enc1, enc2, enc3, enc4 = "";
     var i = 0;

     do {
        chr1 = input.charCodeAt(i++);
        chr2 = input.charCodeAt(i++);
        chr3 = input.charCodeAt(i++);

        enc1 = chr1 >> 2;
        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
        enc4 = chr3 & 63;

        if (isNaN(chr2)) {
           enc3 = enc4 = 64;
        } else if (isNaN(chr3)) {
           enc4 = 64;
        }

        output = output +
           keyStr.charAt(enc1) +
           keyStr.charAt(enc2) +
           keyStr.charAt(enc3) +
           keyStr.charAt(enc4);
        chr1 = chr2 = chr3 = "";
        enc1 = enc2 = enc3 = enc4 = "";
     } while (i < input.length);

  s = output;
 ]]
 return s
end
```

