## Ruby Support

### Hello World

```lua
require "Underscript"

function puts(s) _script.ruby
 [[
 puts(s)
 ]]
end

puts("Hello World!")
```

### Function Example

```lua
require "Underscript"

function crc32(s) _script.ruby
 [[
 def rb_crc32(c) 
     r = 0xFFFFFFFF 
     c.each_byte do |b| 
         r ^= b 
         8.times do 
           r = (r>>1) ^ (0xEDB88320 * (r & 1)) 
         end 
     end 
     r ^ 0xFFFFFFFF 
 end
 s = rb_crc32(s)
 ]]
 return s
end
```