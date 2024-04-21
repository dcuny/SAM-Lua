-- Simple wave file library
-- Based on Euphoria source code by Daryl Van Den Brink
-- vandenbrink@micronet.net.au


local SAMPLE_RATE = 44100

-- Functions to write binary file information


-- Write an integer out as 4 bytes
local function writeInt(fn,x)
  local b4=string.char(x%256) 
  x=(x-x%256)/256

  local b3=string.char(x%256) 
  x=(x-x%256)/256

  local b2=string.char(x%256) 
  x=(x-x%256)/256

  local b1=string.char(x%256) 
  x=(x-x%256)/256

  fn:write(b4,b3,b2,b1)
end


-- Write a short integer out as 2 bytes
local function writeShort(fn, x)
  local b4=string.char(x%256) 
  x=(x-x%256)/256
  local b3=string.char(x%256)
  fn:write(b4,b3)
end



-- Write an integer out as 3 bytes
local function write24Bits(fn,x)    
  local b3=string.char(x%256) 
  x=(x-x%256)/256

  local b2=string.char(x%256) 
  x=(x-x%256)/256

  local b1=string.char(x%256) 
  x=(x-x%256)/256

  fn:write(b3,b2,b1)
end




-- Given integer wavedata, write a monophonic .wav file out at the
-- requested bit resolution (8, 16, or 24 bit).
function writeWaveFile(filename, bits, wavedata)
  -- write wave data to a wave file
  -- return 0 if successful, -1 if the file couldn't be created

  local fn = io.open(filename, "wb")
  if fn == nil then
    return -1
  end

  local channels = 1    -- mono
  local sps = SAMPLE_RATE     -- cd sample rate
  -- local bits = 8        -- bits per sample: 8, 16 or 24
  local wavelen = #wavedata  -- size of data

  local block_align = channels * bits / 8
  local datasize = wavelen * block_align
  local filesize = datasize + 44

  -- RIFF tag
  fn:write("RIFF")

  -- filesize
  writeInt(fn, filesize-8)

  -- WAVE tag
  fn:write("WAVE");

  -- fmt tag
  fn:write("fmt ")
  writeInt(fn, 16)

  writeShort(fn, 1)        -- format tag
  writeShort(fn, channels) -- number of channels
  writeInt(fn, sps)  -- samples per second
  writeInt(fn, sps * block_align)    -- average bytes per second
  writeShort(fn, block_align) -- block align
  writeShort(fn, bits)        -- bits per sample

  -- data tag
  fn:write("data")
  writeInt(fn, datasize)


  -- what size bits?
  if bits == 8 then
    -- loop through the data
    for i=1,wavelen do
      -- change range from -1..1 to 0..255
      local x = math.floor(wavedata[i] * 127) + 128
      fn:write(string.char(x))
    end

  elseif bits == 16 then
    for i=1,wavelen do
      -- get a value

      local x = math.floor(wavedata[i] * 32767)

      -- convert to a positive value
      if x < 0 then
        x = x + 65536
      end

      -- write two bytes
      writeShort(fn, x)        

    end

  elseif bits == 24 then
    for i=1,wavelen do
      -- get a value
      local x = math.floor(wavedata[i] * 8388607)

      -- convert to a positive value
      if x < 0 then
        x = x + 16777216
      end

      -- write 3 bytes
      write24Bits(fn, x)

    end

  end

  io.close(fn)

end
