component = require("component")
computer = require("computer")
event = require("event")
term = require("term")
shell = require("shell")
unicode = require("unicode")
serialization = require("serialization")
internet = require("internet")
serialization = require("serialization")
os = require("os")
sleep = os.sleep
abs = math.abs
gpu = component.gpu

-----------------Настройки--------------------

event.shouldInterrupt = function () return false end -- Отключение ctrl + shift + c as interupted

local coolDownUpdateSleep = 1000 -- CoolDown update timer online status
local listxCordToDraw = 20 -- С какой левой координаты начинать рисовать список с админским составом
local xCordToDrawNickNameToPlus = 18 -- Сколько приплюсовать к базовой координате listxCordToDraw для отрисовки ник нейма
------ Список тех кто может что либо изменять в ПУ. ------
        ---ВНИМАНИЕ!!!--- Менять ники во всех полях.
adminListAccos = { ["3_1415926535"] = "3_1415926535", ["myself"] = "myself",
    ["Kristallik__"] = "Kristallik__",
    ["oorr"] = "oorr",
    ["DrZip"] = "DrZip",
    ["Francuzz214"] = "Francuzz214",
    ["ValkyrieFX"] = "ValkyrieFX",
    ["busa"] = "busa" }
adminListNoAccos = { "3_1415926535", "myself", "Kristallik__", "oorr", "DrZip", "Francuzz214", "ValkyrieFX", "busa"}

------ Список тех кто может что либо изменять в ПУ. ------ 

local githubApiToken = "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"
local githubOwner = "Kristallik1785"
local githubRepo = "OpenAdmins"
local githubFileName = "data" --ОЧЕНЬ ВАЖНО, на github создать этот файл руками, почему? Данная программа не умеет создавать этот файл на github автоматически. Если не создашь, будет ошибка загрузки данных на github
local githubDoneApiLink = "https://api.github.com/repos/"..githubOwner.."/"..githubRepo.."/contents/"..githubFileName

--------------------Настройки--------------------


---- from /lib/pastebin со своими доработками ---
if not component.isAvailable("internet") then
    io.stderr:write("Для работы этой программы требуется интернет плата")
    return
end

function findFilesystem()
    for address in component.list("filesystem") do
        if address ~= computer.tmpAddress() and not component.invoke(address, "isReadOnly") then
            filesystem = component.proxy(address)
            return true
        end
    end
    if not filesystem then
        error("Component FileSystem not found!")
    end
end

findFilesystem()
writeFS = function(path, mode, data)
    local handle = filesystem.open(path, mode)
    filesystem.write(handle, data)
    filesystem.close(handle)
end

readFS = function(path)
    local error, jsonStringFile = pcall(function() return assert(io.open(path)) end)
    if not error then return nil end 
    return jsonStringFile:read("*all")
end

---------------------------------Минифицированная json lib взято от сюда https://raw.githubusercontent.com/rxi/json.lua/master/json.lua --------------------------------
local a={_version="0.1.2"}local b;local c={["\\"]="\\",["\""]="\"",["\b"]="b",["\f"]="f",["\n"]="n",["\r"]="r",["\t"]="t"}local d={["/"]="/"}for e,f in pairs(c)do d[f]=e end;local function g(h)return"\\"..(c[h]or string.format("u%04x",h:byte()))end;local function i(j)return"null"end;local function k(j,l)local m={}l=l or{}if l[j]then error("circular reference")end;l[j]=true;if rawget(j,1)~=nil or next(j)==nil then local n=0;for e in pairs(j)do if type(e)~="number"then error("invalid table: mixed or invalid key types")end;n=n+1 end;if n~=#j then error("invalid table: sparse array")end;for o,f in ipairs(j)do table.insert(m,b(f,l))end;l[j]=nil;return"["..table.concat(m,",").."]"else for e,f in pairs(j)do if type(e)~="string"then error("invalid table: mixed or invalid key types")end;table.insert(m,b(e,l)..":"..b(f,l))end;l[j]=nil;return"{"..table.concat(m,",").."}"end end;local function p(j)return'"'..j:gsub('[%z\1-\31\\"]',g)..'"'end;local function q(j)if j~=j or j<=-math.huge or j>=math.huge then error("unexpected number value '"..tostring(j).."'")end;return string.format("%.14g",j)end;local r={["nil"]=i,["table"]=k,["string"]=p,["number"]=q,["boolean"]=tostring}b=function(j,l)local s=type(j)local t=r[s]if t then return t(j,l)end;error("unexpected type '"..s.."'")end;function a.encode(j)return b(j)end;local u;local function v(...)local m={}for o=1,select("#",...)do m[select(o,...)]=true end;return m end;local w=v(" ","\t","\r","\n")local x=v(" ","\t","\r","\n","]","}",",")local y=v("\\","/",'"',"b","f","n","r","t","u")local z=v("true","false","null")local A={["true"]=true,["false"]=false,["null"]=nil}local function B(C,D,E,F)for o=D,#C do if E[C:sub(o,o)]~=F then return o end end;return#C+1 end;local function G(C,D,H)local I=1;local J=1;for o=1,D-1 do J=J+1;if C:sub(o,o)=="\n"then I=I+1;J=1 end end;error(string.format("%s at line %d col %d",H,I,J))end;local function K(n)local t=math.floor;if n<=0x7f then return string.char(n)elseif n<=0x7ff then return string.char(t(n/64)+192,n%64+128)elseif n<=0xffff then return string.char(t(n/4096)+224,t(n%4096/64)+128,n%64+128)elseif n<=0x10ffff then return string.char(t(n/262144)+240,t(n%262144/4096)+128,t(n%4096/64)+128,n%64+128)end;error(string.format("invalid unicode codepoint '%x'",n))end;local function L(M)local N=tonumber(M:sub(1,4),16)local O=tonumber(M:sub(7,10),16)if O then return K((N-0xd800)*0x400+O-0xdc00+0x10000)else return K(N)end end;local function P(C,o)local m=""local Q=o+1;local e=Q;while Q<=#C do local R=C:byte(Q)if R<32 then G(C,Q,"control character in string")elseif R==92 then m=m..C:sub(e,Q-1)Q=Q+1;local h=C:sub(Q,Q)if h=="u"then local S=C:match("^[dD][89aAbB]%x%x\\u%x%x%x%x",Q+1)or C:match("^%x%x%x%x",Q+1)or G(C,Q-1,"invalid unicode escape in string")m=m..L(S)Q=Q+#S else if not y[h]then G(C,Q-1,"invalid escape char '"..h.."' in string")end;m=m..d[h]end;e=Q+1 elseif R==34 then m=m..C:sub(e,Q-1)return m,Q+1 end;Q=Q+1 end;G(C,o,"expected closing quote for string")end;local function T(C,o)local R=B(C,o,x)local M=C:sub(o,R-1)local n=tonumber(M)if not n then G(C,o,"invalid number '"..M.."'")end;return n,R end;local function U(C,o)local R=B(C,o,x)local V=C:sub(o,R-1)if not z[V]then G(C,o,"invalid literal '"..V.."'")end;return A[V],R end;local function W(C,o)local m={}local n=1;o=o+1;while 1 do local R;o=B(C,o,w,true)if C:sub(o,o)=="]"then o=o+1;break end;R,o=u(C,o)m[n]=R;n=n+1;o=B(C,o,w,true)local X=C:sub(o,o)o=o+1;if X=="]"then break end;if X~=","then G(C,o,"expected ']' or ','")end end;return m,o end;local function Y(C,o)local m={}o=o+1;while 1 do local Z,j;o=B(C,o,w,true)if C:sub(o,o)=="}"then o=o+1;break end;if C:sub(o,o)~='"'then G(C,o,"expected string for key")end;Z,o=u(C,o)o=B(C,o,w,true)if C:sub(o,o)~=":"then G(C,o,"expected ':' after key")end;o=B(C,o+1,w,true)j,o=u(C,o)m[Z]=j;o=B(C,o,w,true)local X=C:sub(o,o)o=o+1;if X=="}"then break end;if X~=","then G(C,o,"expected '}' or ','")end end;return m,o end;local _={['"']=P,["0"]=T,["1"]=T,["2"]=T,["3"]=T,["4"]=T,["5"]=T,["6"]=T,["7"]=T,["8"]=T,["9"]=T,["-"]=T,["t"]=U,["f"]=U,["n"]=U,["["]=W,["{"]=Y}u=function(C,D)local X=C:sub(D,D)local t=_[X]if t then return t(C,D)end;G(C,D,"unexpected character '"..X.."'")end;function a.decode(C)if type(C)~="string"then error("expected argument of type string, got "..type(C))end;local m,D=u(C,B(C,1,w,true))D=B(C,D,w,true)if D<=#C then G(C,D,"trailing garbage")end;return m end
---------------------------------Минифицированная json lib взято от сюда https://raw.githubusercontent.com/rxi/json.lua/master/json.lua --------------------------------
    
json = a

function uploadOnGithub(jsonStringToSend)

  gpuFill(2, 3, resW - 2, 1, " ", greyColors[1], greyColors[1])
  gpuSet(nil, 3, "Загрузка данных на github", colorsTable[1], 0x399999)

    function request()
        local handle, data, chunk = internet.request(githubDoneApiLink), ""

        while true do
            chunk = handle.read(math.huge)

            if chunk then
                data = data .. chunk
            else
                break
            end
        end
        if not data then return false, "Не удачная попытка получить sha" end
    
        handle, data, chunk = internet.request(
            githubDoneApiLink,
            '{"message": "txt file", "content": "'..dataCard.encode64(jsonStringToSend)..'", "sha":"'..json.decode(data)["sha"]..'"}',
            { ['Authorization'] = "Bearer " .. githubApiToken }, "PUT"), ""

        while true do
            chunk = handle.read(math.huge)

            if chunk then
                data = data .. chunk
            else
                break
            end
        end

        if not data then return false, "Не удачная попытка обновления файла конфигурации в github-e" end

        return true, data
    end

    local result, response = request()

  if result and type(response) ~= "nil" then
     return true, "https://github.com/"..githubOwner.."/"..githubRepo.."/blob/main/"..githubFileName
  else
    return false, tostring(response)
  end
end


if not component.isAvailable("data") or not pcall(function() component.data.random(100) end) then error("Компонент карта данных 2-го уровня или уровнем выше, не найден! Установи его во внутрений слот компьютера") end
if not component.isAvailable("redstone") or not pcall(function() component.data.random(100) end) then error("Компонент красный контроллер не найден рядом с компьютером! Он нужен для автозапуска и автозагрузки программы, при смерти самого комптьюера, установи этот блок в плотную к системному блоку!") end

component.redstone.setWakeThreshold(15)
component.redstone.setWakeThreshold(15)
component.redstone.setWakeThreshold(15)
dataCard = component.data


for admin = 1, #adminListNoAccos do
    computer.addUser(adminListNoAccos[admin])
end
function is_admin(nickName)
    for i = 1, #adminListNoAccos do
        if adminListNoAccos[i] == nickName then return true end 
    end
    return false
end


gpu.setResolution(146, 46)

resW, resH = gpu.getResolution()

function clear() gpu.fill(1, 1, resW, resH, " ") end

borderLineOne = { "┌", "─", "┐", "└", "│", "┘" }

local genderTypeTable = {
{["genderType"] = "&bМужчина"},
{["genderType"] = "&bЖенщина"}
}

usersCompoundList = {} 
groupsList = {}

colorsTable = {
    ["0"] = 0x333333, 
    ["1"] = 0x0000ff, 
    ["2"] = 0x00ff00, 
    ["3"] = 0x24b3a7, 
    ["4"] = 0xff0000, 
    ["5"] = 0x8b00ff, 
    ["6"] = 0xffa500, 
    ["7"] = 0xbbbbbb, 
    ["8"] = 0x808080, 
    ["9"] = 0x996d00, 
    ["a"] = 0x66ff66, 
    ["b"] = 0x00ffff, 
    ["c"] = 0xff6347,
    ["d"] = 0xff00ff,
    ["e"] = 0xffff00,
    ["f"] = 0xffffff,
    ["k"] = 0x0d6efd
    
}

greyColors = { --grey colors
      [1] = 0x0f0f0f, [9]  = 0x878787,
      [2] = 0x1e1e1e, [10] = 0x969696,  
      [3] = 0x2d2d2d, [11] = 0xa5a5a5,
      [4] = 0x3c3c3c, [12] = 0xb4b4b4,
      [5] = 0x4b4b4b, [13] = 0xc3c3c3,
      [6] = 0x5a5a5a, [14] = 0xd2d2d2,
      [7] = 0x696969, [15] = 0xe1e1e1,
      [8] = 0x787878, [16] = 0xf0f0f0,
}

availableColorsAsString = "[0xc3c3c3]Список доступных цветов: [0x333333]&0[0x3c3c3c]; [0x0000ff]&1[0x3c3c3c]; [0x00ff00]&2[0x3c3c3c]; [0x24b3a7]&3[0x3c3c3c]; [0xff0000]&4[0x3c3c3c]; [0x8b00ff]&5[0x3c3c3c]; [0xffa500]&6[0x3c3c3c]; [0xbbbbbb]&7[0x3c3c3c]; [0x808080]&8[0x3c3c3c]; [0x0000ff]&9[0x3c3c3c]; [0x66ff66]&a[0x3c3c3c]; [0x00ffff]&b[0x3c3c3c]; [0xff6347]&c[0x3c3c3c]; [0xff00ff]&d[0x3c3c3c]; [0xffff00]&e[0x3c3c3c]; [0xffffff]&f[0x3c3c3c];"

signalHandler = function()
    local singal_one, signal_two, signal_three, signal_four, signal_five, signal_six = computer.pullSignal(0)
    local signal = {[1] = singal_one, [2] = signal_two, [3] = signal_three, [4] = signal_four, [5] = signal_five, [6] = signal_six}
    if signal[1] == "fakeEvent" then 
           local singal_one, signal_two, signal_three, signal_four, signal_five, signal_six = computer.pullSignal(0)
        signal = {[1] = singal_one, [2] = signal_two, [3] = signal_three, [4] = signal_four, [5] = signal_five, [6] = signal_six}
    end
    if signal  then 
        if is_admin(tostring(signal[5])) or is_admin(tostring(signal[6])) or is_admin(tostring(signal[4])) then
            return signal
        end
    end
    return {[0] = signal[0], [1] = "none", [2] = "", [3] = 0, [4] = 0, [5] = 0, [6] = 0}
end

function tableCopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[tableCopy(orig_key)] = tableCopy(orig_value)
        end
        setmetatable(copy, tableCopy(getmetatable(orig)))
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

function setForeGroundColor(colorIndex)
    gpu.setForeground(colorsTable[colorIndex] == nil and 0xff0000 or colorsTable[colorIndex])
end 
 
gpuFill = function (x, y, w, h, symbol, background, foreground)
    if background ~= nil then
        gpu.setBackground(background)
    end

    if foreground ~= nil then
        gpu.setForeground(foreground)
    end

    gpu.fill(x, y, w, h, symbol)
end


guiFillColorBackground = function(colorToFeelBgrn)
    gpuFill(1, 1, resW, resH, " ", colorToFeelBgrn)
end


gpuSet = function (x, y, str, background, foreground)
    if background ~= nil then
        gpu.setBackground(background)
    end

    if foreground ~= nil then
        gpu.setForeground(foreground)
    end

    gpu.set(x or math.floor(math.abs(resW / 2 + 1 - unicode.len(str) / 2)), y, str)
end

gpuSetColorText = function(x, y, str, background)
    gpu.setBackground(background)
    if not x then
        x = math.floor(math.abs(resW / 2 + 1 - unicode.len(str:gsub("%[%w+]", "")) / 2))
    end

    local begin = 1

    while true do
        local b, e, color = str:find('%[0x(%x%x%x%x%x%x)]', begin)
        local precedingString = str:sub(begin, b and (b - 1))

        if precedingString then
            gpu.set(x, y, precedingString)
            x = x + unicode.len(precedingString)
        end

        if not color then
            break
        end

        gpu.setForeground(tonumber(color, 16))
        begin = e + 1
    end
end

function drawRoundFrame(backgroundColor, foregroundColor, copyrightDrawStatus)
    ------TOP------
        gpuFill(1,1,resW, 1, "▀", backgroundColor, foregroundColor)
    ------TOP------


    ------BOTTOM------
        gpuFill(1, resH, resW, 1, "▄", backgroundColor, foregroundColor)
    ------------------
    if copyrightDrawStatus then
    ------copyright BOTTOM------
    gpuSet(abs(resW - 30), resH, " Автор: DraconicTech ", backgroundColor, foregroundColor)
    ---------------------------
    end
    ------LEFT------
        gpuFill(1,1,1, resW, "█", backgroundColor, foregroundColor)
    ------------------

    ------RIGHT------
        gpuFill(resW, 1, 1, resW, "█", backgroundColor, foregroundColor)
    -----------------

end

button = {}

function button:new(tbl, x, y, buttonWidth, buttonHeight, textOnButton, backgroundToFill, foreground, eventToFunctionExec)
    -- свойства
    local obj= {}
    obj.x = x == nil and abs(resW / 2 - buttonWidth / 2) or x 
    obj.y = y
    obj.buttonHeight = buttonHeight
    obj.textOnButton = textOnButton
    obj.lenTextOnButton = unicode.len(tostring(obj.textOnButton))
    obj.buttonWidth = buttonWidth == nil and obj.lenTextOnButton or buttonWidth
    obj.backgroundToFill = backgroundToFill
    obj.foreground = foreground
    obj.eventTrapSignalMath = {["touch"] = {["xWithButtonWidth"] = x == nil and abs(resW / 2 - (obj.buttonWidth / 2) + obj.buttonWidth - 1) or abs(obj.x + obj.buttonWidth - 1),
                                            ["yWithButtonHeight"] = abs((obj.y + obj.buttonHeight ) - 1)}}
    obj.xCordTextOnButton = x == nil and math.floor(obj.x + (obj.buttonWidth / 2) - obj.lenTextOnButton / 2) or 
                                       math.floor(obj.x + (obj.buttonWidth / 2) - obj.lenTextOnButton / 2)
                                       obj.eventToFunctionExec = eventToFunctionExec
    function obj:getTextOnButton()
        return self.textOnButton 
    end

    function obj:draw()
        gpuFill(self.x, self.y, self.buttonWidth, self.buttonHeight, " ", self.backgroundToFill, self.foreground)
        gpuSet(self.xCordTextOnButton, abs(self.y + self.buttonHeight / 2), self.textOnButton, self.backgroundToFill, self.foreground)
    end

    function obj:eventTrap(signal)
        if signal[1] == "touch" and signal[3] >= self.x and signal[3] <= self.eventTrapSignalMath["touch"]["xWithButtonWidth"] 
        and signal[4] >= self.y and signal[4] <= self.eventTrapSignalMath["touch"]["yWithButtonHeight"] then
            return true
        end
        return false
    end

    function obj:onEvent(signal)
        if self:eventTrap(signal) then
            local result = eventToFunctionExec == nil and true or eventToFunctionExec()
            return result
        end
        return false
    end

    function obj:reDrawTextOnButton(foreground)
        gpuSet(self.xCordTextOnButton, abs(self.y + self.buttonHeight / 2), self.textOnButton, self.backgroundToFill, foreground)
    end

    function obj:destroy(fillColorBackground)
        gpuFill(self.x, self.y, self.buttonWidth, self.buttonHeight, " ", fillColorBackground)
        self = {}
    end

    obj:draw()
    setmetatable(obj, self)
    self.__index = self; 
    return obj
end


fieldSymbolInput = {}

function fieldSymbolInput:new(x, y, lengthField, cursorSymbol, customInitTextOnField, canInputOnlyNumbers)
    local obj = {}
    obj.onFocusedStatus = false
    obj.debugStatus = false
    obj.x = x
    obj.y = y
    obj.lengthField = lengthField
    obj.cursorSymbol = cursorSymbol
    obj.savedText = ""
    obj.xPosC = obj.x + 1
    obj.signalHandler = signalHandler
    obj.canFocused = false
    obj.canInputOnlyNumbers = canInputOnlyNumbers == nil and false or canInputOnlyNumbers
    
    obj.backgroundColor = 0xb4b4b4

    obj.customInitTextOnField = tostring(customInitTextOnField)

    local privateVariable = {}

    privateVariable.xPosC = obj.x + 1
    privateVariable.xWithLengthField = obj.x + obj.lengthField

    function obj:changeOnFocusedStatus(status)
        self.onFocusedStatus = status
    end

    function obj:dropFocused(signal)
        if signal[3] >= 2 + privateVariable.xWithLengthField and signal[3] <= 4 + privateVariable.xWithLengthField then
            self.onFocusedStatus = false
        end
    end

    function obj:drawCrossButton()
        -----сивол X на кнопке стирания-------- 0x343a40
        gpuSet(self.x + self.lengthField + 2, self.y, " × ", 0x343a40, 0x0d6efd)
        -----сивол X на кнопке стирания-------
    end
    function obj:drawInit()
        --------------
        gpuFill(self.x, self.y, 2 + self.lengthField, 1, " ", self.backgroundColor)
        gpuSet(1 + self.x, self.y, self.customInitTextOnField, self.backgroundColor, 0x212529)
        ---------------0x212529
        self:drawCrossButton()
    end

    obj:drawInit()


    function obj:customMatch(symbol)
        if not self.canInputOnlyNumbers then
            local result = symbol:match("^[A-Za-z0-9А-Я-а-я -_#&]")
            return result == nil and (symbol == "]" and symbol or (symbol == "[" and symbol or (symbol == "-" and symbol or nil))) or result
        end
        if tonumber(self.savedText == "" and 0 or self.savedText) < 100 then
            return symbol:match("^[0-9]")
        end
        return nil
    end

    function obj:blinkCrossButton()
        gpuSet((3) + (self.lengthField) + self.x,  self.y, "×", 0x343a40, 0x00b600)
        sleep(0.001)
        gpuSet((3) + (self.lengthField) + self.x,  self.y, "×", 0x343a40, 0x0d6efd)
    end

    function obj:drawCursor()
        gpu.setBackground(self.backgroundColor)
        gpu.setForeground(0xffc107)
        gpu.set(self.xPosC, self.y, self.cursorSymbol)
    end

    function obj:blinkCursor()
        gpu.setBackground(self.backgroundColor)
        gpu.setForeground(0x00b600)
        gpu.set(self.xPosC, self.y, self.cursorSymbol)
        sleep(0.1)
        gpu.setForeground(0xffc107)
        gpu.set(self.xPosC, self.y, self.cursorSymbol)
    end
    
    function obj:reDrawText(text)
        gpuFill(self.x, self.y, 2 + self.lengthField, 1, " ", self.backgroundColor)
        gpuSet(self.x + 1, self.y, tostring(text), self.backgroundColor, 0x212529)
    end

    function obj:modifySavedTextAndPreCalcCursor(text)
        self.savedText = text
        self.xPosC = self.x + unicode.len(self.savedText) + 1
        self:reDrawText(self.savedText)
    end

    function obj:reDrawTextWithCursor(text)
        gpuFill(self.x, self.y, 2 + self.lengthField, 1, " ", self.backgroundColor)
        gpuSet(self.x + 1, self.y, tostring(text), self.backgroundColor, 0x212529)
        self:drawCursor()
    end

    function obj:reDrawTextWithCursorAndCross(text)
        self:reDrawTextWithCursor(text)
        self:drawCrossButton()
    end

    function obj:lenSavedText()
        return unicode.len(self.savedText)
    end

    function obj:eventTrap(signal)
        if signal[1] == "touch" then
            if signal[3] >= 2 + privateVariable.xWithLengthField and signal[3] <= 4 + privateVariable.xWithLengthField then --прожал X, отчистить
                obj:blinkCrossButton()
                self.xPosC = self.x + 1
                self.savedText = ""
                self:reDrawTextWithCursor("")
                obj:debug(signal)
                -- return "erase"
            end
            if signal[3] <= self.lengthField and signal[3] >= 4 + privateVariable.xWithLengthField or signal[4] ~= self.y then 
                return "focusedDroped"
            end
        elseif signal[1] == "key_up" and signal[1] ~= "key_down" then
            local char, sChar = signal[3], signal[4]
            local TextLen = self:lenSavedText()
            local symbol = unicode.char(char)
            if signal[3] == 127 or signal[4] == 211 then --прожал delete, отчистить
                self:blinkCrossButton()
                self.xPosC = self.x + 1
                self.savedText = ""
                self:reDrawTextWithCursor("")
                obj:debug(signal)
                -- return "erase"
            end
            if self:customMatch(symbol) ~= nil and TextLen ~= self.lengthField then
                if self.xPosC == 1 + self.x then -- пишу символ в самом конце строки
                    local afterCur = unicode.sub(self.savedText, 1 + self.x - self.xPosC, TextLen)
                    self.xPosC = self.xPosC + 1
                    self.savedText = symbol .. afterCur
                    self:reDrawTextWithCursor(self.savedText)
                    return true

                elseif self.xPosC ~= self.x and 1 + self.xPosC >= 1 + self.x + unicode.len(self.savedText) then -- Проверка, находится ли курсор в начале строки и символ текст
                    self.xPosC = self.xPosC + 1
                    self.savedText = self.savedText .. unicode.char(signal[3])
                    self:reDrawTextWithCursor(self.savedText)
                    return true

                elseif self.xPosC >= 1 + self.x and self.xPosC <= 1 + self.x + TextLen then -- пишу символ в центре строки                    
                    self.savedText = unicode.sub(self.savedText, 1, self.xPosC - 1 - self.x) .. symbol .. unicode.sub(self.savedText, self.xPosC - self.x, TextLen)
                    self.xPosC = self.xPosC + 1
                    self:reDrawTextWithCursor(self.savedText)
                    return true
                end
            end
            if signal[1] == "key_up" and signal[4] == 28 then
                return "focusedDroped"
            end
        elseif signal[1] ~= "key_up" and signal[1] == "key_down" then
            if signal[4] == 205 and self.xPosC <= self:lenSavedText() + self.x and self.xPosC ~= 1 + self.x + self.lengthField then -- 205 стрелка в право
                self.xPosC = self.xPosC + 1 -- Стрелка в право, курсор двигается в право.
                self:reDrawTextWithCursor(self.savedText)
            elseif signal[4] == 203 and self.xPosC >= 2 + self.x then -- 203 стрелка в лево
                self.xPosC = self.xPosC - 1 -- Стрелка в лево, курсор двигается в лево.
                self:reDrawTextWithCursor(self.savedText)
            end
            if signal[3] and signal[4] == 14 then --Стирал очка
                if self.xPosC >= 1 + self.x then 
                    local TextLen = self:lenSavedText()
                    if self.xPosC == 1 + self.x + TextLen and TextLen > 0 then
                        if self.xPosC ~= 1 + self.x then self.xPosC = self.xPosC - 1 end
                            self.savedText = unicode.sub(self.savedText, 1, TextLen - 1)
                            self:reDrawTextWithCursor(self.savedText)
                            if self.savedText == "" then return "erase" end
                            return true

                        elseif self.xPosC >= 3 + self.x and self.xPosC <= self.x + TextLen then
                            self.savedText = unicode.sub(self.savedText, 1, self.xPosC - 2 - self.x)..unicode.sub(self.savedText, self.xPosC - self.x, TextLen)
                            self.xPosC = self.xPosC - 1
                            self:reDrawTextWithCursor(self.savedText)
                            if self.savedText == "" then return "erase" end
                            return true

                        elseif self.xPosC == 2 + self.x then
                            self.savedText = unicode.sub(self.savedText, 2, TextLen)
                            self.xPosC = self.xPosC - 1
                            self:reDrawTextWithCursor(self.savedText)
                            return true
                    end
                    return false
                end
            end
        elseif signal[1] == "clipboard" then
            local stringGeted = tostring(signal[3])
            local stringToPaste = ""
            if unicode.len(stringGeted) > self.lengthField then stringToPaste = unicode.sub(stringGeted, 1, self.lengthField) else stringToPaste = stringGeted end
            self.savedText = stringToPaste
            self:modifySavedTextAndPreCalcCursor(self.savedText)
        end
    end

    function obj:canFocusedToggle(bool)
        self.canFocused = bool
    end

    function obj:isFocused(signal)
        if signal and signal[1] == "touch" and signal[3] >= self.x and signal[3] <= self.x + self.lengthField and signal[4] == self.y then
            return true
        end
        return false
    end

    function obj:isDropFocused(signal)
        if self:eventTrap(signal) == "focusedDroped" then
            return true
        end
        return false
    end
    function obj:onFocused(_SIGNAL, errorText)
        if self:isFocused(_SIGNAL) then
            if self.canFocused then
                self.backgroundColor = 0xf8f9fa
                self:reDrawTextWithCursor(self.savedText)
                local blinkCursorCount = 0
                while true do
                    computer.pushSignal("fakeEvent")
                    blinkCursorCount = blinkCursorCount + 1
                    if blinkCursorCount == 50 then self:blinkCursor() blinkCursorCount = 0 end
                    if self:isDropFocused(self:signalHandler()) then
                        self.backgroundColor = 0xb4b4b4
                        self:reDrawText(tostring(self.savedText))
                        return "focusedDroped"
                    end
                end
            else
                self.backgroundColor = 0xf8f9fa
                self:reDrawText(tostring(errorText)) --"НЕ ВЫБРАЛ ГРУППУ!"
                computer.beep(600, 0.1)
                self.backgroundColor = 0xb4b4b4
                self:reDrawText(tostring(self.customInitTextOnField))
                return
            end
        end
    end


    function obj:debug(signal)
        -- if self.debugStatus then
        --     local x = 41
        --     gpuFill(abs(x- 1), 1, 50, 8, " ", 0xf8f9fa)
        --     gpuSet(x, 2, tostring(signal[3]), color.white, color.yellow)
        --     gpuSet(x, 3, tostring(signal[4]), color.white, color.yellow)
        --     gpuSet(x, 4, "xPosC: " .. tostring(self.xPosC), color.background, color.yellow)
        --     gpuSet(x, 5, "TextLen: " .. tostring(unicode.len(self.savedText)), color.background, color.red)
        --     gpuSet(x, 6, "savedText: " .. self.savedText, color.background, color.red)
        -- end
    end

    setmetatable(obj, self)
    self.__index = self
    return obj
end


scrollTableElementAsStringField = {}

function scrollTableElementAsStringField:new(x, y, fieldWidth, tableToUse, elementIndexSelected, elementFieldName, background, foregroundText)

    local obj = {}
    obj.signalHandler = signalHandler

    obj.x = x
    obj.y = y
    obj.fieldWidth = fieldWidth
    obj.tableToUse = tableToUse
    obj.background = background
    obj.foregroundText = foregroundText

    obj.elementIndexSelected = elementIndexSelected
    obj.elementFieldName = elementFieldName

    obj.defaultText = tostring(tableToUse[1] == nil and "&kДанные отсутствуют" or tableToUse[1][elementFieldName]) -- "Не выбрано"
    obj.leftButton = button:new(nil, abs(x - 4), y, 3, 1, "<", greyColors[4], 0x0d6efd, function()  return true end)
    obj.rightButton = button:new(nil, abs(x + fieldWidth + 4), y, 3, 1, ">", greyColors[4], 0x0d6efd, function() return true end)

    function obj:drawInit()
        gpuFill(abs(self.x - 1), self.y, self.fieldWidth + 5,1, " ", self.background)
        drawColoredText(self.x, self.y, self.background, self.defaultText)
    end
    obj:drawInit()

    function obj:toggleElementIndexSelected(index)
        self.elementIndexSelected = index
    end

    function obj:modifyDefaultText(text)
        self.defaultText = text
    end

    function obj:drawSelectedItem(text)
        gpuFill(abs(self.x - 1), self.y, self.fieldWidth + 5,1, " ", self.background)
        drawColoredText(self.x, self.y, self.background, text)
    end

    function obj:eventTrap(signal)
        if tableToUse[1] ~= nil then
            if self.leftButton:onEvent(signal) then
                if self.elementIndexSelected > 1 then
                    self.elementIndexSelected = self.elementIndexSelected - 1
                    self:drawSelectedItem(tableToUse[self.elementIndexSelected][elementFieldName])
                    return "leftButtonTouched"
                end
            elseif self.rightButton:onEvent(signal) then
                if self.elementIndexSelected < #self.tableToUse then
                    self.elementIndexSelected = self.elementIndexSelected + 1
                    self:drawSelectedItem(tableToUse[self.elementIndexSelected][elementFieldName])
                    return "rightButtonTouched"
                end
            end
        end
        return false
    end

    function obj:onEvent(signal, elementIndexSelected)
        self.elementIndexSelected = elementIndexSelected
        return self:eventTrap(signal)
    end 

    setmetatable(obj, self)
    self.__index = self

    return obj
end



printTable = function (t)
    clear()

    guiFillColorBackground(0x0f0f0f)
    local step = 1
    local printTable_cache = {}
 
    local function sub_printTable( t, indent )
        
        if ( printTable_cache[tostring(t)] ) then
            gpuSet(3, step, indent .. "*" .. tostring(t), 0x1e1e1e, 0xffa500)
        else
            printTable_cache[tostring(t)] = true
            if ( type( t ) == "table" ) then
                for pos, val in pairs( t ) do
                    step = step + 1
                    if ( type(val) == "table" ) then
                    gpuSet(3, step,  indent .. "[" .. pos .. "] => " .. tostring( t ).. " {" , 0x1e1e1e, 0xffa500)
                    sub_printTable( val, indent .. string.rep( " ", unicode.len(pos)+8 ) )
                    gpuSet(3,  step,   indent .. string.rep( " ", unicode.len(pos)+6 ) .. "}"  , 0x1e1e1e, 0xffa500)
                    elseif ( type(val) == "string" ) then
                    gpuSet(3,  step,indent .. "[" .. pos .. '] => "' .. val .. '"' , 0x1e1e1e, 0xffa500)
                    else
                        gpuSet(3, step,indent .. "[" .. pos .. "] => " .. tostring(val) , 0x1e1e1e, 0xffa500)
                    end
                end
            else
                gpuSet(3, step,indent..tostring(t)  , 0x1e1e1e, 0xffa500)
            end
        end
    end
 
    if ( type(t) == "table" ) then
        gpuSet(3,step, tostring(t) .. " {" , 0x1e1e1e, 0xffa500)

        sub_printTable( t, "  " )
        gpuSet(3, step, "}" , 0x1e1e1e, 0xffa500)

    else
        sub_printTable( t, "  " )
    end

    
    -- while true do
    --     signalHandler()
    -- end
    sleep(5)
end

function FormDischarge() 

    clear()
    guiFillColorBackground(greyColors[3])
    local xDrawRoundFrameSmall = 45
    local yDrawRoundFrameSmall = 1

    gpuFill(1, 1, resW, resH, " ", greyColors[1])

    gpu.setBackground(greyColors[3])
    gpu.setForeground(greyColors[1])
    gpu.fill(xDrawRoundFrameSmall, yDrawRoundFrameSmall + 21, 51, 3, " ")

    function createButtons()
        uploadButton = button:new(nil, 37 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 13, 1, "[ ВЫГРУЗИТЬ ]", 0xbbbbbb, 0x333333, function() return true end)
        downloadButton = button:new(nil, 24 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 11, 1, "[ СКАЧАТЬ ]", 0x0d6efd, greyColors[15], function() return true end)
        backButton = button:new(nil, 2 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 9, 1, "[ НАЗАД ]", 0xbbbbbb, 0x333333, function() return true end)
    end

    createButtons()

    ----------------------------------------------------------------------------
    gpuSet(nil, 16, "Меню для выгрузки и загрузки сохранений, с и в, github", greyColors[1], greyColors[14])
    gpuSet(nil, 18, "ВНИМАНИЕ! Нажимая кнопку СКАЧАТЬ, знай что:", greyColors[1], greyColors[14])
    gpuSet(nil, 19, "Сохраненные данные на диске и в оперативной памяти компьютера, перетираются безвозвратно", greyColors[1], greyColors[14])
   ----------------------------------------------------------------------------

    while true do
        local signal = signalHandler()
        
        if uploadButton:onEvent(signal) then 
            local dataSend = json.encode({["usersCompoundList"] = tableCopy(usersCompoundList), ["groupsList"] = tableCopy(groupsList)})

           local  status, result = pcall(function() local status, result = uploadOnGithub(dataSend) return {[1] = status, [2] = result} end)
         
            gpuFill(2, 3, resW - 2, 1, " ", greyColors[1], greyColors[1])
            if result[1] and status then -- обработка исключений
                gpuSet(nil, 3, "Данные успешно загружены на github, ссылка: " ..
                    result[2], colorsTable[1], 0x00ff00)
            else
                if not result[1] and status then
                    gpuSet(nil, 3, "Загрузка на github провалилась! Причина: " .. tostring(result[2])
                    , colorsTable[1], 0xff0000)
                else
                    gpuSet(nil, 3, "Загрузка на github провалилась! Причина: " .. tostring(result),
                        colorsTable[1], 0xff0000)
                end
            end

        end

        if downloadButton:onEvent(signal) then 
            gpuFill(2, 3, resW - 2, 1, " ", greyColors[1], greyColors[1])
            local handle, data, chunk = internet.request(githubDoneApiLink), ""

            while true do
                chunk = handle.read(math.huge)
    
                if chunk then
                    data = data .. chunk
                else
                    break
                end
            end
            local error, getedData = pcall(function() return json.decode(dataCard.decode64(json.decode(data)["content"])) end)

            if not data or data == nil or not error then   
                gpuSet(nil, 3, "Не удачная попытка получения данных с github", colorsTable[1], 0xff0000)
            else
                gpuSet(nil, 3, "Данные с github успешно получены, обновлены и сохранены", colorsTable[1], 0x00ff00)
                usersCompoundList = tableCopy(getedData["usersCompoundList"])
                groupsList = tableCopy(getedData["groupsList"])
            end
        end

        if backButton:onEvent(signal) then return true end
    end
end

local function FormGroupEditor()
    local groupsListModify = tableCopy(groupsList)
    local elementIndexSelected = 1

    clear()
    guiFillColorBackground(greyColors[3])
    local xDrawRoundFrameSmall = 45
    local yDrawRoundFrameSmall = 6

    gpuFill(1, 1, resW, resH, " ", greyColors[1])

    gpu.setBackground(greyColors[3])
    gpu.setForeground(greyColors[1])
    gpu.fill(xDrawRoundFrameSmall, yDrawRoundFrameSmall + 8, 51, 15, " ")

    gpu.fill(xDrawRoundFrameSmall, yDrawRoundFrameSmall + 8, 1, 16, "║")
    gpu.fill(51 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 8, 1, 16, "║")

    gpu.set(xDrawRoundFrameSmall, yDrawRoundFrameSmall + 8, "╔══════════════════════════════════════════════════╗")
    gpu.set(xDrawRoundFrameSmall, yDrawRoundFrameSmall + 23,"╚══════════════════════════════════════════════════╝")

    function createButtons()
            saveGroupSettingsButton = button:new(nil, 37 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 13, 1, "[ СОХРАНИТЬ ]", 0xbbbbbb, 0x333333, function() return true end)
            addNewElementToGroupList = button:new(nil, 37 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 20, 13, 1, "[ ДОБАВИТЬ ]", 0x0d6efd, greyColors[15], function() return true end)
            deleteGroupDataButton = button:new(nil, 23 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 13, 1, "[  УДАЛИТЬ  ]", 0x996dff, 0x333333, function() return true end)
            addNewElementToGroupList = button:new(nil, 37 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 20, 13, 1, "[ ДОБАВИТЬ ]", 0x0d6efd, greyColors[15], function() return true end)
    end

    createButtons()
    local cancelAndBackButton = button:new(nil, 2 + xDrawRoundFrameSmall, yDrawRoundFrameSmall + 22, 10, 1, "[ ОТМЕНА ]", 0xbbbbbb, 0x333333, function() return true end)

    ----------------------------------------------------------------------------
    gpuSet(nil, 16, "Выбери группу для редактирования ▾", greyColors[3], greyColors[14])
    local scrollTableElementAsStringField = scrollTableElementAsStringField:new(52, 18, 35, groupsListModify, 1, "groupName", greyColors[1], 0xccdbf0)
    ----------------------------------------------------------------------------

    function drawGroupNameField()
        ----------------------------------------------------------------------------
        gpuSet(48, 23, "Имя:", greyColors[3], greyColors[14])
        fieldToEditNameGroup = fieldSymbolInput:new(53, 23, 37, "<", groupsListModify[1] == nil and "Группа не выбрана" or groupsListModify[1]["groupName"])
        if groupsListModify[1] ~= nil then fieldToEditNameGroup:canFocusedToggle(true) fieldToEditNameGroup:
            modifySavedTextAndPreCalcCursor(groupsListModify[1]["groupName"]) else fieldToEditNameGroup:
            modifySavedTextAndPreCalcCursor("Группа не выбрана") 
        end
        ----------------------------------------------------------------------------
    end

    drawGroupNameField()

    function drawWeightAndGroupName()
        gpu.setBackground(greyColors[1])
        gpu.fill(1, 1, 44, resH, " ")
        gpu.fill(1, 1, resW, 3, " ")
        if #groupsListModify ~= 0 then
            gpuSetColorText(nil, 2, availableColorsAsString, greyColors[1])
            for i = 1, #groupsListModify do
                drawColoredText(3, (resH / 2) - (#groupsListModify / 2) - 1, greyColors[1], "&7 ИМЯ ГРУППЫ:")
                drawColoredText(7, (resH / 2) - (#groupsListModify / 2) + i, greyColors[1], "&7".. groupsListModify[i]["groupName"])
            end
        end
    end

    if #groupsListModify ~= 0 then
        drawWeightAndGroupName()
    end

    local function drawInitTextOnField(elementIndexSelected)
        fieldToEditNameGroup:modifySavedTextAndPreCalcCursor(groupsListModify[elementIndexSelected]["groupName"])
    end

    while true do
        local signal = signalHandler()

        if deleteGroupDataButton:onEvent(signal) then
            table.remove(groupsListModify, elementIndexSelected)
            if groupsListModify[1] == nil then
                drawGroupNameField()
                drawWeightAndGroupName()
                -- scrollTableElementAsStringField:drawSelectedItem("&k" .. scrollTableElementAsStringField.defaultText)
                scrollTableElementAsStringField:drawSelectedItem("&kНажми кнопку добавить")
            else
                elementIndexSelected = groupsListModify[1] == nil and 1 or (abs(elementIndexSelected - 1) == 0 and 1 or abs(elementIndexSelected - 1))
                drawInitTextOnField(elementIndexSelected)
                drawWeightAndGroupName()
                scrollTableElementAsStringField:drawSelectedItem(groupsListModify[elementIndexSelected]["groupName"])
            end
        end
        if addNewElementToGroupList:onEvent(signal) then
            table.insert(groupsListModify, #groupsListModify == 0 and 1 or #groupsListModify, { ["groupName"] = "&f".."Новая группа" .. tostring(#groupsListModify).." "})
            if groupsListModify[1] ~= nil then
                fieldToEditNameGroup:canFocusedToggle(true)
            end
            local error, status = pcall(function() table.sort(groupsListModify, function(a, b) return a.groupName < b.groupName end) end) --Сортировка списка по убыванию
            elementIndexSelected = #groupsListModify == 0 and 1 or #groupsListModify
            drawInitTextOnField(elementIndexSelected)
            drawWeightAndGroupName()
            scrollTableElementAsStringField:drawSelectedItem(groupsListModify[elementIndexSelected]["groupName"])
            createButtons()
        end

        
        if fieldToEditNameGroup:onFocused(signal, "Отсутствуют данные, добавь!") == "focusedDroped"  then
            groupsListModify[elementIndexSelected].groupName = tostring(fieldToEditNameGroup.savedText)
            scrollTableElementAsStringField:drawSelectedItem(fieldToEditNameGroup.savedText)
        end
        
        if saveGroupSettingsButton:onEvent(signal) then groupsList = tableCopy(groupsListModify) return true end
        if cancelAndBackButton:onEvent(signal) then return true end

        local status = scrollTableElementAsStringField:onEvent(signal, elementIndexSelected)
        if groupsListModify[1] ~= nil then
            if status == "leftButtonTouched" then
                if elementIndexSelected > 1 then
                    elementIndexSelected = elementIndexSelected - 1
                    drawInitTextOnField(elementIndexSelected)
                end
            elseif status == "rightButtonTouched" then
                if elementIndexSelected < #groupsListModify then
                    elementIndexSelected = elementIndexSelected + 1
                    drawInitTextOnField(elementIndexSelected)
                end
            end
        end
    end
end

function FormSettingsMenu()
  local groupListToOperate = {}
  local usersCompoundListModify = tableCopy(usersCompoundList)
  local elementIndexSelected = 1
  local genderIndexSelected = usersCompoundListModify[1] ~= nil and (usersCompoundListModify[1]["genderType"] == genderTypeTable[1]["genderType"] and 1 or 2) or 1
  local groupIndexSelected = 1

  
  clear()
  guiFillColorBackground(greyColors[1])
  drawRoundFrame(0x333333,  0xbbbbbb)
  local cancelAndBackButton = button:new(nil, 30, resH, 10, 1, "[ ОТМЕНА ]", 0xbbbbbb, 0x333333, function() return true end)
  local settingsSaveButton = button:new(nil, abs(resW - 28), resH, 13, 1, "[ СОХРАНИТЬ ]", 0xbbbbbb, 0x333333, function () return true end)
  local settingsGroupButton = button:new(nil, abs(resW - 55), resH, 13, 1, "[ Настройка групп ]", 0xbbbbbb, 0x333333, function() FormGroupEditor() return true end)
  local dischargeButton = button:new(nil, abs(resW - 75), resH, 12, 1, "[ ВЫГРУЗКА ]", 0xbbbbbb, 0x333333, function() FormDischarge() return true end)

  gpuFill(2,7, abs(resW - 2), 1, "▀", greyColors[1], greyColors[3])
  gpuFill(2, 2, resW - 2, 5, " ", greyColors[2])


  gpuSetColorText(nil, 9, availableColorsAsString, greyColors[1])
  usersCompoundListModify = tableCopy(drawUsersCompoundList(usersCompoundListModify, false))
  
    function selectUsersCompoundListModifyItem(erase, oldElementIndexSelected)
        if usersCompoundListModify[1] ~= nil and groupsList ~= nil then

             function drawOfLineUsersCompoundList(indexElement, colorIndex) 
                gpuFill(2, usersCompoundListModify[indexElement]["yCord"], resW - 2, 1, "X", greyColors[colorIndex], greyColors[colorIndex])
                
                drawColoredText(listxCordToDraw, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                    usersCompoundListModify[indexElement]["groupName"])

                    drawColoredText(listxCordToDraw + xCordToDrawNickNameToPlus, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex], usersCompoundListModify[indexElement]["userName"])
                drawColoredText(listxCordToDraw + 40, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                    usersCompoundListModify[indexElement]["genderType"])
                drawColoredText(listxCordToDraw + 60, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                    usersCompoundListModify[indexElement]["onlineStatus"])
                drawColoredText(listxCordToDraw + 80, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                    "&8Дискорд: " .. usersCompoundListModify[indexElement]["discord"])
            end

            function drawOfLineUsersCompoundList_moded(indexElement, colorIndex)
                local listYCordToDraw = (resH / 2) - ((#usersCompoundListModify) / 2) -- С какой верхней координаты начинать рисовать список с админским составом
                local yCord = 0
                  for i= 1, #usersCompoundListModify do
                      yCord = math.modf(listYCordToDraw + i)
                      usersCompoundListModify[i]["yCord"] = yCord
                  end
                  
                for i = 1, 2 do
                    gpuFill(2, usersCompoundListModify[indexElement]["yCord"], resW - 2, 1, "X", greyColors[colorIndex], greyColors[colorIndex])
                    
                    drawColoredText(listxCordToDraw, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                        usersCompoundListModify[indexElement]["groupName"])

                        drawColoredText(listxCordToDraw + xCordToDrawNickNameToPlus, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex], usersCompoundListModify[indexElement]["userName"])
                    drawColoredText(listxCordToDraw + 40, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                        usersCompoundListModify[indexElement]["genderType"])
                    drawColoredText(listxCordToDraw + 60, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                        usersCompoundListModify[indexElement]["onlineStatus"])
                    drawColoredText(listxCordToDraw + 80, usersCompoundListModify[indexElement]["yCord"], greyColors[colorIndex],
                        "&8Дискорд: " .. usersCompoundListModify[indexElement]["discord"])
                end
            end

            if erase then    
                drawOfLineUsersCompoundList(oldElementIndexSelected, 1)
            end
            drawOfLineUsersCompoundList(elementIndexSelected, 3)
        end
    end
    selectUsersCompoundListModifyItem(false)
    
    function updateTopBarData()
        pcall(function() 
            genderIndexSelected = usersCompoundListModify[elementIndexSelected]["genderType"] == genderTypeTable[1]["genderType"] and 1 or 2
            groupIndexSelected = 1
            groupListToOperate = {}
            groupListToOperate = tableCopy(groupsList)
            table.insert(groupListToOperate, groupIndexSelected, {["groupName"] = usersCompoundListModify[elementIndexSelected]["groupName"]})
            scrollGroupSelect = scrollTableElementAsStringField:new(9, 5, 30, groupListToOperate, 1, "groupName",
            greyColors[1], 0xccdbf0) --уберёшь, будет весело (P.S не очень :) )
            
            scrollGroupSelect:drawSelectedItem(usersCompoundListModify[elementIndexSelected].groupName)
            
            scrollGenderSelect:drawSelectedItem(usersCompoundListModify[elementIndexSelected].genderType)
    
            fieldForNickNameEdit:modifySavedTextAndPreCalcCursor(usersCompoundListModify[elementIndexSelected].userName)
            fieldForDiscordEdit:modifySavedTextAndPreCalcCursor(usersCompoundListModify[elementIndexSelected].discord)
        end)
    end
    
    function createTopElementsOfMenu()

        if usersCompoundListModify[1] == nil and groupsList ~= nil then
            
            scrollGroupSelect = {}
            scrollGenderSelect = {}
            fieldForNickNameEdit = {}
            fieldForDiscordEdit = {}
            gpuFill(2, 2, resW - 2, 5, " ", greyColors[2])
            createButtons()
            gpuSetColorText(nil, 5, "[0xffff00]Нажми кнопку добавить, что бы приступить к редактированию этого списка", greyColors[2])
        elseif groupsList ~= nil then
            gpuFill(2,5, abs(resW - 2), 1, " ", greyColors[2])
            ---------------------------------------------------------------------------
            gpuSetColorText(5, 3, "[0xd2d2d2]Выбор группы    [0x878787]|", greyColors[2])
            scrollGroupSelect = scrollTableElementAsStringField:new(9, 5, 30, groupListToOperate, 1, "groupName",
                greyColors[1], 0xccdbf0)
            ----------------------------------------------------------------------------

            ----------------------------------------------------------------------------
            gpuSet(82, 3, "Пол:", greyColors[2], greyColors[14])
            scrollGenderSelect = scrollTableElementAsStringField:new(86, 5, 6, genderTypeTable, genderIndexSelected,
                "genderType", greyColors[1], 0xccdbf0)
            ----------------------------------------------------------------------------

            --------------------------------------------------------------------------- clipboard support --------------------
            gpuSet(49, 3, "Nick-name:", greyColors[2], greyColors[14])
            fieldForNickNameEdit = fieldSymbolInput:new(49, 5, 25, "<",
                usersCompoundListModify[1] == nil and "Отсутствуют данные" or
                usersCompoundListModify[1]["userName"])
            ----------------------------------------------------------------------------

            --------------------------------------------------------------------------- clipboard support --------------------
            gpuSet(102, 5, "Discord:", greyColors[2], greyColors[14])
            fieldForDiscordEdit = fieldSymbolInput:new(113, 5, 25, "<",
                usersCompoundListModify[1] == nil and "Отсутствуют данные" or
                usersCompoundListModify[1]["discord"])
            ----------------------------------------------------------------------------
            fieldForNickNameEdit:modifySavedTextAndPreCalcCursor(usersCompoundListModify[elementIndexSelected].userName)
            fieldForDiscordEdit:modifySavedTextAndPreCalcCursor(usersCompoundListModify[elementIndexSelected].discord)
        end
    end

    function createButtons()
        if usersCompoundListModify[1] ~= nil and groupsList[1] ~= nil then
            ------------- сдвигание позиции элеменета compound относительно всех в списке -------------
                upElementInUsersCompoundList = button:new(nil, 26, 3, 3, 1, "▲", greyColors[3], 0x009200, function() return true end)
                    gpuSet(30, 3, "ПОЗИЦИЯ", greyColors[2], greyColors[14])
                downElementInUsersCompoundList = button:new(nil, 38, 3,3, 1, "▼", greyColors[3], 0x009200, function() return true end)
            ------------- сдвигание позиции элеменета compound относительно всех в списке -------------
            saveElementSelectedOfUsersCompoundList = button:new(nil, 100, 2, 13, 1, "[ СОХРАНИТЬ ]", 0x00b600, 0x333333, function() return true end)
            addNewElementToUsersCompoundList = button:new(nil, 115, 2, 12, 1, "[ ДОБАВИТЬ ]", 0x0d6efd, greyColors[13], function() return true end)
            deleteLineFromUsersCompoundButton = button:new(nil, 130, 2, 13, 1, "[  УДАЛИТЬ  ]", 0x996dff, 0x333333, function() return true end)
        else
            deleteLineFromUsersCompoundButton = {}
            if groupsList[1] == nil then
                gpuSetColorText(nil, 5, "[0x0d6efd]Сначала добавь новую группу, для того что бы добавлять кого-то в список мод состава", greyColors[2])
            end
            addNewElementToUsersCompoundList = button:new(nil, 115, 2, 13, 1, "[ ДОБАВИТЬ ]", 0x0d6efd, greyColors[13], function() return true end)
        end
    end
    createButtons()
    
    if groupsList[1] == nil then
        -- gpuSet(38, 5, tostring(groupIndexSelected), colorsTable[1], 0x00ffff) -- отлад-очка
        gpuFill(2, 2, resW - 2, 5, " ", greyColors[2])
        gpuSetColorText(nil, 5, "[0xffff00]Сначала добавь в группы название должности. Только после этого ты сможешь редактировать этот список", greyColors[2])
    else
        createTopElementsOfMenu()
        fieldForNickNameEdit.canFocused = true 
        fieldForDiscordEdit.canFocused = true
        elementIndexSelected = 1
        updateTopBarData()
    end

    while true do
        local signal = signalHandler()
        if usersCompoundListModify[1] ~= nil then 
            local status = scrollGenderSelect:onEvent(signal, genderIndexSelected)
                if status == "leftButtonTouched" then
                    if genderIndexSelected > 1 then
                        genderIndexSelected = genderIndexSelected - 1
                        scrollGenderSelect:drawSelectedItem(genderTypeTable[genderIndexSelected]["genderType"])
                    end
                elseif status == "rightButtonTouched" then
                    if genderIndexSelected < #genderTypeTable then
                        genderIndexSelected = genderIndexSelected + 1
                        scrollGenderSelect:drawSelectedItem(genderTypeTable[genderIndexSelected]["genderType"])
                    end
                end

                local status2 = scrollGroupSelect:onEvent(signal, groupIndexSelected)
                if status2 == "leftButtonTouched" then
                    if groupIndexSelected > 1 then
                        groupIndexSelected = groupIndexSelected - 1
                    end
                elseif status2 == "rightButtonTouched" then
                    if groupIndexSelected < #groupListToOperate then
                        groupIndexSelected = groupIndexSelected + 1
                    end
                end
        pcall(function()
            if fieldForNickNameEdit:onFocused(signal, "оо какие вкусные блинчикиии =)") == "focusedDroped" or fieldForDiscordEdit:onFocused(signal, "питца вкусная, ммм.") == "focusedDroped" then
                --Error текст ни когда не высветится, это так пасхалка :D, эти if нужны для того что бы можно было кликнуть на поле и редактировать его.
            end end)
        end

        if addNewElementToUsersCompoundList:onEvent(signal) and groupsList[1] ~= nil then
            groupListToOperate = {}
            groupListToOperate = tableCopy(groupsList)
            table.insert(usersCompoundListModify, #usersCompoundListModify == 0 and 1 or #usersCompoundListModify + 1, {["groupName"] = groupListToOperate[1]["groupName"], ["userName"] = "&6Отсутствует",  ["genderType"] = genderTypeTable[1]["genderType"], ["onlineStatus"] = "&4offline", ["discord"] = "Отсутствует"})
            elementIndexSelected = #usersCompoundListModify == 0 and 1 or #usersCompoundListModify
            createTopElementsOfMenu()
            createButtons()
            fieldForNickNameEdit.canFocused = true 
            fieldForDiscordEdit.canFocused = true 
            updateTopBarData()
            drawUsersCompoundList(usersCompoundListModify, false)
            selectUsersCompoundListModifyItem(false)    
        end
        
        pcall(function()
        if deleteLineFromUsersCompoundButton:onEvent(signal) then
            table.remove( usersCompoundListModify, elementIndexSelected)
            elementIndexSelected = 1
            if usersCompoundListModify[1] == nil then
                gpuFill(3, 10, 143, 25, " ", greyColors[1])
                deleteLineFromUsersCompoundButton:destroy(greyColors[2])
                deleteLineFromUsersCompoundButton = {}
                elementIndexSelected = 1
                createTopElementsOfMenu()
                updateTopBarData()
                computer.beep(900, 0.1)
            else
                
                updateTopBarData()
                drawUsersCompoundList(usersCompoundListModify, false)
                selectUsersCompoundListModifyItem(false)
            end
        end end)

        if usersCompoundListModify[1] ~= nil and signal and signal[1] == "touch" then
            for i = 1, #usersCompoundListModify do  --select item of usersCompoundListModify
                if usersCompoundListModify[i]["yCord"] == signal[4] and usersCompoundListModify[i] ~= nil then
                    local oldElementIndexSelected = elementIndexSelected
                    elementIndexSelected = i
                    groupListToOperate = {}
                    selectUsersCompoundListModifyItem(true, oldElementIndexSelected)
                    updateTopBarData()
                    break
                end
             end

            if saveElementSelectedOfUsersCompoundList:onEvent(signal) then --Кнопка сохранение выбранной текущей позиции
                usersCompoundListModify[elementIndexSelected].groupName = groupListToOperate[groupIndexSelected]["groupName"]
                usersCompoundListModify[elementIndexSelected].genderType = genderTypeTable[genderIndexSelected]["genderType"]
                usersCompoundListModify[elementIndexSelected].userName = tostring(fieldForNickNameEdit.savedText)
                usersCompoundListModify[elementIndexSelected].discord = tostring(fieldForDiscordEdit.savedText)
                updateTopBarData()
                selectUsersCompoundListModifyItem(false)    
            end
            pcall(function() --эхх ну и хуитаа =(
                if upElementInUsersCompoundList:onEvent(signal) then
                    if #usersCompoundListModify > 1 and elementIndexSelected ~= 1 then
                        groupListToOperate = {}
                        local elementUsersCompoundListModifyChache = tableCopy(usersCompoundListModify[elementIndexSelected])
                        table.remove(usersCompoundListModify, elementIndexSelected)
                        table.insert(usersCompoundListModify, elementIndexSelected - 1, elementUsersCompoundListModifyChache)
                        -- local oldElementIndexSelected = elementIndexSelected
                        drawOfLineUsersCompoundList_moded(elementIndexSelected, 1)
                        elementIndexSelected = elementIndexSelected - 1
                        updateTopBarData()
                        drawOfLineUsersCompoundList_moded(elementIndexSelected, 3)
                    end
                end
                if downElementInUsersCompoundList:onEvent(signal) then
                    if #usersCompoundListModify > 1 and #usersCompoundListModify ~= elementIndexSelected then
                        groupListToOperate = {}
                        local elementUsersCompoundListModifyChache = tableCopy(usersCompoundListModify[elementIndexSelected])
                        selectUsersCompoundListModifyItem(false)
                        table.remove(usersCompoundListModify, elementIndexSelected)
                        table.insert(usersCompoundListModify, elementIndexSelected + 1, elementUsersCompoundListModifyChache)
                        -- local oldElementIndexSelected = elementIndexSelected
                        drawOfLineUsersCompoundList_moded(elementIndexSelected, 1)
                        elementIndexSelected = elementIndexSelected + 1
                        updateTopBarData()
                        drawOfLineUsersCompoundList_moded(elementIndexSelected, 3)
                    end
                end
            end)
            -- gpuSet(38, 5, tostring(groupIndexSelected), colorsTable[1], 0x00ffff) -- отлад-очка
        end

        if cancelAndBackButton:onEvent(signal) then return true end
        if settingsSaveButton:onEvent(signal) then 
            usersCompoundList = tableCopy(usersCompoundListModify) 
            return true end
        if settingsGroupButton:onEvent(signal) then  return true end
        if dischargeButton:onEvent(signal) then FormDischarge() return true end
    end
end

settingsButton = button:new(nil, 14, resH, 13, 1, "[ НАСТРОИТЬ ]", 0xbbbbbb, 0x333333, function() return FormSettingsMenu() end)

function initDraw()
  guiFillColorBackground(greyColors[1])
  drawRoundFrame(0x333333,  0xbbbbbb, true)
  gpuSetColorText(nil, 4, "[0x8b00ff]|Список администрации DraconicTech 1.7.10|", greyColors[1])
  settingsButton:draw()
end

initDraw()

function drawColoredText(x, y, backgroundColor, text) --Цветной текст

    gpu.setBackground(backgroundColor)
    pcall(function ()
	local n = 1
	for i = 1, unicode.len(text) do
		if unicode.sub(text, i, i) == "&" then
		    setForeGroundColor(unicode.sub(text, i + 1, i + 1))
		elseif unicode.sub(text, i - 1, i - 1) ~= "&" then
			gpu.set(x+n,y, unicode.sub(text, i,i))
			n = n + 1
		end
	end
end)
    if string.find(text, "&") == nil then gpu.setForeground(0x00ff00) end
end

function drawUsersCompoundList(tableToDraw, zebra, isSort)

    if tableToDraw[1] ~= nil then  
        gpuFill(2, 10, 144, 25, " ", greyColors[1])
          local listYCordToDraw = (resH / 2) - ((#tableToDraw) / 2) -- С какой верхней координаты начинать рисовать список с админским составом
          local yCord = 0
            for i= 1, #tableToDraw do
                yCord = math.modf(listYCordToDraw + i)
                tableToDraw[i]["yCord"] = yCord
                drawColoredText(listxCordToDraw, yCord, greyColors[1], tableToDraw[i]["groupName"])
                drawColoredText(listxCordToDraw + xCordToDrawNickNameToPlus, yCord, greyColors[1], tableToDraw[i]["userName"])
                drawColoredText(listxCordToDraw + 40, yCord, greyColors[1], tableToDraw[i]["genderType"])
                drawColoredText(listxCordToDraw + 60, yCord, greyColors[1], tableToDraw[i]["onlineStatus"])
                drawColoredText(listxCordToDraw + 80, yCord, greyColors[1], "&8Дискорд: "..tableToDraw[i]["discord"])
            end
            return tableToDraw
      end
  return {}
end

local fsData = readFS("/home/data")
if fsData ~= nil and fsData ~= "" then 
    fsData = json.decode(fsData)
    usersCompoundList = tableCopy(fsData["usersCompoundList"])
    groupsList = tableCopy(fsData["groupsList"])
end


usersCompoundList = tableCopy(drawUsersCompoundList(usersCompoundList, true))

function updateWhoOnlineStatus() 
    
    local function drawOfLineUsersCompoundList_forUpdateWhoOnlineStatus(indexElement, colorIndex) 
        gpuFill(2, usersCompoundList[indexElement]["yCord"], resW - 2, 1, "X", greyColors[colorIndex], greyColors[colorIndex])
        
        drawColoredText(listxCordToDraw, usersCompoundList[indexElement]["yCord"], greyColors[colorIndex],
        usersCompoundList[indexElement]["groupName"])

            drawColoredText(listxCordToDraw + xCordToDrawNickNameToPlus, usersCompoundList[indexElement]["yCord"], greyColors[colorIndex], usersCompoundList[indexElement]["userName"])
        drawColoredText(listxCordToDraw + 40, usersCompoundList[indexElement]["yCord"], greyColors[colorIndex],
        usersCompoundList[indexElement]["genderType"])
        drawColoredText(listxCordToDraw + 60, usersCompoundList[indexElement]["yCord"], greyColors[colorIndex],
        usersCompoundList[indexElement]["onlineStatus"])
        drawColoredText(listxCordToDraw + 80, usersCompoundList[indexElement]["yCord"], greyColors[colorIndex],
            "&8Дискорд: " .. usersCompoundList[indexElement]["discord"])
    end

    if usersCompoundList[1] ~= nil then

        local function filterUserName(userName) --Регуляр ОЧКА, вырезаю цвет вида &3 из никнейма 
            strintToReturn = ""
            local begin = 1
            while true do
                local b, e, color = userName:find('%&(%x)', begin)
                local precedingString = userName:sub(begin, b and (b - 1))
                if precedingString then
                    strintToReturn = strintToReturn .. precedingString
                end
                if not color then
                    break
                end
                begin = e + 1
            end
            return strintToReturn
        end 
        for i= 1, #usersCompoundList do
            local userName = filterUserName(usersCompoundList[i]["userName"])
            -- printTable({userName}) --отлад очка
            computer.removeUser(userName)
            if computer.addUser(userName) then --is true тогда чел будет со статусом online
                computer.removeUser(userName)
                if usersCompoundList[i]["onlineStatus"] == "&4offline" then 
                    usersCompoundList[i]["onlineStatus"] = "&2online"
                    drawOfLineUsersCompoundList_forUpdateWhoOnlineStatus(i, 1)
                end
            else
                computer.removeUser(userName)
                if usersCompoundList[i]["onlineStatus"] == "&2online" then 
                    usersCompoundList[i]["onlineStatus"] = "&4offline"
                    drawOfLineUsersCompoundList_forUpdateWhoOnlineStatus(i, 1)
                end
            end
        end
        for admin = 1, #adminListNoAccos do
            computer.addUser(adminListNoAccos[admin])
        end  
    end
end
local i = 1

while true do
  i = i + 1
  local signal = signalHandler()
  if settingsButton:onEvent(signal) then  initDraw() usersCompoundList = tableCopy(drawUsersCompoundList(usersCompoundList, true)) 
    local data = json.encode({["usersCompoundList"] = tableCopy(usersCompoundList), ["groupsList"] = tableCopy(groupsList)})
    writeFS("/home/data", "w", data)
end 
  if i >= coolDownUpdateSleep then i = 1 updateWhoOnlineStatus() end
end

