{-# LANGUAGE
    NoImplicitPrelude
#-}

{-
Copyright (c) 2008, 2009
Russell O'Connor

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

-- |Names for colours.
-- Names taken from SVG 1.1 specification,
-- <http://www.w3.org/TR/SVG11/types.html#ColorKeywords>.
--
-- 'readColourName' takes a string naming a colour (must be all lowercase)
-- and returns the colour.
-- Fails if the name is not recognized.
module Graphics.Colour
 (
  readColourName
 ,aliceblue
 ,antiquewhite
 ,aqua
 ,aquamarine
 ,azure
 ,beige
 ,bisque
 ,black
 ,blanchedalmond
 ,blue
 ,blueviolet
 ,brown
 ,burlywood
 ,cadetblue
 ,chartreuse
 ,chocolate
 ,coral
 ,cornflowerblue
 ,cornsilk
 ,crimson
 ,cyan
 ,darkblue
 ,darkcyan
 ,darkgoldenrod
 ,darkgray
 ,darkgreen
 ,darkgrey
 ,darkkhaki
 ,darkmagenta
 ,darkolivegreen
 ,darkorange
 ,darkorchid
 ,darkred
 ,darksalmon
 ,darkseagreen
 ,darkslateblue
 ,darkslategray
 ,darkslategrey
 ,darkturquoise
 ,darkviolet
 ,deeppink
 ,deepskyblue
 ,dimgray
 ,dimgrey
 ,dodgerblue
 ,firebrick
 ,floralwhite
 ,forestgreen
 ,fuchsia
 ,gainsboro
 ,ghostwhite
 ,gold
 ,goldenrod
 ,gray
 ,grey
 ,green
 ,greenyellow
 ,honeydew
 ,hotpink
 ,indianred
 ,indigo
 ,ivory
 ,khaki
 ,lavender
 ,lavenderblush
 ,lawngreen
 ,lemonchiffon
 ,lightblue
 ,lightcoral
 ,lightcyan
 ,lightgoldenrodyellow
 ,lightgray
 ,lightgreen
 ,lightgrey
 ,lightpink
 ,lightsalmon
 ,lightseagreen
 ,lightskyblue
 ,lightslategray
 ,lightslategrey
 ,lightsteelblue
 ,lightyellow
 ,lime
 ,limegreen
 ,linen
 ,magenta
 ,maroon
 ,mediumaquamarine
 ,mediumblue
 ,mediumorchid
 ,mediumpurple
 ,mediumseagreen
 ,mediumslateblue
 ,mediumspringgreen
 ,mediumturquoise
 ,mediumvioletred
 ,midnightblue
 ,mintcream
 ,mistyrose
 ,moccasin
 ,navajowhite
 ,navy
 ,oldlace
 ,olive
 ,olivedrab
 ,orange
 ,orangered
 ,orchid
 ,palegoldenrod
 ,palegreen
 ,paleturquoise
 ,palevioletred
 ,papayawhip
 ,peachpuff
 ,peru
 ,pink
 ,plum
 ,powderblue
 ,purple
 ,red
 ,rosybrown
 ,royalblue
 ,saddlebrown
 ,salmon
 ,sandybrown
 ,seagreen
 ,seashell
 ,sienna
 ,silver
 ,skyblue
 ,slateblue
 ,slategray
 ,slategrey
 ,snow
 ,springgreen
 ,steelblue
 ,tan
 ,teal
 ,thistle
 ,tomato
 ,turquoise
 ,violet
 ,wheat
 ,white
 ,whitesmoke
 ,yellow
 ,yellowgreen
 )
where

import GHC.Base (Double, Monad (..), String, ($), (++))
import Text.Show (Show (..))
import Data.Colour.SRGB
import Data.Colour (black)

readColourName :: (Monad m) => String -> m (Colour Double)
readColourName "aliceblue" = return aliceblue
readColourName "antiquewhite" = return antiquewhite
readColourName "aqua" = return aqua
readColourName "aquamarine" = return aquamarine
readColourName "azure" = return azure
readColourName "beige" = return beige
readColourName "bisque" = return bisque
readColourName "black" = return black
readColourName "blanchedalmond" = return blanchedalmond
readColourName "blue" = return blue
readColourName "blueviolet" = return blueviolet
readColourName "brown" = return brown
readColourName "burlywood" = return burlywood
readColourName "cadetblue" = return cadetblue
readColourName "chartreuse" = return chartreuse
readColourName "chocolate" = return chocolate
readColourName "coral" = return coral
readColourName "cornflowerblue" = return cornflowerblue
readColourName "cornsilk" = return cornsilk
readColourName "crimson" = return crimson
readColourName "cyan" = return cyan
readColourName "darkblue" = return darkblue
readColourName "darkcyan" = return darkcyan
readColourName "darkgoldenrod" = return darkgoldenrod
readColourName "darkgray" = return darkgray
readColourName "darkgreen" = return darkgreen
readColourName "darkgrey" = return darkgrey
readColourName "darkkhaki" = return darkkhaki
readColourName "darkmagenta" = return darkmagenta
readColourName "darkolivegreen" = return darkolivegreen
readColourName "darkorange" = return darkorange
readColourName "darkorchid" = return darkorchid
readColourName "darkred" = return darkred
readColourName "darksalmon" = return darksalmon
readColourName "darkseagreen" = return darkseagreen
readColourName "darkslateblue" = return darkslateblue
readColourName "darkslategray" = return darkslategray
readColourName "darkslategrey" = return darkslategrey
readColourName "darkturquoise" = return darkturquoise
readColourName "darkviolet" = return darkviolet
readColourName "deeppink" = return deeppink
readColourName "deepskyblue" = return deepskyblue
readColourName "dimgray" = return dimgray
readColourName "dimgrey" = return dimgrey
readColourName "dodgerblue" = return dodgerblue
readColourName "firebrick" = return firebrick
readColourName "floralwhite" = return floralwhite
readColourName "forestgreen" = return forestgreen
readColourName "fuchsia" = return fuchsia
readColourName "gainsboro" = return gainsboro
readColourName "ghostwhite" = return ghostwhite
readColourName "gold" = return gold
readColourName "goldenrod" = return goldenrod
readColourName "gray" = return gray
readColourName "grey" = return grey
readColourName "green" = return green
readColourName "greenyellow" = return greenyellow
readColourName "honeydew" = return honeydew
readColourName "hotpink" = return hotpink
readColourName "indianred" = return indianred
readColourName "indigo" = return indigo
readColourName "ivory" = return ivory
readColourName "khaki" = return khaki
readColourName "lavender" = return lavender
readColourName "lavenderblush" = return lavenderblush
readColourName "lawngreen" = return lawngreen
readColourName "lemonchiffon" = return lemonchiffon
readColourName "lightblue" = return lightblue
readColourName "lightcoral" = return lightcoral
readColourName "lightcyan" = return lightcyan
readColourName "lightgoldenrodyellow" = return lightgoldenrodyellow
readColourName "lightgray" = return lightgray
readColourName "lightgreen" = return lightgreen
readColourName "lightgrey" = return lightgrey
readColourName "lightpink" = return lightpink
readColourName "lightsalmon" = return lightsalmon
readColourName "lightseagreen" = return lightseagreen
readColourName "lightskyblue" = return lightskyblue
readColourName "lightslategray" = return lightslategray
readColourName "lightslategrey" = return lightslategrey
readColourName "lightsteelblue" = return lightsteelblue
readColourName "lightyellow" = return lightyellow
readColourName "lime" = return lime
readColourName "limegreen" = return limegreen
readColourName "linen" = return linen
readColourName "magenta" = return magenta
readColourName "maroon" = return maroon
readColourName "mediumaquamarine" = return mediumaquamarine
readColourName "mediumblue" = return mediumblue
readColourName "mediumorchid" = return mediumorchid
readColourName "mediumpurple" = return mediumpurple
readColourName "mediumseagreen" = return mediumseagreen
readColourName "mediumslateblue" = return mediumslateblue
readColourName "mediumspringgreen" = return mediumspringgreen
readColourName "mediumturquoise" = return mediumturquoise
readColourName "mediumvioletred" = return mediumvioletred
readColourName "midnightblue" = return midnightblue
readColourName "mintcream" = return mintcream
readColourName "mistyrose" = return mistyrose
readColourName "moccasin" = return moccasin
readColourName "navajowhite" = return navajowhite
readColourName "navy" = return navy
readColourName "oldlace" = return oldlace
readColourName "olive" = return olive
readColourName "olivedrab" = return olivedrab
readColourName "orange" = return orange
readColourName "orangered" = return orangered
readColourName "orchid" = return orchid
readColourName "palegoldenrod" = return palegoldenrod
readColourName "palegreen" = return palegreen
readColourName "paleturquoise" = return paleturquoise
readColourName "palevioletred" = return palevioletred
readColourName "papayawhip" = return papayawhip
readColourName "peachpuff" = return peachpuff
readColourName "peru" = return peru
readColourName "pink" = return pink
readColourName "plum" = return plum
readColourName "powderblue" = return powderblue
readColourName "purple" = return purple
readColourName "red" = return red
readColourName "rosybrown" = return rosybrown
readColourName "royalblue" = return royalblue
readColourName "saddlebrown" = return saddlebrown
readColourName "salmon" = return salmon
readColourName "sandybrown" = return sandybrown
readColourName "seagreen" = return seagreen
readColourName "seashell" = return seashell
readColourName "sienna" = return sienna
readColourName "silver" = return silver
readColourName "skyblue" = return skyblue
readColourName "slateblue" = return slateblue
readColourName "slategray" = return slategray
readColourName "slategrey" = return slategrey
readColourName "snow" = return snow
readColourName "springgreen" = return springgreen
readColourName "steelblue" = return steelblue
readColourName "tan" = return tan
readColourName "teal" = return teal
readColourName "thistle" = return thistle
readColourName "tomato" = return tomato
readColourName "turquoise" = return turquoise
readColourName "violet" = return violet
readColourName "wheat" = return wheat
readColourName "white" = return white
readColourName "whitesmoke" = return whitesmoke
readColourName "yellow" = return yellow
readColourName "yellowgreen" = return yellowgreen
readColourName x = fail $
  "Data.Colour.Names.readColourName: Unknown colour name "++show x

aliceblue :: Colour Double
aliceblue = sRGB24 240 248 255

antiquewhite :: Colour Double
antiquewhite = sRGB24 250 235 215

aqua :: Colour Double
aqua = sRGB24 0 255 255

aquamarine :: Colour Double
aquamarine = sRGB24 127 255 212

azure :: Colour Double
azure = sRGB24 240 255 255

beige :: Colour Double
beige = sRGB24 245 245 220

bisque :: Colour Double
bisque = sRGB24 255 228 196

-- black is reexported from Data.Colour

blanchedalmond :: Colour Double
blanchedalmond = sRGB24 255 235 205

blue :: Colour Double
blue = sRGB24 0 0 255

blueviolet :: Colour Double
blueviolet = sRGB24 138 43 226

brown :: Colour Double
brown = sRGB24 165 42 42

burlywood :: Colour Double
burlywood = sRGB24 222 184 135

cadetblue :: Colour Double
cadetblue = sRGB24 95 158 160

chartreuse :: Colour Double
chartreuse = sRGB24 127 255 0

chocolate :: Colour Double
chocolate = sRGB24 210 105 30

coral :: Colour Double
coral = sRGB24 255 127 80

cornflowerblue :: Colour Double
cornflowerblue = sRGB24 100 149 237

cornsilk :: Colour Double
cornsilk = sRGB24 255 248 220

crimson :: Colour Double
crimson = sRGB24 220 20 60

cyan :: Colour Double
cyan = sRGB24 0 255 255

darkblue :: Colour Double
darkblue = sRGB24 0 0 139

darkcyan :: Colour Double
darkcyan = sRGB24 0 139 139

darkgoldenrod :: Colour Double
darkgoldenrod = sRGB24 184 134 11

darkgray :: Colour Double
darkgray = sRGB24 169 169 169

darkgreen :: Colour Double
darkgreen = sRGB24 0 100 0

darkgrey :: Colour Double
darkgrey = sRGB24 169 169 169

darkkhaki :: Colour Double
darkkhaki = sRGB24 189 183 107

darkmagenta :: Colour Double
darkmagenta = sRGB24 139 0 139

darkolivegreen :: Colour Double
darkolivegreen = sRGB24 85 107 47

darkorange :: Colour Double
darkorange = sRGB24 255 140 0

darkorchid :: Colour Double
darkorchid = sRGB24 153 50 204

darkred :: Colour Double
darkred = sRGB24 139 0 0

darksalmon :: Colour Double
darksalmon = sRGB24 233 150 122

darkseagreen :: Colour Double
darkseagreen = sRGB24 143 188 143

darkslateblue :: Colour Double
darkslateblue = sRGB24 72 61 139

darkslategray :: Colour Double
darkslategray = sRGB24 47 79 79

darkslategrey :: Colour Double
darkslategrey = sRGB24 47 79 79

darkturquoise :: Colour Double
darkturquoise = sRGB24 0 206 209

darkviolet :: Colour Double
darkviolet = sRGB24 148 0 211

deeppink :: Colour Double
deeppink = sRGB24 255 20 147

deepskyblue :: Colour Double
deepskyblue = sRGB24 0 191 255

dimgray :: Colour Double
dimgray = sRGB24 105 105 105

dimgrey :: Colour Double
dimgrey = sRGB24 105 105 105

dodgerblue :: Colour Double
dodgerblue = sRGB24 30 144 255

firebrick :: Colour Double
firebrick = sRGB24 178 34 34

floralwhite :: Colour Double
floralwhite = sRGB24 255 250 240

forestgreen :: Colour Double
forestgreen = sRGB24 34 139 34

fuchsia :: Colour Double
fuchsia = sRGB24 255 0 255

gainsboro :: Colour Double
gainsboro = sRGB24 220 220 220

ghostwhite :: Colour Double
ghostwhite = sRGB24 248 248 255

gold :: Colour Double
gold = sRGB24 255 215 0

goldenrod :: Colour Double
goldenrod = sRGB24 218 165 32

gray :: Colour Double
gray = sRGB24 128 128 128

grey :: Colour Double
grey = sRGB24 128 128 128

green :: Colour Double
green = sRGB24 0 128 0

greenyellow :: Colour Double
greenyellow = sRGB24 173 255 47

honeydew :: Colour Double
honeydew = sRGB24 240 255 240

hotpink :: Colour Double
hotpink = sRGB24 255 105 180

indianred :: Colour Double
indianred = sRGB24 205 92 92

indigo :: Colour Double
indigo = sRGB24 75 0 130

ivory :: Colour Double
ivory = sRGB24 255 255 240

khaki :: Colour Double
khaki = sRGB24 240 230 140

lavender :: Colour Double
lavender = sRGB24 230 230 250

lavenderblush :: Colour Double
lavenderblush = sRGB24 255 240 245

lawngreen :: Colour Double
lawngreen = sRGB24 124 252 0

lemonchiffon :: Colour Double
lemonchiffon = sRGB24 255 250 205

lightblue :: Colour Double
lightblue = sRGB24 173 216 230

lightcoral :: Colour Double
lightcoral = sRGB24 240 128 128

lightcyan :: Colour Double
lightcyan = sRGB24 224 255 255

lightgoldenrodyellow :: Colour Double
lightgoldenrodyellow = sRGB24 250 250 210

lightgray :: Colour Double
lightgray = sRGB24 211 211 211

lightgreen :: Colour Double
lightgreen = sRGB24 144 238 144

lightgrey :: Colour Double
lightgrey = sRGB24 211 211 211

lightpink :: Colour Double
lightpink = sRGB24 255 182 193

lightsalmon :: Colour Double
lightsalmon = sRGB24 255 160 122

lightseagreen :: Colour Double
lightseagreen = sRGB24 32 178 170

lightskyblue :: Colour Double
lightskyblue = sRGB24 135 206 250

lightslategray :: Colour Double
lightslategray = sRGB24 119 136 153

lightslategrey :: Colour Double
lightslategrey = sRGB24 119 136 153

lightsteelblue :: Colour Double
lightsteelblue = sRGB24 176 196 222

lightyellow :: Colour Double
lightyellow = sRGB24 255 255 224

lime :: Colour Double
lime = sRGB24 0 255 0

limegreen :: Colour Double
limegreen = sRGB24 50 205 50

linen :: Colour Double
linen = sRGB24 250 240 230

magenta :: Colour Double
magenta = sRGB24 255 0 255

maroon :: Colour Double
maroon = sRGB24 128 0 0

mediumaquamarine :: Colour Double
mediumaquamarine = sRGB24 102 205 170

mediumblue :: Colour Double
mediumblue = sRGB24 0 0 205

mediumorchid :: Colour Double
mediumorchid = sRGB24 186 85 211

mediumpurple :: Colour Double
mediumpurple = sRGB24 147 112 219

mediumseagreen :: Colour Double
mediumseagreen = sRGB24 60 179 113

mediumslateblue :: Colour Double
mediumslateblue = sRGB24 123 104 238

mediumspringgreen :: Colour Double
mediumspringgreen = sRGB24 0 250 154

mediumturquoise :: Colour Double
mediumturquoise = sRGB24 72 209 204

mediumvioletred :: Colour Double
mediumvioletred = sRGB24 199 21 133

midnightblue :: Colour Double
midnightblue = sRGB24 25 25 112

mintcream :: Colour Double
mintcream = sRGB24 245 255 250

mistyrose :: Colour Double
mistyrose = sRGB24 255 228 225

moccasin :: Colour Double
moccasin = sRGB24 255 228 181

navajowhite :: Colour Double
navajowhite = sRGB24 255 222 173

navy :: Colour Double
navy = sRGB24 0 0 128

oldlace :: Colour Double
oldlace = sRGB24 253 245 230

olive :: Colour Double
olive = sRGB24 128 128 0

olivedrab :: Colour Double
olivedrab = sRGB24 107 142 35

orange :: Colour Double
orange = sRGB24 255 165 0

orangered :: Colour Double
orangered = sRGB24 255 69 0

orchid :: Colour Double
orchid = sRGB24 218 112 214

palegoldenrod :: Colour Double
palegoldenrod = sRGB24 238 232 170

palegreen :: Colour Double
palegreen = sRGB24 152 251 152

paleturquoise :: Colour Double
paleturquoise = sRGB24 175 238 238

palevioletred :: Colour Double
palevioletred = sRGB24 219 112 147

papayawhip :: Colour Double
papayawhip = sRGB24 255 239 213

peachpuff :: Colour Double
peachpuff = sRGB24 255 218 185

peru :: Colour Double
peru = sRGB24 205 133 63

pink :: Colour Double
pink = sRGB24 255 192 203

plum :: Colour Double
plum = sRGB24 221 160 221

powderblue :: Colour Double
powderblue = sRGB24 176 224 230

purple :: Colour Double
purple = sRGB24 128 0 128

red :: Colour Double
red = sRGB24 255 0 0

rosybrown :: Colour Double
rosybrown = sRGB24 188 143 143

royalblue :: Colour Double
royalblue = sRGB24 65 105 225

saddlebrown :: Colour Double
saddlebrown = sRGB24 139 69 19

salmon :: Colour Double
salmon = sRGB24 250 128 114

sandybrown :: Colour Double
sandybrown = sRGB24 244 164 96

seagreen :: Colour Double
seagreen = sRGB24 46 139 87

seashell :: Colour Double
seashell = sRGB24 255 245 238

sienna :: Colour Double
sienna = sRGB24 160 82 45

silver :: Colour Double
silver = sRGB24 192 192 192

skyblue :: Colour Double
skyblue = sRGB24 135 206 235

slateblue :: Colour Double
slateblue = sRGB24 106 90 205

slategray :: Colour Double
slategray = sRGB24 112 128 144

slategrey :: Colour Double
slategrey = sRGB24 112 128 144

snow :: Colour Double
snow = sRGB24 255 250 250

springgreen :: Colour Double
springgreen = sRGB24 0 255 127

steelblue :: Colour Double
steelblue = sRGB24 70 130 180

tan :: Colour Double
tan = sRGB24 210 180 140

teal :: Colour Double
teal = sRGB24 0 128 128

thistle :: Colour Double
thistle = sRGB24 216 191 216

tomato :: Colour Double
tomato = sRGB24 255 99 71

turquoise :: Colour Double
turquoise = sRGB24 64 224 208

violet :: Colour Double
violet = sRGB24 238 130 238

wheat :: Colour Double
wheat = sRGB24 245 222 179

white :: Colour Double
white = sRGB24 255 255 255

whitesmoke :: Colour Double
whitesmoke = sRGB24 245 245 245

yellow :: Colour Double
yellow = sRGB24 255 255 0

yellowgreen :: Colour Double
yellowgreen = sRGB24 154 205 50
