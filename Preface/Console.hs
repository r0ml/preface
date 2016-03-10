{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Preface.Console where

import Preface.Imports
import Preface.Stringy
import Preface.FFITemplates

csi :: [Int] -> ByteString -> ByteString
csi args code = strConcat ["\ESC[" ,  intercalate ";" (map (asByteString . show) args), code ]

type ConsoleString = ByteString

consolePeach :: ConsoleString
consolePeach = consoleSetExtendedColor 202

consoleAzure :: ConsoleString
consoleAzure = consoleSetExtendedColor  27

treset :: ConsoleString
treset = "\ESC[m" 

charNotEquals :: Char
charNotEquals = '\x2260'

charCheck :: Char
charCheck = '\x2714'

charLeftArrow :: Char
charLeftArrow = '\x2b05'

charRightArrow :: Char
charRightArrow = '\x279c'

neq :: ConsoleString
neq = strConcat [consoleSetAttr ConsoleAttrBold , " ", stringleton (asByte charNotEquals)  , " " , treset]

consoleSetAttr :: ConsoleAttr -> ConsoleString
consoleSetAttr x = csi [fromEnum x] "m"

[enum|ConsoleAttr 
   Bold 1 Faint 2 Normal 22 Italicized 3 NotItalicized 23
   Underlined 4 DoubleUnderlined 21 NotUnderlined 24
   SlowBlink 5 FastBlink 6 NoBlink 25
   Invisible 8 Visible 28
   SwapFgBg 7 UnswapFgBg 27

   DullBlack 30 DullRed 31 DullGreen 32 DullYellow 33 DullBlue 34
   DullMagenta 35 DullCyan 36 DullWhite 37 

   VividBlack 40 VividRed 41 VividGreen 42 VividYellow 43 VividBlue 44
   VividMagenta 45 VividCyan 46 VividWhite 47 
|]

consoleHideCursor :: ConsoleString
consoleHideCursor = "\ESC[?25l"

consoleShowCursor :: ConsoleString
consoleShowCursor = "\ESC[?25h"

consoleSetTitle :: Stringy a => a -> ConsoleString
consoleSetTitle t = asByteString $ strConcat ["\ESC]0;",filter (/= '\007') (asString t),"\007" ]

consoleSetColor :: ConsoleAttr -> ConsoleString
consoleSetColor x = consoleSetAttr x
consoleSetBackgroundColor :: Int -> ConsoleString
consoleSetBackgroundColor x = consoleSetAttr (toEnum (10+ fromEnum x))
consoleSetExtendedColor :: Int -> ConsoleString
consoleSetExtendedColor x = csi [38,5,x] "m"
consoleSetExtendedBackgroundColor :: Int -> ConsoleString
consoleSetExtendedBackgroundColor x = csi [48,5,x] "m"
consoleSetRGB :: Int -> Int -> Int -> ConsoleString
consoleSetRGB r g b = csi [38,2,r,g,b] "m"
consoleSetBackgroundRGB :: Int -> Int -> Int -> ConsoleString
consoleSetBackgroundRGB r g b = csi [48,2,r,g,b] "m"

consoleCursorUp :: Int -> ConsoleString
consoleCursorUp n = csi [n] "A"
consoleCursorDown :: Int -> ConsoleString
consoleCursorDown n = csi [n] "B"
consoleCursorForward :: Int -> ConsoleString
consoleCursorForward n = csi [n] "C"
consoleCursorBackward :: Int -> ConsoleString
consoleCursorBackward n = csi [n] "D"

consoleClearScreen :: ConsoleString
consoleClearScreen = csi [2] "J"
consoleClearToScreenBeginning :: ConsoleString
consoleClearToScreenBeginning = csi [1] "J"
consoleClearToScreenEnd :: ConsoleString
consoleClearToScreenEnd = csi [0] "J"

consoleClearLine :: ConsoleString
consoleClearLine = csi [2] "K"
consoleClearToLineBeginning :: ConsoleString
consoleClearToLineBeginning = csi [1] "K"
consoleClearToLineEnd :: ConsoleString
consoleClearToLineEnd = csi [0] "K"

consoleSetPosition :: Int -> Int -> ConsoleString
consoleSetPosition n m = csi [n+1, m+1] "H"

consoleSetColumn :: Int -> ConsoleString
consoleSetColumn n = csi [n+1] "G"

consoleDeleteChar :: ConsoleString
consoleDeleteChar = csi [1] "P"

consoleInsertChar :: Word8 -> ConsoleString
consoleInsertChar a = strConcat [csi [4] "h", stringleton a]

consoleForwardChar :: ConsoleString
consoleForwardChar = "\ESC[C"

consoleBackwardChar :: ConsoleString
consoleBackwardChar = "\ESC[D"

