{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myManageHook = composeAll [
  className =? "Do"        --> doFloat
  ,className =? "Zenity"   --> doFloat
  ]

myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ gnomeConfig
       { logHook = dynamicLogWithPP (prettyPrinter dbus),
         modMask = mod4Mask,
         normalBorderColor  = "#666",
         focusedBorderColor = "#e3efc0",
         manageHook         = myManageHook <+> manageDocks,
         layoutHook         = avoidStruts $ smartBorders $ myLayout
       }

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput = dbusOutput dbus
    , ppTitle = pangoSanitize
    , ppCurrent = \x -> pangoSanitize (x ++ " ::")
    , ppVisible = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden = const ""
    , ppUrgent = pangoColor "red"
    , ppLayout = const ""
    , ppSep = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs
