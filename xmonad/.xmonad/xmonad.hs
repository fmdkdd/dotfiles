{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Actions.Warp
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeys)

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W

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
    (xmonad $ gnomeConfig
     { logHook = dynamicLogWithPP (prettyPrinter dbus),
       modMask = mod4Mask,
       normalBorderColor  = "#073642", -- dark
       -- normalBorderColor  = "#eee8d5", -- light
       focusedBorderColor = "#268bd2",
       manageHook         = myManageHook <+> manageDocks,
       layoutHook         = avoidStruts $ smartBorders $ myLayout
     }
     `additionalKeys`
     ([
         ((mod4Mask, xK_h), windows W.focusUp)
      , ((mod4Mask, xK_k), windows W.focusDown)
      , ((mod4Mask .|. shiftMask, xK_h), windows W.swapUp)
      , ((mod4Mask .|. shiftMask, xK_k), windows W.swapDown)
      , ((mod4Mask, xK_j), sendMessage Shrink)
      , ((mod4Mask .|. shiftMask, xK_Delete), spawn "gnome-session-quit --power-off")
      , ((mod4Mask .|. shiftMask, xK_q), spawn "gnome-session-quit")
      , ((mod4Mask, xK_w), warpToWindow 0.5 0.5)
      ]
      ++
      [((m .|. mod4Mask, key), do
           screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_n, xK_e, xK_i] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]))

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput = dbusOutput dbus
    , ppTitle = pangoSanitize
    , ppCurrent = \x -> pangoSanitize (x ++ " ::")
    , ppVisible = pangoColor "#268bd2" . wrap "(" ")" . pangoSanitize
    , ppHidden = const ""
    , ppUrgent = pangoColor "#d33682"
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
