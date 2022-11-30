import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

import Data.Maybe (fromJust)
import Data.Tree
import Data.Monoid
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing

-- Color variables
import Colors.Solarized

myStartupHook = do
  spawn "picom --experimental-backend &" 
  spawn "sxhkd"
  spawnOnce "mechvibes"

myLayout = tiled ||| Mirror tiled ||| Full
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myWorkspaces = [" dev ", " www ", " vlc ", " gam ", " ssc "]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "Gimp"            --> doFloat
    , className =? "notification"    --> doFloat
    , className =? "pinentry-gtk-2"  --> doFloat
    , className =? "splash"          --> doFloat
    , className =? "toolbar"         --> doFloat
    , className =? "Yad"             --> doCenterFloat
    , title =? "Oracle VM VirtualBox Manager"  --> doFloat
    , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 2 )
    , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 2 )
    , className =? "mpv"             --> doShift ( myWorkspaces !! 3 )
    , className =? "Gimp"            --> doShift ( myWorkspaces !! 4 )
    , className =? "Mechvibes"       --> doShift ( myWorkspaces !! 5 )
    , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
    , isFullscreen -->  doFullFloat
    ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myNormColor :: String
myNormColor = colorBack

myFocusColor :: String
myFocusColor = color15

main :: IO ()
main = do
  xmproc0 <- spawnPipe ("xmobar ~/.xmobarrc")
  xmonad $ ewmh $ docks $ def
    { modMask = mod4Mask
    , manageHook = myManageHook <+> manageDocks
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts $ spacingWithEdge 4 $ myLayout
    , terminal = "st"
    , startupHook = myStartupHook
    , focusedBorderColor = myFocusColor
    , normalBorderColor = myNormColor
    , borderWidth = 2
    , focusFollowsMouse = False
    , logHook = dynamicLogWithPP $ xmobarPP
        { ppOrder       = \(ws:l:t:_) -> [ws,t]
        , ppOutput = \x -> hPutStrLn xmproc0 x 
        , ppCurrent = xmobarColor colorBack color04
        , ppVisible = xmobarColor color06 color07 
        , ppHidden = xmobarColor colorBack color07
        , ppHiddenNoWindows = xmobarColor colorBack color03  
        , ppTitle = xmobarColor colorBack color04 . shorten 60 . wrap " " " "
        , ppSep =  "<fc=" ++ color03 ++ "> <fn=1>|</fn> </fc>"
        , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
        , ppExtras  = [windowCount]
        }
    }
