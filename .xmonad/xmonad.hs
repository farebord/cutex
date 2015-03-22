import XMonad

import System.Exit

-- Cambio de Workspaces
import XMonad.Actions.CycleWS(nextWS)

-- Manejo de Bordes
import XMonad.Layout.NoBorders(smartBorders)

-- Manejo de Docks. Paneles
import XMonad.Hooks.ManageDocks(avoidStruts,manageDocks)

import XMonad.Hooks.ManageHelpers

-- Fullscreen ventanas
import XMonad.Layout.Fullscreen(fullscreenFull, fullscreenEventHook, fullscreenManageHook)

-- Cambiarle el nombre al WM
import XMonad.Hooks.SetWMName(setWMName)

-- Para xmobar
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, xmobarPP, xmobarColor, xmobarStrip, ppOutput, ppTitle, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppUrgent, ppSep, shorten)

-- Keys
import qualified XMonad.StackSet    as W
import qualified Data.Map           as M

altMask = mod1Mask

myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth = 1

myFgColor = "#6CB359"
myBgColor = "#202020"
myHighlightedFgColor = "#FFFFFF"
myHighlightedBgColor = myFgColor 

myCurrentWsFgColor = myHighlightedFgColor
myCurrentWsBgColor = "#6CB359"
myVisibleWsFgColor = "#6CB359"
myVisibleWsBgColor = "#303030"
myHiddenWsFgColor = "#909090"
myHiddenWsBgColor = "#256b12"
myHiddenEmptyWsFgColor = myHiddenWsFgColor 
myHiddenEmptyWsBgColor = myBgColor 
myUrgentWsFgColor = myBgColor
myUrgentWsBgColor = "#F0F0F0"
myTitleFgColor = myFgColor

myWorkspaces = [" 1 "," 2 "," 3 "," 4 "]

myNormalBorderColor = "#424242"
myFocusedBorderColor = "#646464"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return                 ), spawn $ XMonad.terminal conf)
    , ((modm, xK_c                      ), kill)
    , ((altMask, xK_F4                  ), kill)
    , ((modm, xK_space                  ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space    ), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_Tab                    ), windows W.focusDown)
    , ((modm, xK_j                      ), windows W.focusDown)
    , ((modm, xK_k                      ), windows W.focusUp)
    , ((modm, xK_m                      ), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_j        ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k        ), windows W.swapUp)
    , ((modm, xK_h                      ), sendMessage Shrink)
    , ((modm, xK_l                      ), sendMessage Expand)
    , ((modm, xK_t                      ), withFocused $ windows . W.sink)
    , ((altMask .|. controlMask, xK_Tab ), nextWS)   

    -- Apps spawn
    , ((modm, xK_r                      ), spawn "dmenu_run -nb '#202020' -nf '#909090' -sb '#6CB359' -sf white -p ruN -fn 'Envy Code R:pixelsize=15'")
    , ((0, 0x01008ff12                  ), spawn "amixer set Master toggle")
    , ((0, 0x01008ff13                  ), spawn "amixer set Master 5%+")
    , ((0, 0x01008ff11                  ), spawn "amixer set Master 5%-")
    , ((0, 0x01008ff02                  ), spawn "xbacklight -inc 5")
    , ((0, 0x01008ff03                  ), spawn "xbacklight -dec 5 ")    
    , ((modm, xK_p                      ), spawn "/home/youruser/.bin/switchdisplay.sh")
    , ((modm .|. shiftMask, xK_q        ), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_r        ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    
myLayoutHook = smartBorders $ avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myManageHook = fullscreenManageHook <+> manageDocks 

myStartupHook = setWMName "LG3D"

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaults {
        logHook = dynamicLogWithPP $ xmobarPP 
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor myTitleFgColor "" . shorten 40
            , ppCurrent = xmobarColor myCurrentWsFgColor myCurrentWsBgColor
            , ppVisible = xmobarColor myVisibleWsFgColor myVisibleWsBgColor
            , ppHidden = xmobarColor myHiddenWsFgColor myHiddenWsBgColor
            , ppHiddenNoWindows = xmobarColor myHiddenEmptyWsFgColor myHiddenEmptyWsBgColor
            , ppUrgent = xmobarColor myUrgentWsFgColor myUrgentWsBgColor . xmobarStrip
            , ppSep = xmobarColor myHiddenWsFgColor "" " | "
            }     
    }

defaults = defaultConfig
    { modMask               = mod4Mask
    , focusFollowsMouse     = myFocusFollowsMouse
    , borderWidth           = myBorderWidth
    , workspaces            = myWorkspaces
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , keys                  = myKeys
    , startupHook           = myStartupHook
    , layoutHook            = fullscreenFull myLayoutHook
    , manageHook            = myManageHook 
    , terminal              = myTerminal
    , handleEventHook       = fullscreenEventHook
}
