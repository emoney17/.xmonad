import XMonad
import XMonad.Operations
import System.Exit

-- Actions
import XMonad.Actions.WithAll

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- Layouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

myTerminal	     = "alacritty"
myBrowser	     = "firefox"
myEmacs		     = "emacsclient -c -a 'emacs' --eval '(dashboard-open)'"
myDmenu		     = "dmenu_run -fn 'Hack Nerd Font-9'"

myBorderWidth        = 2
myNormalBorderColor  = "#666666"
myFocusedBorderColor = "#c3e88d"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
   where
     toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
     toggleStrutsKey XConfig{ modMask = m } = (m, xK_v) -- Toggle bar

myConfig = def
	{ modMask = mod4Mask	-- Change mod key
	, layoutHook = myLayout	-- Use custom layout
	, manageHook = myManageHook -- Match on certain windows
	, startupHook = myStartupHook
	, terminal = myTerminal
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
	}
	`additionalKeysP`
	[ ("M-b", 	        spawn myBrowser)
	, ("M-<Return>", 	spawn myTerminal)
	, ("M-e", 	        spawn myEmacs)
	, ("M-d",	        spawn myDmenu)
	, ("M-q", 		kill)
	, ("M-S-r", 		spawn "xmonad --restart")
	, ("M-s", 		unGrab *> spawn "scrot -s")
	, ("M-S-<Space>", 	sinkAll)
	, ("M-S-e", 		io (exitWith ExitSuccess))
	]

myManageHook :: ManageHook
myManageHook = composeAll
	[ className =? "Gimp" --> doFloat
	, isDialog	      --> doFloat
	]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
    where
    	threeCol = renamed [Replace "ThreeCol"]
		$ magnifiercz' 1.3
		$ ThreeColMid nmaster delta ratio -- Magnification on threeCol
    	-- threeCol = ThreeColMid nmaster delta ratio
    	tiled 		= Tall nmaster delta ratio
    	nmaster 	= 1	-- Number of masters
    	ratio  		= 1/2	-- Proportion
    	delta 		= 3/100	-- Pecent to increment when resizing

myXmobarPP :: PP
myXmobarPP = def
	{ ppSep =  "<fc=#666666> | </fc>"
	, ppTitleSanitize 	    = xmobarStrip
	, ppCurrent 		    = xmobarColor "#c3e88d" "" . wrap " " "" . xmobarBorder "Bottom" "#c3e88d" 2
        , ppVisible = xmobarColor "#c3e88d" "" . wrap " " " "
        , ppHidden = xmobarColor "#82AAFF" "" . wrap " " ""
        , ppHiddenNoWindows = xmobarColor "#F07178" "" . wrap " " ""
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }

myStartupHook :: X()
myStartupHook = do
	spawnOnce "emacs --daemon &"
	spawnOnce "picom &"
	spawnOnce "nitrogen --restore &"
