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
myDmenu		     = "dmenu_run -fn 'monospace-11'"

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
     toggleStrutsKey XConfig{ modMask = m } = (m, xK_v)

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
	{ ppSep 		    = magenta " . "
	, ppTitleSanitize 	    = xmobarStrip
	, ppCurrent 		    = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
	, ppHidden 		    = white . wrap " " ""
	, ppHiddenNoWindows 	    = lowWhite . wrap " " ""
	, ppUrgent 	            = red . wrap (yellow "!") (yellow "!")
	, ppOrder 	      	    = \[ws, l, _, wins] -> [ws, l, wins] -- Show workspaces, layout name, window name
	, ppExtras 	       	    = [logTitles formatFocused formatUnfocused] -- Show title of all windows
	}
      where
      	formatFocused 	= wrap (white    "[") (white    "]") . magenta . ppWindow
      	formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

	-- Windows should have *some* title which should not exceed a cetain length
	ppWindow :: String -> String
	ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) .shorten 30

	blue, lowWhite, magenta, red, white, yellow :: String -> String
	magenta		= xmobarColor "#ff79c6" ""
	blue		= xmobarColor "#bd93f9" ""
	white		= xmobarColor "#f8f8f2" ""
	yellow		= xmobarColor "#f1fa8c" ""
	red	        = xmobarColor "#ff5555" ""
	lowWhite	= xmobarColor "#bbbbbb" ""

myStartupHook :: X()
myStartupHook = do
	spawnOnce "picom &"
	spawnOnce "nitrogen --restore &"
	spawnOnce "emacs --daemon &"
