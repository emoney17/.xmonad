import XMonad

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

-- Layouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

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
	}
	`additionalKeysP`
	[ ("M-b", spawn "firefox")
	, ("M-e", spawn "emacs")
	, ("M-s", unGrab *> spawn "scrot -s")
	]

myManageHook :: ManageHook
myManageHook = composeAll
	[ className =? "Gimp" --> doFloat
	, isDialog	      --> doFloat
	]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
    where
    	threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio -- Magnification on threeCol
    	-- threeCol = ThreeColMid nmaster delta ratio
    	tiled 		= Tall nmaster delta ratio
    	nmaster 	= 1	-- Number of masters
    	ratio  		= 1/2	-- Proportion
    	delta 		= 3/100	-- Pecent to increment when resizing

myXmobarPP :: PP
myXmobarPP = def
	{ ppSep 		= magenta " . "
	, ppTitleSanitize 	= xmobarStrip
	, ppCurrent 		= wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
	, ppHidden 		= white . wrap " " ""
	, ppHiddenNoWindows 	= lowWhite . wrap " " ""
	, ppUrgent 		= red . wrap (yellow "!") (yellow "!")
	, ppOrder 		= \[ws, l, _, wins] -> [ws, l, wins] -- Show workspaces, layout name, window name
	, ppExtras 		= [logTitles formatFocused formatUnfocused] -- Show title of all windows
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
	red		= xmobarColor "#ff5555" ""
	lowWhite	= xmobarColor "#bbbbbb" ""
