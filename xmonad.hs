import XMonad

-- Basics
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

-- Layouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

main :: IO ()
main = xmonad $ def
	{ modMask = mod4Mask	-- Change mod key
	, layoutHook = myLayout	-- Use custom layout
	}
	`additionalKeysP`
	[ ("M-b", spawn "firefox")
	, ("M-s", unGrab *> spawn "scrot -s")
	]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio -- Magnification on threeCol
    -- threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1		-- Number of masters
    ratio  = 1/2	-- Proportion
    delta = 3/100	-- Pecent to increment when resizing
