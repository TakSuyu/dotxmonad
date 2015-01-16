import           XMonad
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.Warp
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet                  as W

import           System.Taffybar.Hooks.PagerHints (pagerHints)

import qualified Data.Map                         as Map

main :: IO ()
main = xmonad . ewmh . pagerHints $ defaultConfig'

defaultConfig' = defaultConfig { keys       = keys'
                               , layoutHook = layoutHook'
                               , manageHook = manageHook' <+> manageDocks
                               , terminal   = "termite"
                               , modMask    = mod4Mask  -- Start key
                               }

layoutHook' = smartBorders . avoidStruts $ normal ||| Grid ||| full
  where
    normal = Tall 1 (3/100) (1/2)
    full = noBorders (fullscreenFull Full)

manageHook' = manageHook defaultConfig <+> composeAll [ isFullscreen --> doFullFloat ]

-- Combine custom keys with the defaults
keys' x = modifiedKeys x `Map.union` keys defaultConfig x

-- Custom key bindings
modifiedKeys conf@( XConfig {XMonad.modMask = modm}) = Map.fromList $

    -- Stop mouse from wiggling when I got my hands on the keyboard switching windows
    [ ((modm, xK_Tab), windows W.focusDown >> warpToWindow 0.99 0.99)

    -- Spawn emacs and have it persist through systemd
    , ((modm, xK_backslash), spawn "emacsclient -c")
    ]

    ++

    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f) >> warpToWindow 0.99 0.99)
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    ++

    -- Swap workspace with active workspace
    [ ((modm .|. controlMask, k), windows $ swapWithCurrent i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    ]

