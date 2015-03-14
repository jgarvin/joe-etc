
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

{-# OPTIONS_GHC -fglasgow-exts #-} -- required for XMonad.Layout.MultiToggle

import XMonad
import System.Exit

import Debug.Trace

import System.IO
import System.Environment
import System.FilePath
import System.Process

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CopyWindow
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.UpdatePointer

-- For sawfish'esq jump-or-exec functionality
import XMonad.ManageHook
import XMonad.Actions.WindowGo

-- For moving pidgin windows that want attention to my current desktop
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WindowBringer

import XMonad.Config.Gnome

import XMonad.Prompt.Shell

import Data.Char
import XMonad.Layout.SimpleDecoration
import qualified XMonad.Layout.MultiToggle as MultiToggle

import XMonad.Layout.Magnifier

import PickBrowser

import Data.Monoid                (mappend)
import XMonad.Layout.MagicFocus   (followOnlyIf)
import Data.List                  (isInfixOf)

import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9", "0"]

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys browser browser_name editor conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_t), spawn $ XMonad.terminal conf)

    -- toggle gnome panel visibility
    --, ((modMask,  xK_f), sendMessage ToggleStruts)

    -- launch emacs
    , ((modMask .|. shiftMask,  xK_w), runOrRaiseNext editor (className =? "Emacs" <||> className =? "emacs" <||> className =? "Emacs23"))

    , ((modMask .|. shiftMask,  xK_b), runOrRaiseNext browser (className =? browser_name <||> className =? (capitalizeWord browser_name)))

    -- close focused window
    , ((modMask , xK_x     ), kill)

    -- Rotate through the available layout algorithms
    , ((modMask,               xK_bracketright ), sendMessage NextLayout)

    -- Toggle decorations
    -- , ((modMask,               xK_d ), sendMessage (MultiToggle.Toggle DECORATIONS) )

    -- Toggle zoom
    -- , ((modMask,               xK_z ), sendMessage (MultiToggle.Toggle MAGNIFICATION) )

    -- Resize viewed windows to the correct size
    --, ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_e     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_h     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_e     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_h     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_n    ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_i     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask .|. shiftMask,               xK_h     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)

    -- Move screens
    , ((modMask               , xK_BackSpace ), prevScreen)
    , ((modMask .|. shiftMask , xK_BackSpace ), shiftPrevScreen >> prevScreen)
    , ((modMask               , xK_space     ), nextScreen)
    , ((modMask .|. shiftMask , xK_space     ), shiftNextScreen >> nextScreen)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++

    -- --
    -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Left, xK_Right, xK_Up] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "Brood War"      --> doFloat
    , className =? "Rti"            --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "Do"             --> doIgnore
    , className =? "gnome-panel"                        --> doFloat
    , (stringProperty "WM_NAME")   =? "VLC"             --> doFullFloat
    , (stringProperty "WM_NAME")   =? "SpreadEditor"    --> doFullFloat
    , (stringProperty "WM_NAME")   =? "Insert Spread"   --> doFullFloat
    , (stringProperty "WM_NAME")   =? "Choose Contract" --> doFullFloat
    , appName   =? "VLC (XVideo output)" --> doFloat
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    , isFullscreen                  --> doFullFloat]

main = do
  home_folder <- getEnv "HOME"
  editor <- return (home_folder ++ "/etc/bin/launch-emacs")
  browser_name <- return preferred_browser
  xmonad $ defaults editor home_folder ((head . lines) browser_name)

-- decoFunc = simpleDeco shrinkText (defaultTheme { decoWidth = 9999999, fontName = "-*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*", inactiveColor = "black", activeColor = "black", activeTextColor = "red", inactiveTextColor = "green" } )
-- decoFunc = simpleDeco

myTheme = defaultTheme { decoWidth = -1, fontName = "-*-helvetica-bold-r-*-*-18-*-*-*-*-*-*-*", inactiveColor = "black", activeColor = "black", activeTextColor = "red", inactiveTextColor = "green" }
          
-- myDeco = simpleDeco shrinkText myTheme (layoutHook gnomeConfig)

-- data DECORATIONS = DECORATIONS deriving (Read, Show, Eq, Typeable)
-- instance MultiToggle.Transformer DECORATIONS Window where
--     transform _ x k = k (myDeco x) (\(myDeco x') -> x')

-- data DECORATIONS = DECORATIONS deriving (Read, Show, Eq, Typeable)
-- instance MultiToggle.Transformer DECORATIONS Window where
--     transform _ x k = k (simpleDeco x) (\(simpleDeco x') -> x)

-- data MAGNIFICATION = MAGNIFICATION deriving (Read, Show, Eq, Typeable)
-- instance MultiToggle.Transformer MAGNIFICATION Window where
-- 		 transform _ x k = k (magnifiercz 1.2 x)

-- Focus follows mouse is annoying for skype, when you mouse over
-- the video window it brings up the buddy list
followEventHook event = followOnlyIf (notSkype) event
  where notSkype =
          runQuery (fmap (not . ("Call with" `isInfixOf`)) (stringProperty "WM_NAME")) (ev_window event)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults editor home_folder browser_name = gnomeConfig {
      -- simple stuff
        terminal           = joinPath [home_folder, "etc/bin/homeshell"],
        focusFollowsMouse  = True,
        borderWidth        = 5,
        modMask            = controlMask .|. mod1Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#0000ff",

      -- key bindings
        keys               = myKeys browser_name browser_name editor,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = smartBorders
                             $ avoidStruts
                             -- $ myDeco,
                             $ simpleDeco shrinkText myTheme
                             $ layoutHook gnomeConfig,
        handleEventHook    = handleEventHook gnomeConfig `mappend` followEventHook,
        manageHook         = myManageHook <+> manageDocks <+> manageHook gnomeConfig,
        logHook            = ewmhDesktopsLogHook >> updatePointer (Relative 0.5 0.5)
    }
