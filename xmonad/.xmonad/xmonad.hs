-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)

-- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask -- Window key

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "qutebrowser"

myEditor :: String
myEditor = "emacsclient -c -a emacs"

myBorderWidth :: Dimension
myBorderWidth = 1

myNormColor :: String
myNormColor = "#1D2330"

myFocusColor :: String
myFocusColor = "#e1acff"

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount =
  gets
    $ Just
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce
    "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 22 &"
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "DISPLAY=:0 feh --bg-scale --randomize ~/Pictures/.wallpaper/* &"
  spawnOnce "picom -f &"
  spawnOnce "copyq &"
  setWMName "LG3D"

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch = "emacsclient -a='' -nc --frame-parameters='(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = customFloating $ W.RationalRect l t w h
                where
                  h = 0.9
                  w = 0.9
                  t = 0.95 -h
                  l = 0.95 -w

mySpacing
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing'
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall = renamed [Replace "tall"] $ limitWindows 12 $ mySpacing 4 $ ResizableTall
  1
  (3 / 100)
  (4.5 / 7)
  []
magnify =
  renamed [Replace "magnify"]
    $ magnifier
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3 / 100) (1 / 2) []
monocle = renamed [Replace "monocle"] $ limitWindows 20 Full
floats =
  renamed [Replace "floats"]
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 20 simplestFloat
grid =
  renamed [Replace "grid"]
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)
spirals = renamed [Replace "spirals"] $ mySpacing' 8 $ spiral (6 / 7)
threeCol =
  renamed [Replace "threeCol"] $ limitWindows 7 $ mySpacing' 4 $ ThreeCol
    1
    (3 / 100)
    (1 / 2)
threeRow =
  renamed [Replace "threeRow"]
    $ limitWindows 7
    $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
    $ Mirror
    $ ThreeCol 1 (3 / 100) (1 / 2)
tabs = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
                                $ tabbed shrinkText myTabConfig
 where
  myTabConfig = def { fontName = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                    , activeColor         = "#292d3e"
                    , inactiveColor       = "#3e445e"
                    , activeBorderColor   = "#292d3e"
                    , inactiveBorderColor = "#292d3e"
                    , activeTextColor     = "#ffffff"
                    , inactiveTextColor   = "#d0d0d0"
                    }

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def { swn_font    = "xft:Sans:bold:size=60"
                       , swn_fade    = 1.0
                       , swn_bgcolor = "#000000"
                       , swn_color   = "#FFFFFF"
                       }

-- The layout hook
myLayoutHook =
  avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ mkToggle
    (NBFULL ?? NOBORDERS ?? EOT)
    myDefaultLayout
 where
               -- I've commented out the layouts I don't use.
  myDefaultLayout =
    tall
      ||| magnify
      ||| noBorders monocle
      ||| floats
                    -- ||| grid
      ||| noBorders tabs
                                 -- ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
 where
  doubleLts '<' = "<<"
  doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces =
  clickable
    . (map xmobarEscape)
    $ ["dev", "www", "sys", "chat", "game", "doc", "mus", "vid", "misc"]
 where
  clickable l =
    [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>"
    | (i, ws) <- zip [1 .. 9] l
    , let n = i
    ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out
     -- the full name of my workspaces.
      [ className =? "htop" --> doShift (myWorkspaces !! 7)
      , className =? "firefox" --> doShift (myWorkspaces !! 1)
      , className =? "discord" --> doShift (myWorkspaces !! 3)
      , className =? "Steam" --> doShift (myWorkspaces !! 4)
      , className =? "emacs" --> doShift (myWorkspaces !! 0)
      , className =? "Spotify" --> doShift (myWorkspaces !! 6)
      , className =? "copyq" --> doFloat
      , className =? "mpv" --> doFloat
     -- , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     -- , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
      ]
    <+> namedScratchpadManageHook myScratchPads

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
  [ ("M-C-r"            , spawn "xmonad --recompile")      -- Recompiles xmonad
  , ("M-S-r"            , spawn "xmonad --restart")        -- Restarts xmonad
  , ("M-S-q"            , io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal
  , ("M-<Return>"       , spawn myTerminal)

    -- Windows
  , ("M-S-c"            , kill1)                           -- Kill the currently focused client
  , ("M-S-a"            , killAll)                         -- Kill all windows on current workspace

    -- Floating windows
  , ("M-f"              , sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
  , ("M-t"              , withFocused $ windows . W.sink)  -- Push floating window back to tile
  , ("M-S-t"            , sinkAll)                       -- Push ALL floating windows to tile

    -- Windows navigation
  , ("M-m"              , windows W.focusMaster)     -- Move focus to the master window
  , ("M-j"              , windows W.focusDown)       -- Move focus to the next window
  , ("M-k"              , windows W.focusUp)         -- Move focus to the prev window
        --, ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
  , ("M-S-j"            , windows W.swapDown)      -- Swap focused window with next window
  , ("M-S-k"            , windows W.swapUp)        -- Swap focused window with prev window
  , ("M-<Backspace>"    , promote)         -- Moves focused window to master, others maintain order
  , ("M1-S-<Tab>"       , rotSlavesDown)      -- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>"       , rotAllDown)         -- Rotate all the windows in the current stack
        --, ("M-S-s", windows copyToAll)
  , ("M-C-s"            , killAllOtherCopies)

        -- Layouts
  , ("M-<Tab>"          , sendMessage NextLayout)                -- Switch to next layout
  , ("M-C-M1-<Up>"      , sendMessage Arrange)
  , ("M-C-M1-<Down>"    , sendMessage DeArrange)
  , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-S-<Space>"      , sendMessage ToggleStruts)         -- Toggles struts
  , ("M-S-n"            , sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
  , ("M-<KP_Multiply>"  , sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
  , ("M-<KP_Divide>"    , sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
  , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
  , ("M-S-<KP_Divide>"  , decreaseLimit)                -- Decrease number of windows
  , ("M-h"              , sendMessage Shrink)                       -- Shrink horiz window width
  , ("M-l"              , sendMessage Expand)                       -- Expand horiz window width
  , ("M-C-j"            , sendMessage MirrorShrink)               -- Shrink vert window width
  , ("M-C-k"            , sendMessage MirrorExpand)               -- Exoand vert window width

    -- Scratchpads
        -- , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-<Space>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "emacs-scratch")

    -- Firefox
  , ("C-e f"            , spawn "firefox")

    -- Qutebrowser
  , ("C-e q"            , spawn "qutebrowser")

    -- ytfzf
  , ("C-e y"            , spawn "ytfzf -D")

    -- change background
  , ("C-e b", spawn "/home/vladovidiu/.config/feh.sh")

    -- emacs everywhere
  , ("C-e v"            , spawn "doom everywhere")

    -- Emacs (CTRL-e followed by a key)
  , ("C-e e"            , spawn "emacsclient -c -a ''")                            -- start emacs
  ]

 where
  nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
  nonEmptyNonNSP =
    WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc <- spawnPipe "xmobar -x 0 /home/vladovidiu/.config/xmobar/xmobarrc"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
