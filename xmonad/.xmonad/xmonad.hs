{-__/\\\_______/\\\__/\\\\____________/\\\\____________________________________________________/\\\__
 _\///\\\___/\\\/__\/\\\\\\________/\\\\\\___________________________________________________\/\\\__
  ___\///\\\\\\/____\/\\\//\\\____/\\\//\\\___________________________________________________\/\\\__
   _____\//\\\\______\/\\\\///\\\/\\\/_\/\\\_____/\\\\\_____/\\/\\\\\\____/\\\\\\\\\___________\/\\\__
    ______\/\\\\______\/\\\__\///\\\/___\/\\\___/\\\///\\\__\/\\\////\\\__\////////\\\_____/\\\\\\\\\__
     ______/\\\\\\_____\/\\\____\///_____\/\\\__/\\\__\//\\\_\/\\\__\//\\\___/\\\\\\\\\\___/\\\////\\\__
      ____/\\\////\\\___\/\\\_____________\/\\\_\//\\\__/\\\__\/\\\___\/\\\__/\\\/////\\\__\/\\\__\/\\\__
       __/\\\/___\///\\\_\/\\\_____________\/\\\__\///\\\\\/___\/\\\___\/\\\_\//\\\\\\\\/\\_\//\\\\\\\/\\_
        _\///_______\///__\///______________\///_____\/////_____\///____\///___\////////\//___\///////\//__-}

import Control.Arrow (first)
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Semigroup
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isDialog, isFullscreen)
import XMonad.Hooks.SetWMName
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- Preferences
------------------------------------------------------------------------
windowsKey = mod4Mask 
myBorderWidth = 4 
myFont = "xft:Hack:"
myTerminal = "alacritty"
myBrowser = "/usr/bin/firefox"
mySpotify = "spicetify restore backup apply"
myWhatsapp = "whatsapp-for-linux"
myFileManager = "thunar"
myTorrent="transmission-gtk"
myVM_Manager="vmware"
myEmail="bluemail"
------------------------------------------------------------------------
-- Colors
------------------------------------------------------------------------
--XMONAD
myXMonadBorderColor = "#E8A2AF"
myXMonadFocusColor = "#643FFF"
--XMOBAR
myXMobarCurrentWSColor = "#643FFF"
myXMobarActiveWSColor = "#F28FAD"
myXMobarEmptyWSColor = "#00ffd0"
myXMobarWindowNameColor = "#F28FAD"
--XPromptBaseConfig
myXPbgColor = "#643FFF" --"#292d3e"
myXPfgColor = "#E8A2AF" --"#d0d0d0"
myXPbgHLight = "#FFFFFF" --"#c792ea"
myXPfgHLight = "#89DCEB" --"#000000"
myXPborderColor = "#535974"

------------------------------------------------------------------------
-- Startup Hooks
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "$HOME/.autostart.sh"
  setWMName "LG3D"

------------------------------------------------------------------------
-- Main Function
------------------------------------------------------------------------
main :: IO ()
main = do
  xmobar <- spawnPipe "/usr/bin/xmobar ~/.xmobar.hs"
  xmonad $
    ewmh
      def
        { manageHook = myManageHook <+> manageDocks,
          logHook =
            dynamicLogWithPP $
              namedScratchpadFilterOutWorkspacePP $
                xmobarPP
                  { ppOutput = hPutStrLn xmobar,
                    ppCurrent = xmobarColor myXMobarCurrentWSColor "" . \s -> "<fn=2>\61832</fn>", --" <fn=2>\61713</fn>",
                    ppHidden = xmobarColor myXMobarActiveWSColor "" . \s -> " <fn=10>\61713</fn>",
                    ppHiddenNoWindows = xmobarColor myXMobarEmptyWSColor "",
                    ppTitle = xmobarColor myXMobarWindowNameColor "" . shorten 85,
                    ppSep = "<fc=#212733>  <fn=1> </fn> </fc>"
                    --ppVisible = xmobarColor "#FFFFFF" "",
                    --ppOrder = \(ws : l : _ : _) -> [ws, l]
                  },
          modMask = windowsKey,
          normalBorderColor = myXMonadBorderColor,
          focusedBorderColor = myXMonadFocusColor,
          keys = myKeys,
          mouseBindings = myMouseBindings,
          layoutHook = myLayoutHook,
          workspaces = myWorkspaces,
          terminal = myTerminal,
          borderWidth = myBorderWidth,
          startupHook = myStartupHook,
          handleEventHook = myHandleEventHook
        }

------------------------------------------------------------------------
-- KeyBindings
------------------------------------------------------------------------
myKeys :: XConfig l0 -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = windowsKey}) =
  M.fromList $
    map
      (first $ (,) windowsKey) -- WindownKey + <Key>
       [ -- 
        (xK_c, spawn "xmonad --recompile && xmonad --restart"), -- Recompile & Restarts xmonad
        (xK_Delete, io exitSuccess), -- Quits xmonad
        (xK_x, spawn "archlinux-logout"), -- Logout Screen
        (xK_q, kill1), -- Quit the currently focused client
        (xK_F1, killAll), -- Quit All Windows in WorkSpace
        (xK_v, spawn "pavucontrol"), -- Open Volume Manager
        (xK_s, shellPrompt xPromptConfig), -- Run XPrompt
        (xK_e, spawn myTerminal), -- Run Terminal
        (xK_w, spawn myBrowser), -- Run Browser
        (xK_n, spawn $ myBrowser ++ " -private-window"), -- Run PrivateBrowser
        (xK_a, spawn myFileManager), -- Open File Manager
        (xK_space, sendMessage NextLayout), -- Cycle through the available layout algorithms
        (xK_F2, sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles full width
        (xK_F3, sinkAll), -- Push all windows back into tiling
        (xK_Tab, nextWS), -- Next WorkSpace
        (xK_comma, windows W.focusDown), -- Move focus <-
        (xK_period, windows W.focusUp), -- Move focus ->
        (xK_t, spawn myTorrent), --
        (xK_y, spawn myVM_Manager), --
        (xK_m, spawn myEmail) --
       ] 
  ++ map
      (first $ (,) (windowsKey .|. shiftMask)) -- WindownKey + ShiftKey + <Key>
       [ -- XPrompt
        (xK_s, S.promptSearchBrowser browserXPConfig myBrowser mySearchEngines),
        (xK_c, calcPrompt calcXPConfig "=")
       ] 
  ++ map
      (first $ (,) controlMask) -- Control + <Key>
       [ --
        (xK_e, spawn mySpotify), -- Spotify
        (xK_comma, decWindowSpacing 4), -- Decrease window spacing
        (xK_period, incWindowSpacing 4) -- Increase window spacing
       ]
  ++ map
      (first $ (,) shiftMask) -- Shift + <Key>
       [

       ]
  ++ map
      (first $ (,)  mod1Mask) -- Alt + <Key>
       [ 
        (xK_w, namedScratchpadAction myScratchPads "whatsapp-for-linux"), -- WhatsApp
        (xK_e, namedScratchpadAction myScratchPads "terminal")
       ]
  ++ map
      (first $ (,) 0) -- Only <Key>
       [ -- MultiMediaKeys
        (xF86XK_AudioMute, spawn "amixer -q set Master toggle"),
        (xF86XK_AudioLowerVolume, spawn "amixer -q set Master 10%-"),
        (xF86XK_AudioRaiseVolume, spawn "amixer -q set Master 10%+"),
        (xF86XK_MonBrightnessUp, spawn "xbacklight -inc 5"),
        (xF86XK_MonBrightnessDown, spawn "xbacklight -dec 5"),
        (xF86XK_AudioPlay, spawn "playerctl play-pause"),
        (xF86XK_AudioNext, spawn "playerctl next"),
        (xF86XK_AudioPrev, spawn "playerctl previous"),
        (xF86XK_AudioStop, spawn "playerctl stop"),
        (xK_Print, spawn "scrot '%Y-%m-%d-%s_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir SCREENSHOTS)'") --Screenshot
       ]
  ++   [ -- Misc 
        ((windowsKey, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- Switch windows to WorkSpaces
       ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

------------------------------------------------------------------------
-- MouseBindings
------------------------------------------------------------------------
myMouseBindings conf@(XConfig {XMonad.modMask = windowsKey}) =
  M.fromList $
    map
      (first $ (,) windowsKey) --  WindowsKey + <Key>
      [ (button1, \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
        -- mod-button2, Raise the window to the top of the stack
        (button2, \w -> focus w >> windows W.shiftMaster),
        -- mod-button3, Set the window to floating mode and resize by dragging
        (button3, \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
        -- you may also bind events to the mouse scroll wheel (button4 and button5)

      ]

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces =
  clickable . (map xmobarEscape) $
    ["<fn=5>\61713</fn>", "<fn=5>\61713</fn>", "<fn=5>\61713</fn>", "<fn=5>\61713</fn>", "<fn=5>\61713</fn>"]
  where
    clickable l = ["<action=xdotool key super+" ++ show (i) ++ "> " ++ ws ++ "</action>" | (i, ws) <- zip [1 .. 5] l]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- XPrompt
------------------------------------------------------------------------
xPromptConfig :: XPConfig
xPromptConfig =
  def
    { font = myFont ++ "bold:size=16",
      bgColor = myXPbgColor,
      fgColor = myXPfgColor,
      bgHLight = myXPbgHLight,
      fgHLight = myXPfgHLight,
      borderColor = myXPborderColor,
      promptBorderWidth = 0,
      promptKeymap = xPromptKeymap,
      position = Bottom,
      height = 36,
      historySize = 256,
      historyFilter = id,
      defaultText = [],
      autoComplete = Just 100000, -- set Just 100000 for .1 sec
      showCompletionOnTab = False,
      -- , searchPredicate     = isPrefixOf
      searchPredicate = fuzzyMatch,
      alwaysHighlight = True,
      maxComplRows = Nothing -- set to Just 5 for 5 rows
    }

browserXPConfig :: XPConfig
browserXPConfig =
  xPromptConfig
    { font = myFont ++ "bold:size=20",
      autoComplete = Nothing,
      height = 36,
      bgHLight = "#FFFFFF",
      fgHLight = myXPfgHLight,
      bgColor = "#e78284",
      fgColor = "#303446",
      position = CenteredAt {xpCenterY = 0.1, xpWidth = 0.3},
      historySize = 0
    }

calcXPConfig :: XPConfig
calcXPConfig =
  xPromptConfig
    { font = myFont ++ "bold:size=30",
      autoComplete = Nothing,
      height = 64,
      position = CenteredAt {xpCenterY = 0.3, xpWidth = 0.3},
      bgColor = "#babbf1",
      fgColor = "#292c3c",
      historySize = 0
    }

archwiki, news, reddit, youtube :: S.SearchEngine
archwiki = S.searchEngine "arch" "https://wiki.archlinux.org/index.php?search="
news = S.searchEngine "news" "https://news.google.com/search?q="
reddit = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
youtube = S.searchEngine "yt" "https://www.youtube.com/results?search_query="

mySearchEngines :: S.SearchEngine
mySearchEngines =
  S.namedEngine
    "firefox"
    $ foldr1
      (S.!>)
      [ archwiki,
        news,
        reddit,
        youtube,
        S.google,
        S.wikipedia
      ]

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
    liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where
        f = reverse . dropWhile isSpace

xPromptKeymap :: M.Map (KeyMask, KeySym) (XP ())
xPromptKeymap =
  M.fromList $
    map
      (first $ (,) controlMask) -- control + <key>
      [ (xK_z, killBefore), -- kill line backwards
        (xK_k, killAfter), -- kill line forwards
        (xK_a, startOfLine), -- move to the beginning of the line
        (xK_e, endOfLine), -- move to the end of the line
        (xK_m, deleteString Next), -- delete a character foward
        (xK_b, moveCursor Prev), -- move cursor forward
        (xK_f, moveCursor Next), -- move cursor backward
        (xK_BackSpace, killWord Prev), -- kill the previous word
        (xK_y, pasteString), -- paste a string
        (xK_g, quit), -- quit out of prompt
        (xK_bracketleft, quit)
      ]
      ++ map
        (first $ (,) mod1Mask) --  alt + <key>
        [ (xK_BackSpace, killWord Prev), -- kill the prev word
          (xK_f, moveWord Next), -- move a word forward
          (xK_b, moveWord Prev), -- move a word backward
          (xK_d, killWord Next), -- kill the next word
          (xK_n, moveHistory W.focusUp'), -- move up thru history
          (xK_p, moveHistory W.focusDown') -- move down thru history
        ]
      ++ map
        (first $ (,) 0) -- <key>
        [ (xK_Return, setSuccess True >> setDone True),
          (xK_KP_Enter, setSuccess True >> setDone True),
          (xK_BackSpace, deleteString Prev),
          (xK_Delete, deleteString Next),
          (xK_Left, moveCursor Prev),
          (xK_Right, moveCursor Next),
          (xK_Home, startOfLine),
          (xK_End, endOfLine),
          (xK_Down, moveHistory W.focusUp'),
          (xK_Up, moveHistory W.focusDown'),
          (xK_Escape, quit)
        ]

------------------------------------------------------------------------
-- Space between Tiling Windows
------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 30 10 10 10) True (Border 10 10 10 10) True

------------------------------------------------------------------------
-- Layout Hook
------------------------------------------------------------------------
myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        T.toggleLayouts full $
          mkToggle (NBFULL ?? NOBORDERS ?? MIRROR ?? EOT) myDefaultLayout
  where
    myDefaultLayout =  grid
--      withBorder myBorderWidth tall
        ||| noBorders full
        ||| magnify
        ||| mirror
        ||| noBorders tabs

------------------------------------------------------------------------
-- Tiling Layouts
------------------------------------------------------------------------
--tall =
-- renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Tall</fc>"] $
--    smartBorders $
--      windowNavigation $
--        subLayout [] (smartBorders Simplest) $
--          limitWindows 8 $
--            mySpacing 5 $
--              ResizableTall 1 (3 / 100) (1 / 2) []

grid =
  renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Grid</fc>"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 12 $
            mySpacing 5 $
              mkToggle (single MIRROR) $
                Grid (16 / 10)

mirror =
  renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Mirror</fc>"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 6 $
            mySpacing 5 $
              Mirror $
                ResizableTall 1 (3 / 100) (1 / 2) []

full =
  renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Full</fc>"] $
    Full

magnify =
  renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Magnify</fc>"] $
    magnifier $
      limitWindows 12 $
        mySpacing 8 $
          ResizableTall 1 (3 / 100) (1 / 2) []

tabs =
  renamed [Replace "<fc=#95e6cb><fn=2> \61449 </fn>Tabs</fc>"]
  -- I cannot add spacing to this layout because it will
  -- add spacing between window and tabs which looks bad.
  $
    tabbed shrinkText myTabConfig
  where
    myTabConfig =
      def
        { fontName = myFont ++ "regular:pixelsize=11",
          activeColor = "#292d3e",
          inactiveColor = "#3e445e",
          activeBorderColor = "#292d3e",
          inactiveBorderColor = "#292d3e",
          activeTextColor = "#ffffff",
          inactiveTextColor = "#d0d0d0"
        }

------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "discord" "discord" (appName =? "discord") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "spotify" "spotify" (appName =? "spotify") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "nautilus" "nautilus" (className =? "Org.gnome.Nautilus") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "ncmpcpp" launchMocp (title =? "ncmpcpp") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "whatsapp-for-linux" myWhatsapp (appName =? "whatsapp-for-linux") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "terminal" launchTerminal (title =? "scratchpad") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
  ]
  where
    launchMocp = myTerminal ++ " -t ncmpcpp -e ncmpcpp"
    launchTerminal = myTerminal ++ " -t scratchpad"

------------------------------------------------------------------------
-- Floats
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      className =? "MEGAsync" --> doFloat,
      className =? "mpv" --> doCenterFloat,
      className =? "Gthumb" --> doCenterFloat,
      className =? "Ristretto" --> doCenterFloat,
      className =? "feh" --> doCenterFloat,
      className =? "Galculator" --> doCenterFloat,
      className =? "Gcolor3" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "Downloads" --> doFloat,
      className =? "Save As..." --> doFloat,
      className =? "Xfce4-appfinder" --> doFloat,
      className =? "Org.gnome.NautilusPreviewer" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7),
      className =? "Xdg-desktop-portal-gtk" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7),
      className =? "Thunar" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7),
      className =? "Sublime_merge" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7),
      isFullscreen --> doFullFloat,
      isDialog --> doCenterFloat
    ]
    <+> namedScratchpadManageHook myScratchPads

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
  where
    floating = doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)
