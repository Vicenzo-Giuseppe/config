{-__/\\\_______/\\\__/\\\\____________/\\\\____________________________________________________/\\\__
 _\///\\\___/\\\/__\/\\\\\\________/\\\\\\___________________________________________________\/\\\__
  ___\///\\\\\\/____\/\\\//\\\____/\\\//\\\___________________________________________________\/\\\__
   _____\//\\\\______\/\\\\///\\\/\\\/_\/\\\_____/\\\\\_____/\\/\\\\\\____/\\\\\\\\\___________\/\\\__
    ______\/\\\\______\/\\\__\///\\\/___\/\\\___/\\\///\\\__\/\\\////\\\__\////////\\\_____/\\\\\\\\\
     ______/\\\\\\_____\/\\\____\///_____\/\\\__/\\\__\//\\\_\/\\\__\//\\\___/\\\\\\\\\\___/\\\////\\\__
      ____/\\\////\\\___\/\\\_____________\/\\\_\//\\\__/\\\__\/\\\___\/\\\__/\\\/////\\\__\/\\\__\/\\\
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
import XMonad.Actions.CycleWS (WSType (EmptyWS, AnyWS, WSIs),  moveTo, nextWS, shiftTo)
import XMonad.Actions.DynamicWorkspaces
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
import XMonad.Prompt.DirExec
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runInTerm, runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce


------------------------------------------------------------------------
-- Preferences
------------------------------------------------------------------------
windowsKey = mod4Mask

myTerminal = "alacritty"

myBrowser = "/usr/bin/firefox"

myFileManager = "thunar"

myEmail = "mailspring"

myFont = "xft:mononoki Nerd Font:"

myWhatsapp = "whatstux "

myTelegram = "telegram-desktop"

mySpotify = ""

myTorrent = "transmission-gtk"

myVM = "vmware"

myAPITestManager = "insomnia"

myBorderWidth = 0

myFocusFollowsMouse = False

------------------------------------------------------------------------
-- Colors
------------------------------------------------------------------------
myXMonadBorderColor = "#E8A2AF"

myXMonadFocusColor = "#643FFF"

myXMobarCurrentWSColor = "#643FFF"

myXMobarActiveWSColor = "#F28FAD"

myXMobarEmptyWSColor = "#00ffd0"

myXMobarWindowNameColor = "#F28FAD"

myXPromptbgColor = "#643FFF"

myXPromptfgColor = "#E8A2AF"

myXPromptbgHLight = "#FFFFFF"

myXPromptfgHLight = "#89DCEB"

myXPromptborderColor = "#535974"

------------------------------------------------------------------------
-- Startup Hooks
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "$HOME/.sh/.autostart.sh"
  setWMName "LG3D"

------------------------------------------------------------------------
-- Main Function
------------------------------------------------------------------------
main :: IO ()
main = do
  xmobar <- spawnPipe "/usr/bin/xmobar ~/.sh/.xmobar.hs"
  xmonad $
     ewmh $
      def
        { manageHook = myManageHook <+> manageDocks,
          logHook =
            dynamicLogWithPP $
                xmobarPP
                  { ppOutput = hPutStrLn xmobar,
                    ppCurrent = xmobarColor myXMobarCurrentWSColor "" . \s -> "<fn=2>\61832</fn>",
                    ppHidden = xmobarColor myXMobarActiveWSColor "" . \s -> " <fn=10>\61713</fn>",
                    ppHiddenNoWindows = xmobarColor myXMobarEmptyWSColor "",
                    ppTitle = xmobarColor myXMobarWindowNameColor "" . shorten 85,
                    ppSep = "<fc=#212733>  <fn=1> </fn> </fc>"
                  },
          modMask = windowsKey,
          normalBorderColor = myXMonadBorderColor,
          focusFollowsMouse = myFocusFollowsMouse,
          focusedBorderColor = myXMonadFocusColor,
          keys = myKeys,
          mouseBindings = myMouseBindings,
          layoutHook = myLayoutHook,
          workspaces = myWorkspaces,
          terminal = myTerminal,
          borderWidth = myBorderWidth,
          startupHook = myStartupHook,
          handleEventHook = fullscreenEventHook <+> myHandleEventHook
          }

------------------------------------------------------------------------
-- KeyBindings
------------------------------------------------------------------------
myKeys :: XConfig l0 -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = windowsKey}) =
  M.fromList $
    map
      (first $ (,) windowsKey) -- WindowsKey + <Key>
      [ (xK_q, kill1),
        (xK_w, spawn myBrowser),
        (xK_e, spawn myTerminal),
        (xK_r, spawn $ runTerminal ++ "~/.sh/xmdr.sh"),
        (xK_t, sinkAll),
        (xK_a, spawn myFileManager),
        (xK_s, S.promptSearchBrowser browserXPConfig myBrowser mySearchEngines),
        (xK_d, spawn myEmail),
        (xK_f, spawn myVM),
        (xK_g, spawn myTorrent),
        (xK_z, shellPrompt xPromptConfig),
        (xK_x, dirExecPromptNamed xPromptConfig spawn "/home/vicenzo/.sh/" "Run:Scripts $ "),
        (xK_v, spawn "pavucontrol"),
        (xK_Escape, spawn "archlinux-logout"),
        (xK_Tab, nextWS),
        (xK_space, sendMessage NextLayout),
        (xK_comma, windows W.focusDown),
        (xK_period, windows W.focusUp)
      ]
      ++ map
        (first $ (,) (windowsKey .|. shiftMask)) -- WindownKey + ShiftKey + <Key>
        [ (xK_q, killAll),
          (xK_w, spawn $ myBrowser ++ " -private-window"),
          (xK_e, spawn $ myTerminal ++ " --hold --working-directory ~/Code -e ~/.local/bin/lvim"),
          (xK_a, calcPrompt calcXPConfig "="),
          (xK_s, spawn mySpotify),
          (xK_d, spawn myAPITestManager),
          (xK_Escape, io exitSuccess),
          (xK_Tab, shiftTo Next AnyWS)
        ]
      ++ map
        (first $ (,) controlMask) -- Control + <Key>
        [ (xK_comma, decWindowSpacing 4),
          (xK_period, incWindowSpacing 4)
        ]
      ++ map
        (first $ (,) shiftMask) -- Shift + <Key>
        []
      ++ map
        (first $ (,) mod1Mask) -- Alt + <Key>
        [ (xK_q, spawn myTelegram),
          (xK_w, spawn myWhatsapp),
          (xK_d, spawn "discord"),
          (xK_e, namedScratchpadAction myScratchPads "terminal")
        ]
      ++ map
        (first $ (,) 0) -- Only <Key>
        [ (xF86XK_AudioMute, spawn "amixer -q set Master toggle"),
          (xF86XK_AudioLowerVolume, spawn "amixer -q set Master 10%-"),
          (xF86XK_AudioRaiseVolume, spawn "amixer -q set Master 10%+"),
          (xF86XK_MonBrightnessUp, spawn "xbacklight -inc 5"),
          (xF86XK_MonBrightnessDown, spawn "xbacklight -dec 5"),
          (xF86XK_AudioPlay, spawn "playerctl play-pause"),
          (xF86XK_AudioNext, spawn "playerctl next"),
          (xF86XK_AudioPrev, spawn "playerctl previous"),
          (xF86XK_AudioStop, spawn "playerctl stop"),
          (xK_Print, spawn "scrot '%Y-%m-%d-%s_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir SCREENSHOTS)'")
        ]
      ++ [ ((shift .|. windowsKey, k), windows $ f i)
           | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5], -- WindowsKey + 1 .. 5 # Go to WorkSpace
             (f, shift) <- [(W.greedyView, 0), (W.shift, shiftMask)] --  WindowsKey + ShiftKey + 1 .. 5 # Move Window to WorkSpace
         ]
      ++ [ ((windowsKey, xK_c), dirExecPromptNamed xPromptConfig fn "/home/vicenzo/.sh/" "RunTerminal:Scripts $ ") | (fn) <- [(runInTerm " --hold ")]
         ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
    runTerminal = myTerminal ++ " --hold -e "

------------------------------------------------------------------------
-- MouseBindings
------------------------------------------------------------------------
myMouseBindings conf@(XConfig {XMonad.modMask = windowsKey}) =
  M.fromList $
    map
      (first $ (,) windowsKey) --  WindowsKey + <Key>
      [ (button1, \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
        (button2, \w -> focus w >> windows W.shiftMaster),
        (button3, \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
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
      bgColor = myXPromptbgColor,
      fgColor = myXPromptfgColor,
      bgHLight = myXPromptbgHLight,
      fgHLight = myXPromptfgHLight,
      borderColor = myXPromptborderColor,
      promptBorderWidth = 0,
      promptKeymap = xPromptKeymap,
      position = Bottom,
      height = 36,
      historySize = 256,
      historyFilter = id,
      defaultText = [],
      autoComplete = Just 100000,
      showCompletionOnTab = False,
      searchPredicate = fuzzyMatch,
      alwaysHighlight = True,
      maxComplRows = Nothing
    }

browserXPConfig :: XPConfig
browserXPConfig =
  xPromptConfig
    { font = myFont ++ "bold:size=20",
      autoComplete = Nothing,
      height = 36,
      bgHLight = "#FFFFFF",
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

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
    liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where
        f = reverse . dropWhile isSpace

archwiki, news, reddit, youtube, google :: S.SearchEngine
archwiki = S.searchEngine "aw" "https://wiki.archlinux.org/index.php?search="
news = S.searchEngine "n" "https://news.google.com/search?q="
reddit = S.searchEngine "r" "https://www.reddit.com/search/?q="
youtube = S.searchEngine "yt" "https://www.youtube.com/results?search_query="
google = S.searchEngine "gg" "https://www.google.com/search?q="

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
        google,
        S.wikipedia
      ]

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
    myDefaultLayout =
      grid
        ||| full
        ||| Main.magnify
        ||| mirror
        ||| tabs

------------------------------------------------------------------------
-- Tiling Layouts
------------------------------------------------------------------------
grid =
  renamed [Replace " <fc=#b7bdf8><fn=2> \61449 </fn>Grid</fc>"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 12 $
            mySpacing 5 $
              mkToggle (single MIRROR) $
                Grid (16 / 10)

mirror =
  renamed [Replace " <fc=#b7bdf8><fn=2> \62861 </fn>Mirror</fc>"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 6 $
            mySpacing 5 $
              Mirror $
                ResizableTall 1 (3 / 100) (1 / 2) []

full =
  renamed [Replace " <fc=#b7bdf8><fn=2> \62556 </fn>Full</fc>"] $
    Full

magnify =
  renamed [Replace " <fc=#b7bdf8><fn=2> \61618 </fn>Magnify</fc>"] $
    magnifier $
      limitWindows 12 $
        mySpacing 8 $
          ResizableTall 1 (3 / 100) (1 / 2) []

tabs =
  renamed [Replace "<fc=#b7bdf8><fn=2> \62162 </fn>Tabs</fc>"] $
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
    NS "whatstux" myWhatsapp (appName =? "whatstux") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "telegram" myTelegram (title =? "Telegram") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "insomnia" myAPITestManager (title =? "Insomnia") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "vieb" "vieb" (title =? "Vieb") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7),
    NS "terminal" launchTerminal (appName =? "float-window") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
  ]
  where
    launchTerminal = myTerminal ++ " --class float-window"

------------------------------------------------------------------------
-- Floats
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      className =? "dialog" --> doFloat,
      className =? "Downloads" --> doFloat,
      className =? "Save As..." --> doFloat,
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
