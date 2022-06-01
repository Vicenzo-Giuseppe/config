#!/bin/env bash
# Picom Animations Helper
#----------------- 
red='\033[1;31m'
rset='\033[0m'
grn='\033[1;32m'
ylo='\033[1;33m'
blue='\033[1;34m'
#-----------------
echo -e "$grn  ----------------------------------------------------------------$rset"
echo -e "$blue                       Picom Animations :: $blue                 $rset"
echo -e "$grn  ----------------------------------------------------------------$rset$red\n"
#-----------------
echo -e "$ylo
--animations
  Run animations for window geometry changes (movement and scaling).

--animation-for-open-window
  Which animation to run when opening a window.
  Must be one of none, fly-in, zoom,
  slide-down, slide-up, slide-left, slide-right
  (default: none).

--animation-for-transient-window
  Which animation to run when opening a transient window.
  Must be one of none, fly-in, zoom,
  slide-down, slide-up, slide-left, slide-right
  (default: none).

--animation-for-unmap-window
  Which animation to run when hiding (e.g. minimize) a window.
  Must be one of auto, none, fly-in, zoom,
  slide-down, slide-up, slide-left, slide-right
  slide-in, slide-out
  (default: auto).

--animation-for-workspace-switch-in
  Which animation to run on switching workspace for windows
  comming into view.
  IMPORTANT: window manager must set _NET_CURRENT_DESKTOP
  before doing the hide/show of windows
  Must be one of auto, none, fly-in, zoom,
  slide-down, slide-up, slide-left, slide-right
  slide-in, slide-out
  (default: auto).

--animation-for-workspace-switch-out
  Which animation to run on switching workspace for windows
  going out of view.
  IMPORTANT: window manager must set _NET_CURRENT_DESKTOP
  before doing the hide/show of windows
  Must be one of auto, none, fly-in, zoom,
  slide-down, slide-up, slide-left, slide-right
  slide-in, slide-out
  (default: auto).

--animation-stiffness
  Stiffness (a.k.a. tension) parameter for animation (default: 200.0).

--animation-dampening
  Dampening (a.k.a. friction) parameter for animation (default: 25.0).

--animation-window-mass
  Mass parameter for animation (default: 1.0).

--animation-delta
  The time between steps in animation, in milliseconds. (> 0, defaults to 10).

--animation-force-steps
  Force animations to go step by step even if cpu usage is high
  (default: false)

--animation-clamping
  Whether to clamp animations (default: true)

-i opacity
  Opacity of inactive windows. (0.1 - 1.0)

-e opacity
  Opacity of window titlebars and borders. (0.1 - 1.0)

-G
  Don't draw shadows on DND windows

-b
  Daemonize process.

--show-all-xerrors
  Show all X errors (for debugging).

--config path
  Look for configuration file at the path. Use /dev/null to avoid
  loading configuration file.

--write-pid-path path
  Write process ID to a file.

--shadow-color color
  Color of shadow, as a hex RGB string (defaults to #000000)

--shadow-red value
  Red color value of shadow (0.0 - 1.0, defaults to 0).

--shadow-green value
  Green color value of shadow (0.0 - 1.0, defaults to 0).

--shadow-blue value
  Blue color value of shadow (0.0 - 1.0, defaults to 0).

--inactive-opacity-override
  Inactive opacity set by -i overrides value of _NET_WM_OPACITY.

--inactive-dim value
  Dim inactive windows. (0.0 - 1.0, defaults to 0)

--active-opacity opacity
  Default opacity for active windows. (0.0 - 1.0)

--corner-radius value
  Sets the radius of rounded window corners. When > 0, the compositor
  will round the corners of windows. (defaults to 0).

--rounded-corners-exclude condition
  Exclude conditions for rounded corners.

--mark-wmwin-focused
  Try to detect WM windows and mark them as active.

--shadow-exclude condition
  Exclude conditions for shadows.

--fade-exclude condition
  Exclude conditions for fading.

--mark-ovredir-focused
  Mark windows that have no WM frame as active.

--no-fading-openclose
  Do not fade on window open/close.

--no-fading-destroyed-argb
  Do not fade destroyed ARGB windows with WM frame. Workaround of bugs
  in Openbox, Fluxbox, etc.

--shadow-ignore-shaped
  Do not paint shadows on shaped windows. (Deprecated, use
  --shadow-exclude 'bounding_shaped' or
  --shadow-exclude 'bounding_shaped && !rounded_corners' instead.)

--detect-rounded-corners
  Try to detect windows with rounded corners and don't consider
  them shaped windows. Affects --shadow-ignore-shaped,
  --unredir-if-possible, and possibly others. You need to turn this
  on manually if you want to match against rounded_corners in
  conditions.

--detect-client-opacity
  Detect _NET_WM_OPACITY on client windows, useful for window
  managers not passing _NET_WM_OPACITY of client windows to frame
  windows.

--refresh-rate val
  Specify refresh rate of the screen. If not specified or 0, we
  will try detecting this with X RandR extension.

--vsync
  Enable VSync"
#-----------------
