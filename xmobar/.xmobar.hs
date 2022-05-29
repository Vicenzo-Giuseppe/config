Config { font    = "xft:Ubuntu:weight=bold:pixelsize=13:antialias=true:hinting=true,Font Awesome 6 Free Regular:pixelsize=14"
       , additionalFonts = [ "xft:Mononoki Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=13"
                           , "xft:FontAwesome:pixelsize=13"                         -- Weather
                           , "xft:Font Awesome 6 Free Solid:pixelsize=10"           -- MPD music player
                           , "xft:Font Awesome 6 Free Regular:pixelsize=13"         -- Tiling
                           , "xft:Ubuntu:pixelsize=13:antialias=true:hinting=true"  -- Networks
                           , "xft:Font Awesome 6 Brands:pixelsize=13"               -- Brand Icons
                           ]
       , bgColor = "#212733"
       , fgColor = "#ff6c6b"
       , position = TopSize L 100 30
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [
                    -- Time and date
                      Run Date "<fc=#bae67e><fn=5> </fn> %a, %d | %b</fc> <fc=#212733> | |</fc> <fc=#f27983><fn=5></fn> %H:%M </fc>" "date" 50

                      -- Cpu usage in percent
                    , Run Cpu ["-t", "<fn=2> </fn><total>% |","-H","50","--high","red"] 20

                      -- Cpu core temperature monitor
                    , Run MultiCoreTemp
                      ["-t", "<fn=2></fn> <avg> °C",
                        "-L", "20", "-H", "80"
                      ] 20

                      -- Ram used in percent
                    , Run Memory ["-t", "<fn=2> </fn> <usedratio>% | <used>M"] 20

                      -- Disk space free | System and external HDD's
                    , Run DiskU [("/", "<fn=2></fn>  SSD: <free>")] [] 60
                               -- , ("/mnt/4TBZ", "Z: <free>")
                               -- , ("/mnt/4TBX", "X: <free> free")] [] 60

                    -- System update (with yay)
                    , Run Com ".xmonad/scripts/xmobar/sysupdate" [] "sysupdate" 36000

                    -- UnsafeStdinReader
                    , Run UnsafeStdinReader
        
                    -- Volume
                    , Run Volume "default" "Master"
                      ["-t","<status> <volume>%"
                      , "--"
                      , "-O", ""
                      , "-o",""
                      ] 10

                    -- MPD Music server
                    , Run MPD ["-t","<action=`mpc prev`><fc=#73d0ff><fn=4>\xf048</fn></fc></action> <action=`mpc play`><fc=#bae67e><fn=2>\xf144</fn></fc></action> <action=`mpc next`><fc=#73d0ff><fn=4>\xf051</fn></fc></action> <fc=#212733>a</fc> <artist> - <title>","-h","127.0.0.1","-p","6601"] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<fc=#212733> | | </fc> <fc=#73d0ff><fn=2></fn></fc> \
       \<fc=#212733> | </fc> %UnsafeStdinReader% \
       \<fc=#212733> | </fc>  <fc=#73d0ff> <action=`alacritty -e htop`> %cpu% %multicoretemp%  </action> </fc> \
       \<fc=#212733> | </fc>  <fc=#ff79c6> <action=`alacritty -e htop`>%memory%</action> </fc> \
--       \<fc=#212733> | </fc>  <fc=#d4bfff> <action=`alacritty -e htop`><fn=2></fn> %ping% ms</action> </fc> \
--       \<fc=#212733> | </fc>  <action=`alacritty -e watch protonvpn-cli status`><fc=#ffd580> <fn=2></fn> VPN: %protonvpn% </fc> <fc=#bae67e><fn=6>(%vpnsrvload%)</fn></fc></action> \
       \<fc=#212733> | </fc>  <fc=#73d0ff> </fc>} \
--       \<fc=#c7c7c7>%mpd%</fc> \
       \{<fc=#212733> | </fc>  <fc=#73d0ff> </fc> \
--       \<fc=#212733> | </fc>  <fc=#ffd580><fn=3></fn> %btc% </fc> \
--       \<fc=#212733> | </fc>  <fc=#9aadf2><fn=7></fn> %eth% </fc> \
       \<fc=#212733> | </fc>  <fc=#ff79c6> <action=`alacritty -e watch df -h`>%disku%</action> </fc> \
--       \<fc=#212733> | </fc>   <fc=#d4bfff><fn=2></fn>  <action=`alacritty -e yay -Syu`>%sysupdate%</action> </fc> \
--       \<fc=#212733> | </fc>  <fc=#95e6cb> <fn=2></fn> %default:Master% </fc> \
       \<fc=#212733> | </fc>  %date% <fc=#212733> | | </fc>"
       }
