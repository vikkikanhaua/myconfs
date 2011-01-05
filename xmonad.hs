-- Main --
import XMonad
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.ManageHook
import XMonad.Operations

-- Hooks --
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

-- Utils --
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad

-- Layout --
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.CenteredMaster
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile

-- Extra --
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.GridSelect
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified Data.Map as M

import Graphics.X11.Xlib
import Data.Char

import System.Exit

-- default declaraions
myFont               = "-artwiz-snap-normal-r-normal--10-100-75-75-p-90-iso8859-1"
myTerminal           = "urxvtc"
focusColor           = "#60ff45"
textColor            = "#c0c0a0"
lightTextColor       = "#fffff0"
backgroundColor      = "#304520"
lightBackgroundColor = "#456030"
urgentColor          = "#ffc000"

-- custom managehook
myManageHook = (composeAll . concat $
  [ [className =? c                 --> doShift "web"    |  c    <- myWebs    ] -- move webs to web
  , [className =? c                 --> doCenterFloat    |  c    <- myFloats  ] -- float my floats classes
  , [title     =? t                 --> doCenterFloat    |  t    <- myFloatsT ] -- float my floats titles
  ]) <+> manageDocks <+> manageScratchPad

  where
    -- names
    myFloats  = ["Gimp", "MPlayer", "Vlc", "Xmessage", "Save As", "XFontSel", "feh"]
    myFloatsT = ["Downloads", "Add-ons", "Preferences"]
    myWebs    = ["Navigator", "Firefox", "Chromium", "Namoroka"]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#659fdb"

borderWidth' :: Dimension
borderWidth' = 1

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font        = myFont
  , bgColor     = backgroundColor
  , fgColor     = textColor
  , fgHLight    = lightTextColor
  , bgHLight    = lightBackgroundColor
  , position    = Bottom
  , height      = 12
  , historySize = 100
  , borderColor = lightBackgroundColor
  }

-- manage the scratchpad
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where
    -- height, width as % screensize
    h = 0.435
    w = 0.70

    t = 0.25
    l = 0.15

-- custom pp
myPP h = defaultPP
  { ppCurrent = wrap "^fg(#fea63c)^bg(#382d22)" "^fg()^bg()" . pad
  , ppHidden = wrap "^fg(#66aabb)" "^fg()" . noScratchPad
  , ppHiddenNoWindows = wrap "^fg(grey40)" "^fg()" . namedOnly
  , ppSep = ""
  , ppUrgent = wrap "^fg(#000000)^bg(#cd5c5c)" "^fg()^bg()" . dzenStrip
  , ppWsSep = ""
  , ppLayout = dzenColor "grey60" "" .
      (\x -> case x of
        "Tall"        -> "|-,-| "
        "Mirror Tall" -> "|_,_| "
        "Grid"        -> "|+,+| "
        "Full"        -> "|   | "
        "Simple Float"-> "float "
      )
  , ppTitle  = map toLower . shorten 70
  , ppOutput = hPutStrLn h
  }

  where
    -- filter out NSP
    noScratchPad ws = if ws == "NSP" then "" else pad ws
    namedOnly ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""

-- layout list
myLayout = avoidStruts
           $ smartBorders
           $ onWorkspace "main" simpleFloat
           $ onWorkspace "web"  ( centerMaster Grid )
           $ onWorkspace "term" Full
           $ Mirror tiled ||| tiled ||| Grid ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled      = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster    = 1

    -- Default proportion of screen occupied by master pane
    ratio      = 1/2

    -- Percent of screen to increment by when resizing panes
    delta      = 3/100

-- custom keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
       [((modMask,                  xK_Return),             spawn myTerminal)
      , ((0,                       0x1008ff2f),             spawn "alock -auth md5:file=/home/vikki/docs/passphrase")
      , ((modMask,                  xK_Home  ),             spawn "sudo shutdown -r now")
      , ((modMask,                  xK_End   ),             spawn "sudo shutdown -h now")
      , ((modMask,                  xK_a     ),             spawn "evince")
      , ((modMask,                  xK_g     ),             goToSelected defaultGSConfig)
      , ((modMask,                  xK_o     ),             spawn "ooffice")
      , ((modMask,                  xK_r     ),             spawn "ranwall")
      , ((0,                        xK_Print ),             spawn "scrot screenie-%H-%M-%d-%b.png -q 100")
      , ((modMask .|. controlMask,  xK_Left  ),             spawn "ncmpcpp prev")
      , ((0,                       0x1008ff19),             spawn "ncmpcpp toggle")
      , ((modMask,                 0x1008ff19),             spawn "echo pause > ~/.mplayer/mplayer_fifo")
      , ((modMask .|. controlMask,  xK_s     ),             spawn "ncmpcpp stop")
      , ((modMask .|. controlMask,  xK_Right ),             spawn "ncmpcpp next")
      , ((0,                       0x1008ff11),             spawn "amixer -q set Master 2-")
      , ((0,                       0x1008ff13),             spawn "amixer -q set Master 2+")
      , ((0,                       0x1008ff12),             spawn "amixer -q set Master toggle")
      , ((0,                       0x1008ff1b),             spawn "firefox")
      , ((modMask,                  xK_c     ),             spawn "chromium")
      , ((modMask .|. controlMask,  xK_b     ),             spawn "favsong -b")
      , ((modMask,                  xK_f     ),             spawn "favsong")
      , ((modMask,                  xK_e     ),             spawn "eject -T")
      , ((modMask,                  xK_x     ),             spawn "xterm")
      , ((modMask .|. shiftMask,    xK_c     ),             kill)

      , ((modMask,                  xK_z     ),             viewEmptyWorkspace)
      , ((modMask,                  xK_p     ),             shellPrompt myXPConfig)

-- layouts
      , ((modMask,               xK_space    ),             sendMessage NextLayout)
      , ((modMask .|. shiftMask, xK_space    ),             setLayout $ XMonad.layoutHook conf)
      , ((modMask,               xK_b        ),             sendMessage ToggleStruts)

-- floating layer stuff
      , ((modMask,               xK_t        ),             withFocused $ windows . W.sink)
      , ((0,                     xK_F12      ),             scratchPad)

-- focus
      , ((modMask,               xK_Tab      ),             windows W.focusDown)
      , ((modMask .|. shiftMask, xK_Tab      ),             windows W.focusUp)
      , ((modMask,               xK_m        ),             windows W.focusMaster)
      , ((modMask,               xK_BackSpace),             focusUrgent)

-- swapping
      , ((modMask .|. shiftMask, xK_Return   ),             windows W.shiftMaster)
      , ((modMask .|. shiftMask, xK_j        ),             windows W.swapDown  )
      , ((modMask .|. shiftMask, xK_k        ),             windows W.swapUp    )

-- increase or decrease number of windows in the master area
      , ((modMask              , xK_comma    ),             sendMessage (IncMasterN 1))
      , ((modMask              , xK_period   ),             sendMessage (IncMasterN (-1)))

-- resizing
      , ((modMask,               xK_h        ),             sendMessage Shrink)
      , ((modMask,               xK_l        ),             sendMessage Expand)
      , ((modMask .|. shiftMask, xK_q        ),             io (exitWith ExitSuccess))
      , ((modMask,               xK_q        ),             broadcastMessage ReleaseResources >> restart "xmonad" True)
     ]
     ++
-- mod-[1..9] %! Switch to workspace N
-- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    where
      scratchPad = scratchpadSpawnActionTerminal myTerminal

statusBarCmd = "dzen2 -bg '#1a1a1a' -fg '#f1f1f1' -w 650 -h 20 -e '' -fn '-artwiz-snap-normal-r-normal--10-100-75-75-p-90-iso8859-1' -ta l"

main = do
	bar <- spawnPipe statusBarCmd
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig
                   { manageHook = myManageHook
          	   , workspaces = ["main","web","term","media","else"]
          	   , layoutHook = myLayout
          	   , logHook = dynamicLogWithPP $ myPP bar
          	   , modMask = mod4Mask
          	   , normalBorderColor  = myNormalBorderColor
          	   , focusedBorderColor = myFocusedBorderColor
                   , terminal           = myTerminal
          	   , focusFollowsMouse  = myFocusFollowsMouse
          	   , keys = keys'
          	   }
-- END
