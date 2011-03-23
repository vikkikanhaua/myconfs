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
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.CenteredMaster
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile

-- Extra --
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.GridSelect
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified Data.Map as M

import Graphics.X11.Xlib
import Data.Char

import System.Exit

-- default declaraions
myFont               = "-*-montecarlo-bold-r-normal-*-11-*-*-*-*-*-*-*"
myTerminal           = "urxvtc"
focusColor           = "#60ff45"
textColor            = "#f1f1f1"
lightTextColor       = "#fea63c"
backgroundColor      = "#1a1a1a"
lightBackgroundColor = "grey20"
myBitmapsPath        = "/home/vikki/.xmonad/dzen/"

-- custom managehook
myManageHook = (composeAll . concat $
  [ [className =? c  --> doShift "web"  |  c  <- myWebs   ] -- move webs to web
  , [className =? c  --> doCenterFloat  |  c  <- myFloats ] -- float my floats classes
  , [title     =? t  --> doCenterFloat  |  t  <- myFloatsT] -- float my floats titles
  ]) <+> manageDocks <+> manageScratchPad

  where
    -- names
    myFloats  = ["Gimp", "MPlayer", "Vlc", "Xmessage", "Save As", "XFontSel", "feh"]
    myFloatsT = ["Downloads", "Add-ons", "Preferences"]
    myWebs    = ["Navigator", "Firefox", "Chromium", "Namoroka"]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

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
  , position    = Top
  , height      = 20
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
  { ppCurrent = wrap "^fg(#f1f1f1)" "^fg()"
  , ppHidden = wrap "^fg(#659fdb)" "^fg()" . noScratchPad
  , ppHiddenNoWindows = wrap "^fg(grey30)" "^fg()" . namedOnly
  , ppSep = " "
  , ppUrgent = wrap "^fg(red)" "^fg()" . dzenStrip
  , ppWsSep = " "
  , ppLayout = dzenColor "grey40" "" .
      (\x -> case x of
        "Tall"        -> wrapBitmap "tall.xbm"
        "Mirror Tall" -> wrapBitmap "mtall.xbm"
        "Full"        -> wrapBitmap "full.xbm"
        "Simple Float"->            "<>"
      )
  , ppTitle  = wrap "^fg(#fea63c)" "^fg()" . shorten 75
  , ppOutput = hPutStrLn h . pad
  }

  where
    -- filter out NSP
    noScratchPad ws = if ws == "NSP" then "" else ws
    namedOnly ws = if any (`elem` ws) ['a'..'z'] then ws else ""
    wrapBitmap bitmap = "^i(" ++ myBitmapsPath ++ bitmap ++ ")"

-- layout list
myLayout = avoidStruts
           $ smartBorders
           $ onWorkspace  "main" simpleFloat
           $ onWorkspaces ["web", "term"]  Full
           $ Mirror tiled ||| tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- custom keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
       [((modMask,                  xK_Return),             spawn myTerminal)
      , ((0,                       0x1008ff2f),             spawn "alock -auth md5:file=/home/vikki/docs/passphrase")
      , ((modMask,                  xK_Home  ),             spawn "sudo shutdown -r now")
      , ((modMask,                  xK_End   ),             spawn "sudo shutdown -h now")
      , ((modMask,                  xK_a     ),             spawn "evince")
      , ((modMask,                  xK_g     ),             goToSelected defaultGSConfig)
      , ((modMask,                  xK_o     ),             spawn "libreoffice")
      , ((modMask,                  xK_r     ),             spawn "ranwall")
      , ((0,                        xK_Print ),             spawn "scrot screenie-%H-%M-%S-%d-%b.png -q 100")
      , ((modMask,                  xK_Left  ),             spawn "mpc -q prev")
      , ((0,                       0x1008ff19),             spawn "mpc -q toggle")
      , ((modMask,                 0x1008ff19),             spawn "echo pause > ~/.mplayer/mplayer_fifo")
      , ((modMask,                  xK_s     ),             spawn "mpc -q stop")
      , ((modMask,                  xK_Right ),             spawn "mpc -q next")
      , ((0,                       0x1008ff11),             spawn "amixer -q set 'Master Front' 2-")
      , ((0,                       0x1008ff13),             spawn "amixer -q set 'Master Front' 2+")
      , ((0,                       0x1008ff12),             spawn "amixer -q set 'Master Front' toggle")
      , ((0,                       0x1008ff1b),             spawn "firefox")
      , ((modMask,                  xK_c     ),             spawn "chromium")
      , ((modMask .|. controlMask,  xK_b     ),             spawn "favsong -b")
      , ((modMask,                  xK_f     ),             spawn "favsong")
      , ((modMask,                  xK_e     ),             spawn "eject -T")
      , ((modMask,                  xK_u     ),             spawn "devmon -c; notify DEVICE SAFE TO REMOVE")
      , ((modMask,                  xK_x     ),             spawn "xterm")
      , ((modMask .|. shiftMask,    xK_c     ),             kill)

      , ((modMask,                  xK_z     ),             viewEmptyWorkspace)
      , ((modMask,                  xK_Escape),             toggleWS)
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
      , ((modMask .|. shiftMask, xK_q        ),             myExit)
      , ((modMask,               xK_q        ),             myRestart)
     ]
     ++
-- mod-[1..9] %! Switch to workspace N
-- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    where
      scratchPad = scratchpadSpawnActionTerminal myTerminal
      myExit     = spawn "kill `pgrep devmon`" >> io (exitWith ExitSuccess)
      myRestart  = spawn "kill `pgrep devmon`" >> broadcastMessage ReleaseResources >> restart "xmonad" True


statusBarCmd = "dzen2 -bg '#1a1a1a' -fg '#f1f1f1' -w 650 -h 20 -e '' -fn " ++ myFont ++ " -ta l"

startup :: X ()
startup = do
          spawn "devmon -g --exec-on-drive \"notify %f mounted on %d\" --exec-on-disc \"notify DISC mounted on %d\""

main = do
	bar <- spawnPipe statusBarCmd
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig
                   { manageHook         = myManageHook
          	   , workspaces         = ["main","web","term","media","else"]
          	   , layoutHook         = myLayout
          	   , logHook            = dynamicLogWithPP $ myPP bar
                   , startupHook        = startup
          	   , modMask            = mod4Mask
          	   , normalBorderColor  = myNormalBorderColor
          	   , focusedBorderColor = myFocusedBorderColor
                   , terminal           = myTerminal
          	   , focusFollowsMouse  = myFocusFollowsMouse
          	   , keys               = keys'
          	   }
-- END
