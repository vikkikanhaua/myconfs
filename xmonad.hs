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
import XMonad.Layout.Accordion
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

---------------------------------------------------------------------------------------------------------------------------------------------

myFont               = "-*-terminus-*-r-normal-*-12-120-*-*-*-*-iso8859-*"
focusColor           = "#60ff45"
textColor            = "#c0c0a0"
lightTextColor       = "#fffff0"
backgroundColor      = "#304520"
lightBackgroundColor = "#456030"
urgentColor          = "#ffc000"

---------------------------------------------------------------------------------------------------------------------------------------------

myManageHook = (composeAll . concat $
  [ [className =? c                 --> doShift "web"    |  c    <- myWebs    ] -- move webs to web
  , [className =? c                 --> doCenterFloat    |  c    <- myFloats  ] -- float my floats classes
  , [title     =? t                 --> doCenterFloat    |  t    <- myFloatsT ] -- float my floats titles
  ]) <+> manageDocks <+> manageScratchPad

  where

    -- names
    myFloats  = ["Gimp","MPlayer","Vlc","Zenity","VirtualBox","Xmessage","Save As","XFontSel","feh"]
    myFloatsT = ["Downloads","Add-ons","Shiretoko Preferences","About Shiretoko"]
    myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","Google-chrome","Chromium"]
    

tiled = ResizableTall 1 (2/100) (1/2) []

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ddeedd"

borderWidth' :: Dimension
borderWidth' = 2

myTerminal = "urxvtc"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font        = myFont
  , bgColor     = backgroundColor
  , fgColor     = textColor
  , fgHLight    = lightTextColor
  , bgHLight    = lightBackgroundColor
  , position    = Bottom
  , height      = 16
  , historySize = 100
  , borderColor = lightBackgroundColor
  }

-- manage the scratchpad
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where

    -- height, width as % screensize
    h = 0.32
    w = 0.65

    t = 0.62
    l = 0.22

---------------------------------------------------------------------------------------------------------------------------------------------

myPP h = defaultPP 
  { ppCurrent = wrap "^fg(#fea63c)^bg(#382d22)" "^fg()^bg()" . pad
  , ppHidden = wrap "^fg(#66aabb)" "^fg()" . noScratchPad
  , ppHiddenNoWindows = wrap "^fg(grey40)" "^fg()" . namedOnly
  , ppSep = ""
  , ppUrgent = wrap "!^fg(#e9c789)" "^fg()"
  , ppWsSep = ""
  , ppLayout = dzenColor "grey30" "" .
      (\x -> case x of
        "Tall" -> "^i(/home/vikki/.xmonad/dzen/tall.xbm) "
        "Mirror Tall" -> "^i(/home/vikki/.xmonad/dzen/mtall.xbm) "
        "Accordion" -> "Accordion "
        "Simple Float" -> "float "
        "Full" -> "^i(/home/vikki/.xmonad/dzen/full.xbm) "
      )
  , ppTitle = dzenColor "#fffff0" "" . shorten 450
  , ppOutput = hPutStrLn h
  }

  where

    noScratchPad ws = if all (`elem` ws) "NSP" then "" else pad ws
    
    namedOnly ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""

---------------------------------------------------------------------------------------------------------------------------------------------

-- Define layout list
myLayout = avoidStruts 
           $ smartBorders
           $ onWorkspace "term" Accordion
           $ onWorkspaces ["web","tmux"] Full 
           $ Mirror tiled ||| tiled
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled      = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster    = 1

    -- Default proportion of screen occupied by master pane
    ratio      = 1/2

    -- Percent of screen to increment by when resizing panes
    delta      = 3/100
     
---------------------------------------------------------------------------------------------------------------------------------------------

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
       [((modMask,                  xK_Return),             spawn myTerminal)
      , ((0,                       0x1008ff2f),             spawn "slock")
      , ((modMask,                  xK_Home  ),             spawn "sudo shutdown -r now")
      , ((modMask,                  xK_End   ),             spawn "sudo shutdown -h now")
      , ((modMask,                  xK_e     ),             spawn "evince")
      , ((modMask,                  xK_g     ),             goToSelected defaultGSConfig)
      , ((modMask,                  xK_o     ),             spawn "ooffice")
      , ((modMask,                  xK_r     ),             spawn "ranwall")
      , ((controlMask,       	    xK_Print ),             spawn "scrot screenie-%H-%M-%d-%b.png -q 100")    
      , ((modMask .|. controlMask,  xK_Left  ),             spawn "mpc --no-status prev")
      , ((0,                       0x1008ff19),             spawn "mpc --no-status toggle")
      , ((modMask,                 0x1008ff19),             spawn "echo pause > ~/.mplayer/mplayer_fifo")
      , ((modMask .|. controlMask,  xK_s     ),             spawn "mpc --no-status stop")
      , ((modMask .|. controlMask,  xK_Right ),             spawn "mpc --no-status next")
      , ((modMask .|. controlMask,  xK_r     ),             spawn "mpc --no-status random")
      , ((0,                       0x1008ff11),             spawn "aumix -v-6")
      , ((0,                       0x1008ff13),             spawn "aumix -v+6")
      , ((0,                       0x1008ff12),             spawn "amixer set Master toggle")
      , ((0,                       0x1008ff1b),             spawn "firefox") 
      , ((modMask,                  xK_c     ),             spawn "chromium") 
      , ((modMask .|. controlMask,  xK_b     ),             spawn "favsong -b")
      , ((modMask,                  xK_f     ),             spawn "favsong") 
      , ((modMask,                  xK_d     ),             spawn "eject -T") 
      , ((modMask .|. shiftMask,    xK_c     ),             kill)
      
      , ((modMask,                  xK_z     ),             viewEmptyWorkspace)
      , ((modMask,                  xK_p     ),             shellPrompt myXPConfig)

-- layouts
      , ((modMask,               xK_space ),                sendMessage NextLayout)
      , ((modMask .|. shiftMask, xK_space ),                setLayout $ XMonad.layoutHook conf)
      , ((modMask,               xK_b     ),                sendMessage ToggleStruts)

-- floating layer stuff
      , ((modMask,               xK_t     ),                withFocused $ windows . W.sink)
      , ((modMask,               xK_s     ),                scratchPad)
      
-- focus
      , ((modMask,               xK_Tab   ),                windows W.focusDown)
      , ((modMask .|. shiftMask, xK_Tab   ),                windows W.focusUp)
      , ((modMask,               xK_m     ),                windows W.focusMaster)

-- swapping
      , ((modMask .|. shiftMask, xK_Return),                windows W.shiftMaster)
      , ((modMask .|. shiftMask, xK_j     ),                windows W.swapDown  )
      , ((modMask .|. shiftMask, xK_k     ),                windows W.swapUp    )

-- increase or decrease number of windows in the master area
      , ((modMask              , xK_comma ),                sendMessage (IncMasterN 1))
      , ((modMask              , xK_period),                sendMessage (IncMasterN (-1)))

-- resizing
      , ((modMask,               xK_h     ),                sendMessage Shrink)
      , ((modMask,               xK_l     ),                sendMessage Expand)
      , ((modMask .|. shiftMask, xK_q     ),                io (exitWith ExitSuccess))
      , ((modMask,               xK_q     ),                broadcastMessage ReleaseResources >> restart "xmonad" True)
     ]
     ++
-- mod-[1..9] %! Switch to workspace N
-- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    where
      
      scratchPad = scratchpadSpawnActionTerminal myTerminal
      
---------------------------------------------------------------------------------------------------------------------------------------------

statusBarCmd1 = "dzen2 -bg '#1a1a1a' -fg '#fffff0' -h 14 -w 670 -e '' -fn '-*-terminus-*-r-normal-*-12-120-*-*-*-*-iso8859-*' -ta l"

---------------------------------------------------------------------------------------------------------------------------------------------

main = do 
	bar <- spawnPipe statusBarCmd1 
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig
                   { manageHook = myManageHook
          	   , workspaces = ["term","web","tmux","dld","else"]
          	   , layoutHook = myLayout
          	   , logHook = dynamicLogWithPP $ myPP bar
          	   , modMask = mod4Mask    -- Rebind Mod to the Windows key
          	   , normalBorderColor  = myNormalBorderColor
          	   , focusedBorderColor = myFocusedBorderColor
                   , terminal           = myTerminal
          	   , focusFollowsMouse  = myFocusFollowsMouse
          	   , keys = keys'
          	   }

---------------------------------------------------------------------------------------------------------------------------------------------
