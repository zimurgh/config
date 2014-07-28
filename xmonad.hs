-------------------------------
-- File:    xmonad.hs
-- Version: 0.2
-- Author:  Michael Carpenter
-- Date: 6/8/13
-------------------------------

-- Core
import XMonad
import XMonad.Config (defaultConfig)
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
-- Layouts
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Prompt.Window
-- Utils
import XMonad.Util.Run


-- The Basics
myTerminal	= "urxvtc"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth	= 2
myModMask	= mod4Mask
myWorkspaces	= ["$","audio","chat","dev","doc","mail","minecraft"]
myNormalBorderColor = "grey"
myFocusedBorderColor = "green"


-- Dzen
myXmonadBar = "dzen2 -x '0' -y '0' -h '20' -w '1600' -ta 'l' -fg '#F0F8FF' -bg '#1B1D1E'"

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    	-- launch a terminal
    	[ ((modm .|. mod1Mask	    , xK_Return )	, spawn $ XMonad.terminal conf)
 
    	-- close focused window
    	, ((modm	                , xK_d )	    , kill)
 
     	-- Rotate through the available layout algorithms
    	, ((modm               	    , xK_space )	, sendMessage NextLayout)
 
    	--  Reset the layouts on the current workspace to default
    	, ((modm .|. mod1Mask	    , xK_space )	, setLayout $ XMonad.layoutHook conf)
 
    	-- Resize viewed windows to the correct size
    	, ((modm               	    , xK_r )	    , refresh)
 
    	-- Move focus to the next window
    	, ((modm               	    , xK_j )	    , windows W.focusDown)
 
    	-- Move focus to the previous window
    	, ((modm               	    , xK_k )	    , windows W.focusUp)
 
    	-- Move focus to the master window
    	, ((modm               	    , xK_m )	    , windows W.focusMaster)
 
    	-- Swap the focused window and the master window
    	, ((modm               	    , xK_Return )	, windows W.swapMaster)
 
    	-- Swap the focused window with the next window
    	, ((modm .|. mod1Mask	    , xK_j )	    , windows W.swapDown)
 
    	-- Swap the focused window with the previous window
    	, ((modm .|. mod1Mask	    , xK_k )	    , windows W.swapUp)
 
    	-- Shrink the master area
    	, ((modm               	    , xK_h )	    , sendMessage Shrink)
 
    	-- Expand the master area
    	, ((modm               	    , xK_l )	    , sendMessage Expand)
 
    	-- Push window back into tiling
    	, ((modm               	    , xK_L )	    , withFocused $ windows . W.sink)
 
    	-- Increment the number of windows in the master area
    	, ((modm              	    , xK_comma )	, sendMessage (IncMasterN 1))
 
    	-- Deincrement the number of windows in the master area
    	, ((modm              	    , xK_period )	, sendMessage (IncMasterN (-1)))
 
    	-- Toggle the status bar gap
    	-- Use this binding with avoidStruts from Hooks.ManageDocks.
    	-- See also the statusBar function from Hooks.DynamicLog.
    	--
    	, ((modm              	    , xK_B )	    , sendMessage ToggleStruts)
 
    	-- Quit xmonad
    	, ((modm .|. mod1Mask	    , xK_q )	    , io (exitWith ExitSuccess))
 
    	-- Restart xmonad
   	    , ((modm              	    , xK_q )	    , spawn "xmonad --recompile; xmonad --restart")
	
	    , ((modm              	    , xK_u )	    , moveTo Next NonEmptyWS)

	    , ((modm              	    , xK_e )	    , moveTo Prev NonEmptyWS)

	    , ((modm .|. mod1Mask	    , xK_u )	    , shiftToNext >> nextWS)

        , ((modm .|. mod1Mask       , xK_e )        , shiftToPrev >> prevWS)

	    , ((modm .|. mod1Mask	    , xK_p )	    , windowPromptBring defaultXPConfig)
	
	    , ((modm		            , xK_t )	    , addWorkspacePrompt defaultXPConfig)

	    , ((modm .|. mod1Mask		, xK_d )	    , removeWorkspace)

	    , ((modm .|. mod1Mask       , xK_b )	    , selectWorkspace defaultXPConfig)

        , ((modm                    , xK_b )        , windowPromptGoto defaultXPConfig)

        , ((modm                    , xK_o )        , shellPrompt defaultXPConfig)

	    -- My Keys
	    
    	, ((modm .|. mod1Mask	    , xK_f )	    , spawn "firefox")

        , ((modm .|. mod1Mask       , xK_m )        , spawn "urxvtc -e mutt")

        , ((modm .|. mod1Mask       , xK_v )        , spawn "virtualbox")

        , ((modm .|. mod1Mask       , xK_z )        , spawn "zathura")

    	]

-- Layout
myLayoutHook = avoidStruts (tiled ||| Circle ||| Grid ||| spiral (6/7)) ||| full
	where
		tiled = Tall nmaster delta ratio
		nmaster = 1
		ratio = 1/2
		delta = 3/100
		full = noBorders $ Full

-- Colors
colorYellow   = "#af8700"
colorOrange   = "#d75f00"
colorRed      = "#d70000"
colorMagenta  = "#af005f"
colorViolet   = "#5f5faf"
colorBlue     = "#0087ff"
colorCyan     = "#afaf"
colorGreen    = "#5f8700"

-- Dark
base03        = "#1c1c1c"
base02        = "#262626"
base01        = "#585858"
base00        = "#626262"
base0         = "#808080"
base1         = "#8a8a8a"
base2         = "#e4e4e4"
base3         = "#ffffd7"

-- Manage Hooks
--myManageHook = return ()

-- Log Hooks
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor "#ebac54" "#1B1D1E" . pad
  , ppVisible         = dzenColor "cyan" "#1B1D1E" . pad
  , ppHidden          = dzenColor "#f0f8ff" "#1B1D1E" . pad
  , ppHiddenNoWindows = dzenColor "#7b7b7b" "#1B1D1E" . pad
  , ppUrgent          = dzenColor "#ff0000" "#1B1D1E" . pad
  , ppWsSep           = " "
  , ppSep             = "  |  "
  , ppTitle           = (" " ++) . dzenColor "#f0f8ff" "#1B1D1E" . dzenEscape
  , ppOutput          = hPutStrLn h
   }

main = do
  dzenLeftBar <- spawnPipe myXmonadBar	
  xmonad $ defaultConfig
		{terminal		          = myTerminal
		, focusFollowsMouse	  = myFocusFollowsMouse
		, borderWidth		      = myBorderWidth
		, modMask		          = myModMask
		, workspaces		      = myWorkspaces
		, normalBorderColor	  = myNormalBorderColor
		, focusedBorderColor	= myFocusedBorderColor

		-- key bindings
		, keys			          = myKeys
		--, mouseBindings		= myMouseBindings

		-- hooks, layouts
		, layoutHook		      = myLayoutHook
		--, manageHook		      = manageHook defaultConfig <+> myManageHook
		--, handleEventHook	= myEventHook
	    , logHook		          = myLogHook dzenLeftBar >> setWMName "LG3D"
		--, startupHook		= myStarupHook
		}
