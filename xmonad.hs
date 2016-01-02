-------------------------------
-- File:    xmonad.hs
-- Version: 0.2
-- Author:  Michael Carpenter
-- Date:    6/8/13
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
import XMonad.Actions.TopicSpace
-- Layouts
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.ToggleLayouts
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
import XMonad.Prompt.XMonad
-- Utils
import XMonad.Util.Paste
import XMonad.Util.Run


-- The Basics
myTerminal = "urxvtc"
myFocusFollowsMouse = True
myBorderWidth = 2
myModMask = controlMask
myWorkspaces = ["$","blog","chat","dev","doc","log","music","monitor","video"]
myNormalBorderColor = "grey"
myFocusedBorderColor = "green"


-- Dzen
myXmonadBar = "dzen2 -x '0' -y '0' -h '20' -w '1920' -ta 'l' -fg '#F0F8FF' -bg '#1B1D1E'"

-- XMobar
myXMobar = "xmobar"

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        [ ((modm                    , xK_Return )       , windows W.swapMaster)
        , ((modm .|. mod1Mask       , xK_Return )       , spawn $ XMonad.terminal conf)
        , ((modm                    , xK_space )        , sendMessage NextLayout)
        , ((modm .|. mod1Mask       , xK_space )        , setLayout $ XMonad.layoutHook conf)

        -- NUMBER ROW (PROGRAMMER DVORAK)
        , ((mod4Mask                , xK_dollar )       , shellPrompt defaultXPConfig)
        --, ((modm .|. mod1Mask       , xK_dollar )       , return ())
        --, ((modm                    , xK_ampersand )    , return ())
        --, ((modm .|. mod1Mask       , xK_ampersand )    , return ())
        --, ((modm                    , xK_bracketleft )  , return ())
        --, ((modm .|. mod1Mask       , xK_bracketleft )  , return ())
        --, ((modm                    , xK_braceleft )    , return ())
        --, ((modm .|. mod1Mask       , xK_braceleft )    , return ())
        --, ((modm                    , xK_braceright )   , return ())
        --, ((modm .|. mod1Mask       , xK_braceright )   , return ())
        --, ((modm                    , xK_parenleft )    , return ())
        --, ((modm .|. mod1Mask       , xK_parenleft )    , return ())
        --, ((modm                    , xK_equal )        , return ())
        --, ((modm .|. mod1Mask       , xK_equal )        , return ())
        --, ((modm                    , xK_asterisk )     , return ())
        --, ((modm .|. mod1Mask       , xK_asterisk )     , return ())
        --, ((modm                    , xK_parenright )   , return ())
        --, ((modm .|. mod1Mask       , xK_parenright )   , return ())
        --, ((modm                    , xK_plus )         , return ())
        --, ((modm .|. mod1Mask       , xK_plus )         , return ())
        --, ((modm                    , xK_bracketright   , return ())
        --, ((modm .|. mod1Mask       , xK_bracketright   , return ())
        --, ((modm                    , xK_exclam         , return ())
        --, ((modm .|. mod1Mask       , xK_exclam         , return ())
        --, ((modm                    , xK_numbersign     , return ())
        --, ((modm .|. mod1Mask       , xK_numbersign     , return ())

        -- TOP ROW (PROGRAMMER DVORAK)
        --, ((modm                    , xK_semicolon )    , return ())
        --, ((modm .|. mod1Mask       , xK_semicolon )    , return ())
        , ((modm                    , xK_comma )        , sendMessage (IncMasterN 1))
        --, ((modm .|. mod1Mask       , xK_comma          , return ())
        , ((modm                    , xK_period )       , sendMessage (IncMasterN (-1)))
        --, ((modm .|. mod1Mask       , xK_period         , return ())
        --, ((modm                    , xK_p )            , return ())
        , ((modm .|. mod1Mask       , xK_p )            , windowPromptBring defaultXPConfig)
        --, ((modm                    , xK_y )            , return ())
        --, ((modm .|. mod1Mask       , xK_y )            , return ())
        , ((modm                    , xK_f )            , sendMessage (Toggle "Full"))
        , ((modm .|. mod1Mask       , xK_f )            , spawn "chromium")
        --, ((modm                    , xK_g )            , return ())
        --, ((modm .|. mod1Mask       , xK_g )            , return ())
        --, ((modm                    , xK_c )            , return ())
        --, ((modm .|. mod1Mask       , xK_c )            . return ())
        , ((modm                    , xK_r )            , refresh)
        --, ((modm .|. mod1Mask       , xK_r )            , return ())
        --, ((modm                    , xK_l )            , return ())
        , ((modm .|. mod1Mask       , xK_l )            , sendMessage Expand)
        --, ((modm                    , xK_L )          , withFocused $ windows . W.sink)
        --, ((modm                    , xK_slash )        , return ())
        --, ((modm .|. mod1Mask       , xK_slash )        , return ())
        --, ((modm                    , xK_at )           , return ())
        --, ((modm .|. mod1Mask       , xK_at )           , return ())
        --, ((modm                    , xK_backslash )    , return ())
        --, ((modm .|. mod1Mask       , xK_backslash )    , return ())

        -- HOME ROW (PROGRAMMER DVORAK)
        --, ((modm                    , xK_a )            , return ())
        --, ((modm .|. mod4Mask       , xK_a )            , return ())
        , ((modm                    , xK_o )            , shellPrompt defaultXPConfig)
        --, ((modm .|. mod4Mask       , xK_o )            , return ())
        , ((modm                    , xK_e )            , moveTo Prev NonEmptyWS)
        , ((modm .|. mod1Mask       , xK_e )            , shiftToPrev >> prevWS)
        , ((modm                    , xK_u )            , moveTo Next NonEmptyWS)
        , ((modm .|. mod1Mask       , xK_u )            , shiftToNext >> nextWS)
        --, ((modm                    , xK_i )            , return ())
        --, ((modm .|. mod1Mask       , xK_i )            , return ())
        , ((modm                    , xK_d )            , kill)
        , ((modm .|. mod1Mask       , xK_d )            , removeWorkspace)
        --, ((modm                    , xK_h )            , return ())
        , ((modm .|. mod1Mask       , xK_h )            , sendMessage Shrink)
        --, ((modm                    , xK_t )            , return ())
        , ((modm .|. mod1Mask       , xK_t )            , addWorkspacePrompt defaultXPConfig)
        --, ((modm                    , xK_n )            , return ())
        --, ((modm .|. mod1Mask       , xK_n )            , return ())
        --, ((modm                    , xK_s )            , return ())
        , ((modm .|. mod1Mask       , xK_s )            , withFocused $ windows . W.sink)
        --, ((modm                    , xK_minus )        , return ())
        --, ((modm .|. mod1Mask       , xK_minus )        , return ())

        -- BOTTOM ROW (PROGRAMMER DVORAK)
        --, ((modm                    , xK_colon )        , return ())
        --, ((modm .|. mod1Mask       , xK_colon )        , return ())
        , ((modm                    , xK_q )            , spawn "xmonad --recompile; xmonad --restart")
        , ((modm .|. mod1Mask       , xK_q )            , io (exitWith ExitSuccess))
        , ((modm                    , xK_j )            , windows W.focusDown)
        , ((modm .|. mod1Mask       , xK_j )            , windows W.swapDown)
        , ((modm                    , xK_k )            , windows W.focusUp)
        , ((modm .|. mod1Mask       , xK_k )            , windows W.swapUp)
        --, ((modm                    , xK_x )            , return ())
        --, ((modm .|. mod1Mask       , xK_x )            , return ())
        , ((modm                    , xK_b )            , selectWorkspace defaultXPConfig)
        , ((modm                    , xK_m )            , windows W.focusMaster)
        , ((modm .|. mod1Mask       , xK_m )            , spawn "urxvtc -title mutt -name mutt -e mutt")
        --, ((modm                    , xK_w )            , return ())
        --, ((modm .|. mod1Mask       , xK_w )            , return ())
        --, ((modm                    , xK_v )            , return ())
        --, ((modm .|. mod1Mask       , xK_v )            , return ())
        --, ((modm                    , xK_z )            , return ())
        , ((modm .|. mod1Mask       , xK_z )            , spawn "zathura")

        -- MISC KEYS (PROGRAMMER DVORAK)
        , ((modm                    , xK_Insert)        , pasteSelection)
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

-- Log Hooks
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent         = xmobarColor "#ebac54" "#1B1D1E" . pad
  , ppVisible         = xmobarColor "cyan"    "#1B1D1E" . pad
  , ppHidden          = xmobarColor "#f0f8ff" "#1B1D1E" . pad
  , ppHiddenNoWindows = xmobarColor "#7b7b7b" "#1B1D1E" . pad
  , ppUrgent          = xmobarColor "#ff0000" "#1B1D1E" . pad
  , ppWsSep           = " "
  , ppSep             = "  |  "
  , ppTitle           = (" " ++) . xmobarColor "#f0f8ff" "#1B1D1E"
  , ppOutput          = hPutStrLn h
  }

myManageHook =
    composeAll  [ className =? "Steam"
                  --> (liftX $ addHiddenWorkspace "games") >> doShift "games"
                , title =? "Minecraft Launcher"
                  --> (liftX $ addHiddenWorkspace "games") >> doShift "games"
                ]

myEventHook = undefined

myStartupHook = undefined

main = do
  statusBar <- spawnPipe myXMobar
  xmonad =<< xmobar defaultConfig
    { terminal            = myTerminal
    , focusFollowsMouse   = myFocusFollowsMouse
    , borderWidth         = myBorderWidth
    , modMask             = myModMask
    , workspaces          = myWorkspaces
    , normalBorderColor   = myNormalBorderColor
    , focusedBorderColor  = myFocusedBorderColor

    -- key bindings
    , keys                = myKeys
    --, mouseBindings       = myMouseBindings

    -- hooks, layouts
    , layoutHook          = myLayoutHook
    , manageHook          = manageHook defaultConfig <+> myManageHook
    --, handleEventHook     = myEventHook
    , logHook             = myLogHook statusBar >> setWMName "LG3D"
    --, startupHook         = myStartupHook
    }
