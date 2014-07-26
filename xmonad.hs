-------------------------------------------------------------------------------
-- xmonad.hs
-- Author: Daniel Jackson <ninegrid@thinkhard.net>
-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO (Handle, hPutStrLn) 

-- utils
import XMonad.Util.Run (spawnPipe)

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.Tabbed

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = do 
  h1 <- spawnPipe "xmobar -x 0"
  h2 <- spawnPipe "xmobar -x 1"
  xmonad $ withUrgencyHookC NoUrgencyHook urgentConfig $ myConfig h1 h2

-------------------------------------------------------------------------------
-- Configs --
myConfig h1 h2 = defaultConfig { workspaces = workspaces'
                               , modMask = modMask'
                               --, startupHook = setWMName = "LG3D"
                               , borderWidth = borderWidth'
                               , normalBorderColor = normalBorderColor'
                               , focusedBorderColor = focusedBorderColor'
                               , terminal = terminal'
                               , keys = keys'
                               , layoutHook = layoutHook'
                               , manageHook = manageHook'
                               , logHook = logHook' h1 h2
                               }

-------------------------------------------------------------------------------
-- logHook --
logHook' h1 h2 = dynamicLogWithPP customPP { ppOutput = hPutStrLn h1 }
              >> dynamicLogWithPP customPP { ppOutput = hPutStrLn h2 }

-------------------------------------------------------------------------------
-- Helpers --
-- avoidMaster:  Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r:rs) -> W.Stack t [r] rs
  otherwise           -> c

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         , className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> doFloat
                         , className =? "Vlc"       --> doFloat
                         , className =? "FreeMind"  --> doFloat
                         , className =? "Vym"       --> doFloat
                         , className =? "gephi"     --> doFloat
                         , manageDocks
                         , insertPosition Below Newer
                         , transience'
                         ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#268bd2" "" . wrap "☛" "☚"
                     , ppVisible = xmobarColor "#268bd2" "" . wrap "☞" "☜"
                     , ppHidden = xmobarColor "#2aa198" ""
                     , ppHiddenNoWindows = xmobarColor "#93a1a1" ""
                     , ppUrgent = xmobarColor "#d33682" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#268bd2" ""
                     , ppTitle =  xmobarColor "#d33682" "" . shorten 80
                     , ppSep = xmobarColor "#eee8d5" "" " ▒ "
                     }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1
normalBorderColor'  = "#eee8d5"
focusedBorderColor' = "#d33682"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = map show [1..9]

-- layouts
layoutHook' = avoidStruts $ tile ||| mtile ||| tab ||| full ||| mag 
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = named "[]=" $ smartBorders rt
    mtile = named "M[]=" $ smartBorders $ Mirror rt
    tab = named "T" $ noBorders $ tabbed shrinkText tabTheme1
    full = named "[]" $ noBorders Full
    mag = magnifier (Tall 1 (3/100) (1/2)) ||| Full

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvt +sb"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "dmenu_run -i -p '> ' -nb '#fdf6e3' -nf '#657b83' -sb '#eee8d5' -sf '#d33682'") 
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_i     ), spawn "xcalib -d :0 -i -a")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- gap for bars
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)
    , ((modMask,               xK_y     ), focusUrgent)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- magnifying
    , ((modMask              , xK_plus  ), sendMessage MagnifyMore)
    , ((modMask              , xK_minus ), sendMessage MagnifyLess)
    , ((modMask              , xK_o     ), sendMessage ToggleOff)
    , ((modMask .|. shiftMask, xK_o     ), sendMessage ToggleOn)
    , ((modMask .|. shiftMask, xK_m     ), sendMessage Toggle)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
