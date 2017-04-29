--
-- xmonad setup
--

import IO
import System.Exit

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

import qualified Data.Map as M
import qualified XMonad.StackSet as W

--
-- Setup values as required
--

myTerminal		= "konsole"
myFocusFollowsMouse	= False
myBorderWidth		= 5
myNormalBorderColor	= "#c0c0c0"
myFocusedBorderColor	= "#2020c0"
myLayoutHook            = avoidStruts ((Tall 1 0.01 0.5) ||| Full)
myLogHook h             = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h }

keysToAdd x    = [ ((modMask x .|. shiftMask, xK_Tab), windows W.focusUp)
                 , ((modMask x,               xK_F4),  kill)
                 , ((modMask x,               xK_F10), io (exitWith ExitSuccess))
                 , ((modMask x,               xK_F11), broadcastMessage ReleaseResources >> restart "xmonad" True)
                 , ((modMask x,               xK_F12), spawn "xlock")
                 ]
keysToRemove x = [  (modMask x .|. shiftMask, xK_c)
                 ,  (modMask x .|. shiftMask, xK_q)
                 ,  (modMask x .|. shiftMask, xK_w)
                 ,  (modMask x .|. shiftMask, xK_e)
                 ,  (modMask x .|. shiftMask, xK_r)
                 ,  (modMask x,               xK_q)
                 ,  (modMask x,               xK_w)
                 ,  (modMask x,               xK_e)
                 ,  (modMask x,               xK_r)
                 ]
addKeys x      = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
myKeys x       = foldr M.delete (addKeys x) (keysToRemove x)

--
-- Now run xmonad with all the settings from above
--

main = do
       pipe <- spawnPipe "xmobar"
       xmonad $ defaultConfig
           { terminal           = myTerminal
           , focusFollowsMouse  = myFocusFollowsMouse
           , borderWidth        = myBorderWidth
           , normalBorderColor  = myNormalBorderColor
           , focusedBorderColor = myFocusedBorderColor
           , keys               = myKeys
           , layoutHook         = myLayoutHook
           , logHook            = myLogHook pipe
           }
