module Events where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM

data Event =
    EventKeyPress !GLFW.Key !GLFW.ModifierKeys
  | EventKeyRelease !GLFW.Key !GLFW.ModifierKeys

keyCallback :: TQueue Event -> Window -> Key -> Int -> KeyState ->
               ModifierKeys -> IO ()
keyCallback queue _ key _ KeyState'Pressed modifiers =
  atomically $ writeTQueue queue (EventKeyPress key modifiers)
keyCallback queue _ key _ KeyState'Released modifiers =
  atomically $ writeTQueue queue (EventKeyRelease key modifiers)
keyCallback _ _ _ _ KeyState'Repeating _ = return ()

setupEvents :: Window -> TQueue Event -> IO ()
setupEvents window queue = do
  setKeyCallback window $ Just (keyCallback queue)
