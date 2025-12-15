module ElmLike (
  runProgram,
  Widget(..),
) where

import Control.Concurrent -- threadDelay
import Data.Ix
import Data.Word -- Word32
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

type UiNodePtr = Ptr ()

foreign import ccall "elmlike StartGui"
  _StartGui :: IO ()
foreign import ccall "elmlike PollEventSignal"
  _PollEventSignal :: IO CInt
foreign import ccall "elmlike MakeTextNode"
  _MakeTextNode :: CString -> Word32 -> IO UiNodePtr
foreign import ccall "elmlike ConnectNodesAtSameLevel"
  _ConnectNodesAtSameLevel :: UiNodePtr -> UiNodePtr -> IO ()
foreign import ccall "elmlike DrawNodes"
  _DrawNodes :: UiNodePtr -> IO ()
foreign import ccall "elmlike PushNodesToRight"
  _PushNodesToRight :: UiNodePtr -> IO ()

-- EventSignal: Keep these signal definitions in sync with
-- the definition in the elmlike clib.
data EventSignal = EventSignal_NONE | EventSignal_QUIT deriving (Show, Enum)

intToEventSignal :: CInt -> EventSignal
intToEventSignal 1 = EventSignal_QUIT
intToEventSignal _ = EventSignal_NONE

-- Widgets
data Widget = Text String | Button String String -- TODO button should have some 'onClick' events
-- TODO Add `Row [Widget]` and `Column [Widget]`.

data Program model_type command_type = Program {
  model    :: model_type,
  updateFn :: model_type -> command_type -> model_type,
  viewFn   :: model_type -> [Widget]
}

-- runProgram:
--   Starts the ElmLike program with the `initialModel` as the starting model.
--   `upateFn` is triggered when a command is received and the program's model is updated to the
--   result of the update.
--   `viewFn` controls the visual representation of the program based on the state.
runProgram :: model_type -> (model_type -> command_type -> model_type) -> (model_type -> [Widget]) -> command_type -> IO ()
runProgram initialModel updateFn viewFn cmd = do
  _StartGui

  programLifecycleStep (Program {
    model    = initialModel,
    updateFn = updateFn,
    viewFn   = viewFn
  }) cmd

programLifecycleStep :: Program model_type command_type -> command_type -> IO ()
programLifecycleStep program cmd = do
  let
      widget_creation_actions = map convertWidgetToNode ((viewFn program) (model program))
  
  -- Execute all creation actions once to get the list of pointers
  all_widget_ui_nodes <- sequence widget_creation_actions
  
  do
    connectNodePairsStartingAtIndex all_widget_ui_nodes 0
    if (length all_widget_ui_nodes) > 0
      then do
        let head_node = all_widget_ui_nodes !! 0
        _PushNodesToRight head_node 
        _DrawNodes head_node
      else pure ()

  do
    signal_raw <- _PollEventSignal
    let
      signal = intToEventSignal signal_raw
      in case signal of
        EventSignal_NONE -> programLifecycleStep (programRunUpdate program cmd) cmd
        EventSignal_QUIT -> putStrLn "TODO quit application"

programRunUpdate :: Program model_type command_type -> command_type -> Program model_type command_type
programRunUpdate program cmd = Program {
  model    = (updateFn program) (model program) cmd,
  updateFn = (updateFn program),
  viewFn   = (viewFn program)
}

-- Recursively connect all nodes in `nodes` by connecting every pair to
-- each other.
connectNodePairsStartingAtIndex :: [UiNodePtr] -> Int -> IO ()
connectNodePairsStartingAtIndex nodes offset
  | not(in_bounds nodes offset) = return ()
  | otherwise = do
                  let
                    left  = nodes !! offset
                    right = if in_bounds nodes (offset + 1)
                      then nodes !! (offset + 1)
                      else nullPtr
                  _ConnectNodesAtSameLevel left right
                  connectNodePairsStartingAtIndex nodes (offset + 1)

-- Returns true if `idx` is within the bounds of `arr`.
in_bounds :: [a] -> Int -> Bool
in_bounds arr idx = idx < (length arr)

convertWidgetToNode :: Widget -> IO UiNodePtr
convertWidgetToNode (Text content) = withCString content $ \content_cstr -> do
  ui_node_ptr <- _MakeTextNode content_cstr 24
  return ui_node_ptr
convertWidgetToNode  (Button _ _) = convertWidgetToNode (Text "button placeholder")

drawWidget :: Widget -> String
drawWidget widget = case widget of
  (Text body) -> body
  (Button button_text _) -> button_text

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- TODO: Putting the example application here until I figure out a better
-- structure. I definitely do not want to have the program state in the same
-- place as the runtime implementation.
--
data AppState = AppState { counterValue :: Int }
type Model = AppState

exampleInitModel :: Model
exampleInitModel = AppState { counterValue = 0 }

-- The update function needs to be able to take a command,
-- process the current state and generate a new state based on
-- that command.
data Command = Increment | Decrement | None
exampleUpdateFn :: Model -> Command -> Model
exampleUpdateFn model Increment = AppState { counterValue = (counterValue model) + 1 }
exampleUpdateFn model Decrement = AppState { counterValue = (counterValue model) - 1 }
exampleUpdateFn model _ = model

exampleViewFn :: Model -> [Widget]
exampleViewFn model = [Text (show (counterValue model)), Text "more stuff", Text "another element"]

exampleProgram :: IO ()
exampleProgram = do
  runProgram exampleInitModel exampleUpdateFn exampleViewFn Increment

foreign export ccall exampleProgram :: IO ()

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
