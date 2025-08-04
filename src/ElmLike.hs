module ElmLike (
  runProgram,
  Widget(..),
) where

import Control.Concurrent -- threadDelay
import Foreign.C.Types

foreign import ccall "elmlike start_gui"
  _start_gui :: IO ()
foreign import ccall "elmlike stop_gui"
  _stop_gui :: IO ()
foreign import ccall "elmlike poll_event_signal"
  _poll_event_signal :: IO CInt

-- EventSignal: Keep these signal definitions in sync with
-- the definition in the elmlike clib.
data EventSignal = EventSignal_NONE | EventSignal_QUIT deriving (Show, Enum)

intToEventSignal :: CInt -> EventSignal
intToEventSignal 1 = EventSignal_QUIT
intToEventSignal _ = EventSignal_NONE

-- Widgets
data Widget = Text String | Button String String -- TODO button should have some 'onClick' events

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
  _start_gui

  programLifecycleStep (Program {
    model    = initialModel,
    updateFn = updateFn,
    viewFn   = viewFn
  }) cmd

  _stop_gui

programLifecycleStep :: Program model_type command_type -> command_type -> IO ()
programLifecycleStep program cmd = do
  let
      combined_widgets = (map drawWidget ((viewFn program) (model program)))

      concatWithSpace :: String -> String -> String
      concatWithSpace a b = a ++ " " ++ b

      spaceConcat :: [String] -> String
      spaceConcat lst = (foldl concatWithSpace "") lst

      in putStrLn (spaceConcat combined_widgets)

  do
    signal_raw <- _poll_event_signal
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
