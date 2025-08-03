module ElmLike (
  runProgram,
  Widget(..),
) where

import Control.Concurrent -- threadDelay

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
runProgram initialModel updateFn viewFn cmd = programLifecycleStep (Program {
  model    = initialModel,
  updateFn = updateFn,
  viewFn   = viewFn
}) cmd

programLifecycleStep :: Program model_type command_type -> command_type -> IO ()
programLifecycleStep program cmd = do
  let combined_widgets = map drawWidget ((viewFn program) (model program))

      concatWithSpace :: String -> String -> String
      concatWithSpace a b = a ++ " " ++ b

      spaceConcat :: [String] -> String
      spaceConcat lst = (foldl concatWithSpace "") lst

      in putStrLn (spaceConcat combined_widgets)

  -- TODO: Sleeping here for the sake of simulating state change. In this function,
  -- we are signaling the same command indefinitely to the program's update then sleeping.
  -- What we want instead is a way for events/actions taken within the program lifecycle
  -- to emit the corresponding commands that can be used to generate a new state.
  --
  -- Need to brainstorm how to go about this implementation.
  threadDelay 500000

  programLifecycleStep (programRunUpdate program cmd) cmd

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
