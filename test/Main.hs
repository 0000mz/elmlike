module Main (main) where

import ElmLike

data AppState = AppState { counterValue :: Int }
type Model = AppState

initModel :: Model
initModel = AppState { counterValue = 0 }

-- The update function needs to be able to take a command,
-- process the current state and generate a new state based on
-- that command.
data Command = Increment | Decrement | None
updateFn :: Model -> Command -> Model
updateFn model Increment = AppState { counterValue = (counterValue model) + 1 }
updateFn model Decrement = AppState { counterValue = (counterValue model) - 1 }
updateFn model _ = model

viewFn :: Model -> [Widget]
viewFn model = [Text (show (counterValue model)), Text "more stuff", Text "another element"]

main :: IO ()
main = do
  putStrLn "Running tests..."
  ElmLike.runProgram initModel updateFn viewFn Increment
  putStrLn "Tess finished."
