import Graphics.UI.Gtk
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Data.List (foldl')

main :: IO ()
main = do
  initGUI       -- Initialize the GUI environment
  window <- windowNew  -- Create a new window

  -- Create a vertical box to hold the input field and buttons
  vbox <- vBoxNew False 0
  containerAdd window vbox

  -- Create an entry widget (input field)
  entry <- entryNew
  boxPackStart vbox entry PackNatural 0

  -- Create a grid to organize the buttons
  grid <- tableNew 5 4 True
  boxPackStart vbox grid PackGrow 0

  -- Function to create a number button and its event handler
  let createButton label handler row col = do
        button <- buttonNewWithLabel label
        tableAttachDefaults grid button col (col+1) row (row+1)
        on button buttonActivated handler
        return button

  -- Create number buttons
  mapM_ (\(num, (row, col)) -> createButton (show num) (appendText entry (show num)) row col) $
    zip [1..9] [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]
  createButton "0" (appendText entry "0") 3 1

  -- Create operator buttons
  createButton "+" (appendText entry "+") 0 3
  createButton "-" (appendText entry "-") 1 3
  createButton "*" (appendText entry "*") 2 3
  createButton "/" (appendText entry "/") 3 3
  createButton "." (appendText entry ".") 3 2

  -- Create a 'calculate' button
  buttonCalc <- createButton "=" calculateAndDisplay 3 4

  -- Create a 'clear' button
  buttonClear <- createButton "C" (const (entrySetText entry "")) 0 4

  -- Create a 'clear entry' button
  buttonClearEntry <- createButton "CE" (const (entrySetText entry "")) 1 4

  -- Create a 'backspace' button
  buttonBackspace <- createButton "<-" (backspace entry) 2 4

  -- Event handler for 'calculate' button
  let calculateAndDisplay _ = do
        entryText <- entryGetText entry
        let result = fromMaybe (0 / 0) (calculate entryText) -- Handle errors with NaN
        entrySetText entry (show result)

  -- Define the 'calculate' function
  let calculate expression = evalRPN . toRPN $ expression

  -- Helper function to convert infix expression to RPN (Reverse Polish Notation)
  let toRPN expr = reverse $ foldl' shuntingYard ([], []) expr where
        shuntingYard (out, ops) c
          | isDigit c || c == '.' = (c : head out : tail out, ops)
          | c `elem` "+-*/" = (out, c:ops)
          | otherwise = (out, ops)
        shuntingYard (out, op:ops) _ = (op:out, ops)

  -- Helper function to evaluate RPN expression
  let evalRPN tokens = foldl' evalStep (Just []) tokens where
        evalStep Nothing _ = Nothing
        evalStep (Just stack) token
          | all isDigit token || token == "." = Just (read token : stack)
          | token `elem` ["+", "-", "*", "/"] = case stack of
              (x:y:ys) -> Just (applyOp token y x : ys)
              _ -> Nothing
          | otherwise = Nothing
        applyOp "+" = (+)
        applyOp "-" = (-)
        applyOp "*" = (*)
        applyOp "/" = (/)
        applyOp _ = \_ _ -> 0

  -- Append text to the entry widget
  let appendText entry text _ = do
        entryText <- entryGetText entry
        entrySetText entry (entryText ++ text)

  -- Handle backspace functionality
  let backspace entry _ = do
        entryText <- entryGetText entry
        if not (null entryText)
          then entrySetText entry (init entryText)
          else return ()

  set window [windowTitle := "Calculator", windowDefaultWidth := 300, windowDefaultHeight := 400]

  widgetShowAll window -- Show the window and all its contents
  mainGUI             -- Start the main GUI loop
