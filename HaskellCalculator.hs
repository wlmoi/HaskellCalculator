import Graphics.UI.Gtk
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)

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
  grid <- tableNew 4 4 True
  boxPackStart vbox grid PackGrow 0

  -- Function to create a number button and its event handler
  let createNumberButton num row col = do
        button <- buttonNewWithLabel (show num)
        tableAttachDefaults grid button col (col+1) row (row+1)
        on button buttonActivated $ do
          entryText <- entryGetText entry
          entrySetText entry (entryText ++ show num)
        return button

  -- Create buttons for numbers 0 to 9
  mapM_ (\(num, (row, col)) -> createNumberButton num row col) $
    zip [1..9] [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]
  createNumberButton 0 3 1

  -- Function to create an operator button and its event handler
  let createOperatorButton label row col = do
        button <- buttonNewWithLabel label
        tableAttachDefaults grid button col (col+1) row (row+1)
        on button buttonActivated $ do
          entryText <- entryGetText entry
          -- Append the operator if the last character is not already an operator
          if not (null entryText) && last entryText `elem` "+-*/"
            then return () -- Do nothing if the last character is an operator
            else entrySetText entry (entryText ++ label)
        return button

  -- Create operator buttons
  createOperatorButton "+" 0 3
  createOperatorButton "-" 1 3
  createOperatorButton "*" 2 3
  createOperatorButton "/" 3 3

  -- Create a 'calculate' button
  buttonCalc <- buttonNewWithLabel "="
  tableAttachDefaults grid buttonCalc 3 4 3 4

  -- Event handler for 'calculate' button
  on buttonCalc buttonActivated $ do
    entryText <- entryGetText entry
    let result = fromMaybe 0 (calculate entryText) -- 'calculate' returns Maybe Double
    entrySetText entry (show result)

  -- Create a 'clear' button
  buttonClear <- buttonNewWithLabel "C"
  tableAttachDefaults grid buttonClear 3 4 0 1

  -- Event handler for 'clear' button
  on buttonClear buttonActivated $ entrySetText entry ""

  -- Define the 'calculate' function
  calculate :: String -> Maybe Double
  calculate expression = evalRPN . toRPN $ expression

  -- Helper function to convert infix expression to RPN (Reverse Polish Notation)
  toRPN :: String -> [String]
  toRPN = reverse . foldl shuntingYard ([], [])
    where
      shuntingYard (out, ops) c
        | c `elem` "0123456789" = ([c]:out, ops)
        | c `elem` "+-*/" = (out, c:ops)
        | c == ' ' = (out, ops)
      shuntingYard (out, op:ops) _ = (op:out, ops)

  -- Helper function to evaluate RPN expression
  evalRPN :: [String] -> Maybe Double
  evalRPN = foldl evalStep (Just [])
    where
      evalStep Nothing _ = Nothing
      evalStep (Just stack) token
        | all (`elem` "0123456789") token = Just (read token : stack)
        | token `elem` "+-*/" = case stack of
            (x:y:ys) -> Just ((applyOp token y x) : ys)
            _ -> Nothing
        | otherwise = Nothing
      applyOp "+" = (+)
      applyOp "-" = (-)
      applyOp "*" = (*)
      applyOp "/" = (/)
      applyOp _ = \_ _ -> 0

  set window [windowTitle := "Calculator", windowDefaultWidth := 200, windowDefaultHeight := 200]

  widgetShowAll window -- Show the window and all its contents
  mainGUI             -- Start the main GUI loop
