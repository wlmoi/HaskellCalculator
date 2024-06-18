import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI       -- Initialize the GUI environment
  window <- windowNew  -- Create a new window
  
  -- Create a button for addition '+'
  buttonAdd <- buttonNewWithLabel "+"
  boxPackStart vbox buttonAdd PackNatural 0

  -- Event handler for button '+'
  on buttonAdd buttonActivated $ do
    entryText <- entryGetText entry
    -- Check if the last character is already an operator
    if not (null entryText) && last entryText `elem` "+-*/"
      then return () -- Do nothing if the last character is an operator
      else entrySetText entry (entryText ++ "+") -- Append '+' to the input field
  

  -- Create a 'calculate' button
  buttonCalc <- buttonNewWithLabel "="
  boxPackStart vbox buttonCalc PackNatural 0

  -- Event handler for 'calculate' button
  on buttonCalc buttonActivated $ do
    entryText <- entryGetText entry
    let result = calculate entryText -- 'calculate' is a function you would define to compute the result
    entrySetText entry (show result)

  -- Define the 'calculate' function (this is a placeholder for your actual calculation logic)
  calculate :: String -> Double
  calculate expression = read expression -- This is just a placeholder, replace with actual logic

  -- Create a vertical box to hold the input field and buttons
  vbox <- vBoxNew False 0
  containerAdd window vbox
  
  -- Create an entry widget (input field)
  entry <- entryNew
  boxPackStart vbox entry PackNatural 0
  
  -- Create a button for number '1'
  button1 <- buttonNewWithLabel "1"
  boxPackStart vbox button1 PackNatural 0
  
  -- Event handler for button '1'
  on button1 buttonActivated $ do
    entryText <- entryGetText entry
    entrySetText entry (entryText ++ "1")
  
  set window [windowTitle := "Calculator", windowDefaultWidth := 200, windowDefaultHeight := 200]
  
  widgetShowAll window -- Show the window and all its contents
  mainGUI             -- Start the main GUI loop
