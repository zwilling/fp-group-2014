import System.Directory
import Data.Char
import Data.List

main :: IO ()
main = do
  recipes <- findRecipes
  if (null recipes) then putStrLn "No recipes available..."
  else do
    promptForInput recipes
    meal <- readInput recipes
    if (isExitCommand meal) then putStrLn "Leaving, good bye..."
    else do
      showRecipe meal
      main

findRecipes :: IO [String]
findRecipes = do
  files <- getDirectoryContents "."
  return (map removeEnding (filter (isSuffixOf ending) files))

ending :: String
ending = ".recipe"

removeEnding :: String -> String
removeEnding str = takeWhile (/='.') str

-- list all available meals and ask the user to input the name of the
-- desired meal
promptForInput :: [String] -> IO ()
promptForInput recipes = -- TODO

-- reads the input from the user until he either inputs the name of an
-- existing meal or an exit command (see isExitCommand)
readInput :: [String] -> IO String
readInput recipes = do
  -- TODO read the name of the meal from the command line
  -- the result should be available in the variable 'meal'
  if (not (meal `elem` recipes) && not (isExitCommand meal))
  then do
    putStrLn "I do not know a recipe for this meal, try again..."
    promptForInput recipes
    readInput recipes
  else -- TODO

isExitCommand :: String -> Bool
isExitCommand cmd = (map toLower cmd) `elem` ["quit", "exit", "q", "e"]

-- print the recipe with the given name on the command line
showRecipe :: String -> IO ()
showRecipe meal = -- TODO
