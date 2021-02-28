{-# OPTIONS -Wall #-}

{-|
Module      : GameExe
Description : Funkcje służące do prowadzenia gry. 

Ten moduł zawiera definicje funkcji służących do prowadzenia gry - parsowania i wykonywania akcji od gracza.  
-}

module GameExe where

import GameState
import CommandParserText
import RoomParser
import Control.Monad.Trans.Class
import Text.Parsec
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Text as T
import Moving
import Items 
import GameActions

-- | Funkcja 'readCommand' parsuje wejście od użytkownika. 
-- W przypadku sukcesu zwracana jest odpowiednia akcja. 
readCommand :: ActionsMap -> GameIO (Either ParseError Action)
readCommand actions = lift getLine >>= pure . (parseCommand actions) . T.pack

-- | Funkcja 'mainLoop' pełni rolę pętli, która odpowiada za ciągłe przetwarzanie i wykonywanie komend. 
-- Za każdym razem sprawdzany jest wynik wykoniania akcji, który informuje o tym, czy gra powinna być kontynuowana. 
mainLoop :: ActionsMap -> GameIO ()
mainLoop actions = do    
    result <- readCommand actions
    continue <- case result of 
        Right action -> executeAction actions action
        Left _ -> lift $ putStrLn "I don't know that." >> pure True
    when continue (mainLoop actions)

-- | Funkcja 'startGame' służy do rozpoczęcia gry. 
-- Ładowane i parsowne są odpowiednie pliki, za pomocą funkcji 'mainLoop' przetwarzane są kolejne komendy. 
startGame :: IO ()
startGame = do 
    showWelcome
    roomsParsed <- parseGameRomms
    startRoom <- parseStartRoom   
    actions <- parseActions     
    case roomsParsed of 
        Left err -> putStrLn err 
        Right rooms -> showFirstRoom startRoom rooms >> evalStateT (mainLoop actions) (AdventureGame rooms [] startRoom)

-- | Funkcja 'executeAction' służy wykonaniu danej akcji. 
-- W zwracanej monadzie jest wartość Bool, która informuje czy gra powinna się toczyć dalej, czy zakończyć. 
executeAction :: ActionsMap -> Action -> GameIO Bool
executeAction _ (Move direction) = move direction
executeAction _ (Take item) = takeItem item 
executeAction _ (Put item) = putItem item 
executeAction _ Help = showHelp 
executeAction _ Restart = restartGame 
executeAction _ (SaveState filename) = saveState filename >> return True
executeAction _ (Continue filename) = loadState filename >> return True
executeAction _ Quit = return False 
executeAction _ Inventory = showInventory
executeAction _ Look = showCurrentRoom >> return True
executeAction actions (Use item input) = useItem input item actions
    