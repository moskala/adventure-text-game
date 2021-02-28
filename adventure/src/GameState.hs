{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : GameState
Description : Funkcje i typy danych dotyczące stanu gry. 

Ten moduł zawiera definicje typu stanu gry oraz funkcji z tym związanych.
Zawiera również funkcje odpowiadające za zapis i odczyt stanu gry z pliku. 
-}

module GameState where

import GHC.Generics
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Data.Maybe
import Data.Aeson
import Control.Monad.Trans.Class
import System.FilePath
import System.IO.Error

import RoomParser

-- | Alias dla skrócenia zapisu. 
type GameIO a = StateT AdventureGame IO a 

-- | Typ reprezentujący stan gry
data AdventureGame = AdventureGame {
   gameWorld :: World, -- ^ mapa zawierające wszystkie pokoje, czyli świat gry
   inventory :: [ItemName], -- ^ lista zawierająca posiadane przedmioty
   currentRoomName :: RoomName -- ^ nazwa pokoju w którym aktualnie znajduje się gracz
} deriving (Generic)

-- | Alias dla mapy pokojów, czyli świata gry. 
type World = M.Map RoomName Room

-- | Domyślna implementacja ToJSON dla AdventureGame
instance ToJSON AdventureGame where 

-- | Domyślna implementacja FromJSON dla AdventureGame
instance FromJSON AdventureGame where

-- | Funkcja koduje i zapisuje stan gry typu 'AdventureGame' do podanego pliku. 
-- Plik zostaje zapisany do folderu gameFiles. 
saveStateToFile :: FilePath -> AdventureGame -> IO ()
saveStateToFile fileName game = encodeFile ("gameFiles" </> fileName) game >> putStrLn "State saved!"

-- | Funkcja odczytuje stan gry typu 'AdventureGame' z podanego pliku. 
-- Plik powinien się znajdować w folderze gameFiles. 
loadStateFromFile :: FilePath -> IO (Either String AdventureGame)
loadStateFromFile fileName = eitherDecodeFileStrict $ "gameFiles" </> fileName

-- | Funkcja próbuje załadować stan gry z podanego pliku za pomocą funkcji 'loadStateFromFile'. 
-- Jeśli stan gry zostanie poprawnie sparsowany, jest on ustawiany jako aktualny. 
-- Wpp wypisywany jest błąd. 
loadState :: FilePath -> GameIO ()
loadState file = do 
   result <- lift $ loadStateFromFile file
   case result of 
      Right game -> put game 
      Left err -> lift (putStrLn err)

-- | Funkcja zapisuje stan gry za pomocą funkcji 'saveStateToFile'. 
saveState :: FilePath -> GameIO ()
saveState file = get >>= lift . saveStateToFile file 

-- | Funkcja zwracająca aktualny pokój. 
-- Jeśli pokój nie zostanie odnaleziony poprzez nazwę, zwracany jest "zgubiony pokój".
getCurrentRoom :: AdventureGame -> Room
getCurrentRoom (AdventureGame world _ currentRoom) = fromMaybe lostRoom (M.lookup currentRoom world) 

-- | Funkcja zwracająca aktualny pokój w monadzie Maybe. 
-- Jeśli pokój nie zostanie odnaleziony poprzez nazwę, zwracane jest Nothing
getCurrentRoom' :: AdventureGame -> Maybe Room
getCurrentRoom' (AdventureGame world _ currentRoom) = M.lookup currentRoom world

-- | Funkcja restartuje grę ustawiając nowy stan gry. 
restartGame :: GameIO Bool 
restartGame = do 
   rooms <- lift parseGameRomms
   startRoom <- lift parseStartRoom        
   case rooms of 
        Left err -> lift $ putStrLn err >> return False
        Right r -> put (AdventureGame r [] startRoom) >> lift (putStrLn "Game restarted.") >> return True

-- | Funkcja wyświetla zawartość podanego pliku znajdującego się w folderze gameFiles. 
showGameFile :: FilePath -> IO ()
showGameFile fileName = catchIOError (readFile ("gameFiles" </> fileName) >>= putStrLn) catchErrFun
  where
     catchErrFun err = if isDoesNotExistError err then putStrLn "This file is not defined :(" else ioError err

-- | Funkcja wyświetla plik zawierający instrukcję pomocy dla gracza. 
-- Plik powinien się znajdować w folderze gameFiles i mieć nazwę help.txt. 
showHelp :: GameIO Bool 
showHelp = lift $ showGameFile "help.txt" >> return True

-- | Funkcja wyświetla plik zawierający powitanie dla gracza. 
-- Plik powinien się znajdować w folderze gameFiles i mieć nazwę welcome.txt. 
showWelcome:: IO ()
showWelcome = showGameFile "welcome.txt"

-- | Funkcja wyświetla pokój o podanej nazwie.
-- Jest używana do wyświetlania opisu startowego pokoju. 
showFirstRoom :: RoomName -> World -> IO ()
showFirstRoom roomName world  =  print $ M.findWithDefault lostRoom roomName world 

-- | Funkcja 'processActionWithMessage' służy do wypisywania podanej wiadomości.
processActionWithMessage :: String -> GameIO ()
processActionWithMessage msg = lift $ putStrLn msg

-- | Funkcja 'acceptAction' służy do wypisywania wiadomości o powodzeniu wykonania akcji. 
acceptAction :: GameIO ()
acceptAction = processActionWithMessage "Ok."

-- | Funkcja 'rejectAction' służy do wypisywania wiadomości o niepowodzeniu wykonania akcji. 
rejectAction :: GameIO ()
rejectAction = processActionWithMessage "I don't know that."


