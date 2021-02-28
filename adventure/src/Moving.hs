{-# OPTIONS -Wall #-}

{-|
Module      : Moving
Description : Przemieszczanie się między pokojami 

Ten moduł zawiera funkcje związane z przemieszczaniem się między pokojami. 
-}

module Moving where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import GameState 
import RoomParser

-- | Funkcja przyjmuje nazwę pokoju i wyszukuje go w typie danych gry. 
-- Zwraca Just 'Room' lub 'Nothing' (w przypadku nie znalezienia).
getRoom :: RoomName -> AdventureGame -> Maybe Room 
getRoom roomName game = M.lookup roomName (gameWorld game)

-- | Funkcja przyjmuje nazwę pokoju i podmienia ją jako nazwę aktualnego pokoju w AdventureGame.
changeRoom :: RoomName -> AdventureGame -> AdventureGame
changeRoom room (AdventureGame w inv _) = AdventureGame w inv room

-- | Funkcja sprawdza czy istenieje z pokoju wyjście w zadanym kierunku, 
-- jeśli wyjście istnieje to zostaje zwrócona nazwa pokoju w monadzie 'Maybe'
checkExit :: Direction -> AdventureGame -> Maybe RoomName 
checkExit wantedDirection (AdventureGame world _ currentRoom) = M.lookup currentRoom world >>= \room -> M.lookup wantedDirection (exits room)

-- | Funkcja przyjmuje kierunek typu 'Direction'. 
-- Jeśli wykonanie ruchu w danym kierunku jest możliwe stan gry zostaje zmodyfikowany poprzez wykonanie ruchu.
-- W przeciwnym przypadki wypisywany jest odpowiedni komunikat. 
move :: Direction -> GameIO Bool
move direction = do 
   advGame <- get
   case checkExit direction advGame of 
      Just roomName -> modify' (changeRoom roomName) >> showCurrentRoom >> return True
      Nothing -> lift (putStrLn "There is no way to go that direction.") >> return True

-- | Funkcja wypisuje na ekran pokój w którym aktualnie znajduje się gracz. 
showCurrentRoom :: GameIO ()
showCurrentRoom = gets getCurrentRoom >>= lift . print