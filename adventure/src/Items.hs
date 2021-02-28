{-# OPTIONS -Wall #-}

{-|
Module      : Items
Description : Zabieranie i odkładanie przedmiotów. 

Ten moduł zawiera definicje funkcji związanych z podnoszeniem i odkładaniem przedmiotów w pokoju. 
Znajdują się tu funkcje odpowiadające zarówno za zmianę pokoi jak i zmianę posiadanych przedmiotów. 
-}

module Items where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Text as T
import Data.List
import Data.Maybe

import GameState
import RoomParser

-- | Funkcja przyjmuje nazwę przedmiotu i dodaje przedmiot do inwentarza. 
addItemToInventory :: ItemName -> AdventureGame -> AdventureGame
addItemToInventory item (AdventureGame world itemNames roomName) =  AdventureGame world (item : itemNames) roomName

-- | Funkcja przyjmuje nazwę przedmiotu i usuwa przedmiot do inwentarza.
removeItemFromInventory :: ItemName -> AdventureGame -> AdventureGame
removeItemFromInventory item (AdventureGame world itemNames roomName) =  AdventureGame world (delete item itemNames) roomName

-- | Funkcja przyjmuje nazwę przedmiotu i sprawdza czy znajduje się on w inwentarzu. 
itemPossesion :: ItemName -> AdventureGame -> Bool 
itemPossesion item game = item `elem` inventory game

-- | Funkcja przyjmuje nazwę przedmiotu i sprawdza czy znajduje się on w aktualnym pokoju gracza. 
lookForItemInRoom :: ItemName -> AdventureGame -> Maybe Bool
lookForItemInRoom item game = getCurrentRoom' game >>= (pure . M.member item . items)

-- | Funkcja przyjmuje nazwę przedmiotu oraz pokój i zmienia mapę pokojów poprzez usunięcie przedmiotu z pokoju funkcją 'takeItemChangeRoom'. 
takeItemChangeWorld :: ItemName -> Room -> World -> World
takeItemChangeWorld item room = M.adjust (takeItemChangeRoom item) (name room)

-- | Funkcja przyjmuje nazwę przedmiotu oraz pokój.
-- Zwraca pokój z usuniętym przedmiotem.  
takeItemChangeRoom :: ItemName -> Room -> Room
takeItemChangeRoom item (Room rName desc rItems rExits) = Room rName desc (M.delete item rItems) rExits

-- | Funkcja przyjmuje nazwę przedmiotu i zmienia stan gry poprzez usunięcie go z pokoju. 
takeItemChangeGame :: ItemName -> AdventureGame -> AdventureGame
takeItemChangeGame item g@(AdventureGame world itemNames roomName) = AdventureGame newWorld newInv roomName
  where
      newWorld = takeItemChangeWorld item (getCurrentRoom g) world
      newInv =  item : itemNames

-- | Funkcja przyjuje nazwę przedmiotu, sprawda czy jest on w aktualnym pokoju. 
-- Jeśli przedmiot jest dostępny stan gry zostaje zmodyfikowany poprzez zabranie przedmiotu z pokoju 
-- i umieszczenie go w inwertarzu. 
-- W przeciwnym przypadku jest wypisywany odpowiedni komunikat stan gry nie zostaje zmodyfikowany.
takeItem :: ItemName -> GameIO Bool
takeItem item = do
  advGame <- get
  if fromMaybe False $ lookForItemInRoom item advGame then acceptAction >> modify' (takeItemChangeGame item) else lift (putStrLn "You can't take this.")
  return True

-- | Funkcja przyjmuje nazwę przedmiotu i zmienia stan gry poprzez dodanie go do pokoju.
putItemChangeGame :: ItemName -> AdventureGame -> AdventureGame
putItemChangeGame item g@(AdventureGame world itemNames roomName) = AdventureGame newWorld newInv roomName
  where
      newWorld = putItemChangeWorld item (getCurrentRoom g) world
      newInv =  delete item itemNames

-- | Funkcja przyjmuje nazwę przedmiotu oraz pokój i zmienia mapę pokojów poprzez dodanie przedmiotu z pokoju funkcją 'putItemChangeRoom'. 
putItemChangeWorld :: ItemName -> Room -> World -> World
putItemChangeWorld item room = M.adjust (putItemChangeRoom item) (name room)

-- | Funkcja przyjmuje nazwę przedmiotu oraz pokój.
-- Zwraca pokój z dodanym przedmiotem i domyślym opisem lokalizacji.  
putItemChangeRoom :: ItemName -> Room -> Room
putItemChangeRoom item (Room rName desc rItems rExits) = Room rName desc newInv rExits
  where
      newInv = M.insert item (T.pack "There is " `T.append` item `T.append` T.pack ". ") rItems

-- | Funkcja przyjuje nazwę przedmiotu, sprawda czy jest on w inwentarzu. 
-- Jeśli przedmiot jest dostępny stan gry zostaje zmodyfikowany poprzez położenie przedmiotu w pokoju. 
-- W przeciwnym przypadku jest wypisywany odpowiedni komunikat stan gry nie zostaje zmodyfikowany. 
putItem :: ItemName -> GameIO Bool
putItem item = do
  advGame <- get
  if itemPossesion item advGame then acceptAction >> modify' (putItemChangeGame item) else lift (putStrLn "You don't have that item.")
  return True

-- | Funkcja wypisuje na konsolę aktualny stan posiadanych przedmiotów. 
showInventory :: GameIO Bool
showInventory = get >>= lift . print . inventory >> return True
