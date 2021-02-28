{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : RoomParser
Description : Definicja typów danych związanych z pokojami i parsowanie pokoi. 

Ten moduł zawiera definicje typów związanych z pokojami i przemiszczaniem się między nimi. 
Znajdują się tu również funkcje odpowiedzialne za parsowanie pokoi do gry.  
-}

module RoomParser where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson
-- import Data.Aeson.Types(Parser)
import GHC.Generics
import System.FilePath

-- | Alias dla nazwy pokoju. 
type RoomName = T.Text

-- | Alias dla nazwy przedmiotu. 
type ItemName = T.Text

-- | Alias dla opisu pozycji przedmiotu. 
type PositionDesc = T.Text

-- | Typ do zarządzania przedmiotami w pokoju. 
-- Składa się z mapy, której kluczami są nazwy przedmiotów, a wartościami ich opisy.
type ItemsInRoom = M.Map ItemName PositionDesc

-- | Typ do zarządzania wyjściami z pokoju. 
-- Jest to mapa, której kluczami są kierunki typu 'Direction', a wartościami nazwy pokojów typu 'RoomName'.
type ExitsFromRoom = M.Map Direction RoomName

-- | Typ danych reprezentujący kierunki świata, wykorzystywany do oznaczania wyjść z pokoju. 
data Direction = North | East | South | West deriving (Show, Eq, Generic)

-- | Typ danych reprezentujący pokój. 
data Room = Room {
   name :: RoomName, -- ^ nazwa pokoju
   description :: T.Text, -- ^ opis danego miejsca
   items :: ItemsInRoom, -- ^ zbiór przedmiotów znajdującyh się w pokoju
   exits :: ExitsFromRoom  -- ^ zbiór możliwych wyjść z pokoju
} deriving (Generic)

-- | Implementacja 'Ord' dla typu 'Direction', aby typ 'Direction' można było wykorzystywać jako klucze do map
-- Kierunki są posortowane zgodnie z kierunkiem wskazówek zegara zaczynając od północy.  
instance Ord Direction where
   North <= _ = True
   _ <= North = False
   West <= _ = False
   _ <= West = True
   East <= South = True
   South <= East = False
   East <= East = True
   South <= South = True

-- | Implementacja 'Show' dla typu 'Room'. 
-- W przypadku pokoju wypisywany jest jego opis, a następnie dodawne są opisy położenia dla każdego przedmiotu znajdującego się w pokoju. 
instance Show Room where
   show room = roomDescription ++ itemsInRoom
     where
        roomDescription = T.unpack (description room)
        space = T.pack " "
        itemsInRoom =T.unpack $ M.foldl (\acc item -> T.concat [acc, space, item]) T.empty (items room)

-- | Domyślna implementacja FromJSON dla typu 'Direction' niezbędna do odczytu pokojów z pliku. 
instance FromJSON Direction

-- | Domyślna implementacja ToJSON dla typu 'Direction' niezbędna do zapisu pokojów do pliku. 
instance ToJSON Direction

-- | Domyślna implementacja FromJSONKey dla typu 'Direction' niezbędna odczytu pokojów z pliku.  
instance FromJSONKey Direction

-- | Domyślna implementacja ToJSONKey dla typu 'Direction' niezbędna do zapisu pokojów do pliku.
instance ToJSONKey Direction

-- | Domyślna implementacja FromJSON dla typu 'Room'.
instance FromJSON Room

-- | Domyślna implementacja ToJSON dla typu 'Room'.
instance ToJSON Room

-- | Funkcja parsująca wszystkie pokoje do gry.
-- Plik definiujący pokoje powinien znajdować się w foldere gameFiles i mieć nazwę rooms.json. 
parseGameRomms :: IO (Either String (M.Map RoomName Room))
parseGameRomms = eitherDecode <$> B.readFile ("gameFiles" </> "rooms.json")

-- | Funkcja zwraca zagubiony pokój. 
-- Podczas gry oznacza to, że świat gry był źle zdefiniowany i gracz wszedł do pokoju który nie został zdefiniowany. 
lostRoom :: Room
lostRoom = Room (T.pack "lost") (T.pack "The game has crushed. You are lost! :(") M.empty M.empty

-- | Funkcja zwraca nazwę pokoju od którego rozpoczynana jest gra. 
parseStartRoom :: IO RoomName
parseStartRoom = return $ T.pack "start room"

-- parseDirection' :: String -> Parser Direction 
-- parseDirection' x 
--     | x == "north" = return North 
--     | x == "east" = return East
--     | x == "south" = return South 
--     | x == "west" = return West
--     | otherwise = fail $ "Not known direction: " ++ x 

-- parseRoom :: IO ()
-- parseRoom = do
--  d <- (eitherDecode <$> getJSON) :: IO (Either String [Room])
--  case d of
--   Left err -> putStrLn err
--   Right ps -> print ps


-- simpleRoom :: Room 
-- simpleRoom = Room (T.pack "start room") (T.pack "You are in a large house for great spring.") simpleRoomItems simpleRoomExits

-- simpleRoomItems :: ItemsInRoom 
-- simpleRoomItems = M.fromList [(T.pack "lamp", T.pack "On the table there is a lamp."), (T.pack "food", T.pack "There is food.")]

-- simpleRoomExits :: ExitsFromRoom 
-- simpleRoomExits = M.fromList [(West, T.pack "On the left there is huge door."), (North, T.pack "In front of you there is open window.")]


-- parseGameRomms :: IO (Either String [Room])
-- parseGameRomms = eitherDecode <$> getJSON

-- parseRooms :: [Room] -> M.Map RoomName Room
-- parseRooms rooms = M.fromList $ fmap (\r@(Room roomName _ _ _) -> (roomName, r)) rooms


-- parseSimpleRoom :: IO ()
-- parseSimpleRoom = encodeFile "oneRoom.json" simpleRoom

-- parseMapRoom :: IO ()
-- parseMapRoom = encodeFile "mapRoom.json" (M.insert "start room" simpleRoom M.empty)