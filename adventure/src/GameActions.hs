{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : GameActions
Description : Funkcje dotyczące użycia przedmiotów. 

Ten moduł zawiera definicje funkcji obsługującyh akcje związane z wykorzystaniem przedmiotów.
Dany przedmiot może być użyty tylko jeśli została dla niego zdefiniowana akcja typu 'GameAction' o nazwie 'ActionName'.
-}

module GameActions where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Trans.State
import Data.Maybe
import Data.List
import GHC.Generics
import Data.Aeson
import System.FilePath

import GameState
import RoomParser
import Moving
import Items

-- | Typ do zarządzania akcjami dla przedmiotów. 
-- Jest to mapa, której kluczami są typy 'ActionName' a wartościami 'GameAction. 
type ActionsMap = M.Map ActionName GameAction

-- | Alias dla nazwy akcji. 
type ActionName = T.Text

-- | Alias dla wiadomości w przypadku sukcesu akcji. 
type ActionMessage = String

-- | Alias dla wejścia od użytkownika. 
type UserActionInput = T.Text

-- | Typ danych reprezentujący rodzaje akcji związanych z przedmiotami.
data ActionType = 
    AddItem ItemName -- ^ dodanie przedmiotu
    | RemoveItem ItemName -- ^ usunięcie przedmiotu
    | ReplaceItem ItemName ItemName -- ^ zamienienie przedmiotu
    deriving (Generic)

-- | Typ danych reprezentujący rodzaje wyników akcji związanych z przedmiotami.
data ActionResult = 
    ChangeCurrentRoom RoomName -- ^ zmiana aktualnego pokoju na inny
    | ChangeInventory ActionType -- ^ zmiana posiadanych przedmiotów
    | ChangeRoomInventory ActionType -- ^ zmiana przedmiotów znajdującyh się w aktualnym pokoju
    | DoNothing -- ^ brak zmiany świata gry
    deriving (Generic)

-- | Typ danych reprezentujący warunki pomyślnego wykonania akcji. 
data ActionConditions = ActionConditions {
    roomName :: RoomName, -- ^ nazwa pokoju w którym akcja musi się odbyć
    itemsInRoom :: [ItemName] -- ^ lista przedmiotów znajdujących się w aktualnym pokoju
} deriving (Generic)

-- | Typ danych reprezentujący akcję związaną z przedmiotem. 
data GameAction = GameAction {
    itemName :: ItemName, -- ^ nazwa przedmiotu którego dotyczy akcja
    message :: ActionMessage, -- ^ wiadomość wyświetlania po pozytywnym wykonaniu akcji
    conditions :: ActionConditions, -- ^ warunki niezbędne do pozytywnego wykonania akcji
    actionResult :: ActionResult -- ^ wynik działania akcji 
} deriving (Generic)

-- | Domyśla implementacja FromJSON dla typu ActionType
instance FromJSON ActionType
-- | Domyśla implementacja ToJSON dla typu ActionType
instance ToJSON ActionType 
-- | Domyśla implementacja FromJSON dla typu ActionResult
instance FromJSON ActionResult
-- | Domyśla implementacja ToJSON dla typu ActionResult
instance ToJSON ActionResult
-- | Domyśla implementacja FromJSON dla typu ActionConditions
instance FromJSON ActionConditions
-- | Domyśla implementacja ToJSON dla typu ActionConditions
instance ToJSON ActionConditions
-- | Domyśla implementacja FromJSON dla typu GameAction
instance FromJSON GameAction
-- | Domyśla implementacja ToJSON dla typu GameAction
instance ToJSON GameAction

-- | Funkcja przyjmuje wynik akcji 'ActionResult' i w zależności od jego typu wywłuje odpowiednie funckje zmieniające stan gry. 
executeActionResult :: ActionResult -> AdventureGame -> AdventureGame
executeActionResult (ChangeCurrentRoom rName) = changeRoom rName
executeActionResult (ChangeInventory (AddItem item)) = addItemToInventory item
executeActionResult (ChangeInventory (RemoveItem item)) = removeItemFromInventory item
executeActionResult (ChangeRoomInventory (AddItem item)) = putItemChangeGame item
executeActionResult (ChangeRoomInventory (RemoveItem item)) = (removeItemFromInventory item) . takeItemChangeGame item
executeActionResult (ChangeInventory (ReplaceItem itemTake itemGive)) =  (addItemToInventory itemGive) . (removeItemFromInventory itemTake)
executeActionResult (ChangeRoomInventory (ReplaceItem itemTake itemGive)) = (putItemChangeGame itemGive) . (removeItemFromInventory itemTake) . (takeItemChangeGame itemTake)
executeActionResult DoNothing = id

-- | Funkcja przyjmuje pokój i listę przedmiotów. 
-- Zwraca True jeśli wszystkie przedmioty z listy znajdują się w pokoju. Wpp zwraca False.
doesRoomContainItems :: Room -> [ItemName] -> Bool
doesRoomContainItems room wantedItems = null (M.keys (items room) \\ wantedItems)

-- | Funkcja przyjmuje warunki typu 'ActionConditions' i sprawdza, czy wszystkie warunki do wykonania akcji są spełnione. 
checkConditions :: ActionConditions -> AdventureGame -> Bool
checkConditions cond game = wantedRoom && wantedItemsInRoom
  where
      currentRoom = getCurrentRoom game
      wantedRoom = currentRoomName game == roomName cond
      wantedItemsInRoom = doesRoomContainItems currentRoom (itemsInRoom cond)

-- | Funkcja przyjmuje dane wejściowe od użytkownika, nazwę przedmiotu.
-- Sprawdza czy akcja może zostać wykonana, to znaczy czy dany przedmiot jest w posiadaniu, 
-- czy istnieje akcja o danej nazwie i czy wszystkie potrzebne do niej warunki są spełnione. Zwraca True lub False.
canActionBeExecuted :: UserActionInput -> ItemName -> ActionsMap -> AdventureGame -> Bool
canActionBeExecuted input item actionSet game = itemPossesion item game && M.member input actionSet && checkConditions (conditions action) game
  where
      action = fromJust $ M.lookup input actionSet

-- | Funkcja wyciąga z mapy 'GameAction' dla podanej nazwy akcji. 
-- Uwaga: tą funkcę można wywoływać tylko po sprawdzeniu, że w mapie znajduje się akcja o podanej nazwie. 
getActionResult :: ActionName -> ActionsMap -> GameAction
getActionResult input actionSet = fromJust (M.lookup input actionSet)

-- | Funkcja wykonuje daną akcję poprzez modyfikację stanu gry. 
-- Funkcja wypisuje stosowną wiadomość o sukcesie lub porażce. 
runAction :: ActionName -> ActionsMap -> GameIO ()
runAction input actionSet = modify' (executeActionResult (actionResult gameAction)) >> processActionWithMessage (message gameAction)
  where
      gameAction = getActionResult input actionSet

-- | Funkcja przyjmuje wejście od użytkownika, nazwę przedmiotu i mapę akcji.
-- Akcja zostaje sprawdzona funkcją 'canActionBeExecuted' i ewentualnie wykonana funkcją 'runAction'. 
useItem :: UserActionInput -> ItemName -> ActionsMap -> GameIO Bool
useItem input item actionSet = do
    advGame <- get
    if canActionBeExecuted input item actionSet advGame then runAction input actionSet else processActionWithMessage "Nothing happend."
    return True

-- | Funkca parsuje flik z akcjami. 
-- Plik powinien się znajodwać w folderze gameFiles i mieć nazwę actions. 
loadMapFromFile ::IO (Either String (M.Map ActionName GameAction))
loadMapFromFile = eitherDecodeFileStrict $ "gameFiles" </> "actions"

-- | Funkcja próbuje sparsować plik z akcjami. 
-- Zwraca sparsowaną mapę akcji lub wypisuje wiadomość o porażce i zwraca pustą mapę.
parseActions :: IO ActionsMap
parseActions = do 
    act <- loadMapFromFile
    case act of 
        Right actions -> return actions 
        Left err -> putStrLn err >> putStrLn "Actions not loaded" >> return M.empty 
        
-- | Funkcja sprawdza czy dana akcja z wejścia użytkownika znajduje się w zbiorze wszystkich akcji. 
checkInputActionsMap :: UserActionInput -> ActionsMap -> Bool
checkInputActionsMap = M.member


-- simpleAction :: GameAction
-- simpleAction = GameAction (T.pack "key") "You opended the box." (ActionConditions (T.pack "chamber") [T.pack "box"]) (ChangeRoomInventory $ ReplaceItem (T.pack "box") (T.pack "opened box"))

-- actionMap :: ActionsMap 
-- actionMap = M.fromList [(T.pack "use key", simpleAction), (T.pack "open key", simpleAction)]

-- parseMap :: IO ()
-- parseMap = encodeFile ("gameFiles" </> "actions") actionMap