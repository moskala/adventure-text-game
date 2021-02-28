{-# OPTIONS -Wall #-}

{-|
Module      : CommandParserText
Description : Moduł parsujący komendy użytkownika.

W module znajdują się funkcje związane z parsowaniem tekstu od użytkownika. 
Jest tu deklaracja typu danych 'Action', który reprezentuje obsługiwane przez program komendy. 
Dane wpisywana przez użytkownika są parsowane do odpowiednich akcji. Moduł zawiera również obsługiwane synonimy bazowych komend.  
Dane są przetwarzane w formacie Text. 
-}

module CommandParserText where

import Text.Parsec
import Text.Parsec.Text(Parser)
import Control.Monad.Trans.Class
import qualified Data.Text as T

import RoomParser
import GameActions

-- | Typ danych reprezenujący akcje obsługiwane przez program 
data Action = Move Direction -- ^ akcja przemieszczenia się między pokojami
  | Take ItemName -- ^ akcja zabrania przedmiotu
  | Put ItemName  -- ^ akcja odłożenia przedmiotu
  | Help -- ^ wypisanie na konsolę informacji pomocy
  | Restart -- ^ funkcja restartująca stan gry
  | SaveState String -- ^ zapisanie aktualnego stanu gry do podanego pliku
  | Continue String-- ^ wczytanie zapisanego stanu gry z podanego pliku
  | Quit -- ^ wyjście z programu
  | Inventory -- ^ wypisanie posiadanych przedmiotów
  | Look -- ^ wypisanie opisu pokoju w którym gracz się znajduje
  | Use ItemName UserActionInput -- ^ użycie przedmiotu o danej nazwie w podany sposób
  deriving Show

-- | Akceptowalne synonimy komendy "go".
goSynonyms :: [T.Text]
goSynonyms = map T.pack ["go", "move", "walk", "proceed", "travel"]

-- | Akceptowalne synonimy komendy "take".
takeSynonyms :: [T.Text]
takeSynonyms = map T.pack ["take", "pick", "grab"]

-- | Akceptowalne synonimy komendy "put".
putSynonyms :: [T.Text]
putSynonyms = map T.pack ["put", "leave"]

-- | Akceptowalne synonimy komendy "inventory".
inventorySynonyms :: [T.Text]
inventorySynonyms = map T.pack ["inventory", "inv"]

-- | Funkcja 'findAction' przyjmuje tablicę wyrazów i próbuje dopasować pierwszy wyraz do odpowiedniej komendy (oprócz komendy 'Use'). 
-- Jeśli nastąpi dopasowanie zwracana jest opakowana odpowiednia akcja, w razie potrzeby z jej argumentem.
-- Jeśli nie zajdzie dopasowanie zwracane jest 'Nothing'.
findAction :: [T.Text] -> Maybe Action
findAction (command:arg:_) 
    | command `elem` goSynonyms = Just Move <*> parseDirection arg
    | command `elem` takeSynonyms = Just $ Take arg
    | command `elem` putSynonyms = Just $ Put arg
    | command == T.pack "save" = Just $ SaveState $ T.unpack arg   
    | command == T.pack "continue" = Just $ Continue $ T.unpack arg
    | otherwise = Nothing 
findAction [command] 
  | command == T.pack "look" = Just Look
  | command == T.pack "help" = Just Help 
  | command == T.pack "quit" = Just Quit
  | command == T.pack "restart" = Just Restart 
  | command `elem` inventorySynonyms = Just Inventory
findAction _ = Nothing

-- | Funkcja 'wordParser' parsuje kolejne litery słowa i zamiena je na lowercase.
-- Funckja korzysta z parsera importowanego z modułu 'Text.Parsec.Text'.
wordParser :: Parser T.Text
wordParser = T.toLower <$> (T.pack <$> many1 letter)

-- | Funkcja parsuje wejście rozdzielając dane na podstawie spacji, aby uzyskać osobne słowa z polecenia. 
-- Każde słowo jest parsowane przez funckję 'wordParser'.
inputParser :: Parser [T.Text]
inputParser = wordParser `sepBy` space

-- | Funkcja parsuje wejście za pomocą 'inputParser' oraz próbuje znaleźć odpowiednią akcję dzięki funkcji 'findAction'. 
commandParser :: Parser Action
commandParser = inputParser >>= (\x -> case findAction x of
    Just action -> lift (pure action)
    Nothing  -> fail "Not known command.")

-- | Funkcja parsuje wejście poprzez próbę dopasowania go do komend znajdujących się w zbiorze typu 'ActionsMap'. 
-- Jeśli dopasowanie się powiedzie zwracana jest opakowana akcja 'Use' z odpowiednimi argumentami. 
-- W przeciwnym przypadku zwracane jest 'Nothing'.
parseActionFromMap :: T.Text -> ActionsMap -> Maybe Action
parseActionFromMap input actions = if checkInputActionsMap command actions && length w > 1 then Just (Use y command) else Nothing
    where
        command = T.toLower input
        w@(_:y:_) = T.words command

-- | Funkcja przyjmuje zbiór akcji typu 'ActionsMap' i wejście od użytkownika. 
-- Wejście jest parsowane z użyciem funkcji 'commandParser'. 
-- Jeśli nie zostanie prawidłowo sparsowane to wejście może zostać dopasowane do akcji ze zbioru typu 'ActionsMap'. 
-- Funcja zwraca Right 'Action' jeśli udało się dopasować akcję, lub Left 'String' jeśli wystąpił błąd lub nie nastąpiło żadne dopasowanie. 
parseCommand :: ActionsMap -> T.Text ->  Either ParseError Action
parseCommand actions input = case parse commandParser "" input of
  Right action -> Right action
  Left err -> maybe (Left err) Right (parseActionFromMap input actions)

-- | Funkcja przyjmuje dane Text i próbuje dopasować je do typu 'Direction'. 
-- Zwracane jest Just 'Direction' lub Nothing, jeśli nie nastąpiło przypisanie. 
parseDirection :: T.Text -> Maybe Direction
parseDirection x
    | x == T.pack "north" || x == T.pack "forward" = Just North
    | x == T.pack "east" || x == T.pack "right" = Just East
    | x == T.pack "south" || x == T.pack "back" = Just South
    | x == T.pack "west" || x == T.pack "left" = Just West
    | otherwise = Nothing