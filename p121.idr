--1, 2, 3
module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store)
  where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Quit
             | Size
             | Search String

parseCommand : List String -> Maybe Command
parseCommand ("add" :: rest) = Just (Add (unwords rest))
parseCommand ["get", val] = case all isDigit (unpack val) of
                                 False => Nothing
                                 True => Just (Get (cast val))
parseCommand ["quit"] = Just Quit
parseCommand ["size"] = Just Size
parseCommand ["search", str] = Just (Search str)
parseCommand _ = Nothing

parse : (input : String) -> Maybe Command
parse input = parseCommand (words input)

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
    = let store_items = items store in
          case integerToFin pos (size store) of
               Nothing => Just ("Out of range\n", store)
               Just id => Just (index id (items store) ++ "\n", store)


search_str : Vect size String -> Integer -> String ->
             List (Integer, String)
search_str [] _ _ = []
search_str (x :: xs) id key =
  let rest = search_str xs (id + 1) key in
  if Strings.isInfixOf key x then (id, x) :: rest else rest

search_sub : DataStore -> String -> List (Integer, String)
search_sub (MkData size items) key = search_str items 0 key

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse input of
           Nothing => Just ("Invalid command\n", store)
           Just (Add item) =>
              Just ("ID " ++ show (size store) ++ "\n",
                    addToStore store item)
           Just (Get pos) => getEntry pos store
           Just Size => Just (show (size store) ++ "\n", store)
           Just Quit => Nothing
           Just (Search str) =>
              let items = search_sub store str in
              let response = foldr (\(id, content), acc => show(id) ++ ": "
                                                           ++ content ++ "\n"
                                                           ++ acc)
                                   ""
                                   items
              in Just (response, store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
