-- Cabal file
name:         library-management-system
version:      0.1.0
build-type:   Simple

executable library-management-system
  main-is:     Main.hs
  other-modules:
               Library
               Book
               Member
  build-depends: base >= 4.12 && < 5

-- Main.hs
module Main where

import Library
import Book
import Member

main :: IO ()
main = do
  putStrLn "Welcome to the library management system"
  putStrLn "Please select an option:"
  putStrLn "1. List books"
  putStrLn "2. Check out a book"
  putStrLn "3. Return a book"
  putStrLn "4. List members"
  putStrLn "5. Register a member"
  option <- getLine
  case option of
    "1" -> listBooks
    "2" -> checkOutBook
    "3" -> returnBook
    "4" -> listMembers
    "5" -> registerMember
    _ -> putStrLn "Invalid option selected"

-- Library.hs
module Library (listBooks, checkOutBook, returnBook) where

import Book

type Library = [Book]

library :: Library
library = [Book "The Great Gatsby" "F. Scott Fitzgerald" False,
           Book "To Kill a Mockingbird" "Harper Lee" False,
           Book "Pride and Prejudice" "Jane Austen" False]

listBooks :: IO ()
listBooks = do
  putStrLn "List of books:"
  mapM_ print library

checkOutBook :: IO ()
checkOutBook = do
  putStrLn "Enter the name of the book you want to check out:"
  bookName <- getLine
  let book = findBook bookName library
  case book of
    Just b  -> if isCheckedOut b
               then putStrLn "Sorry, this book is already checked out"
               else do
                 let newLibrary = checkOutBook' bookName library
                 putStrLn "Book checked out successfully"
    Nothing -> putStrLn "Sorry, this book is not available in our library"

returnBook :: IO ()
returnBook = do
  putStrLn "Enter the name of the book you want to return:"
  bookName <- getLine
  let book = findBook bookName library
  case book of
    Just b  -> if isCheckedOut b
               then do
                 let newLibrary = returnBook' bookName library
                 putStrLn "Book returned successfully"
               else putStrLn "Sorry, this book was not checked out from our library"
    Nothing -> putStrLn "Sorry, this book is not available in our library"

findBook :: String -> Library -> Maybe Book
findBook bookName library =
  find (\(Book name _ _) -> name == bookName) library

checkOutBook' :: String -> Library -> Library
checkOutBook' bookName library =
  map (\(Book name author checkedOut) ->
        if name == bookName then Book name author
