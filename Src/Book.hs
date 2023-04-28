module Book
    ( Book (..)
    , createBook
    , readBook
    , updateBook
    , deleteBook
    ) where

import Library
import Main
import Member
import Utils

data Book = Book { bookId :: Int
                 , bookTitle :: String
                 , bookAuthor :: String
                 , bookStatus :: Bool
                 } deriving (Show, Eq)

-- | Create a new book and add it to the library inventory
createBook :: String -> String -> IO ()
createBook title author = do
    let newId = (maximum $ map bookId books) + 1
        newBook = Book newId title author True
    saveBook newBook
    putStrLn "New book added to library inventory."
  where
    saveBook :: Book -> IO ()
    saveBook book = do
        books' <- readInventory "books.txt" :: IO [Book]
        let updatedInventory = book : books'
        writeInventory "books.txt" updatedInventory

-- | Retrieve a book from the library inventory by ID
readBook :: Int -> IO (Maybe Book)
readBook id = do
    books' <- readInventory "books.txt" :: IO [Book]
    let book = find (\b -> bookId b == id) books'
    return book

-- | Update a book's title or author in the library inventory
updateBook :: Int -> String -> String -> IO ()
updateBook id newTitle newAuthor = do
    book <- readBook id
    case book of
        Just oldBook -> do
            let updatedBook = oldBook { bookTitle = newTitle
                                      , bookAuthor = newAuthor
                                      }
            saveBook updatedBook
            putStrLn "Book updated successfully."
        Nothing -> putStrLn "Book not found in inventory."
  where
    saveBook :: Book -> IO ()
    saveBook book = do
        books' <- readInventory "books.txt" :: IO [Book]
        let updatedInventory = map (\b -> if bookId b == bookId book then book else b) books'
        writeInventory "books.txt" updatedInventory

-- | Remove a book from the library inventory
deleteBook :: Int -> IO ()
deleteBook id = do
    book <- readBook id
    case book of
        Just oldBook -> do
            saveInventory $ filter (\b -> bookId b /= id) books
            putStrLn "Book removed from library inventory."
        Nothing -> putStrLn "Book not found in inventory."
  where
    saveInventory :: [Book] -> IO ()
    saveInventory inventory = writeInventory "books.txt" inventory
