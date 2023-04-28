module BookSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Book
import Utils

-- Define the Arbitrary instance for generating valid book IDs
instance Arbitrary BookId where
  arbitrary = choose (1, 100)

-- Define the Arbitrary instance for generating valid book titles
instance Arbitrary BookTitle where
  arbitrary = do
    firstWord <- elements ["The", "A", "An"]
    secondWord <- elements ["Great", "Terrible", "Amazing", "Boring"]
    thirdWord <- elements ["Adventure", "Mystery", "Romance", "Comedy"]
    return $ firstWord ++ " " ++ secondWord ++ " " ++ thirdWord

-- Define the Arbitrary instance for generating valid book authors
instance Arbitrary BookAuthor where
  arbitrary = do
    fName <- elements ["J.K.", "Stephen", "Dan", "Suzanne"]
    lName <- elements ["Rowling", "King", "Brown", "Collins"]
    return $ fName ++ " " ++ lName

-- Define the Arbitrary instance for generating valid book genres
instance Arbitrary BookGenre where
  arbitrary = elements ["Fantasy", "Mystery", "Romance", "Science Fiction"]

-- Define the Arbitrary instance for generating valid book formats
instance Arbitrary BookFormat where
  arbitrary = elements [Hardcover, Paperback, Ebook, Audiobook]

-- Define the Arbitrary instance for generating valid book statuses
instance Arbitrary BookStatus where
  arbitrary = elements [Available, CheckedOut]

-- Define the property tests for the book module
spec :: Spec
spec = do
  describe "Book" $ do
    it "can create a new book" $ do
      property $ \title author genre format -> monadicIO $ do
        -- Create a new book and add it to the book inventory
        let newBook = Book (nextBookId 1) title author genre format Available
        let (bookInventory, _) = addBook Map.empty newBook
        -- Check that the book was successfully added to the book inventory
        Map.lookup (bookId newBook) bookInventory `shouldBe` Just newBook

    it "can update a book's information" $ do
      property $ \title1 author1 genre1 format1 title2 author2 genre2 format2 -> monadicIO $ do
        -- Create a new book and add it to the book inventory
        let newBook = Book (nextBookId 1) title1 author1 genre1 format1 Available
        let (bookInventory, _) = addBook Map.empty newBook
        -- Update the book's information
        let updatedBook = newBook { bookTitle = title2, bookAuthor = author2, bookGenre = genre2, bookFormat = format2 }
        let (updatedInventory, _) = updateBook bookInventory updatedBook
        -- Check that the book was successfully updated in the book inventory
        Map.lookup (bookId updatedBook) updatedInventory `shouldBe` Just updatedBook

    it "can remove a book from the inventory" $ do
      property $ \title author genre format -> monadicIO $ do
        -- Create a new book and add it to the book inventory
        let newBook = Book (nextBookId 1) title author genre format Available
        let (bookInventory, _) = addBook Map.empty newBook
        -- Remove the book from the inventory
        let (updatedInventory, _) = removeBook bookInventory (bookId newBook)
        -- Check that the book was successfully removed from the inventory
        Map.lookup (bookId newBook)
