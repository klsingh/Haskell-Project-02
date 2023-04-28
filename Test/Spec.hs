{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import MemberSpec (spec)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Member
import Book
import Library
import Utils
import Main (InventoryState(..))
import Test.Hspec
import qualified LibrarySpec
import qualified MemberSpec
import qualified BookSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Library Management System" $ do
    LibrarySpec.spec
    MemberSpec.spec
    BookSpec.spec
    
-- Define the Arbitrary instance for generating valid member IDs
instance Arbitrary MemberId where
  arbitrary = choose (1, 100)

-- Define the Arbitrary instance for generating valid book IDs
instance Arbitrary BookId where
  arbitrary = choose (1, 100)

-- Define the Arbitrary instance for generating valid member names
instance Arbitrary MemberName where
  arbitrary = do
    fName <- elements ["John", "Jane", "Bob", "Alice"]
    lName <- elements ["Doe", "Smith", "Johnson", "Williams"]
    return $ fName ++ " " ++ lName

-- Define the Arbitrary instance for generating valid member emails
instance Arbitrary MemberEmail where
  arbitrary = do
    fName <- elements ["john", "jane", "bob", "alice"]
    lName <- elements ["doe", "smith", "johnson", "williams"]
    domain <- elements ["gmail.com", "yahoo.com", "hotmail.com"]
    return $ fName ++ "." ++ lName ++ "@" ++ domain

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

-- Define the property tests for the library management system
spec :: Spec
spec = do
  describe "Library Management System" $ do
    it "can create a new member" $ do
      property $ \name email -> monadicIO $ do
        -- Create a new inventory state with an empty member map
        let inventoryState = InventoryState Map.empty Map.empty 1 1
        -- Create a new member and add it to the inventory state
        let newMember = Member (nextMemberId inventoryState) name email [] True
        let newState = execState (createMember newMember) inventoryState
        -- Check that the member was successfully added to the member map
        Map.lookup (memberId newMember) (members newState) `shouldBe` Just newMember

    it "can update a member's information" $ do
      property $ \name1 email1 name2 email2 -> monadicIO $ do
        -- Create a new inventory state with a member
        let inventoryState = InventoryState Map.empty Map.empty 1
spec :: Spec
spec = do
  describe "Library Management System" $ do
    MemberSpec.spec
    -- other specs go here
