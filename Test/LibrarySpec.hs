module LibrarySpec where

import LibrarySpec
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import BookSpec (spec as bookSpec)
import Member
import Book
import Library
import Utils

main :: IO ()
main = hspec $ do
spec :: Spec
spec = do
  describe "Library Management System" $ do
    bookSpec
    -- other specs go here
  describe "Library Management System" $ do
    describe "Member" $ do
      it "creates a member successfully" $ do
        let initialState = InventoryState Map.empty Map.empty 1 1
            newMember = Member 1 "Alice" "alice@example.com" [] True
            expected = InventoryState (Map.singleton 1 newMember) Map.empty 2 1
            actual = execState (createNewMember newMember) initialState
        actual `shouldBe` expected

    describe "Book" $ do
      it "creates a book successfully" $ do
        let initialState = InventoryState Map.empty Map.empty 1 1
            newBook = Book 1 "The Fellowship of the Ring" "J.R.R. Tolkien" True
            expected = InventoryState Map.empty (Map.singleton 1 newBook) 1 2
            actual = execState (createNewBook newBook) initialState
        actual `shouldBe` expected

    describe "Library" $ do
      it "checks out a book successfully" $ do
        let memberId = 1
            bookId = 1
            member = Member memberId "Alice" "alice@example.com" [bookId] True
            book = Book bookId "The Fellowship of the Ring" "J.R.R. Tolkien" False
            initialState = InventoryState (Map.singleton memberId member) (Map.singleton bookId book) 2 2
            expected = Just $ InventoryState (Map.singleton memberId member { memberBooks = [] })
                                             (Map.singleton bookId book { bookCheckedOut = True })
                                             2
                                             2
            actual = checkoutBook memberId bookId initialState
        actual `shouldBe` expected

      it "returns a book successfully" $ do
        let memberId = 1
            bookId = 1
            member = Member memberId "Alice" "alice@example.com" [] True
            book = Book bookId "The Fellowship of the Ring" "J.R.R. Tolkien" True
            initialState = InventoryState (Map.singleton memberId member) (Map.singleton bookId book) 2 2
            expected = Just $ InventoryState (Map.singleton memberId member { memberBooks = [bookId] })
                                             (Map.singleton bookId book { bookCheckedOut = False })
                                             2
                                             2
            actual = returnBook memberId bookId initialState
        actual `shouldBe` expected

    describe "Utils" $ do
      it "splits a string into a list of words" $ do
        let input = "hello world"
            expected = ["hello", "world"]
            actual = splitString input
        actual `shouldBe` expected

    describe "LibraryIO" $ do
      it "saves the inventory state to a file and loads it successfully" $ do
        let initialState = InventoryState Map.empty Map.empty 1 1
            expected = initialState { nextMemberId = 2, nextBookId = 2 }
        saveInventoryToFile "test_inventory.txt" initialState
        actual <- loadInventoryFromFile "test_inventory.txt"
        actual `shouldBe` Just expected

    describe "LibraryState" $ do
      it "gets the next member ID successfully" $ do
        let initialState = InventoryState Map.empty Map.empty 1 1
           
