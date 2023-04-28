module MemberSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Member
import BookSpec (spec as bookSpec)

spec :: Spec
spec = do
    describe "Library Management System" $ do
        bookSpec
    -- other specs go here
  describe "Member" $ do
    let member = Member 1 "John Doe" "john.doe@example.com" [] True
    describe "memberId" $ do
      it "returns the member's ID" $ do
        memberId member `shouldBe` 1
    describe "memberName" $ do
      it "returns the member's name" $ do
        memberName member `shouldBe` "John Doe"
    describe "memberEmail" $ do
      it "returns the member's email" $ do
        memberEmail member `shouldBe` "john.doe@example.com"
    describe "memberBooks" $ do
      it "returns the member's borrowed books" $ do
        memberBooks member `shouldBe` []
      it "can add a book to the member's borrowed books" $ do
        let book = Book 1 "The Great Gatsby" "F. Scott Fitzgerald" 1925 Nothing
        let member' = borrowBook book member
        memberBooks member' `shouldBe` [book]
      it "can remove a book from the member's borrowed books" $ do
        let book = Book 1 "The Great Gatsby" "F. Scott Fitzgerald" 1925 Nothing
        let member' = borrowBook book member
        let member'' = returnBook book member'
        memberBooks member'' `shouldBe` []
    describe "memberCanBorrow" $ do
      it "returns True if the member can borrow a book" $ do
        memberCanBorrow member `shouldBe` True
      it "returns False if the member has reached the borrowing limit" $ do
        let member' = foldr borrowBook member [Book 1 "Book 1" "Author 1" 2021 Nothing,
                                               Book 2 "Book 2" "Author 2" 2021 Nothing,
                                               Book 3 "Book 3" "Author 3" 2021 Nothing,
                                               Book 4 "Book 4" "Author 4" 2021 Nothing,
                                               Book 5 "Book 5" "Author 5" 2021 Nothing]
        memberCanBorrow member' `shouldBe` False
    describe "updateMemberInfo" $ do
      it "updates the member's name" $ do
        let member' = updateMemberInfo (Just "Jane Doe") Nothing member
        memberName member' `shouldBe` "Jane Doe"
      it "updates the member's email" $ do
        let member' = updateMemberInfo Nothing (Just "jane.doe@example.com") member
        memberEmail member' `shouldBe` "jane.doe@example.com"
      it "doesn't update the member's info if both name and email are Nothing" $ do
        let member' = updateMemberInfo Nothing Nothing member
        member' `shouldBe` member
