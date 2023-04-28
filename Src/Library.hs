module Library (
  Book(..),
  Member(..),
  Library(..),
  addBook,
  removeBook,
  checkoutBook,
  returnBook,
  addMember,
  removeMember,
  updateMember
) where

import qualified Data.Map.Strict as Map

import Member
import Main
import Utils
import Book

data Book = Book {
  bookTitle :: String,
  bookAuthor :: String,
  bookISBN :: String,
  bookCheckedOut :: Bool,
  bookCheckedOutBy :: Maybe Member
} deriving (Eq, Show)

data Library = Library {
  libraryBooks :: Map.Map String Book,
  libraryMembers :: Map.Map String Member
} deriving (Eq, Show)

addBook :: Library -> Book -> Library
addBook library book = Library {
  libraryBooks = Map.insert (bookISBN book) book (libraryBooks library),
  libraryMembers = libraryMembers library
}

removeBook :: Library -> String -> Library
removeBook library isbn = Library {
  libraryBooks = Map.delete isbn (libraryBooks library),
  libraryMembers = libraryMembers library
}

checkoutBook :: Library -> String -> String -> Library
checkoutBook library isbn memberId =
  let bookMap = libraryBooks library
      memberMap = libraryMembers library
      maybeBook = Map.lookup isbn bookMap
      maybeMember = Map.lookup memberId memberMap
  in case (maybeBook, maybeMember) of
    (Just book, Just member) ->
      if bookCheckedOut book then
        library
      else
        Library {
          libraryBooks = Map.insert isbn (book {bookCheckedOut = True, bookCheckedOutBy = Just member}) bookMap,
          libraryMembers = Map.adjust (\m -> m {memberCheckedOut = isbn : memberCheckedOut m}) memberId memberMap
        }
    _ -> library

returnBook :: Library -> String -> String -> Library
returnBook library isbn memberId =
  let bookMap = libraryBooks library
      memberMap = libraryMembers library
      maybeBook = Map.lookup isbn bookMap
      maybeMember = Map.lookup memberId memberMap
  in case (maybeBook, maybeMember) of
    (Just book, Just member) ->
      if not (bookCheckedOut book) || bookCheckedOutBy book /= Just member then
        library
      else
        Library {
          libraryBooks = Map.insert isbn (book {bookCheckedOut = False, bookCheckedOutBy = Nothing}) bookMap,
          libraryMembers = Map.adjust (\m -> m {memberCheckedOut = filter (/= isbn) (memberCheckedOut m)}) memberId memberMap
        }
    _ -> library

addMember :: Library -> Member -> Library
addMember library member = Library {
  libraryBooks = libraryBooks library,
  libraryMembers = Map.insert (memberId member) member (libraryMembers library)
}

removeMember :: Library -> String -> Library
removeMember library memberId = Library {
  libraryBooks = libraryBooks library,
  libraryMembers = Map.delete memberId (libraryMembers library)
}

updateMember :: Library -> Member -> Library
updateMember library member = Library {
  libraryBooks = libraryBooks library,
  libraryMembers = Map.adjust (\_ -> member) (memberId member) (libraryMembers library)
}
