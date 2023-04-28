module Member
  ( Member(..)
  , MemberId
  , getNewMemberInfo
  , getUpdatedMemberInfo
  , getMemberId
  , addMember
  , getMemberById
  , updateMember
  , deleteMember
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils
import Main
import Book
import Library

-- Define the Member type
data Member = Member
  { memberId :: MemberId
  , memberName :: String
  , memberEmail :: String
  , memberBooks :: Map BookId CheckoutRecord
  } deriving (Show, Eq)

-- Define the type alias for member IDs
type MemberId = Int

-- Define the type for book checkout records
type CheckoutRecord = ()

-- Define the type for the member collection
type MemberCollection = Map MemberId Member

-- Initialize an empty member collection
emptyMemberCollection :: MemberCollection
emptyMemberCollection = Map.empty

-- Get information for a new member
getNewMemberInfo :: IO Member
getNewMemberInfo = do
  name <- getInput "Enter member name: "
  email <- getInput "Enter member email: "
  return $ Member 0 name email Map.empty

-- Get information to update an existing member
getUpdatedMemberInfo :: Maybe Member -> IO Member
getUpdatedMemberInfo (Just member) = do
  name <- getInputOrEmpty "Enter member name (leave blank to keep existing value): " (memberName member)
  email <- getInputOrEmpty "Enter member email (leave blank to keep existing value): " (memberEmail member)
  return $ member { memberName = name, memberEmail = email }
getUpdatedMemberInfo Nothing = do
  putStrLn "Member not found."
  getNewMemberInfo

-- Get a member ID from the user
getMemberId :: IO MemberId
getMemberId = readInput "Enter member ID: "

-- Add a new member to the collection
addMember :: MemberCollection -> Member -> MemberCollection
addMember collection member = Map.insert (memberId member) member collection

-- Get a member by ID
getMemberById :: MemberCollection -> MemberId -> Maybe Member
getMemberById collection memberId = Map.lookup memberId collection

-- Update a member
updateMember :: MemberCollection -> MemberId -> (Member -> Member) -> MemberCollection
updateMember collection memberId updateFn = Map.update (Just . updateFn) memberId collection

-- Delete a member by ID
deleteMember :: MemberCollection -> MemberId -> MemberCollection
deleteMember collection memberId = Map.delete memberId collection
