module Main where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Member
import Book
import Library
import Utils

-- Define the state of the library inventory
data InventoryState = InventoryState
  { members :: Map MemberId Member
  , books :: Map BookId Book
  , nextMemberId :: MemberId
  , nextBookId :: BookId
  }

-- Define the initial state of the library inventory
initialState :: InventoryState
initialState = InventoryState Map.empty Map.empty 1 1

-- Define the main function that runs the library management system
main :: IO ()
main = do
  -- Load the library inventory from file or create a new one
  inventory <- loadInventoryFromFile "inventory.txt" >>= \case
    Just inventory -> return inventory
    Nothing -> return initialState
  -- Run the application with the initial inventory state
  evalStateT runApp inventory

-- Define the library management application
runApp :: StateT InventoryState IO ()
runApp = do
  -- Display the main menu and get the user's choice
  liftIO $ putStrLn "Library Management System"
  choice <- liftIO $ getMainMenuChoice
  -- Execute the selected operation and update the inventory state
  case choice of
    MainMenuCreateMember -> do
      member <- liftIO $ getNewMemberInfo (nextMemberId <$> get)
      modify $ \state -> state { members = Map.insert (nextMemberId state) member (members state)
                               , nextMemberId = succ (nextMemberId state)
                               }
      liftIO $ putStrLn "Member created successfully!"
    MainMenuUpdateMember -> do
      memberId <- liftIO $ getMemberId
      member <- liftIO $ getUpdatedMemberInfo (Map.lookup memberId (members <$> get))
      modify $ \state -> state { members = Map.update (\_ -> member) memberId (members state) }
      liftIO $ putStrLn "Member updated successfully."
    MainMenuDeleteMember -> do
      memberId <- liftIO $ getMemberId
      modify $ \state -> state { members = Map.delete memberId (members state) }
      liftIO $ putStrLn "Member deleted successfully."
    MainMenuCreateBook -> do
      book <- liftIO $ getNewBookInfo (nextBookId <$> get)
      modify $ \state -> state { books = Map.insert (nextBookId state) book (books state)
                               , nextBookId = succ (nextBookId state)
                               }
      liftIO $ putStrLn "Book created successfully."
    MainMenuCheckoutBook -> do
      memberId <- liftIO $ getMemberId
      bookId <- liftIO $ getBookId
      modify $ \state -> checkoutBook memberId bookId state
      liftIO $ putStrLn "Book checked out successfully."
    MainMenuReturnBook -> do
      memberId <- liftIO $ getMemberId
      bookId <- liftIO $ getBookId
      modify $ \state -> returnBook memberId bookId state
      liftIO $ putStrLn "Book returned successfully."
    MainMenuPrintInventory -> do
      liftIO $ putStrLn "Library Inventory:"
      liftIO $ printInventory =<< get
  -- Recursively run the application with the updated inventory state
  runApp
