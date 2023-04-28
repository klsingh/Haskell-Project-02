module Utils (
    createFile,
    readFileLines,
    writeFileLines,
    updateFileLine,
    deleteFileLine
) where

import System.IO (readFile, writeFile)
import Data.List (delete)
import Main
import Member
import Book
import Library

-- | Create a new file with the given name.
createFile :: FilePath -> IO ()
createFile name = writeFile name ""

-- | Read the lines of a file and return them as a list.
readFileLines :: FilePath -> IO [String]
readFileLines name = lines <$> readFile name

-- | Write the given lines to a file.
writeFileLines :: FilePath -> [String] -> IO ()
writeFileLines name lines = writeFile name (unlines lines)

-- | Update a line in a file with the given index.
updateFileLine :: FilePath -> Int -> String -> IO ()
updateFileLine name index line = do
    lines <- readFileLines name
    let updatedLines = take index lines ++ [line] ++ drop (index + 1) lines
    writeFileLines name updatedLines

-- | Delete a line in a file with the given index.
deleteFileLine :: FilePath -> Int -> IO ()
deleteFileLine name index = do
    lines <- readFileLines name
    let updatedLines = delete (lines !! index) lines
    writeFileLines name updatedLines
