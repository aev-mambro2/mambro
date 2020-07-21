import System.Environment

{-|
 Module     : SplitLines
 Description: To read files, split by line, and save each line into a new file.
 Copyright  : (c) A.E.Veltstra, 2020
 Maintainer : A.E.Veltstra <aev@sdf.org>
 @since      : 2020-04-29T13:57:00EST
 @version    : 2020-07-20T21:41:00EDT
-}

-- |Writes each contents into its own file. If the file 
-- exists, it gets overwritten.
outputFile :: String -- ^ the data to write out.
              -> String -- ^ the pattern for a file path.
              -> Int -- ^ the sequence number to put into the output file name.
              -> IO () -- ^ returns the action to execute.
outputFile contents namePattern seq
  = writeFile (namePattern ++ "-" ++ (show seq) ++ ".txt") contents

-- |Writes each of the passed-in records to a file that is named 
-- after the output file pattern.
writeFiles :: ([String], String) -- ^ records, output file pattern. Records should be a list of items, each of which is a record that needs to get saved into its own file. Output file pattern: a pattern for naming a new output file. Will be suffixed with -#.txt in which # equals a sequence number.
  -> IO () -- ^ returns the action to execute.
writeFiles (t, outPattern)
  | length t == 0 = (putStrLn "Done")
  | length t == 1 = outputFile (head t) outPattern 1
  | otherwise =
    do outputFile (head t) outPattern (length t)
       writeFiles ((tail t), outPattern)

-- |Params: input file path, output file path pattern.
-- The output file path pattern should expect the working of 
-- the function outputFile: it adds -#.txt, in which # is 
-- the sequence number of the line from the original file.
main :: IO ()
main
  -- read the command-line arguments
  = do [input, output] <- getArgs
       -- read the input file
       content <- readFile input
       -- explode the content into lines
       let xs = lines content
       let outPattern = output
       -- pass those to other function
       writeFiles (xs, outPattern)
