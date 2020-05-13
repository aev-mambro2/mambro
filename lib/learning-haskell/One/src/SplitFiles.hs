  {-|
   - SplitFiles
   - To read files, split by line, and save each line into a new file.
   - @author A.E.Veltstra <aev@sdf.org>
   - @since 2020-04-29T13:57:00EST
   - @version 2020-05-13T10:30:00EDT
   -}

  import System.Environment

  showCount s = show (length s) ++ "\n" 
  
  -- |copies file contents from inputFilePath to file outputFilePath
  copyFile [inputFilePath, outputFilePath] = do
    -- read the input file contents
    -- note the grammar: we are executing an IO String action.
    contents <- readFile inputFilePath
    -- write the contents to the output file
    writeFile outputFilePath contents

  -- |Writes each contents into its own file. If the file 
  -- exists, it gets overwritten.
  --
  -- Params: contents, name pattern, sequence number.
  -- Contents should be the data to write into the output file.
  -- Name pattern should be a pattern for a file path. It should
  --   include a target folder. This function will suffix the 
  --   pattern with -#.txt, in which # is replaced with the passed-
  --   in sequence number.
  -- Sequence number should be the number to put into the output 
  --   file name.
  outputFile :: (String, String, Int) -> IO ()
  outputFile (contents, namePattern, seq) = 
    writeFile (namePattern ++ "-" ++ (show seq) ++ ".txt") contents


  -- |Writes each of the passed-in records to a file that is named 
  -- after the output file pattern.
  -- Params: records, output file pattern.
  -- Records should be a list of items, each of which is a record 
  --   that needs to get saved into its own file.
  -- Output file pattern: see the function outputFile.
  writeFiles :: ([String], String) -> IO ()
  writeFiles (xs, outPattern) 
    | length xs == 0  = (putStrLn "Done")
    | length xs == 1  = outputFile ((head xs), outPattern, (length xs))
    | otherwise = do 
        outputFile ((head xs), outPattern, (length xs))
        writeFiles (tail xs, outPattern)
  

  -- |Params: input file path, output file path pattern.
  -- The output file path pattern should expect the working of 
  -- the function outputFile: it adds -#.txt, in which # is 
  -- the sequence number of the line from the original file.
  main :: IO ()
  main = do
    -- read the command-line arguments
    [input, output] <- getArgs
    -- read the input file
    content <- readFile input
    -- explode the content into lines
    let xs = lines content
    let outPattern = output
    -- pass those to other function
    writeFiles (xs, outPattern)
