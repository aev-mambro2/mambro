  {-
   - SplitFiles
   - To read files, split by line, and save each line into a new file.
   - @author A.E.Veltstra <aev@sdf.org>
   - @since 2020-04-29T13:57:00EST
   - @version 2020-04-29T14:46:00EST
   -}

  import System.Environment

  showCount s = show (length s) ++ "\n" 
  
  -- copies file contents from inputFilePath to file outputFilePath
  copyFile [inputFilePath, outputFilePath] = do
    -- read the input file contents
    contents <- readFile inputFilePath
    -- write the contents to the output file
    writeFile outputFilePath contents

  main = do
    -- read the command-line arguments
    [input, output] <- getArgs
    -- pass those to other function
    copyFile [input, output] 
