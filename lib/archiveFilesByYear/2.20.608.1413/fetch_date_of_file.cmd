@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

:fetch_date_of_file
::Fetches the last modified date of a file.
::
::Parameters:
::1. Output receptor. Will be set to the file date if found.
::2. Folder in which the file lives. Will be concatenated to 3 with \.
::3. Name of the file to review. Will be concatenated to 2 with \.
::
::Returns:
::Date of the file, if found.
::
::Throws / panics:
::- File not found exception.
for %%a in ("%~2\%~3") do set %~1=%%~ta
goto:eof