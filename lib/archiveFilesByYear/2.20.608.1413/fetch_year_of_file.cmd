@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

:fetch_year_of_file
::Fetches the year of the last modified date of a file.
::Assumes that the user's date format is American: MM/dd/yyyy.
::
::Parameters:
::1. Output receptor. Will be set to the year of the file date if found.
::2. Folder in which the file lives. Will be concatenated to 3 with \.
::3. Name of the file to review. Will be concatenated to 2 with \.
::
::Returns:
::Year of the date of the file, if found.
::
::Throws / panics:
::- File not found exception.
SETLOCAL ENABLEEXTENSIONS
  ::Verbosity = [run, debug]
  ::Set to run for less logging.
  ::Set to debug for more logging.
  set verbosity=run

  if %verbosity%==debug (
   echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
  )

  set fetched_year_of_file_last_mod_date=
  call %~dp0\fetch_date_of_file.cmd fetched_year_of_file_last_mod_date "%~2" "%~3"
  if %verbosity%==debug (
    echo fetch_year_of_file.received: [%fetched_year_of_file_last_mod_date%]
  )
  if []==[%fetched_year_of_file_last_mod_date%] (
    set output=
  ) else (
    set output=%fetched_year_of_file_last_mod_date:~6,4%
  )
  if %verbosity%==debug (
    echo fetch_year_of_file.output: [%output%]
  )
(ENDLOCAL & set %~1=%output%)
goto:eof
