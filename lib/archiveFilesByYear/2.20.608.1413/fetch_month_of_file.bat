@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

:fetch_month_of_file
::Fetches the month of the last modified date of a file.
::
::Parameters:
::1. Output receptor. Will be set to the month of the file date if found.
::2. Folder in which the file lives. Will be concatenated to 3 with \.
::3. Name of the file to review. Will be concatenated to 2 with \.
::
::Returns:
::month of the date of the file, if found.
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

  set fetched_month_of_file_last_mod_date=
  call fetch_date_of_file.bat fetched_month_of_file_last_mod_date "%~2" "%~3"
  if %verbosity%==debug (
    echo fetch_month_of_file.received: [%fetched_month_of_file_last_mod_date%]
  )
  if []==[%fetched_month_of_file_last_mod_date%] (
    set output=
  ) else (
    set output=%fetched_month_of_file_last_mod_date:~0,2%
  )
  if %verbosity%==debug (
    echo fetch_month_of_file.output: [%output%]
  )
(ENDLOCAL & set %~1=%output%)
goto:eof
