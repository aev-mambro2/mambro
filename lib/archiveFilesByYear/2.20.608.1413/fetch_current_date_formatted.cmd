@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

:fetch_current_date_formatted
::Fetches the current system date formatted as passed-in.
::
::Parameters:
::1. Output receptor. Will be set to the current date if found.
::2. The date format to apply. See Powershell formats for the Get-Date command.
::
::Returns:
::Current system date, if found, in the requested format.
::
::Throws / panics:
::- If Powershell is not installed or does not recognize the Get-Date command.
::- If the date format is malformed or empty.
SETLOCAL ENABLEEXTENSIONS
  ::Verbosity = [run, debug]
  ::Set to run for less logging.
  ::Set to debug for more logging.
  set verbosity=run

  if %verbosity%==debug (
    echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
    echo fetch_current_date.received_date_format: "%~2"
  )
  
  set myNow=[]
  for /F "tokens=1 USEBACKQ delims=" %%q in (`Powershell -Command "& {Get-Date -format '%~2'}"`) do set myNow=%%q
  if %verbosity%==debug (
    echo fetch_current_date.formatted_date: %myNow%
  )
  if []==[%myNow%] (
    set output=
  ) else (
    set output=%myNow:~0,8%
  )
  if %verbosity%==debug (
    echo fetch_current_date.output: %output%
  )
(ENDLOCAL & set %~1=%output%)
goto:eof