@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion

::Verbosity = [run, debug]
::Set to run for less logging.
::Set to debug for more logging.
set verbosity=run

if %verbosity%==debug (
 echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
)

::folder in which this script lives (suffixed by \)
set test_input_folder=%~dp0
::name of this script
set test_input_file=%~nx0

call :r1
call :r2
exit /B 0

:r1
SETLOCAL ENABLEEXTENSIONS
  set /A yearOfFile=0
  call :fetch_year_of_file yearOfFile %test_input_folder% %test_input_file%
  if %verbosity%==debug (
    echo r1 [%yearOfFile%]
  )
  if []==[%yearOfFile%] (
    echo FAILED test r1.1: got no value at all.
  ) else (
    echo SUCCEEDED test r1.1: got value: %yearOfFile%.
  )
  set /A pos1=%yearOfFile:~0,1%
  if %pos1%==2 (
    ::Between the years 2000 and 3000 this branch is expected.
    echo SUCCEEDED test r1.2: got expected value 2: %pos1%.
  ) else (
    ::Until the year 3000 this branch is not expected.
    echo FAILED test r1.2: expected 2 but got: %pos1%. Check the centory on your system clock!
  )
  set pos5=%yearOfFile:~5,1%
  if [%pos5%]==[] (
    echo SUCCEEDED test r1.3: got expected empty value: [%pos5%].
  ) else (
    echo FAILED test r1.3: expected empty value but got: [%pos5%].
  )
ENDLOCAL
goto:eof


:r2
SETLOCAL ENABLEEXTENSIONS
  set /A monthOfFile=0
  call :fetch_month_of_file monthOfFile %test_input_folder% %test_input_file%
  if %verbosity%==debug (
    echo r2 [%monthOfFile%]
  )
  if []==[%monthOfFile%] (
    echo FAILED test r2.1: got no value at all.
  ) else (
    echo SUCCEEDED test r2.1: got value: %monthOfFile%.
  )
  set pos3=%yearOfFile:~3,1%
  if [%pos3%]==[] (
    echo SUCCEEDED test r2.1: got expected empty value: [%pos3%].
  ) else (
    echo FAILED test r2.1: expected empty value but got: [%pos3%].
  )
ENDLOCAL
goto:eof


:fetch_year_of_file
SETLOCAL ENABLEEXTENSIONS
  set fetched_year_of_file_last_mod_date=
  call :fetch_date_of_file fetched_year_of_file_last_mod_date "%~2" "%~3"
  if %verbosity%==debug (
    echo s1 [%fetched_year_of_file_last_mod_date:~6,4%]
  )
(ENDLOCAL & set %~1=%fetched_year_of_file_last_mod_date:~6,4%)
goto:eof


:fetch_month_of_file
SETLOCAL ENABLEEXTENSIONS
set fetched_month_of_file_last_mod_date=
call :fetch_date_of_file fetched_month_of_file_last_mod_date "%~2" "%~3"
if %verbosity%==debug (
  echo s2 [%fetched_month_of_file_last_mod_date:~0,2%]
)
(ENDLOCAL & set %~1=%fetched_month_of_file_last_mod_date:~0,2%)
goto:eof


:fetch_date_of_file
for %%a in ("%~2\%~3") do set %~1=%%~ta
goto:eof