@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

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
call :r3
exit /B 0

:r1
::Fetch the year of the modification date of a file and test
::whether the found value exists and has a length of 4.
SETLOCAL ENABLEEXTENSIONS
  set /A yearOfFile=0
  call %~dp0\fetch_year_of_file.cmd yearOfFile %test_input_folder% %test_input_file%
  if %verbosity%==debug (
    echo r1.yearOfFile: [%yearOfFile%]
  )
  if []==[%yearOfFile%] (
    echo FAILED test r1.1: got no value at all.
  ) else (
    echo SUCCEEDED test r1.1: got value: %yearOfFile%.
  )
  set /A pos1=%yearOfFile:~0,1%
  ::Between the years 2000 and 3000 this test is expected to succeed.
  if %pos1%==2 (
    echo SUCCEEDED test r1.2: got expected value 2: %pos1%.
  ) else (
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
::Fetch the month of the modification date of a file and test
::whether the found value exists and has a length of 2.
SETLOCAL ENABLEEXTENSIONS
  set /A monthOfFile=0
  call %~dp0\fetch_month_of_file.cmd monthOfFile %test_input_folder% %test_input_file%
  if %verbosity%==debug (
    echo r2.monthOfFile: [%monthOfFile%]
  )
  if []==[%monthOfFile%] (
    echo FAILED test r2.1: got no value at all.
  ) else (
    echo SUCCEEDED test r2.1: got value: %monthOfFile%.
  )
  set pos3=%monthOfFile:~3,1%
  if [%pos3%]==[] (
    echo SUCCEEDED test r2.2: got expected empty value: [%pos3%].
  ) else (
    echo FAILED test r2.2: expected empty value but got: [%pos3%].
  )
ENDLOCAL
goto:eof


:r3
::Fetch the year and month of the modification date of a file and test
::whether that is less than, equal to, or greater than the current
::year and month.
SETLOCAL ENABLEEXTENSIONS
  set /A monthOfFile=0
  call %~dp0\fetch_month_of_file.cmd monthOfFile %test_input_folder% %test_input_file%
  set /A yearOfFile=0
  call %~dp0\fetch_year_of_file.cmd yearOfFile %test_input_folder% %test_input_file%
  set yearMonthOfFile=%yearOfFile%%monthOfFile%
  set currentYearMonth=0
  call %~dp0\fetch_current_date_formatted.cmd currentYearMonth yyyyMM
  if %verbosity%==debug (
    echo r3.yearMonthOfFile: [%yearMonthOfFile%]
    echo r3.currentYearMonth: [%currentYearMonth%]
  )
  if []==[%yearMonthOfFile%] (
    echo FAILED test r3.1: got no value at all.
  ) else (
    echo SUCCEEDED test r3.1: got value: %yearMonthOfFile%.
  )
  if []==[%currentYearMonth%] (
    echo FAILED test r3.2: got no value at all.
  ) else (
    echo SUCCEEDED test r3.2: got value: %currentYearMonth%.
  )
  set comparison=UNKNOWN
  if %yearMonthOfFile% LSS %currentYearMonth% (
    set comparison=less
  )
  if %yearMonthOfFile% EQU %currentYearMonth% (
    set comparison=equals
  )
  if %yearMonthOfFile% GTR %currentYearMonth% (
    set comparison=greater
  )
  echo OUTPUT from test r3.3: %yearMonthOfFile% %comparison% %currentYearMonth%.
  set futureYearMonth=314912
  set comparison=UNKNOWN
  if %yearMonthOfFile% LSS %futureYearMonth% (
    set comparison=less
  )
  if %yearMonthOfFile% EQU %futureYearMonth% (
    set comparison=equals
  )
  if %yearMonthOfFile% GTR %futureYearMonth% (
    set comparison=greater
  )
  if %verbosity%==debug (
    echo OUTPUT from test r3.4: %yearMonthOfFile% %comparison% %futureYearMonth%.
  )
  if %comparison%==less (
    echo SUCCEEDED test r3.4: yearMonthOfFile '%yearMonthOfFile%' is less than a future yearMonth '%futureYearMonth%'
  ) else (
    echo FAILED test r3.4: yearMonthOfFile '%yearMonthOfFile%' expected to be less than a future yearMonth '%futureYearMonth%', but was: %comparison%.
  )
  set yesterYearMonth=105711
  set comparison=UNKNOWN
  if %yearMonthOfFile% LSS %yesterYearMonth% (
    set comparison=less
  )
  if %yearMonthOfFile% EQU %yesterYearMonth% (
    set comparison=equals
  )
  if %yearMonthOfFile% GTR %yesterYearMonth% (
    set comparison=greater
  )
  if %verbosity%==debug (
    echo OUTPUT from test r3.5: %yearMonthOfFile% %comparison% %yesterYearMonth%.
  )
  if %comparison%==greater (
    echo SUCCEEDED test r3.5: yearMonthOfFile '%yearMonthOfFile%' is greater than a past yearMonth '%yesterYearMonth%'
  ) else (
    echo FAILED test r3.5: yearMonthOfFile '%yearMonthOfFile%' expected to be greater than a past yearMonth '%yesterYearMonth%', but was: %comparison%.
  )
ENDLOCAL
goto:eof
