@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-11T15:47:00EDT

:differs_less_than_100
::Test whether the difference between 2 numbers is less than 100.
::This is used for comparing dates formatted as yyyyMMdd,
::where the question is whether they differ more or less than a month.
::With the above format, 20200611 (Jun 11, 2020), differs less than
::a month from 20200512 (May 12, 2020), but more than a month from 
::20200510 (May 10, 2020).
::
::Returns:
::- 1 if the difference is less than 100.
::- 0 otherwise.
SETLOCAL ENABLEEXTENSIONS
  set /A mA=%~2
  set /A mB=%~3
  set /A mZ=1
  set /A output=0
  
  ::Verbosity = [run, debug]
  ::Set to run for less logging.
  ::Set to debug for more logging.
  set verbosity=run

  if %verbosity%==debug (
    echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
    echo dlt100.mA: %mA%.
    echo dlt100.mB: %mB%.
  )
  if []==[%mA%] (
    set /A mZ=0
  )
  if []==[%mB%] (
    set /A mZ=0
  )
  if 0 EQU %mZ% (
    goto:dlt100_exit
  )
  if %mA% LSS %mB% (
    set /A mC=%mB%-%mA%
  ) else (
    set /A mC=%mA%-%mB%
  )
  if %mC% LSS 100 (
    set /A output=1
  )
:dlt100_exit
  if %verbosity%==debug (
    echo dlt100.mC: %mC%.
    echo dlt100.output: %output%.
  )
(ENDLOCAL & set /A %~1=%output%)
goto:eof
