@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion

::Verbosity = [run, debug]
::Set to run for less logging.
::Set to debug for more logging.
set verbosity=run

if %verbosity%==debug (
 echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
)

call :prerequisite
call :r1
call :r2
exit /B 0


:prerequisite
set prereq_buffer1=
call :fetch_current_date prereq_buffer1
if %verbosity%==debug (
  echo prereq buffer1: %prereq_buffer1%
)
goto:eof

:r1
if []==[%prereq_buffer1%] (
  echo FAILED test r1.1: got no value at all.
) else (
  echo SUCCEEDED test r1.1: got value: %prereq_buffer1%.
)
goto:eof

:r2
SETLOCAL ENABLEEXTENSIONS
  set r2_buffer2=%prereq_buffer1:~0,6%
  if %verbosity%==debug (
    echo r2 buffer2: %r2_buffer2%
  )
  if []==[%r2_buffer2%] (
    echo FAILED test r2.1: got no value at all.
  ) else (
    echo SUCCEEDED test r2.1: got value: %r2_buffer2%.
  )
  set /A pos1=%r2_buffer2:~0,1%
  if %pos1%==2 (
    echo SUCCEEDED test r2.2: 1st position in value is the number 2.
  ) else (
    echo FAILED test r2.2: 1st position in value should be 2. Got: %pos1%.
  )
  set pos7=%r2_buffer2:~7,1%
  if [%pos7%]==[] (
    echo SUCCEEDED test r2.3: 7th position in value is empty.
  ) else (
    echo FAILED test r2.3: 7th position in value should be empty. Got: %pos7%.
  )
ENDLOCAL
goto:eof


:fetch_current_date
SETLOCAL ENABLEEXTENSIONS
  set myNow=[]
  set dateFormat=yyyyMMdd
  for /F "tokens=1 USEBACKQ delims=" %%q in (`Powershell -Command "& {Get-Date -format '%dateFormat%'}"`) do set myNow=%%q
  if %verbosity%==debug (
    echo fetch_current_date: %myNow:~0,8%
  )
(ENDLOCAL & set %~1=%myNow:~0,8%)
goto:eof