@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT

SETLOCAL EnableExtensions EnableDelayedExpansion

::Verbosity = [run, debug]
::Set to run for less logging.
::Set to debug for more logging.
set verbosity=debug

if %verbosity%==debug (
 echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
)

call :prerequisite
call :r1
call :r2
exit /B 0


:prerequisite
set dateFormat=yyyyMMdd
set prereq_buffer1=
call fetch_current_date_formatted.bat prereq_buffer1 %dateFormat%
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
  set result=FAILED
  if []==[%r2_buffer2%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r2.1: got a value '%r2_buffer2%'.
  set /A pos1=%r2_buffer2:~0,1%
  if %pos1%==2 (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r2.2: 1st position in value should be 2. Got: '%pos1%'.
  set pos7=%r2_buffer2:~7,1%
  if %verbosity%==debug (
    echo r2 pos7: %pos7%
  )
  if []==[%pos7%] (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r2.3: 7th position in value should be empty. Got: '%pos7%'.
ENDLOCAL
goto:eof
