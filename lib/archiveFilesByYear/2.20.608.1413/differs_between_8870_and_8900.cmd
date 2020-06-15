@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-11T15:49:00EDT

:differs_between_8870_and_8900
::Tests whether 2 dates formatted as yyyyMMdd differ
::between 8870 and 8900 from each other. This is true 
::for dates that roll over a year change, like 
::20200112 and 20191219.
::
::Returns:
::- 1 if the difference falls in the range.
::- 0 otherwise.
SETLOCAL ENABLEEXTENSIONS
  set /A mA=%~2
  set /A mB=%~3

  ::Verbosity = [run, debug]
  ::Set to run for less logging.
  ::Set to debug for more logging.
  set verbosity=run

  if %verbosity%==debug (
    echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
    echo df.mA: %mA%.
    echo df.mB: %mB%.
  )
  set /A mC=1
  set /A mD=0
  set /A mZ=1
  set /A output=0
  if []==[%mA%] (
    set /A mC=0
  )
  if []==[%mB%] (
    set /A mC=0
  )
  if 0 EQU %mC% (
    set /A output=0
    goto:df_exit
  )
  if %mA% LSS %mB% (
    set /A mD=%mB%-%mA%
  ) else (
    set /A mD=%mA%-%mB%
  )
  if %mD% LSS 8870 (
    set /A mZ=0
  )
  if %mD% GTR 8900 (
    set /A mZ=0
  )
  if %mZ% EQU 1 (
    set /A output=1
  )
:df_exit
  if %verbosity%==debug (
    echo df.mC: %mC%.
    echo df.mD: %mD%.
    echo df.mZ: %mZ%.
    echo df.output: %output%.
  )
(ENDLOCAL & set /A %~1=%output%)
goto:eof

