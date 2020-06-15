@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-11T15:50:00EDT


:differs_less_than_a_month
::Tests whether 2 dates formatted as yyyyMMdd differ less than a month
::from each other. 
::See :differs_between_8870_and_8900.
::See :differs_less_than_100.
::
::Returns:
::- 1 if the difference is less than a month.
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
    echo dlm.mA: %mA%.
    echo dlm.mB: %mB%.
  )
  set /A mC=0
  set /A mD=0
  set /A output=0
  call %~dp0\differs_less_than_100.cmd mC %mA% %mB%
  if 1 EQU %mC% (
    set /A output=1
    goto:dlm_exit
  )
  call %~dp0\differs_between_8870_and_8900.cmd mD %mA% %mB%
  if 1 EQU %mD% (
    set /A output=1
  )
:dlm_exit
  if %verbosity%==debug (
    echo dlm.mC: %mC%.
    echo dlm.mD: %mD%.
    echo dlm.output: %output%.
  )
(ENDLOCAL & set /A %~1=%output%)
goto:eof

