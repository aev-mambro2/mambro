@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-10T16:32:00EDT

SETLOCAL EnableExtensions EnableDelayedExpansion

::Verbosity = [run, debug]
::Set to run for less logging.
::Set to debug for more logging.
set verbosity=debug

if %verbosity%==debug (
 echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
)

call :r1
exit /B 0

:r1
set outputLocation=
set inputFolder=c:\
set executableName=7z.exe
call fetch_app_location.bat outputLocation %inputFolder% %executableName%
if %verbosity%==debug (
  echo r1.outputLocation: %outputLocation%
)
if ""=="%outputLocation%" (
  echo FAILED test r1.1: found empty outputLocation. Expected a non-empty value.
) else (
  echo SUCCEEDED test r1.1: found outputLocation: %outputLocation%.
)
set firstThree=%outputLocation:~0,3%
if "c:\"=="%firstThree%" (
  echo SUCCEEDED test r1.2: expected 1st 3 chars of outputLocation to be 'c:\': %firstThree%.
) else (
  echo FAILED test r1.2: expected 1st 3 chars of outputLocation to be 'c:\': %firstThree%.
)
goto:eof
