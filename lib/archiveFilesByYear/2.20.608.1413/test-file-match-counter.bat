@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-10T16:46:00EDT

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
set amount=0
::parent folder of this script
set inputFolder=%~dp0
set pattern=*.bat
call count_files_for_pattern.bat amount %inputFolder% %pattern%
if %verbosity%==debug (
  echo r1.amount: %amount%
)
if ""=="%amount%" (
  echo FAILED test r1.1: found empty amount. Expected a non-empty value.
) else (
  echo SUCCEEDED test r1.1: found amount: %amount%.
)
if 0 LSS %amount% (
  echo SUCCEEDED test r1.2: expected to find at least 1 file. Found: %amount%.
) else (
  echo FAILED test r1.2: expected to find at least 1 file. Found: %amount%.
)
goto:eof
