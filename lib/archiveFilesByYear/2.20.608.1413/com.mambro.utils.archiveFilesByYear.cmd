@echo OFF

::This script archives the files in this directory.
::
::This script presumes that 7-Zip is installed on the computer on which 
::the script is executed. We'll try to locate it using Windows' WHERE.
::
::Author: A.E.Veltstra for Mamiye Brothers, Inc. <edibiz@mambro.com>
::Original: 2019-10-31T14:00:00EST
::Version: 2020-06-12T15:33:00EDT

SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION 

:identifying_ourselves_in_logging
echo.
echo %date%T%time% this script is %~dpnx0
echo.

::Switch verbose logging. Set to 0 to disable, 1 to enable.
set /A isDebugging=%~1

::Switch dry run. Set to 0 to disable (files get archived),
::to 1 to enable (archiving gets skipped).
set /A doDryRun=0

::The file name of this very script. 
set thisScript=%~nx0

::The name (without extension) of this very script. Is used in logging. 
set myScriptName=%~n0

::The folder where this script lives.
set myScriptFolder=%~dp0

call :enumerate_error_levels

set /A CURRENT_INPUT_PARAMS_LEVEL=0
call :read_and_validate_input_arguments CURRENT_INPUT_PARAMS_LEVEL "%~1" "%~2" "%~3" "%~4"

:test_did_params_get_found
if %CURRENT_INPUT_PARAMS_LEVEL% NEQ %PARAMS_FOUND_ALL% (
  call:notify_missing_parameters 
  goto:exit
)

set current_yyyyMMdd=
call %~dp0\fetch_current_date_formatted.cmd current_yyyyMMdd yyyyMMdd
if []==[%current_yyyyMMdd%] (
  call :log_error_failed_to_fetch_yyyyMMdd "%myScriptName%"
  goto:exit
)
call :log_current_yyyyMMdd "%myScriptName%" "%current_yyyyMMdd%"


:determine_whether_matching_files_exist
::Let's find how many files fit the pattern.
set count=0
call %~dp0\count_files_for_pattern.cmd count "%inputFolder%" "%inputFilePattern%"
if %count% EQU 0 (
  call:log_warning_no_files "%myScriptName%" "%inputFolder%" "%inputFilePattern%"
  goto:exit
)

call :log_amount_of_matching_files "%myScriptName%" %count% "%inputFolder%" "%inputFilePattern%"

:lets_find_7zip
if 1 EQU %doDryRun% (
  goto:skip_searching_for_7zip
)
set pathTo7Zip=
call %~dp0\fetch_app_location.cmd pathTo7Zip "c:\" "7z.exe"
if "%pathTo7Zip%"=="" (
  call:notify_missing_7zip "%myScriptName%"
  goto:exit
)
:skip_searching_for_7zip

:archive_each_matching_file
for /f "tokens=* USEBACKQ" %%A in (`dir /B /A-D /OD "%~2\%~3"`) do (
  call :archive_file "%myScriptName%" "%~2" "%%A" "%pathTo7Zip%" "%zipNamePrefix%" %current_yyyyMMdd%
)

:exit
echo.
echo %date%T%time% 
echo Done
ENDLOCAL
exit /B %MY_ERRORLEVEL%


::=========================================================================================
:: Functions
::=========================================================================================

:archive_file
::Expected parameters:
:: 0. Global variable isDebugging. If 0, this method skips itself.
:: 1. Name of this script.
:: 2. Folder in which the file was found.
:: 3. Name of the file to archive.
:: 4. Path to the 7-zip executable.
:: 5. Prefix of the zip file name.
:: 6. The current yyyyMMdd. The file's yyyyMMdd needs to be at least
::    a month older to get archived.
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: Examining file %~2\%~3 for archiving against current yearMonth %6.
  echo.
)
SETLOCAL 
  set /A dateOfFile=0
  call %~dp0\fetch_date_of_file.cmd dateOfFile "%~2" "%~3"
  if "%dateOfFile%"=="0" (
    call :log_error_file_date_not_found "%~1" "%~2" "%~3"
    goto:eof
  )
  set /A fileYearMonthDay=0
  set /A yearOfFile=%dateOfFile:~6,4%
  set fileYearMonthDay=%yearOfFile%%dateOfFile:~0,2%%dateOfFile:~3,2%
  if 1 EQU %isDebugging% (
    echo archive_file.dateOfFile: [%dateOfFile%]
    echo archive_file.yearOfFile: [%yearOfFile%]
    echo archive_file.fileYearMonthDay: [%fileYearMonthDay%]
  )
  if []==[%fileYearMonthDay%] (
    call :log_error_file_yyyyMMdd_not_found "%~1" "%~2" "%~3" "%dateOfFile%"
    goto:eof
  )
  set /A datesCompared=0
  call %~dp0\differs_less_than_a_month.cmd datesCompared %fileYearMonthDay% %6
  if 0 EQU %datesCompared% (
    if 1 EQU %doDryRun% (
      echo archive_file info from "%~1": dry run is enabled, thus skipping the archiving of file "%~2\%~3".
    ) else (
      call "%pathTo7Zip%" a -aou -bb0 -sdel "%~2\%~5-%yearOfFile%.zip" "%~2\%~3"
    )
  ) else (
    call :log_info_file_too_young %~1 %~6 %fileYearMonthDay% %~3 %~2 %datesCompared%
  )
ENDLOCAL
goto:eof


:enumerate_error_levels
::Error levels. Success = 0. 
::Each next is a power of 2 higher than the last.
set /A SUCCESS=0
set /A MY_ERRORLEVEL=%SUCCESS%
set /A FAIL_FOLDER_NOT_FOUND=1
set /A FAIL_FILE_NOT_FOUND=2
set /A FAIL_NOT_ALL_PARAMETERS_FOUND=4
set /A FAIL_7ZIP_NOT_FOUND=8
set /A FAIL_CURRENT_DATETIME_NOT_FOUND=16
set /A FAIL_CURRENT_DATETIME_FORMAT_YIELDED_EMPTY=32
set /A FAIL_FILE_DATE_NOT_FOUND=64
goto:eof


:enumerate_parameter_levels
::Enumerating the paramaters that got found as command-line switches. 
::Each next is a power of 2 higher than the last.
set /A PARAM_DEBUG_LEVEL_FOUND=1
set /A PARAM_INPUT_FOLDER_FOUND=2
set /A PARAM_INPUT_FILE_PATTERN_FOUND=4
set /A PARAM_ZIP_FILE_NAME_PREFIX_FOUND=8
::1|2|4|8=15
set /A PARAMS_FOUND_ALL=15
goto:eof


:log_amount_of_matching_files
::Expected parameters:
:: 0. Global variable isDebugging. If 0, this method skips itself.
:: 1. Name of this script.
:: 2. Amount of files that should be archived.
:: 3. The folder in which they were found.
:: 4. The GLOB file name pattern that the archivable files should match.
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: found %~2 files to archive in input folder '%~3'. They match pattern '%~4'.
  echo.
)
goto:eof


:log_current_date
::Expected parameters:
:: 0. Global variable isDebugging. If 0, this method skips itself.
:: 1. Name of this script.
:: 2. The content of the current date/time. 
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: found current date to be "%~2".
  echo.
)
goto:eof

:log_current_yyyyMMdd
::Expected parameters:
:: 0. Global variable isDebugging. If 0, this method skips itself.
:: 1. Name of this script.
:: 2. The content of the current date/time. 
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: found current yyyyMMdd to be "%~2".
  echo.
)
goto:eof


:log_error_failed_to_fetch_date_time
::Expected parameters:
:: 0. Global variable FAIL_CURRENT_DATETIME_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
::
::Side Effects:
:: 1. Adds global error level FAIL_CURRENT_DATETIME_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_CURRENT_DATETIME_NOT_FOUND%"
echo %date%T%time% Error %FAIL_CURRENT_DATETIME_NOT_FOUND% in %~1: Failed to find current date-time.
echo.
goto:eof


:log_error_failed_to_fetch_yyyyMMdd
::Expected parameters:
:: 0. Global variable FAIL_CURRENT_DATETIME_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
::
::Side Effects:
:: 1. Adds global error level FAIL_CURRENT_DATETIME_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_CURRENT_DATETIME_NOT_FOUND%"
echo %date%T%time% Error %FAIL_CURRENT_DATETIME_NOT_FOUND% in %~1: Failed to find current yyyyMMdd.
echo.
goto:eof


:log_error_file_date_not_found
::Expected parameters:
:: 0. Global variable FAIL_FILE_DATE_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
:: 2. Folder in which the file was found.
:: 3. Name of the file to archive.
::
::Side Effects:
:: 1. Adds global error level FAIL_FILE_DATE_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_FIL_DATE_NOT_FOUND%"
echo %date%T%time% Error %FAIL_FILE_DATE_NOT_FOUND% in %~1: Date not found for file %~2\%~3.
echo.
goto:eof

:log_error_file_yyyyMMdd_not_found
::Expected parameters:
:: 0. Global variable FAIL_FILE_DATE_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
:: 2. Folder in which the file was found.
:: 3. Name of the file to archive.
::
::Side Effects:
:: 1. Adds global error level FAIL_FILE_DATE_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_FILE_DATE_NOT_FOUND%"
echo %date%T%time% Error %FAIL_FILE_DATE_NOT_FOUND% in %~1: Year not found for file %~2\%~3.
echo.
goto:eof


:log_error_found_empty_date_time
::Expected parameters:
:: 0. Global variable FAIL_CURRENT_DATETIME_FORMAT_YIELDED_EMPTY. Echoed to log.
:: 1. Name of this script.
::
::Side Effects:
:: 1. Adds global error level FAIL_CURRENT_DATETIME_FORMAT_YIELDED_EMPTY to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_CURRENT_DATETIME_FORMAT_YIELDED_EMPTY%"
echo %date%T%time% Error %FAIL_CURRENT_DATETIME_FORMAT_YIELDED_EMPTY% in %~1: Current date-time format yielded empty.
echo.
goto:eof


:log_info_file_too_young 
echo %date%T%time% Info in %~1: skipping archiving of file because it is too young: current yyyyMMdd %2 is too close to file yyyyMMdd %3 for file "%~4" in folder "%~5". Comparison returned "%~6".
goto:eof


:log_warning_no_files
::Expected parameters:
:: 0. Global variable isDebugging. If 0, this method skips itself.
:: 1. Name of this script.
:: 2. The folder in which they were found.
:: 3. The GLOB file name pattern that the archivable files should match.
::
::Side Effects:
:: 1. Adds global error level FAIL_FILE_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_FILE_NOT_FOUND%"
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: no files found in input folder '%~2' that match pattern '%~3'. Exiting.
  echo.
)
goto:eof


:notify_missing_parameters
call :notify_usage
SETLOCAL
set /A "MY_ERRORLEVEL|=%FAIL_NOT_ALL_PARAMETERS_FOUND%"
echo You forgot to include, or provided invalid values for the following arguments:
echo.
set /A test1stParam="%CURRENT_INPUT_PARAMS_LEVEL% & %PARAM_DEBUG_LEVEL_FOUND%"
if %test1stParam% NEQ %PARAM_DEBUG_LEVEL_FOUND% echo 1. Debugging.
set /A test2ndParam="%CURRENT_INPUT_PARAMS_LEVEL% & %PARAM_INPUT_FOLDER_FOUND%"
if %test2ndParam% NEQ %PARAM_INPUT_FOLDER_FOUND% echo 2. Input folder.
set /A test3rdParam="%CURRENT_INPUT_PARAMS_LEVEL% & %PARAM_INPUT_FILE_PATTERN_FOUND%"
if %test3rdParam% NEQ %PARAM_INPUT_FILE_PATTERN_FOUND% echo 3. Input file name format.
set /A test4thParam="%CURRENT_INPUT_PARAMS_LEVEL% & %PARAM_ZIP_FILE_NAME_PREFIX_FOUND%"
if %test4thParam% NEQ %PARAM_ZIP_FILE_NAME_PREFIX_FOUND% echo 4. Archive file name prefix.
ENDLOCAL
echo.
echo Please adjust your call and try again.
echo.
echo For support, email edibiz@mambro.com.
echo.
goto:eof


:notify_missing_7zip
set /A "MY_ERRORLEVEL|=%FAIL_7ZIP_NOT_FOUND%"
call :notify_usage
echo Failed to locate 7-zip. This tool is required to archive files. 
echo Either install it into the default programs installation disk, 
echo or change this script to help it find 7-zip. Look for the 
echo "lets_find_7zip" label.
echo.
goto:eof


:notify_usage
echo Usage: %~n0 ^<arguments^>
echo.
echo This script requires having 7-zip installed. It will attempt to find it 
echo on the default programs installation disk. If not found, the program will 
echo log an error and exit.
echo.
echo This script requires command-line arguments. Provide them after the name of this 
echo script, echo separated from each other with a space. Position matters. 
echo.
echo 1. Debugging. Set to 0 for normal operation. Set to 1 to enable verbose logging.
echo 2. Input folder. Specify the location where the files are that need archiving. 
echo    MUST NOT in a back-slash.
echo 3. Input file name format. Specify a filter using GLOB patterns. Only files that 
echo    meet the filter will be included.
echo 4. Archive file name prefix. Will be suffixed with a dash and the year of the 
echo    file's creation.
echo.
echo Any argument values that contain spaces MUST be wrapped in double quotes.
echo.
echo.
echo For example:
echo %~n0 1 "c:\temporary files\" *.log "archive folder"
echo.
goto:eof


:read_and_validate_input_arguments
call :enumerate_parameter_levels

::param 1 is the output receiver, so we start picking up from param 2.
:fetch_param_2
::Set to 1 to enable verbose logging. Set to 0 to disable. Reading from the 1st input argument.
if "%~2"=="1" (
  set /A "isDebugging=1"
  goto:set_found_param_2
)
if "%~2"=="0" (
  set /A "isDebugging=0"
  goto:set_found_param_2
)
goto:fetch_param_3

:set_found_param_2
set /A "%~1|=%PARAM_DEBUG_LEVEL_FOUND%"

:fetch_param_3
set inputFolder=%~3
if "%inputFolder%" NEQ "" (
  set /A "%~1|=%PARAM_INPUT_FOLDER_FOUND%"
)

:fetch_param_4
set inputFilePattern=%~4
if "%inputFilePattern%" NEQ "" (
  set /A "%~1|=%PARAM_INPUT_FILE_PATTERN_FOUND%"
)

:fetch_param_5
set zipNamePrefix=%~5
if "%zipNamePrefix%" NEQ "" (
  set /A "%~1|=%PARAM_ZIP_FILE_NAME_PREFIX_FOUND%"
)
goto:eof
