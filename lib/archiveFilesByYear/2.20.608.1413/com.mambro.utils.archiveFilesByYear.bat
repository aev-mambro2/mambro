@echo OFF

::This script archives the files in this directory.
::
::This script presumes that 7-Zip is installed on the computer on which 
::the script is executed. We'll try to locate it using Windows' WHERE.
::
::Author: A.E.Veltstra for Mamiye Brothers, Inc. <edibiz@mambro.com>
::Original: 2019-10-31T14:00:00EST
::Version: 2020-06-08T16:46:00EDT

SETLOCAL ENABLEDELAYEDEXPANSION 

:identifying_ourselves_in_logging
echo.
echo %date%T%time% this script is %~dpnx0
echo.

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

:lets_find_7zip
set pathTo7Zip=
call :locate_7zip pathTo7Zip
if "%pathTo7Zip%"=="" (
  call:notify_missing_7zip "%myScriptName%"
  goto:exit
)

:determine_whether_matching_files_exist
::Let's find how many files fit the pattern.
set count=0
call :count_files_matching_pattern count "%inputFolder%" "%inputFilePattern%"
if %count% EQU 0 (
  call:log_warning_no_files "%myScriptName%" "%inputFolder%" "%inputFilePattern%"
  goto:exit
)

call :log_amount_of_matching_files "%myScriptName%" %count% "%inputFolder%" "%inputFilePattern%"

set current_date_time=[]
call :fetch_current_date_time current_date_time
set current_year_month=
if []==[current_date_time] (
  set current_year_month=0
) else (
  set current_year_month=%current_date_time:0,6%
)

for /f "tokens=* USEBACKQ" %%A in (`dir /B /A-D /OD "%~2\%~3"`) do (
  call :archive_file "%myScriptName%" "%~2" "%%A" "%pathTo7Zip%" "%zipNamePrefix%" "%current_year_month%"
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
:: 6. The current yearMonth. The file's yearMonth needs to be older to get archived.
if %isDebugging% NEQ 0 (
  echo %date%T%time% Info from %~1: Examining file %~2\%~3 for archivingagainst current yearMonth "%~6".
  echo.
)
SETLOCAL 
  set /A yearOfFile=0
  call :fetch_year_of_file yearOfFile "%~2" "%~3"
  if "%yearOfFile%" EQU 0 (
    call :log_error_file_year_not_found "%~1" "%~2" "%~3"
  ) else (
    set /A monthOfFile=0
    call :fetch_month_of_file monthOfFile "%~2" "%~3"
    if "%monthOfFile%" EQU 0 (
      call :log_error_file_month_not_found "%~1" "%~2" "%~3"
    ) else (
      if "%yearOfFile%%monthOfFile%" LSS "%~6%~7" (
        if %isDebugging% NEQ 0 (
          echo File yearMonth: "%yearOfFile%%monthOfFile%". Current yearMonth: "%~6%~7".
          echo.
          call "%pathTo7Zip%" a -aou -bd -sdel "%~2\%~5-%yearOfFile%.zip" "%~2\%~3"
          echo.
        ) else (
          call "%pathTo7Zip%" a -aou -bb0 -sdel "%~2\%~5-%yearOfFile%.zip" "%~2\%~3"
        )
      ) else (
        call :log_info_file_too_young "%~1" "%~2" "%~3"
      )
    )
  )
ENDLOCAL
goto:eof


:count_files_matching_pattern
::Counts files that match the passed-in pattern.
::
::Expected parameters:
:: 1. Output receiver
:: 2. Folder in which to look for files
:: 3. GLOB file name pattern against which to match existing files
::
::Returns: the amount of matching files.
for /f %%i in ('dir /B /A-D "%~2\%~3" ^| find /c /v ""') do (set /a %~1=%%i)
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
set /A FAIL_FILE_YEAR_NOT_FOUND=64
set /A FAIL_FILE_MONTH_NOT_FOUND=132
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


:fetch_current_date_time
set current_date_time=[]
for /F "tokens=1 delims=" %%q in ('Powershell -Command "& {Get-Date -format "yyyyMMdd'T'HHmmss"|"') do ( set fetch_current_date_time=%%q)
set %~1=%fetch_current_date_time%
goto:eof


:fetch_month_of_file
::Expected parameters:
:: 1. Output receiver
:: 2. Folder in which the file was found.
:: 3. Name of the file to examine.
::
::Returns: the month of the last-modified date.
::
::Note: this operates on the assumption that the date formatting on the 
::server where this script runs, is American, with the month part of the 
::file date returned from positiion 0, measuring 2 characters.
set fetch_month_of_file_last_mod_date=
call :fetch_date_of_file fetch_month_of_file_last_mod_date "%~2" "%~3"
set %~1=%fetch_month_of_file_last_mod_date:~0,2%
goto:eof


:fetch_year_of_file
::Expected parameters:
:: 1. Output receiver
:: 2. Folder in which the file was found.
:: 3. Name of the file to examine.
::
::Returns: the year of the last-modified date.
::
::Note: this operates on the assumption that the date formatting on the 
::server where this script runs, is American, with the year part of the 
::file date returned from positiion 6, measuring 4 characters.
set fetch_year_of_file_last_mod_date=
call :fetch_date_of_file fetch_year_of_file_last_mod_date "%~2" "%~3"
set %~1=%fetch_year_of_file_last_mod_date:~6,4%
goto:eof


:fetch_date_of_file
::Expected parameters:
:: 1. Output receiver
:: 2. Folder in which the file was found.
:: 3. Name of the file to examine.
::
::Returns: the last-modified date.
::See: https://stackoverflow.com/questions/2111333/how-to-get-files-last-modified-date-on-windows-command-line#2116420
for %%a in ("%~2\%~3") do set %~1=%%~ta
goto:eof



:locate_7zip
::Assuming the programs installation folder is c:\. 
::
::Expected parameters:
:: 1. Output receiver
::
::Returns the file path of the installation location of the executable named '7-z.exe'.
FOR /F "tokens=* USEBACKQ" %%F in (`WHERE /r c:\ 7z.exe`) DO set %~1=%%F
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
goto :eof


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


:log_error_file_month_not_found
::Expected parameters:
:: 0. Global variable FAIL_FILE_MONTH_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
:: 2. Folder in which the file was found.
:: 3. Name of the file to archive.
::
::Side Effects:
:: 1. Adds global error level FAIL_FILE_MONTH_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_FILE_MONTH_NOT_FOUND%"
echo %date%T%time% Error %FAIL_FILE_MONTH_NOT_FOUND% in %~1: Month not found for file %~2\%~3.
echo.
goto:eof

:log_error_file_year_not_found
::Expected parameters:
:: 0. Global variable FAIL_FILE_YEAR_NOT_FOUND. Echoed to log.
:: 1. Name of this script.
:: 2. Folder in which the file was found.
:: 3. Name of the file to archive.
::
::Side Effects:
:: 1. Adds global error level FAIL_FILE_YEAR_NOT_FOUND to global parameter MY_ERRORLEVEL.
set /A "MY_ERRORLEVEL|=%FAIL_FILE_YEAR_NOT_FOUND%"
echo %date%T%time% Error %FAIL_FILE_YEAR_NOT_FOUND% in %~1: Year not found for file %~2\%~3.
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
echo %date%T%time% Info in %~1: skipping archiving of file because it is too young: "%~2\%~3".
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
