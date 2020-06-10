@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-10T16:43:00EDT

:count_files_for_pattern
::Counts the amount of files that match the passed-in pattern.
::
::Parameters:
::1. Output receptor. Will be set to the file location if found.
::2. Folder in which to look.
::3. GLOB file name to match.
::
::Returns:
::Amount of matching files, if found.
::
::Throws / panics:
::- Folder not found exception.
for /F %%i in ('dir /B /A-D "%~2\%~3" ^| find /c /v ""') do (set /a %~1=%%i)
goto:eof