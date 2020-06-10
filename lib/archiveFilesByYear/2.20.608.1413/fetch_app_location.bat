@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-10T16:28:00EDT

:fetch_app_location
::Fetches the file path of the passed-in executable.
::This function wraps around the Windows WHERE command.
::
::Parameters:
::1. Output receptor. Will be set to the file location if found.
::2. Folder in which the executable lives. Expected to end in \.
::3. Name of the executable to find.
::
::Returns:
::Location of the executable, if found.
::
::Throws / panics:
::- Folder not found exception.
for /F "tokens=* USEBACKQ" %%F in (`WHERE /r %~2 %~3`) DO set %~1=%%F
goto:eof