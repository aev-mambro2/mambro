@echo OFF

::Archiver Tester. This task runs a script that archives files.
::Author: A.E.Veltstra for Mamiye Brothers, Inc.
::Original: 2020-06-08T16:13:00EDT
::Updated: 2020-06-10T15:51:00EDT

SETLOCAL ENABLEDELAYEDEXPANSION

::Set to 1 to enable verbose logging. Set to 0 to disable. 
set /A isDebugging=1

::To where logging should happen. MUST NOT end in \.
set "loggingFolder=."

::The name of the standard log for the archiving processor.
set "stdOut=%loggingFolder%\archiver-stdOut-tester.log"

::The name of the error log for the archiving processor.
set "stdErr=%loggingFolder%\archiver-stdErr-tester.log"

::The name of the folder where the files live that need archiving. MUST NOT end in \.
set "inputFolder=..\Downloads\shopify-ks-inventory-out-archive-2020"

::How to recognize the input files
set "inputFilePattern=*.*.xml"

::The prefix of the name of the zip file to be generated. It will be suffixed with a year and 
::the zip extension.
set "archiveZipFileNamePrefix=archive"

cmd /D /Q /C "com.mambro.utils.archiveFilesByYear.bat %isDebugging% %inputFolder% %inputFilePattern% %archiveZipFileNamePrefix% 1>>%stdOut% 2>>%stdErr%"

ENDLOCAL

exit /B %errorlevel%
