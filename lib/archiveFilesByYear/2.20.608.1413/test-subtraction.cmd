@ECHO OFF
::Author: A.E.Veltstra
::Original: 2020-06-08T16:13:00EDT
::Version: 2020-06-11T16:38:00EDT

SETLOCAL EnableExtensions EnableDelayedExpansion

::Verbosity = [run, debug]
::Set to run for less logging.
::Set to debug for more logging.
set verbosity=run

if %verbosity%==debug (
 echo Verbose logging enabled. Set verbosity to "run", to log test outcomes only.
)

::folder in which this script lives (suffixed by \)
set test_input_folder=%~dp0
::name of this script
set test_input_file=%~nx0

::This is not looped on purpose:
::calling each separately allows you to deactivate each separately.
call :r1
call :r2
call :r3
call :r4
call :r5
call :r6
call :r7
call :r8
call :r9
call :r10
call :r11
exit /B 0

:r1
::Take 2 "date"-mimicking numbers and subtract them.
::Test whether the difference equals the expected.
SETLOCAL ENABLEEXTENSIONS
  set mX=20200611
  set mY=20200601
  set mZ=10
  set /A mA=%mX%-%mY%
  set result=FAILED
  if %mA% EQU %mZ% (
    set result=SUCCEEDED
  )
  echo %result% test r1: expected %mA% to equal %mZ%.
ENDLOCAL
goto:eof

:r2
::Take 2 "date"-mimicking numbers and subtract them.
::Test whether the difference equals the expected.
SETLOCAL ENABLEEXTENSIONS
  set mX=20200611
  set mY=20200501
  set mZ=110
  set /A mA=%mX%-%mY%
  set result=FAILED
  if %mA% EQU %mZ% (
    set result=SUCCEEDED
  )
  echo %result% test r2: expected %mA% to equal %mZ%.
ENDLOCAL
goto:eof

:r3
::Take 2 "date"-mimicking numbers and subtract them.
::Test whether the difference equals the expected.
SETLOCAL ENABLEEXTENSIONS
  set mX=20210101
  set mY=20201231
  set mZ=8870
  set /A mA=%mX%-%mY%
  set result=FAILED
  if %mA% EQU %mZ% (
    set result=SUCCEEDED
  )
  echo %result% test r3: expected %mA% to equal %mZ%.
ENDLOCAL
goto:eof

:r4
::Take 2 "date"-mimicking numbers and subtract them.
::Test whether the difference equals the expected.
SETLOCAL ENABLEEXTENSIONS
  set mX=20210101
  set mY=20201130
  set mZ=8971
  set /A mA=%mX%-%mY%
  set result=FAILED
  if %mA% EQU %mZ% (
    set result=SUCCEEDED
  )
  echo %result% test r4: expected %mA% to equal %mZ%.
ENDLOCAL
goto:eof

:r5
::Fetch the year,  month, and day of the modification date of a file 
::and test whether that gives any non-zero value.
SETLOCAL ENABLEEXTENSIONS
  set /A dateOfFile=0
  call fetch_date_of_file.cmd dateOfFile %test_input_folder% %test_input_file%
  set /A fileYearMonthDay=0
  set fileYearMonthDay=%dateOfFile:~6,4%%dateOfFile:~0,2%%dateOfFile:~3,2%
  if %verbosity%==debug (
    echo r5.dateOfFile: [%dateOfFile%]
    echo r5.fileYearMonthDay: [%fileYearMonthDay%]
  )
  if []==[%dateOfFile%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r5.dateOfFile: got non-empty value: %dateOfFile%.

  if []==[%fileYearMonthDay%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r5.fileYearMonthDay: got non-empty value: %fileYearMonthDay%.

  if [0]==[%fileYearMonthDay%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r5.fileYearMonthDay: got non-zero value: %fileYearMonthDay%.
ENDLOCAL
goto:eof

:r6
::Tests whether 2 dates which differ more than a month
::and fall in the same year can be found to differ
::more than a month.
SETLOCAL ENABLEEXTENSIONS
  set /A mA=20200611
  set /A mB=20200501
  set /A mC=0
  call %~dp0\differs_less_than_100.cmd mC %mA% %mB%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r6.1: expected any non-empty result: '%mC%'.
  if 0 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r6.2: expected %mA% to differ more than 100 from %mB%.
  set /A mC=0
  call %~dp0\differs_less_than_100.cmd mC %mB% %mA%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r6.3: expected any non-empty result: '%mC%'.
  if 0 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r6.4: expected %mA% to differ more than 100 from %mB%.
ENDLOCAL
goto:eof

:r7
::Tests whether 2 dates which differ less than a month
::and fall in the same year can be found to differ
::less than a month.
SETLOCAL ENABLEEXTENSIONS
  set /A mA=20200611
  set /A mB=20200512
  set /A mC=0
  call %~dp0\differs_less_than_100.cmd mC %mA% %mB%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r7.1: expected any non-empty result: '%mC%'.
  if 1 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r7.2: expected %mA% to differ less than 100 from %mB%.
  set /A mC=0
  call %~dp0\differs_less_than_100.cmd mC %mB% %mA%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r7.3: expected any non-empty result: '%mC%'.
  if 1 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r7.4: expected %mA% to differ less than 100 from %mB%.
ENDLOCAL
goto:eof

:r8
::Tests whether 2 dates which differ less than a month
::and roll over the year's end can be found to differ
::less than a month.
SETLOCAL ENABLEEXTENSIONS
  set mA=20210111
  set mB=20201212
  set mC=0
  call %~dp0\differs_between_8870_and_8900.cmd mC %mA% %mB%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r8.1: expected any non-empty result: '%mC%'.
  if 1 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r8.2: expected %mA% to differ less than a month from %mB%.
  set mC=0
  call %~dp0\differs_between_8870_and_8900.cmd mC %mB% %mA%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r8.3: expected any non-empty result: '%mC%'.
  if 1 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r8.4: expected %mB% to differ less than a month from %mA%.
ENDLOCAL
goto:eof

:r9
::Tests whether 2 dates which differ more than a month
::and roll over the year's end can be found to differ
::more than a month.
SETLOCAL ENABLEEXTENSIONS
  set mA=20210112
  set mB=20201211
  set mC=0
  call %~dp0\differs_between_8870_and_8900.cmd mC %mA% %mB%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r9.1: expected any non-empty result: '%mC%'.
  if 0 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r9.2: expected %mA% to differ more than a month from %mB%.
  set mC=0
  call %~dp0\differs_between_8870_and_8900.cmd mC %mB% %mA%
  if []==[%mC%] (
    set result=FAILED
  ) else (
    set result=SUCCEEDED
  )
  echo %result% test r9.3: expected any non-empty result: '%mC%'.
  if 0 EQU %mC% (
    set result=SUCCEEDED
  ) else (
    set result=FAILED
  )
  echo %result% test r9.4: expected %mB% to differ more than a month from %mA%.
ENDLOCAL
goto:eof

:r10
::Tests that 2 dates which differ less than a month
::can be found to differ less than a month.
SETLOCAL ENABLEEXTENSIONS
  set /A found1=0
  call %~dp0\differs_less_than_a_month.cmd found1 20200701 20200611
  set /A found2=0
  call %~dp0\differs_less_than_a_month.cmd found2 20200130 20191231
  set /A found3=0
  call %~dp0\differs_less_than_a_month.cmd found3 20200131 20191231
  set /A found4=0
  call %~dp0\differs_less_than_a_month.cmd found4 20200101 20191201
  set result1=FAILED
  if 1 EQU %found1% (
    set result1=SUCCEEDED
  )
  set result2=FAILED
  if 1 EQU %found2% (
    set result2=SUCCEEDED
  )
  set result3=FAILED
  if 1 EQU %found3% (
    set result3=SUCCEEDED
  )
  set result4=FAILED
  if 1 EQU %found4% (
    set result4=SUCCEEDED
  )
  echo %result1% test r10.1: 20200701 must differ less than a month from 20200611.
  echo %result2% test r10.2: 20200130 must differ less than a month from 20191231.
  echo %result3% test r10.3: 20200130 must differ less than a month from 20191231.
  echo %result4% test r10.4: 20200101 must differ less than a month from 20191201.
ENDLOCAL
goto:eof


:r11
::Tests that 2 dates which differ more than a month
::can be found to differ more than a month.
SETLOCAL ENABLEEXTENSIONS
  set /A found1=0
  call %~dp0\differs_less_than_a_month.cmd found1 20200711 20200601
  set /A found2=0
  call %~dp0\differs_less_than_a_month.cmd found2 20200131 20191130
  set /A found3=0
  call %~dp0\differs_less_than_a_month.cmd found3 20200201 20191231
  set /A found4=0
  call %~dp0\differs_less_than_a_month.cmd found4 20200102 20191201
  set /A found5=0
  call %~dp0\differs_less_than_a_month.cmd found5 20200612 20200430
  set result1=FAILED
  if 0 EQU %found1% (
    set result1=SUCCEEDED
  )
  set result2=FAILED
  if 0 EQU %found2% (
    set result2=SUCCEEDED
  )
  set result3=FAILED
  if 0 EQU %found3% (
    set result3=SUCCEEDED
  )
  set result4=FAILED
  if 0 EQU %found4% (
    set result4=SUCCEEDED
  )
  set result5=FAILED
  if 0 EQU %found5% (
    set result5=SUCCEEDED
  )
  echo %result1% test r11.1: 20200711 must differ more than a month from 20200601.
  echo %result2% test r11.2: 20200131 must differ more than a month from 20191130.
  echo %result3% test r11.3: 20200201 must differ more than a month from 20191231.
  echo %result4% test r11.4: 20200102 must differ more than a month from 20191201.
  echo %result5% test r11.5: 20200612 must differ more than a month from 20200430.
ENDLOCAL
goto:eof

