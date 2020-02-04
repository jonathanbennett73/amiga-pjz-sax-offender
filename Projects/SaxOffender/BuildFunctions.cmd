@rem Build functions

@echo off
set ASMLIBFUNC=%~1
@shift /1
call :%ASMLIBFUNC% %1 %2 %3 %4 %5 %6 %7 %8 %9
goto exit



@rem Setup initial params
:dosetup
@set DemoName=PJZ-SaxOffender
@set UseIntroWrapper=0
@rem @set launcher=runa500
@rem @set launcher=runa1200_4mb
@rem @set launcher=runa4000_40
@set launcher=runaros
@set UseMiniPacker=0
@set DontPack=0
@set PackerType=CRANKER
@set CrankerFlags=-d progress -tf IntroDepackText.txt
@set ShrinklerFlags=--flash dff180 --text "Loading %DemoName%. Please wait..." --overlap
@set Debug=0
@set DontRun=0
@set DontBuild=0

set ObjDir=Build
set RelDir=Release
set ObjFileList=
set OutDir=vscode-hd0
exit /b



@rem Parse command line
:docmdline
:ParseCommandLine
@if "%1" equ "" goto ParseCommandLineEnd
@if "%1" equ "-debug" set Debug=1
@if "%1" equ "-debug" goto ParameterParsed

@if "%1" equ "-dontrun" set DontRun=1
@if "%1" equ "-dontrun" goto ParameterParsed

@if "%1" equ "-dontbuild" set DontBuild=1
@if "%1" equ "-dontbuild" goto ParameterParsed

@if "%1" equ "-dontpack" set DontPack=1
@if "%1" equ "-dontpack" goto ParameterParsed

@if "%1" equ "-aros" set launcher=runaros
@if "%1" equ "-aros" goto ParameterParsed

@echo Error: Unknown bat file parameter %1
@goto failed
:ParameterParsed
@shift /1
@goto ParseCommandLine
:ParseCommandLineEnd

@if %Debug% EQU 1 set DontPack=1
@if %UseIntroWrapper% EQU 0 set DontPack=1


@rem General build params
set BuildParam=-m68000 -opt-speed -opt-lsl -opt-pea -opt-mul -opt-div -opt-movem -Fhunk -kick1hunks -linedebug -nowarn=62 -x -I Include -D_DEBUG=%Debug% -D_INTROWRAPPER=%UseIntroWrapper% -DUseMiniPacker=%UseMiniPacker%
REM set BuildParam=-m68000 -phxass -showopt -Fhunk -align -kick1hunks -linedebug -nowarn=62 -x -DDebug=%Debug% -DUseMiniPacker=%UseMiniPacker% -I Include -D_INTROWRAPPER=%UseIntroWrapper%
REM set BuildParam=-m68000 -phxass -no-opt -Fhunk -align -kick1hunks -linedebug -nowarn=62 -x -DDebug=%Debug% -DUseMiniPacker=%UseMiniPacker% -I Include -D_INTROWRAPPER=%UseIntroWrapper%
if %UseMiniPacker% NEQ 0 set BuildParam=%BuildParam% -pic

@rem General linker params, strip symbols unless debugging, -k keeps section order so order source files with biggest data first
set LinkParam=-bamigahunk -Bstatic -k
if %Debug% NEQ 1 set LinkParam=%LinkParam% -s

@if %DontBuild% EQU 1 goto NotBuilding
if not exist %ObjDir% md %ObjDir%
if errorlevel 1 goto failed
del %ObjDir%\*.o %ObjDir%\*.info %ObjDir%\*.adf /s /q >NUL
del %OutDir%\%DemoName%Unpacked /s /q >NUL
del %OutDir%\%DemoName% /s /q >NUL
:NotBuilding
exit /b



@rem Assemble framework
:doassembleframework
@if %UseIntroWrapper% EQU 0 goto asmstandalone
call :doassemble Framework\IntroWrapper.s IntroWrapper.o
if errorlevel 1 goto AssembleFrameworkFailed
goto :asmframeworkrest

:asmstandalone
call :doassemble Framework\IntroStandalone.s IntroStandalone.o
if errorlevel 1 goto AssembleFrameworkFailed

:asmframeworkrest
call BuildFunctions.cmd doassemble IntroSharedData.s IntroSharedData.o
if errorlevel 1 goto AssembleFrameworkFailed

call BuildFunctions.cmd doassemble Framework\IntroFramework.s IntroFramework.o
if errorlevel 1 goto AssembleFrameworkFailed

call BuildFunctions.cmd doassemble Framework\IntroLibrary.s IntroLibrary.o
if errorlevel 1 goto AssembleFrameworkFailed

call BuildFunctions.cmd doassemble IntroPreCalc.s IntroPreCalc.o
if errorlevel 1 goto AssembleFrameworkFailed

exit /b 0

:AssembleFrameworkFailed
exit /b 1



@rem Assemble function
:doassemble
set SrcFile=%1 
set ObjFile=%ObjDir%\%2
set ObjFileList=%ObjFileList% %ObjFile%
ECHO ASSEMBLING: %1
vasmm68k_mot_win32.exe %BuildParam% -o %ObjFile% %SrcFile% 
ECHO.
exit /b



@rem Link function
:dolink
echo.
echo LINKING %DemoName%
echo LinkParam: %LinkParam%
echo.

vlink %LinkParam% -o %OutDir%\%DemoName%Unpacked %ObjFileList%
REM vc -O2 -notmpfile -nostdlib -DDebug=%Debug% -DUseMiniPacker=%UseMiniPacker% -o %OutDir%\%DemoName%Unpacked.exe %ObjDir%\%DemoName%Unpacked.o
if errorlevel 1 goto LinkFailed
if not exist %OutDir%\%DemoName%Unpacked goto LinkFailed
exit /b 0
:LinkFailed
exit /b 1



@rem Create ADF for release function
:doadf
if not exist "%RelDir%" md "%RelDir%"
if not exist "%RelDir%\ADF" md "%RelDir%\ADF"
if not exist "%RelDir%\LHA" md "%RelDir%\LHA"
del %RelDir%\*.* /s /q >NUL
del %RelDir%\ADF\*.* /s /q >NUL
del %RelDir%\LHA\*.* /s /q >NUL

@rem general files renamed to demo name
copy "%OutDir%\%DemoName%" "%ObjDir%" /Y
copy "ADF\Demo.readme" "%ObjDir%\%DemoName%.readme" /Y
copy "ADF\Demo.readme.info" "%ObjDir%\%DemoName%.readme.info" /Y
copy "ADF\Demo.info" "%ObjDir%\%DemoName%.info" /Y

@rem ADF files
echo %DemoName% > ADF\s\Startup-Sequence
set ADFFile=%ObjDir%\%DemoName%.adf
adfcreate -f 0 -l %DemoName% "%ADFFile%"
adfinst "%ADFFile%" -i
adfcopy "%ADFFile%" "%OutDir%\%DemoName%" /
adfmakedir "%ADFFile%" s
adfcopy "%ADFFile%" "ADF\s\Startup-Sequence" s
adfcopy "%ADFFile%" "%ObjDir%\%DemoName%.readme" /
adfcopy "%ADFFile%" "ADF\file_id.diz" /
@rem adfcopy "%ADFFile%" "%ObjDir%\%DemoName%.readme.info" /
@rem adfcopy "%ADFFile%" "%ObjDir%\%DemoName%.info" /
copy "%ADFFile%" "%RelDir%\ADF" /Y
copy "%ObjDir%\%DemoName%.readme" "%RelDir%\ADF" /Y
copy "ADF\file_id.diz" "%RelDir%\ADF" /Y
@rem 7z.exe a -tzip -mx=9 "%RelDir%\%DemoName%-ADF.zip" ".\%RelDir%\ADF\*"


@rem Files only
copy "%OutDir%\%DemoName%" "%RelDir%\LHA" /Y
@rem copy "ADF\Demo.info" "%RelDir%\%DemoName%.info" /Y
copy "%ObjDir%\%DemoName%.readme" "%RelDir%\LHA" /Y
@rem copy "%ObjDir%\%DemoName%.readme.info" "%RelDir%" /Y
copy "ADF\file_id.diz" "%RelDir%\LHA" /Y
@set _oldpathlha=%cd%
cd /d "%RelDir%\LHA"
@rem lha.exe a -o5 "..\%DemoName%-FILES.lha" *
cd /d "%_oldpathlha%"
exit /b 0



@rem Pack
:dopack
@rem Pack demo with shrinkler if required
@if %DontPack% NEQ 0 copy %OutDir%\%DemoName%Unpacked %OutDir%\%DemoName%
@if %DontPack% NEQ 0 goto VerifyPack
echo.
echo PACKING %DemoName%

if "%PackerType%"=="CRANKER" goto UseCranker

if %UseMiniPacker% NEQ 0 set ShrinklerFlags=--mini
ECHO ShrinklerFlags: %ShrinklerFlags%
shrinkler %OutDir%\%DemoName%Unpacked %OutDir%\%DemoName% %ShrinklerFlags% --no-progress
goto :VerifyPack

:UseCranker
ECHO CrankerFlags: %CrankerFlags%
cranker -f %OutDir%\%DemoName%Unpacked -o %OutDir%\%DemoName% %CrankerFlags%

:VerifyPack
if not exist %OutDir%\%DemoName% goto PackFailed
exit /b 0
:PackFailed
exit /b 0



:exit
exit /b