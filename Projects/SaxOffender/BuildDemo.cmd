@rem Command Line Options are -dontrun -dontbuild -debug

@echo off
@set _oldpath=%cd%
cd /d %~dp0
call ..\..\toolchain\setpaths.bat

@rem Setup initial values and then override as needed
call BuildFunctions.cmd dosetup
@set UseIntroWrapper=1

@rem Read command line params and set params as needed
call BuildFunctions.cmd docmdline %*

@rem build if needed
@if %DontBuild% equ 1 goto DontBuild

echo.
echo ASSEMBLING %DemoName%
echo BuildParam: %BuildParam%
echo.

REM Assemble all sections and link together. Assemble entry point file first
REM Assemble files that have large data/bss sections first.
call BuildFunctions.cmd doassembleframework
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble DotFlag\DotFlag.s DotFlag.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble EndCredits2\EndCredits.s EndCredits.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble Greetz\Greetz.s Greetz.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble Miami\Miami.s Miami.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble Pic1\Pic1.s Pic1.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble Starfield\Starfield.s Starfield.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble GlenzBobs\GlenzBobs.s GlenzBobs.o
if errorlevel 1 goto failed

call BuildFunctions.cmd doassemble ExplodeVector\ExplodeVector.s ExplodeVector.o
if errorlevel 1 goto failed


@rem Link
call BuildFunctions.cmd dolink
if errorlevel 1 goto failed


@rem Pack demo with shrinkler if required
call BuildFunctions.cmd dopack
if errorlevel 1 goto failed


@rem startup sequence for fs-uae
echo sys:%DemoName% > %OutDir%\s\Startup-Sequence
echo UAEQuit >> %OutDir%\s\Startup-Sequence


@rem create the adf for release/adf testing
call BuildFunctions.cmd doadf
if errorlevel 1 goto failed


echo.
:DontBuild
if %DontRun% equ 1 goto DontRun
if not exist %~dp0%OutDir%\%DemoName% goto failed

@rem Run the demo in toolchain environment (HD)
call ..\..\%launcher% %~dp0%OutDir%\%DemoName%
if errorlevel 1 goto failed
:DontRun

echo SUCCESS
cd /d %_oldpath%
exit /b 0

:failed
echo FAILED
cd /d %_oldpath%
exit /b 1

@rem END





