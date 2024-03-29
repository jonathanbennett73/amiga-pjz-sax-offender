@rem Command Line Options are -dontrun -dontbuild -debug

@echo off
@set _oldpath=%cd%
cd /d %~dp0
call ..\..\toolchain\setpaths.bat

@rem Setup initial values and then override as needed
call BuildFunctions.cmd dosetup

@rem Read command line params and set params as needed
call BuildFunctions.cmd docmdline %*

@rem Overrides
@set UseIntroWrapper=0
@set BuildParam=%BuildParam% -showopt

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

call BuildFunctions.cmd doassemble Miami\Miami.s Miami.o
if errorlevel 1 goto failed


@rem Link
call BuildFunctions.cmd dolink
if errorlevel 1 goto failed


@rem Pack demo with shrinkler if required
call BuildFunctions.cmd dopack
if errorlevel 1 goto failed


@rem startup sequence for fs-uae
echo sys:%DemoName%.exe > %OutDir%\s\Startup-Sequence
echo UAEQuit >> %OutDir%\s\Startup-Sequence

echo.
:DontBuild
if %DontRun% equ 1 goto DontRun
if not exist %~dp0%OutDir%\%DemoName%.exe goto failed

@rem Run the demo in toolchain environment (HD)
call ..\..\%launcher% %~dp0%OutDir%\%DemoName%.exe
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

