@set ExeFileName=%1
@if '%ExeFileName%' EQU '' call %~dp0SetExeFileName.bat
@%~dp0toolchain\startwinuae.bat a4000_60 KICK31 %ExeFileName% %2