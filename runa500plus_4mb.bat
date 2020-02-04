@set ExeFileName=%1
@if '%ExeFileName%' EQU '' call %~dp0SetExeFileName.bat
@%~dp0toolchain\startwinuae.bat a500plus_4MB KICK204 %ExeFileName% %2