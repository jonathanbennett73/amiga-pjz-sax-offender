@set ExeFileName=%1
@if '%ExeFileName%' EQU '' call %~dp0SetExeFileName.bat
@%~dp0toolchain\startwinuae.bat a1200_4mb KICK30 %ExeFileName% %2