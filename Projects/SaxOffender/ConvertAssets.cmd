@echo off
@set _oldpath=%cd%
cd /d %~dp0
call ..\..\toolchain\setpaths.bat

if not exist AssetsConverted md AssetsConverted
if errorlevel 1 goto failed
del AssetsConverted\* /s /q >NUL
if errorlevel 1 goto failed

@rem Convert music modules
REM copy Assets\mod.Antiriad_10_2.0 ..\..\DH1\Modules /Y
REM call ..\..\ConvertModToP61 dh1\modules\mod.Antiriad_10_2.0 Projects\Starfield2\AssetsConverted TwoFiles_delta
REM copy Assets\mod.!retromancer ..\..\DH1\Modules /Y
REM call ..\..\ConvertModToP61 dh1\modules\mod.!retromancer Projects\PJZ1\AssetsConverted TwoFiles
REM if errorlevel 1 goto failed
REM cd /d %~dp0

@rem Convert any graphic assets
if exist assetlist.txt (
	kingcon @assetlist.txt
	if errorlevel 1 goto failed
)

cd /d AssetsConverted

@rem Pack data
@REM Examples with depackers in IntroLibrary.s
@REM shrinkler -3 -d -p Precalc_Logo_320x256x5.BPL Precalc_Logo_320x256x5.BPL.shr
@REM cranker -cd -f Precalc_Logo_320x256x5.BPL -o Precalc_Logo_320x256x5.BPL.cra
@REM packfire -l Precalc_Logo_320x256x5.BPL Precalc_Logo_320x256x5.BPL.pkf
@rem lz4 -9 --no-frame-crc Precalc_Logo_320x256x5.BPL Precalc_Logo_320x256x5.BPL.lz4



@rem Precalc
call :compress Precalc_Logo_320x256x5.BPL
call :compress Precalc_Logo_Sil_320x256x1.BPL
call :compress Precalc_Dedication_320x256x5.BPL

@rem Starfield/logos
call :compress PJZ_logo2019_320x256x4_inter.BPL
call :compress PJZ_logo2019_Sil_320x256x1.BPL

@rem Greetz
call :compress Greetz_Font_8x16x2.BPL
call :compress Greetz_Font_8x16x1.BPL

@rem Play it again sam
call :compress PlayItAgainSam_320x256x5_inter.BPL

@rem Palms
call :compress Miami_Palm_1056x256x1.BPL
call :compress Miami_BirdAnim_32x32_8Frames.SPR

@rem Warp
call :compress Warp_352x272x1_0.BPL
call :compress Warp_352x272x1_1.BPL
call :compress Warp_352x272x1_2.BPL
call :compress Warp_352x272x1_3.BPL
call :compress Warp_352x272x1_4.BPL
call :compress Warp_352x272x1_5.BPL
call :compress Warp_352x272x1_6.BPL

@rem End credits
call :compress EndCredits_Overlay_352x272x3_inter.BPL
call :compress NeonDays_320x256x5_inter.BPL
call :compress NeonDays_320x256x5.BPL
call :compress NeonDays_320x256x4.BPL
call :compress NeonDays_320x256x3_inter.BPL


echo SUCCESS
cd /d %_oldpath%
exit /b 0

:failed
echo FAILED
cd /d %_oldpath%
exit /b 1


:compress
@rem lz4 -9 --no-frame-crc %1 %1.lz4
nrv2x -es -o %1.nrv2s %1
@rem doynamite68k_lz -o %1.doy %1
@rem cranker -cd -f %1 -o %1.cra

@rem arjbeta a -m7 -jm %1.arj %1
@rem arj2raw %1.arj %1 %1.am7
@rem del %1.arj

@rem packfire -l %1 %1.pkl
@rem packfire -t %1 %1.pkt

@rem shrinkler -3 -d -p %1 %1.shr
exit /b
