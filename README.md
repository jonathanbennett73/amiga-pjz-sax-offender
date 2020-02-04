# Planet Jazz - Sax Offender (Amiga A500 Demo Source)

## What is it?

This is the 68000 assembler source code for the Planet Jazz ["Sax Offender"](https://www.pouet.net/prod.php?which=) demo.

It consists a a very cut down Windows build environment based on the [Lemon toolchain](https://www.pouet.net/prod.php?which=65625) by Hannibal.

## How to Use

1. Go into the Projects\SaxOffender folder
2. Open "_DevCmdPrompt.cmd" to bring up a command prompt
3. Run ConvertAssets.cmd to convert all the graphic assets
4. Run BuildDemo.cmd to build and run the demo in an AROS environment

Each demo "part" is in its own folder under Projects\SaxOffender\\\<part>

The "Framework" folder is probably the most generic and useful as it contains all the helper functions, libraries, startup code, etc.

Any problems [email me](mailto:jon@autoitscript.com) or contact Antiriad_UK on [English Amiga Board](http://eab.abime.net/index.php).

## Thanks

Thanks to these people for the tools and scripts used in this demo. It wouldn't have been possible without
you!

* ross - NRV2S Packer
* Hannibal/Lemon. - KingCon
* Pink/Abyss - PreTracker
* bifat/TEK - Cranker
* Soundy/The Deadliners - Gradient Master
* Axis/Oxyron - Planet Rocklobster source
* Frank Wille - VASM 68k
* prb28 - [ASM extension for VSCode](https://github.com/prb28/vscode-amiga-assembly)
* Toni Wilen - WinUAE
