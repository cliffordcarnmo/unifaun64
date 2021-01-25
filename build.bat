@echo off
SET binaries=bin
SET buildPath=build
SET dataPath=data
SET name=unifaun64
SET build=%name%.prg
SET disk=%name%.d64
SET sourcePath=source
SET source=main.asm
SET compiler=acme.exe
SET compilerArgs=--msvc --color --format cbm -v3 --outfile
SET cruncher=pucrunch.exe
SET cruncherArgs=-x$0801 -c64 -g55 -fshort
SET emulatorPath=%binaries%\vice\bin
SET emulator=x64sc.exe
SET emulatorArgs=%buildPath%\%disk%
SET disktool=c1541.exe
SET disktoolArgs=-format %name%,42 d64 %buildPath%\%disk% -attach %buildPath%\%disk% -write %buildPath%\%build% %name%

%binaries%\%compiler% %compilerArgs% %buildPath%\%build% %sourcePath%\%source%
%binaries%\%cruncher% %cruncherArgs% %buildPath%\%build% %buildPath%\%build%
%emulatorPath%\%disktool% %disktoolArgs%
del %buildPath%\%build%
%emulatorPath%\%emulator% %emulatorArgs%