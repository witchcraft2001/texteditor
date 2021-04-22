rem echo OFF
mkdir build
echo Unmounting old image ...
osfmount.com -D -m X:

echo Assembling ...
C:\asm\sjasm\sjasmplus.exe --lst=ted.lst --lstlab textedit.asm
if errorlevel 1 goto ERR

echo Preparing floppy disk image ...
copy /Y image\dss_image.img build\editor.img
rem Delay before copy image
timeout 2 > nul
osfmount.com -a -t file -o rw -f build/editor.img -m X:
if errorlevel 1 goto ERR
mkdir X:\EDITOR
copy /Y TED.EXE X:\EDITOR
if errorlevel 1 goto ERR
rem Delay before unmount image
timeout 2 > nul
echo Unmounting image ...
osfmount.com -d -m X:
rem Delay before copy image
timeout 2 > nul
goto SUCCESS
:ERR
rem pause
echo Some Building ERRORs!!!
rem exit
goto END
:SUCCESS
echo Copying image to ZXMAK2 Emulator
copy /Y build\EDITOR.img /B %SPRINTER_EMULATOR% /B
echo Done!
:END
pause 0