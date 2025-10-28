@echo off
for /r %%i in (*.mod) do del "%%i" /q
for /r %%i in (*.exe) do del "%%i" /q
echo Archivos eliminados.
pause
