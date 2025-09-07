@echo off
cd /d "%~dp0"
echo Compilando DLL a partir de main.f90 y bisection.f90...

gfortran -shared -static-libgcc -static-libstdc++ -o libcalculos.dll bisection.f90 main.f90

if exist libcalculos.dll (
    echo DLL generada exitosamente: libcalculos.dll
) else (
    echo ERROR: No se generó la DLL.
)
pause
