@echo off
cd /d "%~dp0"
echo Compilando DLL a partir de los archivos de math...

gfortran -shared -static-libgcc -static-libstdc++ -o libmath.dll ^
    utils.f90 tokens.f90 symtab.f90 parser.f90 ast.f90 math.f90 eval.f90 calculator.f90 calc.f90

if exist libmath.dll (
    echo DLL generada exitosamente: libmath.dll
) else (
    echo ERROR: No se generó la DLL.
)
pause



@REM @echo off
@REM cd /d "%~dp0"

@REM echo Compilando DLL a partir de los archivos de math...

@REM gfortran -shared -static-libgcc -static-libstdc++ -o libmath.dll utils.f90 parser.f90 calculator.f90 eval.f90 symtab.f90 tokens.f90 ast.f90 math.f90 calc.f90

@REM if exist libmath.dll (
@REM     echo DLL generada exitosamente: libmath.dll
@REM ) else (
@REM     echo ERROR: No se generó la DLL.
@REM )
@REM pause
