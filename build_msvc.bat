@echo off
REM Build script for MSVC (Visual Studio)
REM Run this from a Visual Studio Developer Command Prompt

echo Building Rexigon with MSVC...

cl /O2 /W3 /D "NDEBUG" /D "_CRT_SECURE_NO_WARNINGS" /Fe:rexigon_msvc.exe main.c engine.c search.c eval.c position.c movegen.c

if %ERRORLEVEL% EQU 0 (
    echo Build successful! Executable: rexigon_msvc.exe
    echo Testing...
    echo xboard | rexigon_msvc.exe
) else (
    echo Build failed!
    exit /b 1
)