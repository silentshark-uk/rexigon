@echo off
echo Building Rexigon Chess Engine...

gcc -std=c99 -O3 -Wall -Wextra -o rexigon.exe main.c engine.c position.c eval.c search.c movegen.c

if %errorlevel% == 0 (
    echo Build successful! Run rexigon.exe to start the engine.
) else (
    echo Build failed!
)

pause