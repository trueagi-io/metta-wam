@ECHO OFF
SETLOCAL EnableDelayedExpansion

SET "SCRIPT=%~f0"
SET "METTALOG_DIR=%~dp0"
SET "ALL_ARGS="
SET "FILE_FOUND=0"
SET "REPL_FOUND=0"

FOR %%A IN (%*) DO (
    REM Check if the argument is --repl
    IF "%%~A"=="--repl" (
        SET "REPL_FOUND=1"
    )
    
    REM Check if the argument is a file with the .metta extension
    IF "%%~xA"==".metta" (
        SET "FILE_FOUND=1"
        REM Replace slashes and add quotes if there are spaces
        SET "ARG=%%~A"
        SET "ARG=!ARG:/=\\!"
        IF "!ARG!" NEQ "%%~A" SET "ARG="!ARG!""
    ) ELSE (
        SET "ARG=%%~A"
    )
    
    REM Add quotes if there are spaces in the argument
    echo !ARG! | findstr /C:" " > nul
    IF NOT ERRORLEVEL 1 SET "ARG="!ARG!""
    SET "ALL_ARGS=!ALL_ARGS! !ARG!"
)

REM Add --repl at the end if no file was specified and --repl was not already found
IF "!FILE_FOUND!"=="0" IF "!REPL_FOUND!"=="0" (
    SET "ALL_ARGS=!ALL_ARGS! --repl"
)

REM Run the command with all arguments
swipl -l "%METTALOG_DIR%src\main\metta_interp.pl" -- --python=false !ALL_ARGS!


