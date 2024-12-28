@echo off

setlocal

REM Store the script’s directory in a Windows-friendly variable
set "SCRIPT_DIR=%~dp0"

REM Convert backslashes to forward slashes for SWI-Prolog, if necessary:
set "METTALOG_DIR=%SCRIPT_DIR:\=/%"

REM Clean up any old buffer files if needed
REM del "%SCRIPT_DIR%\prolog\metta_lang\*.buffer~"

REM Start SWI-Prolog with your Metta interpreter
REM @echo swipl -l "%METTALOG_DIR%prolog/metta_lang/metta_interp" -g do_loon -- %*

swipl -l "%METTALOG_DIR%prolog/metta_lang/metta_interp" -g do_loon -- %* --repl

endlocal

