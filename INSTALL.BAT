@echo off
setlocal EnableExtensions

:: 1. Identify this script's directory (where mettalog.cmd is).
set "SCRIPT_DIR=%~dp0"


REM ----------------------------------------------------------------
REM 1. Get domain & user
REM ----------------------------------------------------------------
set "CURRENT_DOMAIN=%USERDOMAIN%"
set "CURRENT_USER=%USERNAME%"

echo Detected Domain  : %CURRENT_DOMAIN%
echo Detected User    : %CURRENT_USER%
echo.

REM ----------------------------------------------------------------
REM 2. Check admin privileges
REM ----------------------------------------------------------------
>nul 2>&1 net session
if %errorlevel% NEQ 0 (
    echo(
    echo ================================================
    echo  This script must be run with admin privileges.
    echo ================================================
    echo  1^) Right-click the file and choose "Run as administrator", OR
    echo  2^) Open an elevated Command Prompt ^(Run as administrator^) and
    echo      navigate to this script's folder, then run:
    echo          %~nx0
    echo(
    echo ---------------------------------------------------------------
    echo  If you're running this script from a network drive,
    echo  the elevated session might not see that drive mapping.
    echo  Let's try to detect the UNC path for the current drive...
    echo ---------------------------------------------------------------
    echo.

    REM Attempt to detect the UNC path from `net use`.
    set "DRV=%~d0"
    set "UNC="

    echo Current drive is: %DRV%
    echo(
    echo Checking if %DRV% is mapped via "net use"...
    for /f "skip=4 tokens=1,2,3,*" %%A in ('net use') do (
        REM Typically lines look like:
        REM   OK           H:        \\Server\Share   Microsoft Windows Network
        if /i "%%B"=="%DRV%" (
            set "UNC=%%C"
            goto :NONADMIN_FOUND
        )
    )

:NONADMIN_FOUND
    if defined UNC (
        echo     Found UNC for drive %DRV%: %UNC%
        echo(
        echo     To map this drive in the elevated session
		echo.
        echo         net use %DRV% "%UNC%"
		echo         %DRV%
	    echo         cd %SCRIPT_DIR%
    ) else (
        echo     Could not detect a UNC path automatically.
        net use %DRV%
    )

    echo.
    echo Then re-run this script as Administrator from that newly
    echo mapped drive letter.
    echo ---------------------------------------------------------------
    pause
    exit /b 1
)

REM ----------------------------------------------------------------
REM 3. If we get here, we *are* running with administrative privileges.
REM ----------------------------------------------------------------
echo Running as admin: Domain=%CURRENT_DOMAIN%, User=%CURRENT_USER%.

REM (A) Detect or map the UNC in the *admin* session
set "DRV=%~d0"
set "UNC="

echo Current drive is: %DRV%
echo(
echo Checking if %DRV% is mapped via "net use"...
for /f "skip=4 tokens=1,2,3,*" %%A in ('net use') do (
    if /I "%%B"=="%DRV%" (
        set "UNC=%%C"
        goto :ADMIN_MAPPING_FOUND
    )
)

:ADMIN_MAPPING_FOUND
if defined UNC (
    echo Found UNC path for %DRV%: %UNC%
    echo Attempting to ensure this drive is mapped in the admin session...
    net use %DRV% "%UNC%"  >nul 2>&1
    if %errorlevel% NEQ 0 (
        echo(
        echo Failed to map drive %DRV% to %UNC% with user  .
        echo Ensure the credentials are correct and try again manually:
        echo     net use %DRV% "%UNC%"
	    echo     %DRV%
		echo     cd %SCRIPT_DIR%
        pause
        exit /b 1
    )
    echo Drive %DRV% mapped successfully to %UNC% for admin session.
) else (
    echo No UNC mapping found for drive %DRV%.
    echo Attempting to map it to \\server\share with user  ...
    net use >nul 2>&1
    if %errorlevel% NEQ 0 (
        echo(
        echo Failed to map drive %DRV% to \\server\share with user  .
        echo Ensure the share path and credentials are correct and try again manually:
        echo     net use %DRV% 
		echo     %DRV%
		echo     cd %SCRIPT_DIR%
        pause
        exit /b 1
    )
    echo Drive %DRV% successfully mapped to \\server\share for admin session.
)

echo(
echo Script is running with administrative privileges. Proceeding...
echo.

REM ----------------------------------------------------------------
REM 4. EnableLinkedConnections (so Admin context sees mapped drives)
REM ----------------------------------------------------------------
echo Enabling EnableLinkedConnections...
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" ^
    /v EnableLinkedConnections /t REG_DWORD /d 1 /f >nul 2>&1

REM ----------------------------------------------------------------
REM 5. Prompt user: Install or Uninstall the .metta file association
REM ----------------------------------------------------------------
:MENU
@echo(
@echo ===========================================
@echo  .metta File Association
@echo ===========================================
@echo   I = Install
@echo   U = Uninstall
@echo ===========================================
choice /C IU /N /M "Select an option (I/U) then press ENTER: "
if errorlevel 2 goto UNINSTALL
if errorlevel 1 goto INSTALL
goto MENU

REM ----------------------------------------------------------------
REM (A) INSTALL SECTION
REM ----------------------------------------------------------------
:INSTALL
@echo Installing .metta file association...

:: 2. Escape backslashes in SCRIPT_DIR for the .reg file.
set "SCRIPT_DIR_ESCAPED=%SCRIPT_DIR:\=\\%"

:: 3. Name for the .reg file we will generate.
set "REGFILE=%SystemRoot%\temp\metta-association.reg"

@echo Creating registry file: "%REGFILE%"
(
    @echo Windows Registry Editor Version 5.00
    
    @echo ; Associate .metta files with mettalog.cmd
    @echo [HKEY_CLASSES_ROOT\.metta]
    @echo @="MettaFile"
    
    @echo [HKEY_CLASSES_ROOT\MettaFile]
    @echo @="Metta Script File"
    
    @echo ; Set a system icon for .metta files
    @echo [HKEY_CLASSES_ROOT\MettaFile\DefaultIcon]
    @echo @="%%SystemRoot%%\\regedit.exe,0"
    
    @echo [HKEY_CLASSES_ROOT\MettaFile\shell]
    @echo @="open"
    
    @echo ; Default action ^(open^)
    @echo [HKEY_CLASSES_ROOT\MettaFile\shell\open]
    @echo @="Run with Metta"
    
    @echo [HKEY_CLASSES_ROOT\MettaFile\shell\open\command]
    @echo @="\"%SCRIPT_DIR_ESCAPED%mettalog.cmd\" \"%%1\""
    
    @echo ; Add 'Edit with Notepad' to the context menu
    @echo [HKEY_CLASSES_ROOT\MettaFile\shell\edit]
    @echo @="Edit with Notepad"
    
    @echo [HKEY_CLASSES_ROOT\MettaFile\shell\edit\command]
    @echo @="notepad.exe \"%%1\""
)>"%REGFILE%"

@echo Importing "%REGFILE%" into the registry...
reg import "%REGFILE%"
if errorlevel 1 (
    @echo.
    @echo Registry import failed. Make sure you are running as Administrator.
    goto :EOF
)

@echo Installation complete! .metta files should now open with mettalog.cmd.
goto :EOF

REM ----------------------------------------------------------------
REM (B) UNINSTALL SECTION
REM ----------------------------------------------------------------
:UNINSTALL
@echo Uninstalling .metta file association...
@echo Removing registry keys...

reg delete "HKCR\.metta" /f >nul 2>&1
reg delete "HKCR\MettaFile" /f >nul 2>&1

@echo Uninstall complete! .metta file association removed.
goto :EOF

endlocal

pause
:EOF

