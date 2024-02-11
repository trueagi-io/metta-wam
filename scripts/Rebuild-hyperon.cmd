@echo on

set OLDPATH=%CD%
cd /D "%~dp0"
set VSPACE=%CD%
SET HE=%VSPACE%\reqs\hyperon-experimental
SET PYPATH=%VSPACE%\env\Scripts
SET CARGOPATH=%HOMEDRIVE%%HOMEPATH%\.cargo\bin

SET PATH=%PYPATH%;%CARGOPATH%;%PATH%

call %PYPATH%\activate.bat

cd /D %HE%
echo  Prepare environment
DIR %CARGOPATH%
%CARGOPATH%\rustup.exe update stable
python.exe -m pip install pip -U
%CARGOPATH%\cargo install --force cbindgen
%PYPATH%\pip install pyyaml
%PYPATH%\pip install conan==1.60.1
%PYPATH%\conan profile new --detect default
%PYPATH%\python.exe -m pip install pip==23.1.2

echo  Build Hyperon library
cd /D %HE%\lib
%CARGOPATH%\cargo clean
%CARGOPATH%\cargo build
%CARGOPATH%\cargo test
%CARGOPATH%\cargo doc --no-deps
cd /D %HE%

echo Build C and Python API
#trash buil cd build
del /Q /S /F .\build
mkdir build
cd build
cmake ..
make -j4
make check


cd /D %HE%
echo  Install python library and executables
%PYPATH%\pip install -v -e ./python[dev]
%PYPATH%\python.exe -m pip install ./python[dev]

echo  Test
cd /D %HE%\python
%PYPATH%\pytest ./tests
cd /D %HE%

cd /D %HE%\repl
%CARGOPATH%\cargo doc --no-deps --features python
%CARGOPATH%\cargo install --path . --features python
cd /D %HE%

cd %VSPACE%
%PYPATH%\python.exe -m pip install --upgrade pip
%PYPATH%\python.exe -m pip install -r requirements.txt

cd /D %OLDPATH%

