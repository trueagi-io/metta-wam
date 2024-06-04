#!/bin/bash

# Resolve the build directory where the symlink resides
BUILD_DIR=$(realpath $(dirname ${BASH_SOURCE[0]}))

# Check if the script is sourced or executed directly
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "Exiting. This script must be sourced to function correctly."
    echo "Use: source ${BASH_SOURCE[0]}"
    exit 0
else
    echo "The build directory of the sourced script is: $BUILD_DIR"
fi

# Temporarily change directory and run build operations in a subshell
(
  set -e -x -v  # Enable error exit, command echo, and verbose output
  cd "$BUILD_DIR"  # Change to the build directory
  # Execute build commands inside the virtual environment
  source "./venv8/bin/activate"  # Activate the virtual environment

python -m pip install -r python/hyperon/exts/das_gate/requirements.txt
#python -m pip install -r python/requirements.txt

# Prepare environment
rustup update stable
cargo install --force cbindgen
pip install -U pip
pip install conan==1.60.2
conan profile new --detect default | true
#pip install pip==23.1.2
pip install pip==23.2.1

# Build Hyperon library
(
# 3.26.4 cmake 
# Build Hyperon library
cd ./lib
cargo clean
cargo build
cargo test
cargo doc --no-deps
cd ..
)

# Build C and Python API
(
#trash build; 
rm -rf build
mkdir build; cd build
cmake ..
make -j4
make check
cd ..

)

# Install python library and executables
pip install -v -e ./python[dev]
#python3 -m pip install -v -e ./python[dev]
#python3 -m venv venv8 -e .

# Build Hyperon MeTTa-REPL
(
cd ./repl
cargo doc --no-deps
cargo install --path=. 
cd ..

cp -f ~/.cargo/bin/metta ${BUILD_DIR}/venv8/bin/meta-repl 
mv -f ~/.cargo/bin/metta ~/.cargo/bin/metta-repl-min 
)

(
export SCRIPT=~/.cargo/bin/metta-min
echo '#!/bin/bash' > $SCRIPT
echo "source ${BUILD_DIR}/venv8/bin/activate" >> $SCRIPT
echo 'if [ $# -eq 0 ]; then' >> $SCRIPT
echo '  metta-repl' >> $SCRIPT
echo 'else' >> $SCRIPT
echo '  metta $*' >> $SCRIPT
echo 'fi' >> $SCRIPT
chmod +x $SCRIPT
)



# Test
(
cd python
echo pytest ./tests
cd ..
)

)

# After subshell, reactivate the virtual environment for interactive use
source "$BUILD_DIR/venv8/bin/activate"
echo "Virtual environment reactivated for continued use."
return 0

