# Developer Guide

This page is intended for developers who are contributing to or extending the MeTTa project. It provides an overview of the project structure, key directories, and files to help you navigate and understand the codebase.

## Project Structure Overview

Here’s a summary of the main directories and files in the MeTTa project:

### Key Directories

- **[`docs/`](https://github.com/trueagi-io/metta-wam/tree/main/docs)**: Contains documentation, including guides, specifications, and architecture overviews.
- **[`library/`](https://github.com/trueagi-io/metta-wam/tree/main/library)**: Likely contains additional libraries or shared resources used throughout the project.
- **[`notebooks/`](https://github.com/trueagi-io/metta-wam/tree/main/notebooks)**: Jupyter notebooks or similar files for experimenting with code, running tutorials, or exploring data.
- **[`reports/`](https://github.com/trueagi-io/metta-wam/tree/main/reports)**: Directory for reports or analysis related to the project.
- **[`scripts/`](https://github.com/trueagi-io/metta-wam/tree/main/scripts)**: Collection of utility scripts for various tasks like automation, deployment, or maintenance.
- **[`src/canary`](https://github.com/trueagi-io/metta-wam/tree/main/src/canary)**: Main source code for the MeTTa project, including Prolog scripts, logic definitions, and core components.
- **[`src/mettalog/`](https://github.com/trueagi-io/metta-wam/tree/main/src/mettalog)**: Python Support files and configurations specific to the MeTTa language or logic system.
- **[`swipl-devel/`](https://github.com/trueagi-io/metta-wam/tree/main/swipl-devel)**: Specific development files related to SWI-Prolog, possibly for advanced or custom configurations.
- **[`tests/`](https://github.com/trueagi-io/metta-wam/tree/main/tests)**: Test cases and frameworks used to validate the code, including unit tests, integration tests, and regression tests.

### Root Directory

- **[`INSTALL.sh`](https://github.com/trueagi-io/metta-wam/blob/main/INSTALL.sh)**: A script to assist with installing dependencies and setting up the development environment.
- **[`README.md`](https://github.com/trueagi-io/metta-wam/blob/main/README.md)**: The main README file providing an overview of the project, setup instructions, and basic usage.
- **[`requirements.txt`](https://github.com/trueagi-io/metta-wam/blob/main/requirements.txt)**: Lists Python dependencies required by the project.
- **[`setup.py`](https://github.com/trueagi-io/metta-wam/blob/main/setup.py)** / **[`setup.soon`](https://github.com/trueagi-io/metta-wam/blob/main/setup.soon)**: Python setup script for installing the project as a package /  Placeholder for setup configuration (might indicate an upcoming setup change).
- **[`mettalog`](https://github.com/trueagi-io/metta-wam/blob/main/mettalog)** / **[`mettalog.cmd`](https://github.com/trueagi-io/metta-wam/blob/main/mettalog.cmd)**: Command or script related to running MeTTa on Linux/Windows
- **[`compiler-project.vpj`](https://github.com/trueagi-io/metta-wam/blob/main/compiler-project.vpj)**: / **[`hyperon-wam.vpj`](https://github.com/trueagi-io/metta-wam/blob/main/hyperon-wam.vpj)** / **[`hyperon-wam.vpw`](https://github.com/trueagi-io/metta-wam/blob/main/hyperon-wam.vpw)** / **[`hyperon-wam.vpwhist`](https://github.com/trueagi-io/metta-wam/blob/main/hyperon-wam.vpwhist)** / **[`hyperon-wam.vpwwildcardcache`](https://github.com/trueagi-io/metta-wam/blob/main/hyperon-wam.vpwwildcardcache)** / **[`hyperon-wam.vtg`](https://github.com/trueagi-io/metta-wam/blob/main/hyperon-wam.vtg)**: These files are related to the project’s build system, possibly for an IDE or build tool that uses these specific configurations.
- **[`Dockerfile`](https://github.com/trueagi-io/metta-wam/blob/main/Dockerfile)**: Used for containerizing the project, enabling easy setup and deployment within a Docker environment.

---

## Setting Up the Development Environment

To set up the project for development, follow these steps:

1. **Clone the Repository**:
    ```bash
    git clone https://github.com/trueagi-io/metta-wam.git
    cd metta-wam
    ```

2. **Run the Installation Script**:
    Execute the `INSTALL.sh` script to install dependencies:
    ```bash
    ./INSTALL.sh
    ```

3. **Set Up Docker (Optional)**:
    If you prefer using Docker, build and run the container:
    ```bash
    docker build -t metta-wam .
    docker run -it metta-wam
    ```

4. **Activate the Python Environment**:
    If using Python, install dependencies:
    ```bash
    pip install -r requirements.txt
    ```

5. **Run the REPL or Execute Scripts**:
    You can now explore the project’s functionality by running the REPL or other scripts in the [`src/`](https://github.com/trueagi-io/metta-wam/tree/main/src) directory.

---

## Key Development Areas

- **Core Logic and Reasoning**: Explore the core implementation in the [`src/`](https://github.com/trueagi-io/metta-wam/tree/main/src) directory, including logic rules, ontologies, and evaluation mechanisms.
- **Extending the Language**: Use the [`library/`](https://github.com/trueagi-io/metta-wam/tree/main/library) and [`mettalog/`](https://github.com/trueagi-io/metta-wam/tree/main/mettalog) directories to extend the language with new libraries or logic.
- **Testing and Validation**: Use the [`tests/`](https://github.com/trueagi-io/metta-wam/tree/main/tests) directory to add and run test cases to ensure your changes are stable.
