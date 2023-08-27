from setuptools import setup, find_packages


# standalone import of a module (https://stackoverflow.com/a/58423785)
def import_module_from_path(path):
    """Import a module from the given path without executing any code above it
    """
    import importlib
    import pathlib
    import sys

    module_path = pathlib.Path(path).resolve()
    module_name = module_path.stem  # 'path/x.py' -> 'x'
    spec = importlib.util.spec_from_file_location(module_name, module_path)
    module = importlib.util.module_from_spec(spec)

    if module not in sys.modules:
        sys.modules[module_name] = module
        spec.loader.exec_module(module)
    else:
        module = sys.modules
    return module


with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

version = import_module_from_path('metta-vspace/_version.py').__version__

setup(
    name="metta-vspace",
    version=version,
    author="Logicmoo Co",
    author_email="logicmoo@gmail.com",
    description="VSpace wrapper around the MettaLearner interpreter to make it useable from within Python and jupyter notebooks.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/logicmoo/metta-vspace",
    project_urls={
        "Bug Tracker": "https://github.com/logicmoo/metta-vspace/issues",
    },
    #packages=find_packages(exclude=['ftp.vbase.org']),
    packages=["metta-vspace"],
    install_requires=[
        'pyswip'
    ],
    package_dir={"metta-vspace": "metta-vspace"},
    include_package_data=True,
    license="LGPL",
    classifiers=[
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Prolog",
        "License :: OSI Approved :: MIT License",
        "Operating System :: POSIX :: Linux",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: MacOS",
        "Intended Audience :: Developers",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Software Development",
    ],
    python_requires=">=3.6",
    zip_safe=True
)
