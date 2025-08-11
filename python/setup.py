import os
import sys
import shutil
import subprocess
from pathlib import Path

from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext

def resolve_path(path: str) -> str:
    return os.path.abspath(path)

def get_version(rel_path):
    try:
        with open(rel_path) as f:  ver = f.read().splitlines()[0].split("'")[1]
        return ver
    except Exception:
        print(f"Error reading file {rel_path}", file=sys.stderr)


def version_scheme(*args):
    return get_version("./VERSION")

setup(
    url="https://github.com/trueagi-io/metta-wam",
    use_scm_version={'root': '..',
                     'version_scheme': version_scheme,
                     'write_to': 'python/mettalog/_version.py'},
 )
