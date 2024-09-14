# Overview

OpenCog Hyperon is a substantially revised, novel version of OpenCog - which is currently at an active
pre-alpha stage of development and experimentation. One of the focuses in the Hyperon design is a successor
to the OpenCog Classic Atomese language with clear semantics supporting meta-language features,
different types of inference, etc. What we have landed on is an "Atomese 2" language called MeTTa (Meta Type Talk).

In order to get familiar with MeTTa one can visit [MeTTa website](https://metta-lang.dev)
and watch video with different [MeTTa examples explained](https://singularitynet.zoom.us/rec/share/VqHmU37XtbS7VnKY474tkTvvTglsgOIfsI-21MXWxVm_in7U3tGPcfjjiE0P_15R.yUwPdCzEONSUx1EL?startTime=1650636238000).
The examples of MeTTa programs can be found in [./python/tests/scripts](.,/python/tests/scripts) directory.

Please look at the [Python unit tests](./tests) to understand how one can use MeTTa from Python.
More complex usage scenarios are located at [MeTTa examples repo](https://github.com/trueagi-io/metta-examples).
A lot of different materials can be found on [OpenCog wiki server](https://wiki.opencog.org/w/Hyperon).
Also see [MeTTa specification](https://wiki.opencog.org/w/File:MeTTa_Specification.pdf).

If you want to contribute the project please see the [contributing guide](../docs/CONTRIBUTING.md) first.
If you find troubles with the installation, see the [Troubleshooting](#troubleshooting) section below.
For development related instructions see the [development guide](../docs/DEVELOPMENT.md).

One can run MeTTa script from command line:

```
mettalog <name>.metta

```

or
```
mettalog-py <name>.metta
```


Install these to be able to run things like metta-motto under MeTTaLog 
```
python3 -m pip install -e .[dev]
```

Running Python Test REPL
```
python3 sandbox/repl/metta_repl.py

```




![CI](https://github.com/trueagi-io/hyperon-experimental/actions/workflows/ci-auto.yml/badge.svg)



# Using the latest release version

It is the most simple way of getting MeTTa interpreter especially if you are a Python developer.
The following command installs the latest release version from PyPi package repository:
```
python3 -m pip install mettalog
```

After installing package run MeTTa Python based
interpreter/REPL:
```
mettalog-py
```


## Manual installation

### Prerequisites

* Install [MeTTaLog](../)

## Running Python and MeTTa examples

In order to run examples you need to install the Python module. Please ensure to execute the
following command in the top directory of repository:
```
python3 -m pip install -e ./python[dev]
```

After this one can run unit tests within `python` directory using `pytest`:
```
pytest ./tests
```

One can run MeTTa script from command line:
```
mettalog-py ./tests/scripts/<name>.metta
```

Source code of the Python integration library is located under
[./python](./) directory. It contains two main parts. First part is a


## Language support for IDEs

Different IDEs may require different tweaks to support the languages
used in the codebase. The language servers which we use
for development are:
- [MeTTa LSP server](https://github.com/trueagi-io/metta-wam/src/packs/).

