# PLN for Hyperon

## Description

Port of Probabilistic Logic Networs (PLN) for Hyperon/MeTTa.

## Prerequisites

- [hyperon-experimental](https://github.com/trueagi-io/hyperon-experimental)

## Usage

The port is approached from different angles which are

- Proofs as match queries
- Proofs as custom Atom structure
- Proofs as programs, and properties as dependent types

### PLN via Dependent Types

The most advanced approach for now is via dependent types and can be
found under

```
metta/dependent-types
```

The following examples can be run

```
metta metta/dependent-types/DeductionDTLTest.metta
metta metta/dependent-types/ImplicationDirectIntroductionDTLTest.metta
metta metta/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta
```

### Synthesizer

The dependent type approach relies on a generic program synthesizer
that can be found under

```
metta/synthesis
```

More information can be found in the `README.md` file under that
directory.

## Docker

A docker image containing a pre-installed version of Hyperon and PLN
is hosted on Docker Hub and can be run as follows:
```
docker run --rm -ti trueagi/pln
```

Additionally, a Dockerfile to build and update that image is present
under the root folder of that repository.  To build the image from
that local file, one may invoke the following command:
```
docker build -t trueagi/pln .
```

Or, using the URL of that Dockerfile:
```
docker build -t trueagi/pln https://raw.githubusercontent.com/trueagi-io/hyperon-pln/main/Dockerfile
```

## Idris

There is also some Idris code under the `idris` folder to prototype
some aspect of the dependent types port.  This is sometimes easier
because Idris is more mature than MeTTa.  The minimum requirement is
Idris2 version 0.5.1.

## References

Below is a list of references to know more about PLN and its port to
Hyperon/MeTTa:

- [PLN Book](https://link.springer.com/book/10.1007/978-0-387-76872-4)
- [PLN for Procedural and Temporal Reasoning](https://www.researchgate.net/publication/370994045_Probabilistic_Logic_Networks_for_Temporal_and_Procedural_Reasoning)
- [Presentation of the synthesizer at AGI-23](https://odysee.com/@ngeiswei:d/AGI-23---Program-Synthesis-and-Chaining-with-MeTTa:3)
