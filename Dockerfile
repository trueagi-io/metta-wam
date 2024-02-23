# Starting image
FROM ubuntu:22.04

# Install prerequisites
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update
RUN apt install -y python3 python3-pip libpython3-dev git
RUN apt install -y sudo git curl gcc cmake

# Create user
ENV USER=user
RUN useradd -m -G sudo -p "" user
RUN chsh -s /bin/bash user
# SWI packages no longer need the user's user
#USER ${USER}
ENV HOME=/home/${USER}
WORKDIR ${HOME}

# Install hyperonpy

# MeTTaLog is already taking enough time we will have a separate one for Rustr MeTTa
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
RUN sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
ENV PATH="${PATH}:/home/user/.cargo/bin"
RUN cargo install cbindgen

RUN python3 -m pip install conan==1.60.2 pip==23.1.2
ENV PATH="${PATH}:/home/user/.local/bin"
RUN conan profile new --detect default

RUN git clone https://github.com/trueagi-io/hyperon-experimental.git
WORKDIR ${HOME}/hyperon-experimental
RUN mkdir build

WORKDIR ${HOME}/hyperon-experimental/lib
RUN cargo build
RUN cargo test

WORKDIR ${HOME}/hyperon-experimental/build
RUN cmake ..
RUN make
RUN make check

WORKDIR ${HOME}/hyperon-experimental
RUN python3 -m pip install -e ./python[dev]

# Install MeTTaLog

ENV METTALOG_DIR="${HOME}/metta-wam"
ENV PATH="${PATH}:${METTALOG_DIR}"

WORKDIR ${HOME}
# RUN git clone https://github.com/trueagi-io/metta-wam.git

RUN mkdir -p ${METTALOG_DIR}"
WORKDIR ${METTALOG_DIR}
# This COPY is in case we have made local changes 
#         so we dont have to commit to Github to test them out
COPY ./ ./
COPY ./INSTALL.sh ./INSTALL.sh
RUN ./INSTALL.sh --easy


#RUN swipl -l src/main/metta_interp.pl -g qcompile_mettalog


