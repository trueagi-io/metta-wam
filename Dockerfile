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

# MeTTaLog is already taking enough time we will have a separate one for Rustr MeTTa
#RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
#RUN sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
#ENV PATH="${PATH}:/home/user/.cargo/bin"
#RUN cargo install cbindgen

#RUN python3 -m pip install conan==1.60.2 pip==23.1.2
#ENV PATH="${PATH}:/home/user/.local/bin"
#RUN conan profile new --detect default

#RUN git clone https://github.com/trueagi-io/hyperon-experimental.git
#WORKDIR ${HOME}/hyperon-experimental
#RUN mkdir build

#WORKDIR ${HOME}/hyperon-experimental/lib
#RUN cargo build
#RUN cargo test

#WORKDIR ${HOME}/hyperon-experimental/build
#RUN cmake ..
#RUN make
#RUN make check

#WORKDIR ${HOME}/hyperon-experimental
#RUN python3 -m pip install -e ./python[dev]


# Install MeTTaLog
WORKDIR ${HOME}
RUN git clone https://github.com/logicmoo/vspace-metta.git

COPY ./ /home/user/vspace-metta/
WORKDIR ${HOME}/vspace-metta

RUN ./INSTALL.sh --easy

#COPY ./metta_vspace/pyswip /home/user/vspace-metta/
RUN ls -lh /home/user/vspace-metta/mettalog
RUN sudo chmod +x /home/user/vspace-metta/mettalog

#RUN swipl -l metta_vspace/pyswip/metta_interp.pl -g qcompile_mettalog




