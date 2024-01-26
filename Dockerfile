# Starting image
FROM ubuntu:22.04

# Install prerequisites
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update
RUN apt install -y python3 python3-pip libpython3-dev git

# Install SWI-Prolog unstable
RUN apt install -y software-properties-common
RUN apt-add-repository -y ppa:swi-prolog/devel
RUN apt update
RUN apt install -y swi-prolog 

# Install Janus for SWI-Prolog
RUN pip install git+https://github.com/SWI-Prolog/packages-swipy.git

# Install PySWIP for SWI-Prolog
RUN pip install git+https://github.com/logicmoo/pyswip.git

RUN apt install -y sudo git curl gcc cmake

# Create user
ENV USER=user
RUN useradd -m -G sudo -p "" user
RUN chsh -s /bin/bash user
USER ${USER}
ENV HOME=/home/${USER}
WORKDIR ${HOME}


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

# Install SWI-Prolog packages
RUN swipl -g "pack_install(predicate_streams,[interactive(false)])" -t halt
RUN swipl -g "pack_install(logicmoo_utils,[interactive(false)])" -t halt
# RUN swipl -g "pack_install('https://github.com/TeamSPoon/logicmoo_utils.git',[insecure(true),interactive(false),git(true),verify(false)])" -t halt
RUN swipl -g "pack_install(dictoo,[interactive(false)])" -t halt

# Install MeTTaLog
WORKDIR ${HOME}
RUN git clone https://github.com/logicmoo/vspace-metta.git

WORKDIR ${HOME}/vspace-metta

# Update PATH
RUN echo >> ${HOME}/.bashrc
RUN echo "# For MeTTaLog" >> ${HOME}/.bashrc
RUN echo "export PATH=${PATH}:${HOME}/vspace-metta:/home/user/.local/bin:/home/user/.conan/bin" >> ${HOME}/.bashrc


COPY ./mettalog /home/user/vspace-metta/mettalog
COPY ./metta_vspace/pyswip /home/user/vspace-metta/metta_vspace/pyswip
COPY ./MeTTa /home/user/vspace-metta/MeTTa
RUN ls -lh /home/user/vspace-metta/mettalog
RUN sudo chmod +x /home/user/vspace-metta/mettalog

#RUN swipl -l metta_vspace/pyswip/metta_interp.pl -g qcompile_mettalog




