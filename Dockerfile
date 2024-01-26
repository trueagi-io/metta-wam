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

# Create user
ENV USER=user
RUN useradd -m -G sudo -p "" user
RUN chsh -s /bin/bash user
USER ${USER}
ENV HOME=/home/${USER}
WORKDIR ${HOME}

# Install SWI-Prolog packages
RUN swipl -g "pack_install(predicate_streams,[interactive(false)])" -t halt
RUN swipl -g "pack_install('https://github.com/TeamSPoon/logicmoo_utils.git',[insecure(true),interactive(false),git(true),verify(false)])" -t halt
RUN swipl -g "pack_install(dictoo,[interactive(false)])" -t halt

# Install MeTTaLog
WORKDIR ${HOME}
RUN git clone https://github.com/logicmoo/vspace-metta.git

# Update PATH
RUN echo >> ${HOME}/.bashrc
RUN echo "# For MeTTaLog" >> ${HOME}/.bashrc
RUN echo "export PATH=${PATH}:${HOME}/vspace-metta" >> ${HOME}/.bashrc
