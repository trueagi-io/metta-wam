#!/bin/bash


#pip uninstall jupyter
#pip uninstall jupyter_contrib_nbextensions
#pip uninstall jupyter_nbextensions_configurator
#pip uninstall jupyterthemes
#pip uninstall jupyterlab
#pip uninstall jupyterlab-server


#pip install jupyterlab-server
#pip install jupyter
#pip install jupyter_contrib_nbextensions
#pip install jupyter_nbextensions_configurator
#pip install jupyterthemes
#pip install jupyterlab
#pip install jupyter-lsp
#pip install jupyter_lsp
#pip install jupyterlab-lsp

#pip install -U "jupyter-server<2.0.0"

#pip install -force -e reqs/metta_kernel/
#pip install -e reqs/picat_kernel/

#jupyter kernelspec install reqs/picat_kernel
#jupyter kernelspec install metta_vspace
#jupyter kernelspec install reqs/metta_kernel

(
export PYTHONPATH=/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta:.:/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/metta_vspace

export PATH=/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta:/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/reqs/Picat:$PATH

jupyter notebook --allow-root --no-browser --port=17888 --ip 0.0.0.0 --password das
)
