#!/bin/bash


(
pip install --upgrade pip
. ./my-python3-env/bin/activate
pip install --upgrade pip
pip install -r requirements.txt

#pip uninstall jupyter
#pip uninstall jupyter_contrib_nbextensions
#pip uninstall jupyter_nbextensions_configurator
#pip uninstall jupyterthemes
#pip uninstall jupyterlab
#pip uninstall jupyterlab-server


pip install jupyterlab-server
pip install jupyter
pip install jupyter_contrib_nbextensions
pip install jupyter_nbextensions_configurator
pip install jupyterthemes
pip install jupyterlab
pip install jupyter-lsp
pip install jupyter_lsp
pip install jupyterlab-lsp

pip install -U "jupyter-server<2.0.0"


pip install -e reqs/picat_kernel/
jupyter kernelspec install reqs/picat_kernel
pip install -force -e reqs/metta_kernel/
jupyter kernelspec install reqs/metta_kernel
jupyter kernelspec install src
#--user 65534:65534 \
# -d --restart always 



# Function to generate a random 4-digit number
generate_random_number() {
  echo $(( RANDOM % 9000 + 1000 ))
}

# Function to execute on Ctrl+C
handle_sigint() {
  echo "SIGINT caught, cleaning up..."
  docker kill mlhub
  docker rm mlhub
  docker kill ml-hub
  docker rm ml-hub
  docker kill ml-workspace
  docker rm ml-workspace
  exit
}

# Trap SIGINT (Ctrl+C) and call handle_sigint()
trap 'handle_sigint' SIGINT

# Generate a random 4-digit number
AUTH_CODE=$(generate_random_number)

# Run Docker container with the generated AUTH_CODE
echo docker run \
  -d --restart always \
  -p 17888:8080 \
  --name "ml-workspace" \
  -v "${PWD}:/workspace" \
  --env AUTHENTICATE_VIA_JUPYTER="$AUTH_CODE" \
  --shm-size 2512m \
  mltooling/ml-workspace

mkdir -p jupyterhub_data

echo docker run \
  -d --restart always \
  -p 17880:8080 \
  --name "ml-hub" \
  -v "${PWD}:/workspace" \
  --env AUTHENTICATE_VIA_JUPYTER="$AUTH_CODE" \
  --shm-size 2512m \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v jupyterhub_data:/data \
  mltooling/ml-hub:latest

echo ""
echo ""
echo "Authentication code for Jupyter: $AUTH_CODE"
echo ""
echo ""
echo docker run \
  -v "${PWD}:/workspace" \
  --name "ml-hub" \
  -p 17880:8080 \
  --env AUTHENTICATE_VIA_JUPYTER="$AUTH_CODE" \
  --env JUPYTERHUB_API_TOKEN="$AUTH_CODE" \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v jupyterhub_data:/data \
  mltooling/ml-hub:latest

echo "Authentication code for Jupyter: $AUTH_CODE"

echo sleep 10

# Follow Docker logs
# echo ( docker logs -f ml-workspace ) || true

# Clean up
# handle_sigint
export PYTHONPATH=$PYTHONPATH:/opt/logicmoo_opencog/hyperon-wam/src

export PATH=/opt/logicmoo_opencog/hyperon-wam:/opt/logicmoo_opencog/hyperon-wam/reqs/Picat:$PATH

chown logicmoo my-python3-env/ -R
echo ""
echo ""
echo "Authentication code for Jupyter: $AUTH_CODE"
echo ""
echo ""
sudo -u logicmoo jupyter notebook --allow-root --no-browser --port=17888 --ip 0.0.0.0 --password $AUTH_CODE
)



