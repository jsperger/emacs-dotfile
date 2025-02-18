#!/usr/bin/env zsh

# Configuration: Directory where you want the virtual environment to reside
ENV_TARGET_DIR="/Users/jsperger/.emacs.d/var/"
# Configuration: Name of the virtual environment directory you want to create
ENV_NAME="lsp-bridge-env"
# Full path to the virtual environment
ENV_PATH="$ENV_TARGET_DIR$ENV_NAME"

# Ensure target directory exists
if [[ ! -d $ENV_TARGET_DIR ]]; then
  echo "Creating target directory: $ENV_TARGET_DIR"
  mkdir -p "$ENV_TARGET_DIR"
fi

# Check if the virtual environment already exists
if [[ ! -d $ENV_PATH ]]; then
  echo "Creating new virtual environment: $ENV_PATH"
  python3 -m venv "$ENV_PATH"
else
  echo "Virtual environment already exists: $ENV_PATH"
fi

# Activate the virtual environment and install packages
echo "Activating virtual environment: $ENV_PATH"
source "$ENV_PATH/bin/activate"

# Install the required Python packages
echo "Installing packages..."
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging

# Deactivate the environment after installation (optional, but good practice)
deactivate

echo "Setup complete. To use the environment, run 'source $ENV_PATH/bin/activate' in your shell."
