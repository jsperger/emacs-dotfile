#!/usr/bin/env zsh

# Configuration: Directory and name of the virtual environment
ENV_TARGET_DIR="/Users/jsperger/.emacs.d/var/"
ENV_NAME="lsp-bridge-env"
# Full path to the virtual environment
ENV_PATH="$ENV_TARGET_DIR$ENV_NAME"

# Check if the virtual environment exists
if [[ ! -d $ENV_PATH ]]; then
  echo "Error: Virtual environment does not exist at $ENV_PATH"
  exit 1
fi

# Activate the virtual environment
source "$ENV_PATH/bin/activate"

