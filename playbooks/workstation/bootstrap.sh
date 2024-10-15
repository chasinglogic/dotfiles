#!/bin/bash

if [[ ! -x $(which ansible-playbook 2>/dev/null) ]]; then
    echo "Installing prereqs..."
    if [[ -f $(which brew) ]]; then
        brew install ansible
    elif [[ -f $(which apt) ]]; then
        sudo apt install -y ansible build-essential python3
    else
        sudo dnf install -y ansible
    fi
fi

echo "Installing roles"
ansible-galaxy install -r requirements.yml

echo "Running playbook"
ansible-playbook -i inventory playbook.yml
