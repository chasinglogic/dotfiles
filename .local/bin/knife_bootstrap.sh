#!/bin/bash
hostname=""
runlist=""
sshuser=""
group=""

while getopts "u:b:r:d:g:h" flag; do
    case "${flag}" in
        b) hostname="${OPTARG}" ;;
        u) sshuser="${OPTARG}" ;;
        r) runlist="${OPTARG}" ;;
        g) group="${OPTARG}" ;;
        h) echo 'Usage:
knife_bootstrap.sh -g $GROUP -b $BUILDHOST -d $DOMAIN -r $RUNLIST -u $SSHUSER'; exit 0 ;;
    esac
done

if [[ ${hostname} == "" ]]; then
    echo "Must provide hostname with -b flag."
    exit 1
fi

if [[ ${runlist} == "" ]]; then
    rolename="$(echo ${hostname} | awk -F'-' '{ print $1 }')"
    runlist="role[base-build-${rolename}]"
fi

domain="$(echo ${hostname} | sed 's/^[A-z0-9-]*\.//')"
echo "Bootstrapping ${sshuser}@${hostname} with ${runlist} in domain ${domain}"
knife bootstrap -E build -j '{ "'${group}'": { "hostname": "'${hostname}'", "domain": "'${domain}'"}}' -r "${runlist}" -N ${hostname} --sudo --ssh-user ${sshuser} ${hostname} --bootstrap-version 12.14.60
