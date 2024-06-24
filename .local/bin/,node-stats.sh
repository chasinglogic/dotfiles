#!/usr/bin/env bash

FILETYPE="${1:-tabulated}"


WIDTH=$(kubectl get nodes | awk '{ print $1 }' | rg -v NAME | awk '{ print length }' | sort -nr | head -n1)
FIRST_SEPARATOR=$(printf '%*s' "$WIDTH")
SEPARATOR="  "
if [[ "$FILETYPE" == "csv" ]]; then
    SEPARATOR=","
fi

printf "NODE%sMAX PODS%sRUNNING PODS\n" "$FIRST_SEPARATOR" "$SEPARATOR"

for node in $(kubectl get nodes | awk '{ print $1 }' | rg -v NAME); do 
    running_pod_count=$(kubectl get pods --all-namespaces --field-selector spec.nodeName="$node" | grep -c -v NAME)
    node_capacity=$(kubectl get node "$node" -ojsonpath='{.status.capacity.pods}')
    
    buffer_space_count=$(($WIDTH - $(echo "$node" | awk '{ print length }') + 4))
    buffer_spaces=$(printf '%*s' "$buffer_space_count")
    pod_stats_padding=$(printf '%*s' 8)

    printf "%s%s%s%s%s\n" "$node" "$buffer_spaces" "$node_capacity" "$pod_stats_padding" "$running_pod_count"
done
