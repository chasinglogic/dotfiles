#!/bin/bash

echo =========== Create an ubuntu pod ==================
kubectl run ubuntu --image=ubuntu -- bash -c "while true; do echo hello; sleep 10;done"

# Wait for the pod "ubuntu" to contain the status condition of type "Ready"
kubectl wait --for=condition=Ready pod/ubuntu

# Save a sorted list of IPs of all of the k8s SVCs:
kubectl get svc -A|egrep -v 'CLUSTER-IP|None'|awk '{print $4}'|sort -V > ips

# Copy the ip list to owr Ubuntu pod:
kubectl cp ips ubuntu:/

echo =========== Installing dig tool into the pod ===============
kubectl exec -it ubuntu -- apt-get update
kubectl exec -it ubuntu -- apt install -y dnsutils

# Print 7 blank lines
yes '' | sed 7q
echo =========== Print all k8s SVC DNS records ====================
for ip in $(cat ips); do echo -n "$ip "; kubectl exec -it ubuntu -- dig -x $ip +short; done
echo ====== End of list =====================

echo ========= Cleanup  ===============
kubectl delete po ubuntu
rm ips
exit 0
