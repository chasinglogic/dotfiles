export def is_system [name: string] {
    (uname | get operating-system) == $name
}

def kubectl_contexts [] {
    kubectl config get-contexts -o name | split words
}

# Quickly switch or list kubectl contexts
def kctx [
    context?: string@kubectl_contexts # The context to switch to, if not provided lists available contexts
] {
    if ($context == null) {
        kubectl config get-contexts
    } else {
        kubectl config use-context $context
    }
}

def kube_namespaces [] {
    kubectl get ns -o json | from yaml | get items | each {|ns| $ns.metadata.name}
}

def images_in_namespace [namespace: string@kube_namespaces] {
    kubectl get pods -n $namespace -o yaml | from yaml | get items | each {|pod| $pod.spec.containers.0.image} | sort | uniq
}

def --env dotfiles [] {
    cd (dfm where)
}
