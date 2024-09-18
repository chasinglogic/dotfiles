def is_system [name: string] {
    (uname | get operating-system) == $name
}

def is_dark_mode_linux [] {
    let scheme = (
        gdbus call --session --timeout=1000
            --dest=org.freedesktop.portal.Desktop
            --object-path /org/freedesktop/portal/desktop
            --method org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme
    )

    match $scheme {
        "(<<uint32 1>>,)" => true
        _ => false
    }
}

def is_dark_mode_macos [] {
    let preference = (defaults read -g AppleInterfaceStyle)
    $preference == "Dark"
}

def is_dark_mode [] {
    if (is_system "Darwin") {
        is_dark_mode_macos
    } else {
        is_dark_mode_linux
    }
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
