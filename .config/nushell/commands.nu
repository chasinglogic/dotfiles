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
