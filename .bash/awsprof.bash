function awsprof() {
    if [ $1 == "-h" ] || [ $1 == "--help" ]; then
        echo "AWS Profile Switcher:"
        echo "This tool is used for quickly switching between AWS profiles."
        echo "Usage:"
        echo "    awsprof [profilename]"
        exit 0
    fi

    export AWS_DEFAULT_PROFILE="$1"
    export AWS_EB_PROFILE="$1"
    export AWS_ACCESS_KEY_ID="$(aws configure get $1.aws_access_key_id)"
    export AWS_SECRET_ACCESS_KEY="$(aws configure get $1.secret_access_key)"
}
