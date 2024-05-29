#!/usr/bin/env bash
# Yes I know it's wherefore means why.

PREFIX="$1"
TARGET="$2"

echo "Searching for $1"

for secret in $(aws --profile production secretsmanager list-secrets --filter "Key=name,Values=$PREFIX" | jq -r .SecretList[].ARN); do
    echo "Checking $secret"
    FOUND=$(aws secretsmanager get-secret-value --secret-id "$secret" | rg "$TARGET")
    if [[ -n "$FOUND" ]]; then
        echo "Found $TARGET in secret $secret."
        exit 0
    fi
done

echo "Failed to find $TARGET."
exit 10
