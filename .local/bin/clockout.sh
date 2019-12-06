#!/bin/bash

REPOSITORIES=$(projector list)

for repository in $REPOSITORIES; do
    cd $repository
    if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
        echo "$repository is dirty prompting for commit message."
        PAGER="" git diff
        read -p "Commit Message: " commit_message
        if [[ $commit_message != "WIP:"* ]]; then
            commit_message="WIP: $commit_message"
        fi
        git add --all
        git commit -m "$commit_message"
    fi

    if [[ $repository == *"Work"* ]]; then
        fork=$(git remote -v | grep fork)
        if [[ $fork == "" ]]; then
            echo "No fork repository for $repository."
            exit 1
        fi

        git push --force-with-lease --all fork
    else
        git push --all origin
    fi
       
done
