#!/bin/bash

REPOSITORIES=$(projector list)

for repository in $REPOSITORIES; do
    cd $repository

    fork=$(git remote -v | grep fork)
    if [[ $fork != "" && $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
        echo ""
        echo "$repository is dirty prompting for commit message."
        echo ""
        PAGER="" git diff
        read -p "Commit Message: " commit_message
        if [[ $commit_message != "WIP:"* ]]; then
            commit_message="WIP: $commit_message"
        fi
        git add --all
        git commit -m "$commit_message"
    fi

    if [[ $repository == *"dotfiles" ]]; then
        echo "Pushing $repository."
        git push origin master
    elif [[ $fork == "" || $fork == "\n" ]]; then
        echo "No fork repository for $repository. Skipping."
    else
        echo "Pushing $repository."
        git push --force-with-lease --all fork
    fi

done
