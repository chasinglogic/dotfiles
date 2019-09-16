#!/usr/bin/env python3
import os
import sys
from github import Github
from gitlab import Gitlab

if len(sys.argv) != 3:
    print('Usage: gitlab_migration.py github_user/repo gitlab_user/repo')
    print('Make sure to set $GITHUB_TOKEN and $GITLAB_TOKEN')
    sys.exit(1)

    gh = Github(os.getenv('GITHUB_TOKEN'))
    gh_repo = gh.get_repo(sys.argv[0])
    gh_issues = [issue for issue in gh_repo.get_issues()]

    gl = Gitlab('https://gitlab.com', private_token=os.getenv('GITLAB_TOKEN'))
    taskforge = gl.projects.get(sys.argv[1])

    for issue in gh_issues:
        print('Creating ticket:')
        print('\tSummary:', issue.title)
        print('\tDescription:', issue.body)

        gl_issue = taskforge.issues.create({
            'title': issue.title,
            'description': issue.body
        })
        gl_issue.labels = [label.name for label in issue.get_labels()]

        for comment in issue.get_comments():
            print('\t\tComment Body:', comment.body)
            gl_issue.notes.create({
                'body':
                'Original Commenter: {}\n\n{}'.format(comment.user.name,
                                                      comment.body)
            })

            gl_issue.save()
