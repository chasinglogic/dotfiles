#!/usr/bin/env python3
import requests
import os

TOKEN = os.getenv("GITLAB_TOKEN")
BASE_URL = "https://gitlab.com/api/v4/"
HEADERS = {"Private-Token": TOKEN}

save_projects = os.getenv("GITLAB_SAVE_PROJECTS").split(",")

projects = requests.get(
    "https://gitlab.com/api/v4/users/chasinglogic/projects?visibility=public",
    headers=HEADERS,
).json()

projects_to_delete = [
    project for project in projects if project["name"] not in save_projects
]

print([project["name"] for project in projects_to_delete])

for project in projects_to_delete:
    print("Deleting:", project["name"])
    r = requests.delete(BASE_URL + "projects/" + str(project["id"]), headers=HEADERS)
    print(r.status_code)
    print(r.text)
