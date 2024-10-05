import os

import requests

TOKEN = os.getenv("GITLAB_TOKEN")
BASE_URL = "https://gitlab.com/api/v4/"
HEADERS = {"Private-Token": TOKEN}

save_projects = ["chasinglogic.gitlab.io", "praelatus"]

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
