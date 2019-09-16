#!/usr/bin/env python3
import os
import pyyaml
import requests

evg_config = os.path.join(os.getenv('HOME'), '.evergreen.yml')
with open(evg_config) as ec:
    cfg = yaml.load(ec)

res = requests.get('{}/rest/v2/distros'.format(cfg['api_server_host']))
j = res.json()

names = [d['name'] for d in j]
new_names = []

for name in names:
    split = name.split('-')
    new_names.append(split[0]
                     if len(split) < 3 else '-'.join([split[0], split[1]]))

    new_names = list(set(new_names))

for name in new_names:
    print(name)
