#!/usr/bin/env python3
# Grab all files from my home directory which exist in my dotfiles repository

import os
import shutil

THIS_FILE_NAME = os.path.basename(__file__)


def import_from(walker, from_dir, walk_root="files"):
    for root, _, files in walker:
        for f in files:
            # Skip self
            if f == THIS_FILE_NAME:
                continue
            to_path = os.path.join(root, f)
            rel_path = to_path[len(walk_root) + 1 :]
            from_path = os.path.join(from_dir, rel_path)
            print("importing", from_path, "to", to_path)
            if not os.stat(from_path):
                print("Couldn't find: {} skipping.".format(from_path))
                continue
            shutil.copyfile(from_path, to_path)


def main():
    root_dir = os.getenv("DOTFILES_REPOSITORY")
    home_dir = os.getenv("HOME")
    directories = os.listdir(root_dir)
    for directory in directories:
        # Skip the git directory
        if directory == ".git":
            continue
        walk_root = os.path.join(root_dir, directory)
        walker = os.walk(walk_root)
        from_dir = os.path.join(home_dir, directory)
        import_from(walker, from_dir, walk_root=walk_root)


if __name__ == "__main__":
    main()
