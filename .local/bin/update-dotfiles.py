#!/usr/bin/env python3
# Grab all files from my home directory which exist in my dotfiles repository

import os
import shutil


def main():
    root_dir = os.getenv("DOTFILES_REPOSITORY")
    home_dir = os.getenv("HOME")
    walker = os.walk(root_dir)
    for root, directories, files in walker:
        # Don't try to import the .git directory
        if ".git" in directories:
            directories.remove(".git")

        for f in files:
            to_path = os.path.join(root, f)
            rel_path = to_path[len(root_dir) + 1:]
            from_path = os.path.join(home_dir, rel_path)
            print("importing", from_path, "to", to_path)
            try:
                shutil.copyfile(from_path, to_path)
            except FileNotFoundError:
                print("Couldn't find: {} skipping.".format(from_path))


if __name__ == "__main__":
    main()
