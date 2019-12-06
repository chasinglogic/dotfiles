#!/usr/bin/env python3
# Grab all files from my home directory which exist in my dotfiles repository

import os
import shutil
import argparse


def generate_from_path(filename, root_dir, home_dir):
    """Generate the rel and from_paths for filename using root_dir and home_dir."""
    rel_path = filename[len(root_dir) + 1:]
    from_path = os.path.join(home_dir, rel_path)
    return from_path


def import_file(to_path, from_path):
    """Import file from_path to to_path."""
    print("importing", from_path, "to", to_path)
    try:
        shutil.copyfile(from_path, to_path)
    except FileNotFoundError:
        print("Couldn't find: {} skipping.".format(from_path))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--add', dest='files_to_add', action='append')
    parser.add_argument('--dotfiles-repository', dest='repository', default=os.getenv("DOTFILES_REPOSITORY"))
    parser.add_argument('--from-dir', dest='from_dir', default=os.getenv("HOME"))

    args = parser.parse_args()

    root_dir = args.repository
    home_dir = args.from_dir

    if args.files_to_add:
        for f in args.files_to_add:
            from_path = generate_from_path(f, root_dir, home_dir)
            import_file(f, from_path)

    walker = os.walk(root_dir)
    for root, directories, files in walker:
        # Don't try to import the .git directory
        if ".git" in directories:
            directories.remove(".git")

        for f in files:
            to_path = os.path.join(root, f)
            from_path = generate_from_path(to_path, root_dir, home_dir)
            import_file(to_path, from_path)


if __name__ == "__main__":
    main()
