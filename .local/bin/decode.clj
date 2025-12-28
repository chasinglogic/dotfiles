#!/usr/bin/env python3
# vi: ft=python

import argparse
import base64
import sys
from typing import Literal


Encoding = Literal["base64"]


def get_decoder(encoding: Encoding):
    if encoding == "base64":
        return base64.b64decode
    raise SystemExit(f"{encoding} is not a valid encoding")


def decode(encoding: Encoding, data: str) -> str:
    decoder = get_decoder(encoding)
    result_bytes = decoder(data.encode("utf-8"))
    return result_bytes.decode("utf-8")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="decode",
        description="Decode strings using the specified encoding (default: base64)",
    )
    parser.add_argument(
        "-e",
        "--encoding",
        dest="encoding",
        default="base64",
        metavar="<encoding>",
        help="The encoding of the provided strings (default: base64)",
    )
    parser.add_argument(
        "encoded_strings",
        nargs="+",
        help="One or more encoded strings to decode",
    )

    if not argv:
        parser.print_help(sys.stderr)
        return 1

    args = parser.parse_args(argv)

    enc: Encoding = args.encoding  # type: ignore[assignment]

    for s in args.encoded_strings:
        print(decode(enc, s))

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
