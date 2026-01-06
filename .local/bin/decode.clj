#!/usr/bin/env -S uv run --script
# vi: ft=python

import argparse
import base64
import sys
from typing import List


SUPPORTED_ENCODINGS = {"base64"}


def decode_value(encoding: str, data: str) -> str:
    if encoding == "base64":
        try:
            decoded_bytes = base64.b64decode(data)
            return decoded_bytes.decode("utf-8")
        except Exception as exc:  # pragma: no cover - passthrough
            raise ValueError(f"Failed to decode base64 data: {exc}") from exc

    raise ValueError(f"{encoding} is not a supported encoding (only base64 is supported)")


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="decode",
        description="Decode one or more encoded strings.",
    )
    parser.add_argument(
        "-e",
        "--encoding",
        default="base64",
        metavar="ENCODING",
        help="Encoding of provided strings (default: base64)",
    )
    parser.add_argument(
        "encoded_strings",
        nargs="+",
        metavar="STRING",
        help="One or more encoded strings to decode.",
    )

    args = parser.parse_args(argv)

    encoding = args.encoding.lower()
    if encoding not in SUPPORTED_ENCODINGS:
        parser.error(
            f"{encoding} is not a valid encoding. Supported encodings: "
            + ", ".join(sorted(SUPPORTED_ENCODINGS))
        )

    for value in args.encoded_strings:
        try:
            print(decode_value(encoding, value))
        except ValueError as exc:
            sys.stderr.write(str(exc) + "\n")
            return 1

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main(sys.argv[1:]))
