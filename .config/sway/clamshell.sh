#!/bin/sh

# Runs on config reload since sway can forget the clamshell state of the laptop.
#
LAPTOP_OUTPUT="eDP-1"
LID_STATE_FILE="/proc/acpi/button/lid/LID0/state"

read -r LS < "$LID_STATE_FILE"

case "$LS" in
*open)   swaymsg output "$LAPTOP_OUTPUT" enable ;;
*closed) swaymsg output "$LAPTOP_OUTPUT" disable ;;
*)       echo "Could not get lid state" >&2 ; exit 1 ;;
esac
