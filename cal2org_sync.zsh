#!/bin/zsh

source ~/.ical.sh

WGET=/usr/bin/wget
ICAL2ORG=~/.local/bin/ical2orgpy
ICSFILE=/tmp/google_cal.ics
ORGFILE=~/org/google_cal.org

$WGET -O $ICSFILE $ICS_URL
$ICAL2ORG $ICSFILE $ORGFILE
