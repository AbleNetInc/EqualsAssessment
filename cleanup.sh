#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
rm -f "$DIR"/exports/*
sqlite3 "$DIR"/EqDB 'delete from Eq2;'
