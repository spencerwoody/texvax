#!/bin/bash

git add ./data/*
git commit -m "daily update for $(date +"%F")"
git push

