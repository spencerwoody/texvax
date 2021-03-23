#!/bin/bash

git add ./data/*
git add ./data_zip/*
git add ./map_data/*
git commit -m "daily update for $(date +"%F")"
git push

