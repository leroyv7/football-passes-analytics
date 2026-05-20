#!/bin/bash

git add .
git commit -m "$1"
git push -u origin main

echo "Done! Your work has been published to GitHub."