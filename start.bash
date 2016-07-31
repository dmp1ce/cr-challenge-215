#!/bin/bash

# Use Docker to install requirements and run katy_playlist.hs
docker-compose build && docker-compose run haskell bash
