#!/bin/bash
# Iniating services and environment variables
docker pull fpco/stack-build:lts-7.10

# Build the system's docker file
docker build -t docker-lorcan .

# List of the various services making up the file system
myServices=( dir-server auth-server lock-service transaction-service file-server )

# creating containers and starting them
for serv in ${myServices[@]}
do
  echo $serv
  cd $serv
  stack clean
  stack image container      # new container for each service
  nohup docker-compose up &  # running services in the background
  cd ..
  sleep 2
done

# Run client db through docker-compose file
cd use-haskell-client
docker-compose up
