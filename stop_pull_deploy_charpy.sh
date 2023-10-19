#!/bin/sh

set -e

if [[ "$PWD" != "/home/ubuntu" ]]; then
  echo "Need to run script from home directory, not project folder!"
  exit
fi

rm -r charpy-master

wget https://github.com/usnistgov/CharpyCurveFitting/archive/refs/heads/master.zip -O charpy-master.zip
unzip charpy-master.zip
rm charpy-master.zip

sudo docker build -t charpy ./charpy-master
sudo docker rm -f charpy
sudo docker image prune -f
sudo docker run -d -p 8082:3838 --name=charpy charpy
