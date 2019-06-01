#!/usr/bin/env bash

sudo yum install -y git
git clone https://github.com/drassaby/egt.git
sudo yum install -y java-1.8.0-openjdk.x86_64
sudo yum install -y wget

cd ~
wget http://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.rpm
sudo yum install -y scala-2.12.8.rpm

cd egt
curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
sudo yum install -y sbt

sbt package

# Runs P1 Q1 D <strategy set>
c10000 .9 .5 0 4 5 6
10000 .9 .9 0 4 6
10000 .9 .9 0 1 5 9
10000 .5 .6 3 1 5 9
10000 .5 .9 1 1 5 9
10000 .9 .5 0 1 3 5 7 9
