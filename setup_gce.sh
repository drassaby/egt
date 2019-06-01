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


scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .9 .5 0 4 5 6 > 456.out & disown
scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .9 .9 0 4 6 > 46.out & disown
scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .9 .9 0 1 5 9 > 159.out & disown

# Waiting on power to be implemented:
scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .6 .5 3 1 5 9 > 159.out & disown
scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .9 .5 1 1 5 9 > 159.out & disown


# This one will take a long long long time, maybe parallelize further?
scala ~/egt/target/scala-2.12/egt_2.12-0.1.jar 10000 .9 .5 0 1 3 5 7 9 > 13579.out & disown
