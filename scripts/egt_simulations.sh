#!/usr/bin/env bash


STRATEGIES="1 2 3 4 5 6 7 8 9"
#STRATEGIES="1 3 5 7 9"
#STRATEGIES="2 3 4 5 6 7 8"
#STRATEGIES="3 4 5 6 7"
#STRATEGIES="3 3.5 4 4.5 5 5.5 6 6.5 7"
#STRATEGIES="1 9"
#STRATEGIES="4 6"
#STRATEGIES="1 5 9"
#STRATEGIES="4 5 6"

D=0

# Args: #Runs, P1 proportion, Q1 proportion, D(isagreement point), <strategies>
exec java -jar ./egt.jar minimal 100 .9 .9 $D $STRATEGIES
