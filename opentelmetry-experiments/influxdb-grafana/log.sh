#!/usr/bin/env bash

counter=0

while true; do
  echo "test log ${counter}"
  counter=$(( counter + 1 ))
  sleep 1;
done
