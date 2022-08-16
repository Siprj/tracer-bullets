#!/usr/bin/env bash

counter=0

while true; do
  date=$(date +%Y-%m-%dT%H:%M:%S.%NZ)
  echo "{\"timestamp\": \"${date}\", \"body\": \"Nice message ${counter}\", \"log.component\": \"bash stdout logger\", \"severity_text\": \"INFO\", \"random_data\":\"random attribute data ${counter}\"}"
  counter=$(( counter + 1 ))
  # sleep 0.001;
done
