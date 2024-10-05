#!/bin/bash
# Upload a website to s3
aws s3 cp --profile personal --recursive $2 s3://$1/
