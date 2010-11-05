#!/bin/sh

nvidia-smi -a | awk '$1 ~ /Temperature/ {print $3}'
