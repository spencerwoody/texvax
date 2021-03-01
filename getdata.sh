#!/bin/bash

cd ~/texvax

Rscript getdata.R &> console/getdata.Rout

./pushdata.sh
