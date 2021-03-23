#!/bin/bash

cd ~/texvax

Rscript getdata.R &> console/getdata.Rout
Rscript zcta_vaccinations.R &> console/zcta.Rout

./pushdata.sh
