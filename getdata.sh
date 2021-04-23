#!/bin/bash

cd ~/texvax

Rscript getdata.R &> console/getdata.Rout
Rscript zcta_vaccinations_export.R &> console/zcta.Rout

./pushdata.sh
