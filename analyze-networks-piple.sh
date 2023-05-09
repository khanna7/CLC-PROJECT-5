#!/bin/bash

# execute in order
Rscript "EDA.R"
Rscript -e "rmarkdown::render('network-data-analysis.Rmd')"
Rscript -e "rmarkdown::render('hypothesis-ego-vs-alter-covid-behaviors.Rmd')"
Rscript "visualize-networks.R"

