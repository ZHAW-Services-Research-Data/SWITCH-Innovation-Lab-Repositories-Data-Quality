# SWITCH-Innovation-Lab-Repositories-Data-Quality
## Overview & context
This repository contains a R project for the analysis performed during a project that ran from October 2020 until February 2021 as a collaboration between SWITCH and ZHAW Zurich University of Applied Sciences. Research subject was to learn about data reuse in social sciences and humanities: which are the relevant data sources for researches in the social sciences and humanities in Switzerland and which criteria are important when selecting data sources. The report is published

Further information is given in the corresponding report:
Data reuse in the social sciences and humanities : project report of the SWITCH Innovation Lab “Repositories & Data Quality”. Winterthur : ZHAW Zurich University of Applied Sciences, 2021. Available at: https://doi.org/10.21256/zhaw-2404

## Data
The survey data and comprehensive list of resources which is also used for data cleaning by this code is published at Zenodo: https://zenodo.org/record/4609834#.YFr9ybRKhTZ

## How to use this R project
* Download the copy
* Open it in RStudio. In this way, the working directory will be set automatically
* This project uses renv. Run renv::restore() to install all required R packages
* Run the analysis.Rmd. It will take care of creating output directories and downloading the files from zenodo.
