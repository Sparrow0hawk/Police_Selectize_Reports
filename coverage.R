# coverage script
library(covr)
library(here)

covfile <- covr::file_coverage(
                               here('R','police-selectize-functions.R'),
                               here('tests','test-all.R')
                               ) 

covr::codecov(coverage = covfile)
