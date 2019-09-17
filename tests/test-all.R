library(testthat)
library(here)

source(here('R','police-selectize-functions.R'))


test_that("test_load_data", {
  
  csv_path <- here('tests','test_data','test_load.csv')
  
  datalist <- load_data(csv_file = csv_path)
  
  PC_to_LSOA <- datalist$PCdata
  
  police_dataframe <- datalist$policedata
  
  test_PC <- data.frame(read.csv(here('src','PC_to_LSOA_dec2011.csv')))
  
  test_poldf <- data.frame(read.csv(csv_path))
  
  expect_equal(PC_to_LSOA, test_PC)
})

test_that("test_extract_MSOA", {
  
  test_df <- data.frame(read.csv(here('tests','test_data','test_MSOAget.csv')))
  
  PC_frame <- data.frame(read.csv(here('src','PC_to_LSOA_dec2011.csv')))
  
  MSOA_t <- extract_MSOA(test_df, PC_frame)$MSOA_lst
  
  LAD_t <- extract_MSOA(test_df, PC_frame)$LAD_lst
  
  expect_equal(LAD_t[[1]][1], 'E08000035')
  
  expect_equal(MSOA_t[], test_df$MSOA[[2]])
})

# need to start here why is this function weird with test data
test_df$MSOA <- sapply(test_df[,'PartialPostCode'], function(x) as.character(unique(PC_frame[grep(x, PC_frame$PCD7),]$MSOA11CD)))


