library(testthat)
library(here)

source(here('R','police-selectize-functions.R'))


test_that("test_load_data", {
  
  csv_path <- here('tests','test_data','test_load.csv')
  
  datalist <- load_data(csv_file = csv_path)
  
  PC_to_LSOA <- datalist$PCdata
  
  police_dataframe <- datalist$policedata
  
  test_PC <- data.frame(read.csv(here('src','Leeds_PC_to_OA.csv')))
  
  test_poldf <- data.frame(read.csv(csv_path))
  
  expect_equal(PC_to_LSOA, test_PC)
})

test_that("test_extract_MSOA", {
  
  test_df <- data.frame(read.csv(here('tests','test_data','test_MSOAget.csv')))
  
  PC_frame <- data.frame(read.csv(here('src','Leeds_PC_to_OA.csv')))
  
  new_frame <- extract_MSOA(test_df, PC_frame)
  
  expect_equal(new_frame$MSOA[1], 'E02002340,E02002339,E02002336,E02002343,E02002338')
  
  expect_equal(new_frame$LAD[1], 'E08000035')
  
})

test_that("test_get_geojson", {
  
  test_df <- data.frame(read.csv(here('tests','test_data','test_geojson.csv')))
  
  getfile <- get_geojson(test_df)
  
  expect_equal(as.character(getfile@data[[1]][1]), 'E02002330')
  
})
