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

test_that("test_tokenize", {
  
  test_text <- data.frame(read.csv(here('tests','test_data','test_tokenize.csv')))
  
  tokens <- tokenize_corpus(test_text$free_text)
  
  expect_equal(grep("[[:punct:]]", as.character(tokens[[1]])), integer(0) )
  
  expect_true(strsplit(as.character(tokens[[2]]), ' ')[[1]][2] == 'crib')
  
})


test_that("test_getDTM", {
  
  data <- data.frame(read.csv(here('tests','test_data','test_DTM.csv')))
  
  DTM <- build_DocTermMatrix(0.05, 0.8, Corpus(VectorSource(data$tokens)))
  
  expect_true(DTM$dimnames$Terms[1] == 'jesus')
  
})

test_that("test_countMSOA", {
  
  test_data <- data.frame(read.csv(here('tests','test_data','test_countMSOA.csv')))
  
  test_geo <- geojsonio::geojson_read('https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/statistical/eng/msoa_by_lad/E08000035.json', what = 'sp')
  
  OA_count_lst <- count_MSOAs(test_data, test_geo)$OA_count
  
  OA_text <- count_MSOAs(test_data, test_geo)$text_reports
  
  expect_equal(OA_count_lst[OA_count_lst$code == 'E02006875',]$freq, 4)
  
  expect_equal(dim(OA_text)[1], 18)
  
  expect_equal(OA_text$CrimeNotes[6], 
               "Suspect stopped victim in alley intimidated into handing over mobile phone")
})


test_that("test_toMonth", {
  
  test_data <- data.frame(read.csv(here('tests','test_data','test_countMSOA.csv')))
  
  new_frame <- map_Months(test_data)
  
  expect_true('Month2' %in% colnames(new_frame))
  
  expect_equal(new_frame$Month2[6], 9)
})

