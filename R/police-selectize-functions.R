# functions script
library(tidyverse)
library(here)
library(geojsonio)
library(tm)

# helper function
'%!in%' <- function(x,y)!('%in%'(x,y))

#main functions
load_data <- function(csv_file) {
  
  PC_to_LSOA <- data.frame(read.csv(here('src','Leeds_PC_to_OA.csv')))
  
  police_dataframe <- data.frame(read.csv(csv_file))
  
  ret_list <- list('PCdata' = PC_to_LSOA,
                   'policedata' = police_dataframe)
  
  return(ret_list)
}

extract_MSOA <- function(dataframe, PC_frame) {
  
  # remove spaces between postcodes
  dataframe$PartialPostCode <- gsub(' ', '', dataframe$PartialPostCode)
  
  PC_frame$pcd7 <- gsub(' ', '', PC_frame$pcd7)
  
  # get MSOA codes into single character string per row
  new_df <- dataframe %>% 
    left_join(
      (dataframe %>% 
                 select(PartialPostCode) %>% 
                 left_join((PC_frame %>% 
                              select(msoa11cd,pcd7) %>%
                              mutate(partial= substr(pcd7,1,nchar(pcd7)-2))), 
                           by=c("PartialPostCode"="partial")) %>% 
                 distinct(PartialPostCode,msoa11cd) %>% 
                 group_by(PartialPostCode) %>%
                 mutate(MSOA=paste0(msoa11cd,collapse = ",")) %>% 
                 distinct(PartialPostCode, MSOA)),by = c("PartialPostCode"="PartialPostCode")
      )
  
  # get local authority names for each row
  new_df2 <- new_df %>% 
    left_join(
      (new_df %>% 
         select(PartialPostCode) %>% 
         left_join((PC_frame %>% 
                      select(ladcd,pcd7) %>%
                      mutate(partial= substr(pcd7,1,nchar(pcd7)-2))), 
                   by=c("PartialPostCode"="partial")) %>% 
         distinct(PartialPostCode,ladcd) %>% 
         group_by(PartialPostCode) %>%
         mutate(LAD=paste0(ladcd,collapse = ",")) %>% 
         distinct(PartialPostCode, LAD)),by = c("PartialPostCode"="PartialPostCode")
    )
        
  return(new_df2)
}


get_geojson <- function(dataframe) {
  
  # assumed only one LAD in dataframe
  LAD_name <- unique(dataframe$LAD)[1]
  
  # loads geojson file
  geojsonfile <- geojsonio::geojson_read(paste0('https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/statistical/eng/msoa_by_lad/',trimws(LAD_name),'.json'), what = 'sp')
  
  return(geojsonfile)
}
  
  
tokenize_corpus <- function(input_text) {
  
  # performs simple preprocessing of tokens
  # removes punctuation and converts all to lowercase
  # returns processed corpus object
  # remove basic english stopwords
  
  free_text <- enc2utf8(as.character(input_text))
  
  text_corpus <- Corpus(VectorSource(free_text))
  
  text_corpus <- tm_map(text_corpus, removePunctuation)
  
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  
  #Strip digits
  text_corpus <- tm_map(text_corpus, removeNumbers)
  
  #remove stopwords
  text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
  #remove whitespace
  
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  
  return(text_corpus)
}
  
  
build_DocTermMatrix <- function(lower_bound = 0.05, upper_bound = 0.8, corpus) {
  # function for building the document-term matrix with upper and lower bounds
  
  # control settings for documentTermMatrix
  # set lower bound (no words that occur in less than 5% of docs)
  minDocFreq <- length(corpus) * lower_bound
  
  # set upper bound (no words that occur in more than 80% of docs)
  maxDocFreq <- length(corpus) * upper_bound
  
  # create DocumentTermMatrix
  DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
  
  
  return(DTM)
}


count_MSOAs <- function(dataframe, geojsonfile) {
  
  # perform incident counting
  lsoa_full <- as.data.frame(
    table(unlist(strsplit(as.character(dataframe$MSOA), ','), recursive=FALSE))
  )
  
  colnames(lsoa_full) <- c('code','freq')
  
  # get all output area names
  OA_names <- data.frame(geojsonfile$MSOA11CD)
  
  colnames(OA_names) <- c('code')
  
  # if there are missing MSOA codes in the counts of MSOAs
  if (length(OA_names$code[OA_names$code %!in% lsoa_full$code]) != 0) {
    
    # create a dataframe of MSOA codes with a count of 0
    missing_df <- data.frame(OA_names$code[OA_names$code %!in% lsoa_full$code], 0)
    
    # rename column headers
    colnames(missing_df) <- c('code','freq')
    
    # row bind these new 0 rows to existing counts per msoa to lsoa_full
    lsoa_full <- rbind(lsoa_full, missing_df)
    
  }
  # if the dataframe ever changes the 2nd column selected here changes by the number of columns added/removed
  # must select the strsplit col and CrimeNotes col
  reports <- dataframe %>%
    transform(MSOA = strsplit(as.character(MSOA),',')) %>%
    unnest(MSOA)
  
  reports <- reports %>%
    group_by(MSOA) %>%
    summarise(CrimeNotes = paste(CrimeNotes, collapse = '<br> <br>'))
  
  names(reports) <- c('code','CrimeNotes')
  
  lsoa_full <- left_join(lsoa_full, reports, by = 'code')
  
  # return final ordered lsoa plus counts
  lsoa_full <- lsoa_full[match(geojsonfile@data$MSOA11CD, lsoa_full$code),]
 
  ret_list <- list('OA_count' = lsoa_full,
                   'text_reports' = reports)
  
  return(ret_list)
}

map_Months <- function(dataframe) {
  
  mon_str <- c('Jan','Feb','Mar',
               'Apr','May','Jun',
               'Jul','Aug','Sep',
               'Oct','Nov','Dec')
  
  mon_int <- c(1, 2, 3, 4,
               5, 6, 7, 8,
               9, 10, 11, 12)
  
  dataframe$Month2 <- plyr::mapvalues(dataframe$Month,
                                from=mon_str,
                                to=mon_int)
  
  dataframe$Month2 <- as.numeric(as.character(dataframe$Month2))
  
  return(dataframe)
}

monthly_term_vol <- function(DTM, dataframe, terms) {
  # function to calculate the monthly volume of a given term
  
  # create the docterm as matrix
  mat_DTM <- mat_DTM <- as.matrix(DTM)
  
  # determine terms use by month
  DTM_by_month <- aggregate((mat_DTM), 
                            by = list(Month = dataframe$Month2), 
                            function(x)sum(x != 0))
  
  # get total documents per month
  total_docs_month <- aggregate(rowSums(mat_DTM), 
                                by = list(Month = dataframe$Month2), 
                                function(x)sum(x != 0))
  
  # specify columns
  colnames(total_docs_month) <- c('Month','Totals')
  
  combined_frame <- DTM_by_month %>%
    left_join(total_docs_month,
              by = c('Month' = 'Month'))
  
  if (length(terms) != 0) {
    
    combined_frame <- combined_frame[,c('Month',terms, 'Totals')]
  } else {
    
    combined_frame <- combined_frame[, c('Month','Totals'), drop=FALSE]
  }
  
  return(combined_frame)
}

