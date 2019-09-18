# functions script
library(tidyverse)
library(here)
library(geojsonio)


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
  
  
  
  
