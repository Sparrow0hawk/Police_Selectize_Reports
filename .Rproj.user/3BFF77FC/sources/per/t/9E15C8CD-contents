# functions script
library(here)


load_data <- function(csv_file) {
  
  PC_to_LSOA <- data.frame(read.csv(here('src','PC_to_LSOA_dec2011.csv')))
  
  police_dataframe <- data.frame(read.csv(csv_file))
  
  ret_list <- list('PCdata' = PC_to_LSOA,
                   'policedata' = police_dataframe)
  
  return(ret_list)
}

extract_MSOA <- function(dataframe, PC_frame) {
  
  MSOA_lst <- lapply(dataframe[,'PartialPostCode'], function(x) as.character(unique(PC_frame[grep(x, PC_frame$PCD7),]$MSOA11CD)))
  
  LAD_lst <- lapply(dataframe[,'MSOA'], function(x) as.character(unique(PC_frame[PC_frame$MSOA11CD %in% x,]$LAD11CD)))
  
  ret_lst <- list('MSOA_lst' = MSOA_lst,
                 'LAD_lst' = LAD_lst)
  
  return(ret_lst)
}
