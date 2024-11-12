
library(knitr)
library(kableExtra)
library(glue)
library(quarto)
library(lubridate)
library(aws.s3)
library(httr)
library(rvest)
library(xml2 )
library(stringr)
library(data.table)
library(TTR)
library(tidyverse)

############
installed.packages() %>% as.data.frame() %>% 
  select(Package) %>% unlist() -> local_packs

if("couchquant" %in% local_packs){
  library(couchquant)
}else{

    devtools::install(
      paste0(
        substr(getwd(), 
           1, 
           nchar(getwd()) - nchar("crypto_data_analysis")), 
        "couchquant"
      ), 
      repos = NULL, type = "source"
    )
  
  library(couchquant)
}


if(T == F){
  remove.packages("couchquant")

}
