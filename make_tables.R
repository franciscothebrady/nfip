## use get_tables function on CRS pdfs
library(dplyr)
library(readr)
library(pdftools)
library(purrr)
source("get_tables.R", echo = FALSE)


temp <- get_tables("appendix_f/20_crs_508_apr2018.pdf")
temp2 <- get_tables("appendix_f/app-f_crs_508_oct2018.pdf")




