library(dplyr)
library(readr)
# from: https://www.fema.gov/national-flood-insurance-program-community-status-book

url <- "https://www.fema.gov/cis/nation.csv"
download.file(url, basename(url), method = "wget")
raw <- read_csv("nation.csv", col_names = FALSE)
# these are empty header rows 
rows_to_remove <- c("Federal Emergency Management Agency", "Community Status Book Report", 
                    "Nation", "Communities Participating in the National Flood Program")

nfip <- raw %>%
  filter(!grepl(paste(rows_to_remove, collapse = "|"), X1))
  filter(!grepl(is.na(X3)))

# nfip[!is.na(nfip$X3),]


