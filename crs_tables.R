## Appendix F Parsing
library(dplyr)
library(pdftools)
library(stringr)
## here is where all the FEMA NFIP reports are from
## https://www.fema.gov/flood-insurance-manual
## we care about appendix F - CRS Communities
# files in dir
files <- list.files(path = "appendix_f", pattern = ".pdf", full.names = T)
# pdf <- files[1]
# pdf <- files[6]
create_table <- function(pdf){
  require(pdftools)
  require(dplyr)
  require(readr)
  require(stringr)
  require(tidyr)
  require(lubridate)
  # date from file name 
  pdf_date <- as.Date(paste0("01",substr(pdf, nchar(pdf) -10, nchar(pdf) - 4)), format = "%d%b%Y")
  # exceptions for june
  pdf_date <- if_else(is.na(pdf_date), 
                     as.Date(paste0("01",substr(pdf, nchar(pdf) -11, nchar(pdf) - 4)), format = "%d%b%Y"),
                     pdf_date)
  # read in pdf ----
  print(paste0("Now reading pdf ", pdf))
  pages <- pdf_text(pdf)
  # find table 3 
  rightpages <- grep(pattern = "TABLE 3. COMMUNITY RATING SYSTEM ELIGIBLE COMMUNITIES", x = pages, 
                     ignore.case = TRUE)
  # may 2006 exception 
  if(length(rightpages) == 0) {
  rightpages <- grep(pattern = "\nCOMMUNITY", x = pages, 
                            ignore.case = TRUE)
  }
  


  tablist <- list()
  # i <- 21
  # i <- 38
  # i <- 15
  # i <- 23
  i <- 1
  i <- NULL
  for(i in 1:length(pages[rightpages])){
    
    print(paste0("Now reading page ", i))
      table <- pages[rightpages[i]]
      # split table on line breaks
      table <- str_split(table, "\n", simplify = TRUE)
      head(table)
      print(pdf_date)
      # read in as fwf, differs by year ----
      if(lubridate::year(pdf_date) >= 2018){
        # they made the format much nicer in 2018
        table_start <- grep("Status", table, ignore.case = TRUE)[[1]]
        table_end <- stringr::str_which(table, "For the purpose of determining CRS discounts,")
        print(paste("table starts at: ", table_start, " and ends at: ", table_end))
        table <- table[1, (table_start + 1):(table_end - 1)]
        table <- read_fwf(table, col_positions = fwf_widths(c(3, 6, 40, 12, 12, 16, NA)))
        table <- table %>%
        mutate(X2 = str_extract(string = X3, pattern = "\\d+")) %>%
        mutate(X3 = str_remove(string = X3, pattern = "\\d+")) %>%
        select(-X4) %>% 
        mutate(report_dt = pdf_date)
        # rename the columns so they'll rbind correctly
      table <- table %>% 
        separate(X7, into = c("X7","X8","X9", "X10"), sep = "\\s+") %>%
        rename(state_code = X1,
               crs_number = X2,
               community_nm = X3,
               entry_dt = X5,
               effective_dt = X6,
               current_class = X7,
               sfha_disc = X8,
               nonsfha_disc = X9,
               status = X10) 
        # this is so it will merge with earlier tables 
        table <- table %>% mutate(state_name = NA,
                                  community_nm = trimws(community_nm, 'both'))
      } 
      if(lubridate::year(pdf_date) >= 2006 | lubridate::year(pdf_date) < 2011){
        # the tables were much less structured earlier
        table_start <- grep("SFHA", table, ignore.case = TRUE)
        table_end <- stringr::str_which(table, "For the purpose of determining CRS discounts,")
        print(paste("table starts at: ", table_start, " and ends at: ", table_end))
        table <- table[1, (table_start + 1):(table_end - 1)]
        
        table <- read_fwf(table, col_positions = fwf_widths(c(8, 40, 14, 14, NA))) %>%
          mutate(report_dt = pdf_date)
          # single lines are now state names 
          # first find them and turn them into columns

        table <- table %>%
          # group by states 
          group_by(X2) %>%
          mutate(state = X2) %>%
          # keep only states 
          mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
          ungroup() %>%
          # bring forward states
          mutate(state = zoo::na.locf.default(state)) %>%
          # drop state headers
          filter(!is.na(X1)) %>%
          # separate X5 into columns
          separate(X5, c("X5", "X6", "X7","X8"), sep = "\\s+") %>%
          rename(crs_number = X1,
                 community_nm = X2,
                 entry_dt = X3,
                 effective_dt = X4,
                 current_class = X5,
                 sfha_disc = X6,
                 nonsfha_disc = X7,
                 status = X8) 
        # this is so it will merge with earlier tables 
        table <- table %>% mutate(state_code = NA,
                                  community_nm = trimws(community_nm, 'both'))
      } 
      
      # 2012 - 2017 tables 
      table_start <- grep("Status2", table, ignore.case = TRUE)[[1]]
      table_end <- stringr::str_which(table, "For the purpose of determining CRS discounts,")[[1]]
      print(paste("table starts at: ", table_start, " and ends at: ", table_end))
      table <- table[1, (table_start + 1):(table_end - 1)]
      table <- read_fwf(table, col_positions = fwf_widths(c(9, 40, 12, 14, 12, NA))) %>%
        select(-X5) %>%
        mutate(report_dt = pdf_date)
      # single lines are now state names 
      # first find them and turn them into columns
      # state_rows <- str_which(table, paste0(state.name, collapse = "|"))
      table <- table %>%
        # group by states 
        group_by(X2) %>%
        mutate(state = X2) %>%
        # keep only states 
        mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
        ungroup() %>%
        # bring forward states
        mutate(state = zoo::na.locf.default(state)) %>%
        # drop state headers
        filter(!is.na(X1)) %>%
        mutate(X1 = as.character(X1)) 
        # separate X6 into diff columns 
        table <- table %>%
          separate(X6, into = c("X6", "X7", "X8", "X9"), sep = "\\s+") %>%
          rename(state_name = state,
                 crs_number = X1,
                 community_nm = X2,
                 entry_dt = X3,
                 effective_dt = X4,
                 current_class = X6,
                 sfha_disc = X7,
                 nonsfha_disc = X8,
                 status = X9) %>%
          # so it will merge with later tables
          mutate(state_code = NA)
      
    tablist[[i]] <- table
  }

  
  crs_tables <- bind_rows(tablist)
  return(crs_tables)
}

  
