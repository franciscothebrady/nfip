# extract table from pdf pages 
library(dplyr)
library(pdftools)
## get_table
## extract table from given pdf page
## this version takes a second year argument, 
## which specifies how to parse the pdf based
## on the year
# pull one pdf from 2015 
pdf <- list.files(path = "appendix_f/", pattern = "*2015.pdf")
pdf <- pdf[1]
get_tables <- function(pdf, year = 2015) {
  require(lubridate)
  require(dplyr)
  require(pdftools)
  require(stringr)
  # year <- 2008
  pdf_date <- as.Date(paste0("01",substr(pdf, nchar(pdf) -10, nchar(pdf) - 4)), format = "%d%b%Y")
  # exceptions for june
  pdf_date <- if_else(is.na(pdf_date), 
                      as.Date(paste0("01",substr(pdf, nchar(pdf) -11, nchar(pdf) - 4)), format = "%d%b%Y"),
                      pdf_date)
  year <- lubridate::year(pdf_date)
  # more structured ----
  if (year %in% c(2012:2017)) {
    print(paste("Pdf date: ", pdf_date))
    
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "C = Current", x = pages, 
                       ignore.case = TRUE)
    # list of populating tables 
    tablist <- list()
    i <- 1
    i <- NULL
    for(i in 1:length(pages[rightpages])){
    print(paste0("Now reading page ", i))
    table <- pages[rightpages[i]]
    # split table on line breaks
    lines <- str_split(table, "\n", simplify = TRUE)
    # find table start and end
    table_start <- grep("Status2", lines, ignore.case = TRUE)
    table_end <- stringr::str_which(lines, "For the purpose of determining CRS discounts,")
    print(paste("table starts at:", table_start, " and ends at:", table_end))
    table_fwf <- lines[1, (table_start + 1):(table_end - 1)]
    
    table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(9, 40, 14, 14, NA))) %>%
      mutate(report_dt = pdf_date)
    # single lines are now state names 
    # first find them and turn them into columns
    table_df <- table_df %>%
      # group by states 
      mutate(state = X2) %>%
      # keep only states 
      mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
      # bring forward states
      mutate(state = zoo::na.locf.default(state)) %>%
      # drop state headers
      filter(!is.na(X1)) %>%
      # separate X5 into columns
      tidyr::separate(X5, c("X5", "X6", "X7","X8"), sep = "[\\s]+") %>%
      rename(crs_number = X1,
             community_nm = X2,
             entry_dt = X3,
             effective_dt = X4,
             current_class = X5,
             sfha_disc = X6,
             nonsfha_disc = X7,
             status = X8) 
    # this is so it will merge with earlier tables 
    table_df <- table_df %>% 
      mutate(state_code = NA,
             crs_number = as.character(crs_number),
             community_nm = trimws(community_nm, 'both'))
    # add it to the list
    tablist[[i]] <- table_df
    }
    
    df <- bind_rows(tablist)
  }
  # less structured ----
  if (year %in% c(2006:2011)) {
    print(paste("Pdf date: ", pdf_date))
    
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "C = Current", x = pages, 
                       ignore.case = TRUE)
    # list of populating tables 
    tablist <- list()
    i <- 1
    i <- NULL
    for(i in 1:length(pages[rightpages])){
      print(paste0("Now reading page ", i))
      table <- pages[rightpages[i]]
      # split table on line breaks
      lines <- str_split(table, "\n", simplify = TRUE)
      # find table start and end
      table_start <- grep("Non-SFHA", lines, ignore.case = TRUE)[[1]]
      table_end <- stringr::str_which(lines, "For the purpose of determining CRS discounts,")
      print(paste("table starts at:", table_start, " and ends at:", table_end))
      table_fwf <- lines[1, (table_start + 1):(table_end - 1)]
      # changes to the date column in these years 
      table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(9, 40, 12, 12, NA))) %>%
        mutate(report_dt = pdf_date)
      # single lines are now state names 
      # first find them and turn them into columns
      table_df <- table_df %>%
        # group by states 
        mutate(state = X2) %>%
        # keep only states 
        mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
        # bring forward states
        mutate(state = zoo::na.locf.default(state)) %>%
        # drop state headers
        filter(!is.na(X1)) %>%
        # separate X5 into columns
        tidyr::separate(X5, c("X5", "X6", "X7","X8"), sep = "[\\s]+") %>%
        rename(crs_number = X1,
               community_nm = X2,
               entry_dt = X3,
               effective_dt = X4,
               current_class = X5,
               sfha_disc = X6,
               nonsfha_disc = X7,
               status = X8) 
      # this is so it will merge with earlier tables 
      table_df <- table_df %>% 
        mutate(state_code = NA,
               crs_number = as.character(crs_number),
               community_nm = trimws(community_nm, 'both'))
      # add it to the list
      tablist[[i]] <- table_df
    }
    df <- bind_rows(tablist)
  }
  # most structured  ----
  if (year >= 2018) {
    print(paste("Pdf date: ", pdf_date))
    
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "STATUS2", x = pages, 
                       ignore.case = FALSE)
    # list of populating tables 
    tablist <- list()
    i <- 1
    i <- NULL
    for(i in 1:length(pages[rightpages])){
      print(paste0("Now reading page ", i))
      table <- pages[rightpages[i]]
      # split table on line breaks
      lines <- str_split(table, "\n", simplify = TRUE)
      # find table start and end
      table_start <- grep("STATUS2", lines, ignore.case = FALSE)
      table_end <- stringr::str_which(lines, "For the purpose of determining CRS discounts,")
      print(paste("table starts at:", table_start, " and ends at:", table_end))
      table_fwf <- lines[1, (table_start + 1):(table_end - 1)]
      # changes to the date column in these years 
      table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(3, 6, 40, 12, 12, 16, NA)))
      table_df <- table_df %>%
        mutate(X2 = str_extract(string = X3, pattern = "\\d+")) %>%
        mutate(X3 = str_remove(string = X3, pattern = "\\d+")) %>%
        select(-X4) %>% 
        mutate(report_dt = pdf_date)
      # rename the columns so they'll rbind correctly
      table_df <- table_df %>% 
        tidyr::separate(X7, into = c("X7","X8","X9", "X10"), sep = "[\\s]+") %>%
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
      table_df <- table_df %>% mutate(state_name = NA,
                                      crs_number = as.character(crs_number),
                                community_nm = trimws(community_nm, 'both'))
      # add it to the list
      tablist[[i]] <- table_df
    }
    df <- bind_rows(tablist)
  }
  return(df)
}


# tests ----
# 2018 
# pdf <- list.files("appendix_f", pattern = "2018", full.names = T)[1]
# test1 <- get_tables(pdf)
# test1_df <- bind_rows(test1)
