# extract table from pdf pages 
library(dplyr)
library(pdftools)
## get_table
## extract table from given pdf page
## this version determines how to parse the pdf based
## on the year
# pull one pdf from 2015 
pdf <- list.files(path = "data/", pattern = "*2017.pdf", full.names = TRUE)
pdf <- list.files(path = "data/", pattern = "*2013.pdf", full.names = TRUE)
pdf <- list.files(path = "data/", pattern = "*2012.pdf", full.names = TRUE)
pdf <- list.files(path = "data/", pattern = "*2015.pdf", full.names = TRUE)
pdf <- pdf[1]
extract_table <- function(pdf) {
  require(lubridate)
  require(dplyr)
  require(pdftools)
  require(stringr)
  require(readr)
  # get the date 
  pdf_date <- as.Date(paste0("01",substr(pdf, nchar(pdf) -10, nchar(pdf) - 4)), format = "%d%b%Y")
  # exceptions for june
  pdf_date <- if_else(is.na(pdf_date), 
                      as.Date(paste0("01",substr(pdf, nchar(pdf) -11, nchar(pdf) - 4)), format = "%d%b%Y"),
                      pdf_date)
  year <- lubridate::year(pdf_date)

  # 2012 - 2014 ----
  if(year %in% c(2012, 2013, 2014)) {
    print(paste("Pdf date: ", pdf_date))
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "C = Current", x = pages, 
                       ignore.case = TRUE)
    # empty list for tables 
    tablist <- list()
    i <- 1
    i <- NULL
    # loop to parse each page ----
    for(i in 1:length(pages[rightpages])){
      print(paste0("Now reading page ", i))
      table <- pages[rightpages[i]]
      # split table on line breaks
      lines <- str_split(table, "\n", simplify = TRUE)
      # find table start and end
      table_start <- grep("Status2", lines, ignore.case = TRUE)
      table_end <- stringr::str_which(lines, "For the purpose of determining CRS discounts,")
      print(paste("table starts at:", table_start, " and ends at:", table_end))
      # subset lines to beginning and end of the table 
      table_fwf <- lines[1, (table_start + 1):(table_end - 1)]
      # read lines into a df ----
      # here's where the experimentation happens. 
      # mess with the fwf_widths argument to get this right
      # the NA at the end allows the file to read in the last numbers as unstructured text
      table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(9, 40, 14, 14, NA))) %>%
        mutate(report_dt = pdf_date)
      # single lines are now state names 
      # first find them and turn them into columns
      table_df <- table_df %>%
        # group by states 
        mutate(state = X2) %>%
        # bring forward states
        mutate(state = zoo::na.locf.default(state)) %>%
        # get rid of (continued)
        mutate(state = gsub(pattern = "(continued)", "", state, fixed = TRUE)) %>%
        # fix messed up state names ----
        mutate(state = gsub(pattern = "Hawaii County", "Hawaii", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Indianapolis, City of", "Indiana", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Washington, City of", "North Carolina", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Washington County", "North Carolina", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Iowa City, City of", "Iowa", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Kansas City, City of", "Kansas", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Missouri City, City of", "Texas", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "New Jersey Meadowlands", "New Jersey", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Oregon City, City of", "Oregon", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Delaware City, City of", "Delaware", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Colorado Springs, City of", "Colorado", state, fixed = TRUE)) %>%
        mutate(state = gsub(pattern = "Washington Park, Town of", "North Carolina", state, fixed = TRUE)) %>%
        # this code matches the text in the community name variable to actual states 
        mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
        # drop state headers
        filter(!is.na(X1)) %>%
        # separate X5 on spaces 
        tidyr::separate(X5, c("X5", "X6", "X7","X8"), sep = "[\\s]+") %>%
        # rename columns 
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
        # return state abbrev
        mutate(state_code = state.abb[match(state,state.name)],
               crs_number = as.character(crs_number),
               community_nm = trimws(community_nm, 'both'),
               # convert dates to dates 
               entry_dt = mdy(entry_dt),
               effective_dt = mdy(effective_dt))
      # add it to the list
      tablist[[i]] <- table_df
    }
    
    df <- bind_rows(tablist)
  }
  
  # 2015 ----
  if(year == 2015) {
    print(paste("Pdf date: ", pdf_date))
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "C = Current", x = pages, 
                       ignore.case = TRUE)
    # empty list for tables 
    tablist <- list()
    i <- 1
    i <- NULL
    # loop to parse each page ----
    for(i in 1:length(pages[rightpages])){
    print(paste0("Now reading page ", i))
    table <- pages[rightpages[i]]
    # split table on line breaks
    lines <- str_split(table, "\n", simplify = TRUE)
    # find table start and end
    table_start <- grep("Status2", lines, ignore.case = TRUE)
    table_end <- stringr::str_which(lines, "For the purpose of determining CRS discounts,")
    print(paste("table starts at:", table_start, " and ends at:", table_end))
    # subset lines to beginning and end of the table 
    table_fwf <- lines[1, (table_start + 1):(table_end - 1)]
    # read lines into a df ----
    # here's where the experimentation happens. 
    # mess with the fwf_widths argument to get this right
    # the NA at the end allows the file to read in the last numbers as unstructured text
    table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(9, 40, 14, 14, NA))) %>%
      mutate(report_dt = pdf_date)
    # single lines are now state names 
    # first find them and turn them into columns
    table_df <- table_df %>%
      # group by states 
      mutate(state = X2) %>%
      # get rid of (continued)
      mutate(state = gsub(pattern = "(continued)", "", state, fixed = TRUE)) %>%
      # fix messed up state names ----
      mutate(state = gsub(pattern = "Hawaii County", "Hawaii", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Indianapolis, City of", "Indiana", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Washington, City of", "North Carolina", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Washington County", "North Carolina", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Iowa City, City of", "Iowa", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Kansas City, City of", "Kansas", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Missouri City, City of", "Texas", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "New Jersey Meadowlands", "New Jersey", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Oregon City, City of", "Oregon", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Delaware City, City of", "Delaware", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Colorado Springs, City of", "Colorado", state, fixed = TRUE)) %>%
      mutate(state = gsub(pattern = "Washington Park, Town of", "North Carolina", state, fixed = TRUE)) %>%
      # this code matches the text in the community name variable to actual states 
      mutate(state = ifelse(str_detect(state, paste0(state.name, collapse = "|")), state, NA)) %>%
      # bring forward states
      mutate(state = zoo::na.locf.default(state)) %>%
      # drop state headers
      filter(!is.na(X1)) %>%
      # separate X5 on spaces 
      tidyr::separate(X5, c("X5", "X6", "X7","X8"), sep = "[\\s]+") %>%
      # rename columns 
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
      # return state abbrev
      mutate(state_code = state.abb[match(state,state.name)],
             crs_number = as.character(crs_number),
             community_nm = trimws(community_nm, 'both'),
             # convert dates to dates 
             entry_dt = mdy(entry_dt),
             effective_dt = mdy(effective_dt))
    # add it to the list
    tablist[[i]] <- table_df
    }
    
    df <- bind_rows(tablist)
  }
# 2016  ----
  if (year == 2016) {
    print(paste("Pdf date: ", pdf_date))
    
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "COMMUNITY RATING SYSTEM ELIGIBLE COMMUNITIES", x = pages, 
                       ignore.case = TRUE)
    # list of populating tables 
    tablist <- list()
    i <- 1
    i <- NULL
    # loop to parse each page ----
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
      # read lines into a df ----
      table_df <- read_fwf(table_fwf, col_positions = fwf_widths(c(9, 40, 12, 14, NA))) %>%
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
        # get rid of (continued)
        mutate(state = gsub(pattern = "(continued)", "", state, fixed = TRUE)) %>%
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
# 2017  ----
  if (year == 2017) {
    print(paste("Pdf date: ", pdf_date))
    
    pages <- pdf_text(pdf)
    # find table 3 
    # grep for something thats on all the pages of the table
    # this C = Current is on the legend
    rightpages <- grep(pattern = "COMMUNITY RATING SYSTEM ELIGIBLE COMMUNITIES", x = pages, 
                       ignore.case = TRUE)
    # list of populating tables 
    tablist <- list()
    i <- 1
    i <- NULL
    # loop to parse each page ----
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
      # read lines into a df ----
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
        # get rid of (continued)
        mutate(state = gsub(pattern = "(continued)", "", state, fixed = TRUE)) %>%
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
pdf <- list.files(path = "data", pattern = "2015.pdf", full.names = TRUE)
temp <- extract_table(pdf = pdf[1])
table(temp$state)
nov2015 <- extract_table(pdf = pdf[2])
pdf <- list.files(path = "data", pattern = "2014.pdf", full.names = TRUE)
jun2014 <- extract_table(pdf = pdf[1])
temp <- extract_table(pdf = pdf[2])
pdf <- list.files(path = "data", pattern = "2013.pdf", full.names = TRUE)
temp <- extract_table(pdf = pdf[1])
table(temp$state)
pdf <- list.files(path = "data", pattern = "2012.pdf", full.names = TRUE)
temp <- extract_table(pdf = pdf[1])
table(temp$state)

temp <- rbind(apr2015, nov2015, jun2014, oct2014)

# save output 
write_csv(temp, "crs_communities.csv")
