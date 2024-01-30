###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

# setwd("~/nbaSpreads/")
source("99_functions.R")

# Should we exclude play off rows?
years <- c(2018, 2019, 2020, 2021)
months <- c("october", "november", "december",
            "january", "february", "march", 
            "april", "may", "june", 
            "july", "august", "september",
            "october-2019", "october-2020") %>%
  tolower()

yearMonths <- CJ(years, months)

rbind_fill <- function(...){
  rbind(...,fill=TRUE)
}

dt <- 
  foreach(row = iter(yearMonths, by = "row"), .combine = rbind_fill) %do% {
  
    yr <- row$years
    month <- row$month
    
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", 
                yr, "_games-", month, ".html")
  
  webpage <- tryCatch(
    {read_html(url)},
    error = function(cond){return(NA)})
  
  validForPull <- function(webpage){
    if(!is.na(webpage)) { 
      if (webpage %>% html_nodes("#schedule") %>% 
          html_text %>% length != 0) {
          TRUE
        } else {FALSE}
      } else {FALSE}
  }
  
  if(validForPull(webpage)) {
  
  colNames <- webpage %>% 
    html_nodes("#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  
  dates <- webpage %>% 
    html_nodes("#schedule > tbody > tr > th") %>% 
    html_text()
  
  gameId <- webpage %>% 
    html_nodes("#schedule > tbody > tr > th") %>%
    html_attr("csk")
  
  dates  <- dates[dates != "Playoffs"]
  gameId <- gameId[!is.na(gameId)]
  
  table <- webpage %>% 
    html_nodes("#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(colNames) - 1, byrow = TRUE) %>% 
    as.data.table
  
  setnames(table, names(table), setdiff(colNames, "date_game"))
  
  table[, ":="(dates = dates, 
               gameId = gameId,
               url = url,
               yr = yr,
               month = month)]
  
  } else {
    table <- createSchemaTable(getGamesSchema())
  }
  table
}

dt %>% archiveAndSave(getGameDataPath())

# Data checks
if (FALSE) {
  dt <- fread(getGameDataPath())
  dt[, .N, keyby = url]
  dt[, .N, keyby = .(yr, month)]
  
  check <- 
    merge( dt[, .(Naway = .N), keyby = .(yr, visitor_team_name)],
         dt[, .(Nhome = .N), keyby = .(yr, home_team_name)],
         by.x = c("visitor_team_name", "yr"),
         by.y = c("home_team_name", "yr")) %>% 
    .[, Ntotal := Nhome + Naway] %>%
    .[order(Ntotal)]
  
  check[yr == 2018]
}
