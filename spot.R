#' Helper functions module 
#' 
#' The \code {PHEI_functions/spot} contains functions used to:
#' - spot differences between datasets
#' - spot the latest dataset in a directory based on the date in filenames in "yyyymmdd" format
#' - spot whether the data we are retrieving is .csv or .xlsx format and read it in 
#' - spot and read a table within a word document based off of unique text surrounding it

'.__module__.'

#' Looks for new commits in the GitLab repo for PHEI_functions. Changes can be pulled if found.
#' @param pull Set to TRUE by default. If FALSE, the function will only look for changes and update
#' @export

update <- function(pull = TRUE) {
  
  proj_url <- "@hertscc.managed.mango-solutions.com/git/hcc_phei/tools/phei_functions.git"
  
  # check status of local folder by fetching data and then checking git status
  branch_status <- system(paste0("cd ../../phei_functions \n
                                git fetch \n 
                                git status"), intern = TRUE)[2]
  
  # if up to date, the second line should say it is up to date with origin/master
  
  if (branch_status == "Your branch is up to date with 'origin/master'.") {
    
    message("You are up to date with the latest version.")
    
  } else {
    
    if (pull == T) {
      
      # fetch once again, and pull updates
      system(paste0("cd ../../phei_functions
                  git fetch https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url,
                  "\n git pull https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url, " master"))
      
      message("Updates pulled. You are up to date with the latest version.")
      
    } else {
      
      # if pull is set to FALSE, just state that updates were found without pulling. 
      message("Updates found. Please pull the latest version.")
      
    }
  }
  
}

#' Looks for the most recent file in a directory based on the date in the file name and output the file path for reading. Will only work if dates are consistently in the file names.
#' Works best with format %Y%m%d
#' @param path Path of the directory
#' @param name (optional) Unique pattern for the file. e.g. "data_run" or ".csv". If not specified it will will read all files in the directory
#' @param number_from_latest (optional) Index of the sorted vector of files. Default is 1, which fetches the latest file with the latest date. An input of 2 will fetch 
#' the second latest file, etc. A vector can be supplied as well if you wish to get several file paths as output.
#' @param contains_time (optional) Assumes the file name contains _HHMM after dates and additionally sort based off of this. TRUE by default.
#' @param once_per_day (optional) Only relevant if contains_time = TRUE. Will fetch number_from_latest based on the assumption that you only want one file per day,
#' irrespective of the time it was uploaded. Will always fetch the latest one of the day. FALSE by default.
#' @param date_format (optional) Format of the date naming convention used in the directory. "%Y%m%d" by default.
#' @param cut_string (optional)
#' @export

date <- function(path,
                 name = F,
                 number_from_latest = 1,
                 contains_time = T,
                 once_per_day = F,
                 date_format = "%Y%m%d") {

  cut_string = c(1, 9)
  full_names = T
  
  if (name == F) {
    name <- NULL
  }
  
  if (length(path) == 1) {
    loops <- 1
  } else {
    loops <- length(path)
  }
  
  output <- vector()
  
  for (f in 1:loops) {
    files_full <-
      list.files(path[f], pattern = name, full.names = full_names) #get file paths
    files <-
      sub(".[^.]+$", "", files_full) #remove file type (e.g.csv)
    
    if (contains_time == T) {
      dates <- as.numeric(gsub("_", "",
                               substr(
                                 stringr::str_extract(files, "[^/]*$"),
                                 1, 13
                               )))
      
      files_full <-
        files_full[c(which(!is.na(dates)))] #remove potential NAs
      dates <- as.vector(na.omit(dates))
      
      
      if (once_per_day == T) {
        files <- dates[-(which(duplicated(substr(
          sort(dates)
          , 1, 8
        ))) - 1)]
        
        files_full <- files_full[-(which(duplicated(substr(
          sort(dates)
          , 1, 8
        ))) - 1)]
      }
      
    } else {
      dates <- as.Date(substr(
        stringr::str_extract(files, "[^/]*$"),
        cut_string[1],
        cut_string[2]
      ),
      format = date_format) #extract date
      files_full <- files_full[order(dates)] #reorder by date
      
    }
    
    output[f] <-
      tail(files_full, max(number_from_latest))[1:length(number_from_latest)]
    
  }
  
  return(output)
  
}

#' Reads in a dataset that is either in excel or csv format. There is no need to specify the type of dataset. This function is 
#' useful if datasets are regularly changes from one format to the other. The function also supports spot_date so you can 
#' simply input a folder path and the function will find the latest file and read it in. 
#' @param path The path to the file you are reading. If spot_date = T, specify the path the file directory. 
#' @param spot_date (optional) Whether you want to use spot_date to find the latest file path in the directory specified. FALSE by default 
#' @param name_space (optional) Whether you'd like to keep spaces in the column names. If not, input in string what you'd replace the spaces with. 
#' @param date_columns (optional) Input a vector of indices specifying which columns you'd like to convert to Date type
#' @param ... (optional) Extra parameters you'd like to specify into read.csv and read_xlsx
#' @export

dataset <- function(path, 
                    spot_date = FALSE, 
                    name_space = TRUE, 
                    date_columns = NA, 
                    ...) {
  
  # NOTE: IF CSV, SPACES WILL BE REPLACED BY . BY DEFAULT. 
  # path: either path of the file or path of the folder if using spot_date
  # spot_date: whether to use spot_date function to fetch the latest dataset or not
  # name_space: T by default, which will keep the spaces in column names ("Column with space"). Can replace with string e.g. if name_space = "-", column will be "Column-with-space".
  # date_columns: NA by default. Sometimes when exporting xlsx files the date format gets replaced by numerics. In these cases, providing a vector of column indices will correct these (e.g. date_columns = c(1, 3) will fix the first and third columns)
  
  if (spot_date == T) {
    
    path <- spot$date(path)
    
  }
  
  type <- sub(".*\\.", "", path) # check file type
  
  if (type == "csv") {
    
    gc() 
    
    data <- read.csv(path, header = T, na.strings = c(""), stringsAsFactors = F, ...)
    
  } else if (type == "xlsx") {
    
    gc() 
    
    data <- readxl::read_xlsx(path, guess_max = 1048576, .name_repair = "minimal")
    
  } else {
    
    return("Unsupported file type.")
    
  }
  
  # check if there are empty rows
  
  if (sum(is.na(data[1, ])) == length(data)) { #check if the first row is all NA. 
    
    remove_rows <- sum(apply(data, 1, function(x) { sum(is.na(x)) == length(data) })) # in case there are several empty rows
    
    data <- readxl::read_xlsx(path, skip = remove_rows + 1, .name_repair = "minimal")
    
  }
  
  # check if there are Date formatted columns to correct
  if (sum(is.na(date_columns)) != 1) {
    
    if (type == "xlsx") {
      
      date_columns <- date_columns[sapply(data[, date_columns], is.numeric)] #double check that they are actually bugged
      
      if (sum(!is.na(date_columns)) > 0) {
        
        for (c in date_columns) {
          
          data[, c] <- as.Date(as.numeric(data[[c]]), origin = "1899-12-30")
          
        }
        
      }
      
    } else if (type == "csv") {
      
      for (c in date_columns) {
        
        data[, c] <- as.character(data[, c])
        data[, c] <- ifelse(data[, c] == "", NA, data[, c])
        data[, c] <- as.Date(data[, c])
        
      }
      
    }
    
  }
  
  # replace spaces with character of choice if needed
  
  if (name_space != T) {
    
    names(data) <- gsub(" ", name_space, names(data))
    
  }
  
  return(data)
  
}

#' @export

wordtable <- function(path, #path of word document
                             unique_prev, #unique text before the table 
                             unique_after, #unique text after the table
                             cols) { #vector of columns of the table 
  
  # Load document as large string
  doc <- readtext::readtext(path)$text
  col_string <- paste(cols, collapse = "\n")
  num <- length(cols)
  
  ## Find table data
  common_postcodes <- gsub(paste0(unique_prev, "(.+)", unique_after), "\\1", doc)
  common_postcodes <- stringr::str_split(gsub(paste0(".*\n", col_string, "\n"), "", common_postcodes), "\n")
  
  len <- num * round(ceiling(length(common_postcodes[[1]]) / num))
  
  # Extract by column
  cp <- data.frame(First = common_postcodes[[1]][c(seq(1, len, by = num))]) 
  
  if (length(cols) > 1) {
    
    for (c in 2:length(cols)) {
      
      cp[[paste(cols)[c]]] <- common_postcodes[[1]][c(seq(c, len, by = num))] #fill with columns given
      
    }
    
    names(cp) <- cols #rename
    
  } else { 
    
    names(cp) <- cols #rename
    
  }
  
  # Some rows may need to be cut off (residual text).
  stop_here <- which(is.na(cp[[paste(tail(cols, 1))]]))[1] 
  
  if (!is.na(stop_here)) { cp <- cp[- c(stop_here:nrow(cp)), ] }
  
  return(cp)
  
} 
