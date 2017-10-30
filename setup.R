#Set up function
#run this function will set working directory, download required packages and optionally download dataset from given urls.
#This function itself it dependent on "stringr" and "readr" packages
#This function only supports reading csv file now
#... should contain the urls
setup <- function(desired_working_file_name,required_packages,data_urls = FALSE){
  #initialize
  prerequired_packages <- c("readr","stringr")
  required_packages <- c(prerequired_packages,required_packages)
  installed_package_names <- row.names(installed.packages())
  
  #install and load required packages
  for (i in required_packages){
    if (i %in% installed_package_names){
      require(i,character.only = TRUE)
    } else {
      install.packages(i)
      require(i,character.only = TRUE)
    }
  }
  
  # set working directory
  if (str_extract(getwd(),"(?!.*/).*") != desired_working_file_name) {
    if (dir.exists(desired_working_file_name)){
      setwd(desired_working_file_name) 
    } else {
      dir.create(desired_working_file_name)
      setwd(desired_working_file_name)
    }
    message("working directory set to ",desired_working_file_name)
  } else {
    message("working directory unchanged, because already set to desired one")
  }
  
  # download dataset
  if (data_urls == FALSE) {
    message("No data loaded from internet")
  } else {
    dataset_name <- character(length(data_urls))
    for (i in length(data_urls)) {
      dataset_name[i] <- str_extract(data_urls[i],"(?!.*/).*(?=\\.csv)")
      assign(dataset_name[i],read_csv(data_urls[i]),envir = .GlobalEnv)
      message(length(data_urls), " data files loaded from internet")
    }
  }
}
