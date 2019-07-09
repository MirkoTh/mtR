#' Default folder structure of Mirko Thalmann for R projects
#' @title create folder structure
#' @name create_default_folder_structure
#' @param project_path the path of the project folder.
#' @return creates folder structure and exec.R file to run associated .sql and .Rmd files
#' @author Thalmann, M.
#' @importFrom stringr str_replace
#' @importFrom readr write_file
#' @importFrom utils globalVariables
#' @export


create_default_folder_structure = function(project_path = getwd(), project_author = NULL){
  if (is.null(project_author)){
    stop("project_author is not defined")
  }

  library(stringr)
  library(readr)

  setwd(project_path)
  project_vars <- list()
  project_path <- str_replace(project_path, 'O:/', '//JESF01/DocumentH/' )
  dir_names <- c("rendered_content", "data", "sql")
  path_names <- data.frame(dir_names = dir_names,
                           path = paste0(project_path, "/", dir_names))
  lapply(paste(path_names$path), dir.create)
  path_names <- rbind(path_names,
                      cbind(dir_names = "project", path = paste0(project_path)))
  project_vars[["dirs"]] <- path_names
  project_vars[["author"]] <- project_author
  base::save(project_vars, file = "project_vars.Rda")

  r_file <- '
# by default, .sql files in sql folder are run before .rmd files are rendered
library(tidyverse)
library(rmarkdown)

load("project_vars.Rda")

get_path <- function(list_in, dir_name){
paste(list_in[["dirs"]] %>% filter(dir_names == dir_name) %>%
select(path) %>% mutate(path = as.character(path)))
}

project_path <- get_path(project_vars, dir_name = "project")
sql_path <- get_path(project_vars, dir_name = "sql")

file_names <- list()
sql_files_present <- dir(sql_path)
if(length(sql_files_present) > 0){
# only take .sql files
sql_files <- sql_files_present[grepl(".sql", sql_files_present)]
file_names[["sql"]] <- paste0(sql_path, "/", sql_files)
# run .sql files
sql_list <- lapply(file_names[["sql"]], bcagR::jemas_select_from_sql_file)
}

# render .rmd files
rmds <- dir(project_path)[grepl(".Rmd", dir(project_path))]
if (length(rmds) > 0){
lapply(rmds, render)
}
'
  r_file_path <- paste0(project_path, "/exec.R")
  write_file(r_file, r_file_path, append = FALSE)
}
