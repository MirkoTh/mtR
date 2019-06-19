#' create index page for an analysis
#' @title creating an index for an analysis consisting of one or more .Rmd files
#' @name make_index
#' @param project_name the name of the project
#' @return returns nothing, but renders the .Rmd files as .html files
#' @author Thalmann, M.
#' @export


make_index <- function(project_name){
  load("project_vars.Rda")
  # write out .Rmd files to be moved
  project_dir <- as.character(project_vars$dirs$path[project_vars$dirs$dir_names == "project"])
  files_project_tmp <- dir(project_dir)

  if ("index.html" %in% files_project_tmp){
    file.remove(paste0(project_dir, "/", "index.html"))
  }

  files_project <- dir(project_dir)

  files_to_move <- files_project[endsWith(files_project, ".html")]
  target_dir <- as.character(project_vars$dirs$path[project_vars$dirs$dir_names == "rendered_content"])
  # this function should go into mtR package
  copy_file_into_rendered_content <- function(file_name, project_dir, target_dir){
    origin_loc <- paste0(project_dir, "/", file_name)
    target_loc <- paste0(target_dir, "/", file_name)
    file.copy(origin_loc, target_loc)
  }

  # copy and remove .html files
  lapply(files_to_move, copy_file_into_rendered_content, project_dir, target_dir)
  lapply(files_to_move, file.remove)

  l_out <- list()
  idx <- 1
  for (file in files_to_move){
    l_out[[idx]] <- paste0(target_dir, "/", file)
    idx <- idx + 1
  }
  l_out

  links <- list()
  for (i in 1:length(files_to_move)){
    links[[i]] <- paste0("\n#### [", files_to_move[i], "]", "(file:///", l_out[i], ")")
  }

  index_html <- paste("
---
title: 00_index
author:", project_vars[["author"]],"
date:", Sys.Date(),"
output: html_document
---


##", project_name, "\n",
                      paste0(unlist(links), collapse = ""))

  index_path <- paste0(project_path, "/index.Rmd")
  write_file(index_html, index_path, append = FALSE)

  render("index.Rmd")

}
