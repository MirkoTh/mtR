#' Default folder structure of Mirko Thalmann for R projects
#' @title create folder structure
#' @name create_folder_structure
#' @return creates the following folders: R, Rmd, sql, presentations, figures
#' @author Thalmann, M.
#' @importFrom stringr str_c
#' @importFrom rmarkdown render_site
#' @importFrom lubridate now
#' @export

create_folder_structure <- function(){
  dir.create("R")
  dir.create("Rmd")
  dir.create("sql")
  dir.create("presentations")
  dir.create("figures")
}


#' create references
#' @title get href
#' @name get_href
#' @param nm the name of the reference
#' @return creates a reference to the name
#' @author Thalmann, M.
#' @importFrom stringr str_c

get_href <- function(nm){
  first <- str_c("    ", "- text: ", '"', nm, '"\n',
                 "      href: ", nm, ".html")
}

#' get main yml header
#' @title get yml header
#' @name get_yml
#' @param name_website the name of the website to be shown
#' @param title_website the title of the website to be shown
#' @param name_rmd a \code{vector} with the names of the .Rmd file
#' @return returns the yml header of .index
#' @author Thalmann, M.
#' @importFrom stringr str_c str_remove

get_yml <- function(name_website, title_website, name_rmd){
  name <- str_c("name: ", '"', name_website, '"')
  navbar <- "navbar:"
  title <- str_c("  ", "title: ", '"', title_website, '"')
  left <- str_c("  ", "left:\n")
  content <- list()
  for (i in seq_along(name_rmd)){
    content[[i]] <- get_href(str_remove(name_rmd[i], ".Rmd"))
  }
  return(c(name, navbar, title, left, content))
}

#' Create the Index File
#' @title create index
#' @name create_index
#' @param name_project the name of the project
#' @return returns the formatted name of the project with the current date
#' @author Thalmann, M.
#' @importFrom stringr str_c

create_index <- function(name_project){
  txt <- str_c("---\ntitle: ", '"', name_project, '"\n', "---\n\n",
               "Date Project: ", lubridate::now())
  return(txt)
}

#' Render the Project as a Website
#' @title render project
#' @name render_project
#' @param name_project the name of the project
#' @param name_website the name of the website to be shown
#' @param title_website the title of the website to be shown
#' @return renders the project as a website
#' @author Thalmann, M.
#' @importFrom stringr str_c
#' @importFrom rmarkdown render_site
#' @importFrom lubridate now
#' @export

render_project <- function(name_project, name_website, title_website){
  name_rmd <- dir("Rmd")[endsWith(dir("Rmd"), ".Rmd")]
  yml_lines <- get_yml(name_website, title_website, name_rmd)
  index_lines <- create_index(name_project)
  writeLines(unlist(yml_lines), con = "_site.yml")
  writeLines(index_lines, con = "index.Rmd")
  file.copy(str_c("Rmd/", name_rmd), name_rmd)
  rmarkdown::render_site()
  file.remove(name_rmd)
}
