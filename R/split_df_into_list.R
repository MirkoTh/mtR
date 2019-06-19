#' Default ggplot2 plotting scheme of Mirko Thalmann
#' @title split df into list with n_lists elements. remaining rows are put into nth list element.
#' @name split_df_into_list
#' @param x a \code{data.frame}
#' @param n_lists number of required list elements.
#' @return returns a list with n_lists list elements
#' @author Thalmann, M.
#' @export

split_df_into_list = function(x, n_lists) {
  # split df into list with equal nr of values
  # uneven values are filled with last
  n_el_list <- floor(nrow(x)/n_lists)
  n_el_covered <- n_el_list * n_lists
  evens <- rep(seq(1, n_lists), each = n_el_list)
  remainings <- rep(max(evens), nrow(x) - length(evens))
  list_nr <- c(evens, remainings)
  split(x, list_nr)
}
