#' windsorize values in a vector to a given percentile
#' @title trims values above thx to thx
#' @name trim_upper
#' @param x a \code{vector}
#' @param thx a float denoting the threshold above which elements should be trimmed
#' @return returns the trimmed \code{vector}
#' @author Thalmann, M.
#' @importFrom tibble tibble
#' @importFrom dplyr row_number
#' @export

trim_upper <- function(x, thx){
  x_sorted <- tibble(
    rwn = row_number(x),
    val = sort(x),
    perc = row_number(sort(x)) / length(x)
  )
  idx <- x_sorted$perc >= thx
  fill_val <- x_sorted$val[idx][[1]]
  x_sorted$val[idx] <- fill_val
  return(x_sorted$val[x_sorted$rwn])
}
