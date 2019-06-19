#' # calculate mean, length (i.e., n), and sem of df
# dv = variable to be aggregated
# iv = aggregating variables
#' @title plotting in a proper manner
#' @name aggregate_m_n_sem
#' @param df the \code{df} for the aggregation
#' @param dv the dependent variable in the aggragation formula
#' @param iv the independent variables in the aggregation formula
#' @return returns a df with mean, length, and sem of dv ~ iv
#' @author Thalmann, M.
#' @export



aggregate_m_n_sem <- function(df, dv, iv){
  pars <- c("mean", "n", "sem")
  if (length(iv) > 1) {
    out <- as.data.frame(as.matrix(aggregate(df[,dv], by = as.list(df[,iv]), FUN = function(x)
      c(mean(x), length(x), sem = sd(x)/sqrt(length(x))))))
  } else {
    out <- as.data.frame(as.matrix(aggregate(as.formula(paste0(dv, "~ ", iv)), data = df, FUN = function(x)
      c(mean(x), length(x), sem = sd(x)/sqrt(length(x))))))
  }
  names(out)[(ncol(out)-2):ncol(out)] <- pars
  out[, pars] = apply(out[, pars], 2, function(x) as.numeric(as.character(x)))
  out
}




