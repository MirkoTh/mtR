#' extract coefficients from fitted glmnet object into matrix
#' @title extract coefficients easily
#' @name extractcoefs_glmnet
#' @param fit an S3 class \code{glmnet} object
#' @return returns a {data frame} listing independent variables, related log(lambda), and coefficients/beta weights
#' @author Thalmann
#' @importFrom glmnet nonzeroCoef
#' @importFrom reshape melt
#' @importFrom utils globalVariables
#' @export

extractcoefs_glmnet = function(fit){
  which <- nonzeroCoef(fit$beta)
  nwhich <- length(which)
  df <- as.data.frame(as.matrix(fit$beta[which, , drop = FALSE]))
  names(df) <- prettyNum(log(fit[["lambda"]]), digits = 4)
  df[,ncol(df)+1] <- rownames(df)
  df.r <- melt(df)
  names(df.r) <- c("iv", "lambda_log", "beta")
  df.r$lambda_log <- as.numeric(as.character(df.r$lambda_log))
  df.r
}
