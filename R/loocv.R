#' Leave one out cross validation
#'
#' @param model An R object, typically returned by lm or glm.
#'
#' @return Cross validation risk error
#' @export
#'
#' @examples
loocv <- function(model) {
    return(mean((residuals(model) / (1 - hatvalues(model)))^2))
}
