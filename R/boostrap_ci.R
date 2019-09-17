#' Bootstrap confidence interval
#'
#' @param statistic
#' @param simulator
#' @param tboots
#' @param B
#' @param t.hat
#' @param level
#'
#' @return A confidence interval that was generated

#'
#' @examples
#'
#'


bootstrap_ci <- function(statistic = NULL, simulator = NULL, tboots = NULL,
                         B = if (!is.null(tboots)) {ncol(tboots)}, t.hat, level) {
    resample <- function(x) {
        sample(x, replace = TRUE)
    }

    resample.data.frame <- function(data) {
        sample.rows <- resample(1:nrow(data))
        return(data[sample.rows, ]) }

    rboot <- function(statistic, simulator, B) {
        tboots <- replicate(B, statistic(simulator())) if (is.null(dim(tboots))) {
            tboots <- array(tboots, dim = c(1, B)) }
        return(tboots)
    }

    bootstrap <- function(tboots, summarizer, ...) {
        summaries <- apply(tboots, 1, summarizer, ...)
        return(t(summaries))
    }

    equitails <- function(x, alpha) {
        lower <- quantile(x, alpha/2)
        upper <- quantile(x, 1 - alpha/2)
        return(c(lower, upper))
    }

    if (is.null(tboots)) {
        stopifnot(!is.null(statistic)) stopifnot(!is.null(simulator)) stopifnot(!is.null(B))
        tboots <- rboot(statistic, simulator, B)
    }

    alpha <- 1 - level
    intervals <- bootstrap(tboots, summarizer = equitails, alpha = alpha)
    upper <- t.hat + (t.hat - intervals[, 1])
    lower <- t.hat + (t.hat - intervals[, 2])
    CIs <- cbind(lower = lower, upper = upper)
    return(CIs)
}


## Simulator
resamp.resids.cats <- function(){

    resids = residuals(cats.lm)
    newResids = sample(resids, replace=TRUE)

    # resample the residuals from the original model
    newCats = data.frame(Bwt = fatcats$Bwt,
                         Hwt = fitted(cats.lm) + newResids) # create a new dataframe
    return(newCats)
}

# with the original x's but new y's
## Estimator

cats.lm = lm(Hwt ~ 0+Bwt,data=fatcats)
summary(cats.lm)

fitCats <- function(newCats) coef(lm(Hwt~0+Bwt, data=newCats)) # get the coef from OLS fitCats(fatcats) # test the above on original data, should give same coef

cisPara = bootstrap.ci(statistic = fitCats,
                       simulator = resamp.resids.cats,
                       B = 1000, t.hat = fitCats(fatcats), level = 0.95)
