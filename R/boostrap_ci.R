#' Bootstrap confidence interval
#'
#' @param statistic
#' @param simulator
#' @param tboots
#' @param B
#' @param t.hat
#' @param level The specified confidence level
#'
#' @return A confidence interval that was generated

#'
#' @examples
#'

# TODO add @param and example
bootstrap_ci <- function(statistic = NULL, simulator = NULL, tboots = NULL,
                         B = if (!is.null(tboots)) {ncol(tboots)}, t.hat, level) {

    # Takes a sample with replacement for the given data
    resample <- function(x) {
        sample(x, replace = TRUE)
    }

    # Returns the rows from the dataframe that were chosen to be resampled
    resample.data.frame <- function(data) {
        sample.rows <- resample(1:nrow(data))
        return(data[sample.rows, ]) }

    # Generates B bootstrap samples (using the simulator function) and
    # calculates the statistic on them (using statistic). Simulator needs to be
    # a function which returns a surrogate data set in a form suitable for
    # statistic.
    rboot <- function(statistic, simulator, B) {

        tboots <- replicate(B, statistic(simulator()))

        if (is.null(dim(tboots))) {
            tboots <- array(tboots, dim = c(1, B))
        }

        return(tboots)
    }

    # Takes the output of rboot (or tboots if specified) and applies
    # the summarizing function.
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
        stopifnot(!is.null(statistic))
        stopifnot(!is.null(simulator))
        stopifnot(!is.null(B))
        tboots <- rboot(statistic, simulator, B)
    }

    alpha <- 1 - level
    intervals <- bootstrap(tboots, summarizer = equitails, alpha = alpha)
    upper <- t.hat + (t.hat - intervals[, 1])
    lower <- t.hat + (t.hat - intervals[, 2])
    CIs <- cbind(lower = lower, upper = upper)
    return(CIs)
}


test_mean <- 5
test_sd <- 2
make_data <- function(){rnorm(100, mean = test_mean)}
make_data <- function(){iris$Sepal.Length}

bootstrap_ci(statistic = mean, simulator = make_data, B = 100, t.hat = test_mean, level = 0.95)
bootstrap_ci(statistic = mean, simulator = make_data, B = 100, t.hat = test_mean, level = 0.95)


