#' Binary Calibration Plots
#'
#' @param response The response variable from the data
#' @param model A model object
#' @param breaks The number of bins used to create the error bars that will be displayed
#' @param point.color The color of the points in the scatterplot
#' @param line.color The color of the line y = x
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' ch <- read.csv("http://www.stat.cmu.edu/~cshalizi/uADA/15/hw/06/ch.csv")
#' ch.clean <- na.omit(ch)
#' lr.ch <- glm(start ~ . -country -year +I(exports^2), data=ch, family="binomial") # logistic regression
#' binary_calibration_plot(y = ch.clean$start, model = lr.ch, breaks = 0:7/10, point.color = "blue", line.color = "red")
#' library(mgcv)
#' gam.ch <- gam(start~s(exports)+s(schooling)+s(growth)+s(peace)+s(concentration)+s(lnpop)+s(fractionalization)+dominance, # GAM
#' binary_calibration_plot(ch.clean$start, gam.ch, breaks=0:9/10, blue, red)


binary_calibration_plot <- function(response, model, breaks = 0:10/10,
                                    point.color='blue', line.color='red') {
    library(ggplot2)

    # create predictions for our response variable
    fitted.probs = predict(model, type = "response")

    # create bins for the fitted values following the number of breaks
    ind = cut(fitted.probs, breaks)

    # Find the mean response for each of the bins
    freq = tapply(y, ind, mean)

    # find the average probability for the fitted probabilities
    ave.prob = tapply(fitted.probs, ind, mean)

    # Calculate the standard error from the fitted probabilities for each bin
    se = sqrt(ave.prob * (1 - ave.prob) / table(ind))

    # create a data.frame that holds our calculations
    df = data.frame(freq, ave.prob, se)

    g <- ggplot(df, aes(ave.prob, freq)) +
        geom_point(color = point.color) +
        geom_abline(slope = 1, intercept = 0, color = line.color) +
        xlab("Average Predicted Probability") +
        ylab("Observed Frequency") +
        geom_errorbar(ymin = ave.prob - 1.96 * se,
                      ymax = ave.prob + 1.96 * se) +
        xlim(0,1) +
        ylim(0,1) +
        geom_rug(aes(x = fitted.probs, y = fitted.probs),
                 data.frame(fitted.probs), sides = 'b')+
        theme_minimal()

    return(g)
}



