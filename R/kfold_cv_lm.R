#' K-fold Cross Validation for linear models
#'
#' Returns the mean of the mean risks calculated in through k-fold cross
#' validation for each specified formula
#'
#' This function subsets the data given by the data argument into k randomly
#' assigned subsets. Each subset is then broken up into training and testing
#' data for each fold. A linear model is fit for each fold for each formula,
#' and then the MSE is calculated. Once all of the k MSEs are calculated for a
#' formula, the mean of the MSEs are returned.
#'
#' @param data The dataset you would like to run k-fold cross validation on
#' @param formulae One or more strings that are converted to a Formula object
#' @param nfolds The number of folds you would like to use, default to 5
#'
#' @return The mean of the mean risks calculated in each iteration of the cross
#' validation for each specified formula
#'
#' @examples
#' kfold_cv_lm(data = iris, formulae = "Sepal.Length ~ Sepal.Width", nfolds = 5)
#' kfold_cv_lm(data = iris, formulae = c("Sepal.Length ~ Sepal.Width",
#' "Sepal.Length ~ Petal.Length"),
#'  nfolds = 5)

kfold_cv_lm <- function(data, formulae, nfolds = 5) {

    data <- na.omit(data)

    # convert to formula object
    formulae <- sapply(formulae, as.formula)

    responses <- sapply(formulae, function(form) all.vars(form)[1])

    names(responses) <- as.character(formulae)

    n <- nrow(data)

    # assign each data point to a fold
    fold.labels <- sample(rep(1:nfolds, length.out = n))

    # Pre-allocate space fo hold the MSEs that will be calculated
    MSEs <- matrix(NA, nrow = nfolds, ncol = length(formulae))
    colnames <- as.character(formulae)

    for (fold in 1:nfolds) {

        # Split into testing and training data
        # get the row indexes for the rows that match the currect fold iteration
        test.rows <- which(fold.labels == fold)
        train <- data[-test.rows, ]
        test <- data[test.rows, ]

        for (form in 1:length(formulae)) {
            # build a lm
            current.model <- lm(formula = formulae[[form]], data = train)

            # Create predictions
            predictions <- predict(current.model, newdata = test)

            # Calculate mean square error for the test errors
            test.responses <- test[, responses[form]]
            test.errors <- test.responses - predictions
            MSEs[fold, form] <- mean(test.errors^2)
        }
    }
    # Return the mean of the MSEs
    return(colMeans(MSEs))
}

