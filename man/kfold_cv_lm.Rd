% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfold_cv_lm.R
\name{kfold_cv_lm}
\alias{kfold_cv_lm}
\title{K-fold Cross Validation for linear models}
\usage{
kfold_cv_lm(data, formulae, nfolds = 5)
}
\arguments{
\item{data}{The dataset you would like to run k-fold cross validation on}

\item{formulae}{One or more strings that are converted to a Formula object}

\item{nfolds}{The number of folds you would like to use, default to 5}
}
\value{
The mean of the mean risks calculated in each iteration of the cross
validation for each specified formula
}
\description{
Returns the mean of the mean risks calculated in through k-fold cross
validation for each specified formula
}
\details{
This function subsets the data given by the data argument into k randomly
assigned subsets. Each subset is then broken up into training and testing
data for each fold. A linear model is fit for each fold for each formula,
and then the MSE is calculated. Once all of the k MSEs are calculated for a
formula, the mean of the MSEs are returned.
}
\examples{
kfold_cv_lm(data = iris, formulae = "Sepal.Length ~ Sepal.Width", nfolds = 5)
kfold_cv_lm(data = iris, formulae = c("Sepal.Length ~ Sepal.Width",
"Sepal.Length ~ Petal.Length"),
 nfolds = 5)
}
