\name{trainSupv}
\alias{trainSupv}

\title{Train a Classifier}
\description{
  Trains a classifier for supervised classification of record pairs.
}
\usage{
trainSupv(dataset, method, use.pred = FALSE, omit.possible = TRUE, convert.na = TRUE, ...)
}

\arguments{
  \item{dataset}{Object of class \code{\link{RecLinkData}}. Training data.}
  \item{method}{The classification method to use. One of \code{"svm"},
    \code{"rpart"},\code{"ada"},\code{"bagging"},\code{"nnet"}}
  \item{use.pred}{Logical. Wether to use result of previous classification 
    instead of true matching status.}
  \item{omit.possible}{Logical. Wether to remove pairs labeled as possible
    links or with unknown status.}
  \item{convert.na}{Logical. Wether to convert \code{NA}s to 0 in the
    comparison patterns.}
  \item{\dots}{Further arguments for the training method.}
}
\details{
  The given dataset is used as training data for a supervised classification.
  Either the true matching status has to be known for a sufficient number of
  data pairs or the data must have been classified previously, e.g. by using
  \code{\link{emClassify}} or \code{\link{classifyUnsup}}. In the latter case,
  argument \code{use.pred} has to be set to \code{TRUE}.
  
  The supported classifiers, as identified by \code{method} are:
  \describe{
    \item{svm}{Support vector machine, see \code{\link[e1071]{svm}}.}
    \item{rpart}{Recursive partitioning tree, see \code{\link{rpart}}.}
    \item{ada}{Stochastic boosting model, see \code{\link[ada]{ada}}.}
    \item{bagging}{Bagging with classification trees, see \code{\link[ipred]{bagging}}.}
    \item{nnet}{Single-hidden-layer neural network, see \code{\link[nnet]{nnet}}.}
  }
  Arguments in \code{...} are passed to the corresponding function.

  Most classifiers cannot handle \code{NA}s in the data, so by default these
  are converted to 0 before training.
  
  By \code{omit.possible = TRUE}, possible links or pairs with unknown status
  are excluded from the trainings set. Setting this argument to \code{FALSE}
  allows three-class-classification (links, non-links and possible links), but
  the results are usually poor.
}

\value{
  An object of class \code{RecLinkClassif} with the following components:
  \item{train}{A copy of \code{dataset}.}
  \item{model}{The model returned by the underlying training function.}
  \item{method}{A copy of the argument \code{method}}
}

\author{Andreas Borg}

\seealso{\code{\link{classifySupv}} for classifying with the trained model, 
  \code{\link{classifyUnsup}} for unsupervised classification}

\examples{
# Train a rpart decision tree with additional parameter minsplit
data(RLdata500)
pairs=compare.dedup(RLdata500, identity=identity.RLdata500,
                    blockfld=list(1,3,5,6,7))
model=trainSupv(pairs, method="rpart", minsplit=5)
summary(model)
}
\keyword{classif}