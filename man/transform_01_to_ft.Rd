% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transform_01_to_FT.R
\name{transform_01_to_ft}
\alias{transform_01_to_ft}
\title{Transform 01 to FT}
\usage{
transform_01_to_ft(x)
}
\arguments{
\item{x}{the vector to be tested and transformed.}
}
\value{
The transformed vector if a transformation is possible.
If no transformation is possible, the original vector
returned.
}
\description{
If the vector is a 0/1 vector, it is converted to a logical one
TRUE/FALSE vector. This transformation is performed only if
the vector contains only values 0, 1, or NA. If this is not the case
returns the original variable.
This transformation can be done on numeric, string, and factor vectors.
}
\examples{
vector <- c(0, 1, 0, 1, 1, 1, 0)
transform_01_to_ft(vector)
}
\seealso{
Other vector calculations: 
\code{\link{academic_year}()},
\code{\link{clean_multiple_underscores}()},
\code{\link{interval_round}()},
\code{\link{month_name}()},
\code{\link{sum_0_1}()}

Other booleans: 
\code{\link{test_01}()}
}
\concept{booleans}
\concept{vector calculations}
