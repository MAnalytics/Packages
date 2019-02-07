\name{rates_to_props}
\alias{rates_to_props}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert count or rates to proportion
}
\description{
Function to convert counts or rates to proportion
}
\usage{
rates_to_props(rates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{rates}{
A matrix or data.frame
}
  \item{id_field}{
[TRUE or FALSE] Whether or not the first field of the matrix (or data.frame) is the unique (id) field The default is FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{props}{A matrix or data.frame of proportions}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Monsuru Adepeju
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
rates_to_props(GM_crime_data, id_field = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
