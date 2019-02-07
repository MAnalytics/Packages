\name{remove_all_whiteSpaces}
\alias{remove_all_whiteSpaces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove any whitespaces in a matrix or data.frame
}
\description{
Function to remove any whitespaces in a matrix or data.frame
}
\usage{
remove_all_whiteSpaces(dat, head = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix or data.frame
}
  \item{head}{
[TRUE or FALSE] Whether or not the matrix (or data.frame) contains column headings [default TRUE]
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{dat_Cleaned}{A matrix or data.frame}
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
remove_all_whiteSpaces(GM_crime_data)
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
