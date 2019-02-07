\name{na_null_inf_Filler}
\alias{na_null_inf_Filler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Imputing
}
\description{
Function to replace 'NA', 'null','Null', 'Inf' entries in a matrix or data.frame by the 'Mean', 'Minimum' or 'Maximum' value of the respective field.
}
\usage{
na_null_inf_Filler(dat, id_field, fill_with)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix or data.frame
}
 \item{dat}{
A matrix or data.frame
}
  \item{id_field}{
[TRUE or FALSE] Whether or not the first field of the matrix (or data.frame) is the unique (id) field The default is FALSE.
}
  \item{fill_with}{
["Mean", "Min" or "Max"] Value to fill to replace the entry with. The default is "Mean" of the respective field.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{datF}{A matrix or data.frame}
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
na_null_inf_Filler(GM_crime_data, id_field = FALSE, fill_with = "Mean")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
