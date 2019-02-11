\name{na_null_inf_Filler}
\alias{na_null_inf_Filler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Imputing
}
\description{
This function replaces any cells of the values 'NA', 'na', 'null','Null' or 'Inf' in a matrix or data.frame with the 'Mean', 'Minimum' or 'Maximum' value of either the column or row in which the cell is located.
}
\usage{
na_null_inf_Filler(dat, id_field, fill_with)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{dat}{
A matrix or data.frame
}
  \item{id_field}{
[TRUE or FALSE] Whether or not the first field of the matrix (or data.frame) is the unique (id) field The default is FALSE.
}
  \item{fill_with}{
["Mean_col", "Min_col", "Max_col, "Mean_row", "Min_row" or "Max_row"] Value to replace ['NA', 'na', 'null','Null', 'Inf'] with. The default is "Mean_col", meaning to imput the average values of the field in which the cell is located.
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
Monsuru Adepeju; Samuel Langton
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
na_null_inf_Filler(GM_crime_data, id_field = FALSE, fill_with = "Mean_col")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
