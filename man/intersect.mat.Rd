\name{intersect.mat}
\alias{intersect.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
intersect.mat(mat1, mat2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat1}{
%%     ~~Describe \code{mat1} here~~
}
  \item{mat2}{
%%     ~~Describe \code{mat2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mat1, mat2) 
{
    if (!is.matrix(mat1)) 
        mat1 <- t(as.matrix(mat1))
    if (!is.matrix(mat2)) 
        mat2 <- t(as.matrix(mat2))
    match(intersect(apply(mat2, 1, paste, collapse = "_"), apply(mat1, 
        1, paste, collapse = "_")), apply(mat2, 1, paste, collapse = "_"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
