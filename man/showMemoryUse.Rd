\name{showMemoryUse}
\alias{showMemoryUse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showMemoryUse(sort = "size", decreasing = FALSE, limit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sort}{
%%     ~~Describe \code{sort} here~~
}
  \item{decreasing}{
%%     ~~Describe \code{decreasing} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
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
function (sort = "size", decreasing = FALSE, limit) 
{
    objectList <- ls(parent.frame())
    oneKB <- 1024
    oneMB <- 1048576
    oneGB <- 1073741824
    memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text = x)))))
    memListing <- sapply(memoryUse, function(size) {
        if (size >= oneGB) 
            return(paste(round(size/oneGB, 2), "GB"))
        else if (size >= oneMB) 
            return(paste(round(size/oneMB, 2), "MB"))
        else if (size >= oneKB) 
            return(paste(round(size/oneKB, 2), "kB"))
        else return(paste(size, "bytes"))
    })
    memListing <- data.frame(objectName = names(memListing), 
        memorySize = memListing, row.names = NULL)
    if (sort == "alphabetical") 
        memListing <- memListing[order(memListing$objectName, 
            decreasing = decreasing), ]
    else memListing <- memListing[order(memoryUse, decreasing = decreasing), 
        ]
    if (!missing(limit)) 
        memListing <- memListing[1:limit, ]
    print(memListing, row.names = FALSE)
    return(invisible(memListing))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
