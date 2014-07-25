\name{all_debugged}
\alias{all_debugged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
all_debugged(where = search(), show_empty = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{where}{
%%     ~~Describe \code{where} here~~
}
  \item{show_empty}{
%%     ~~Describe \code{show_empty} here~~
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
function (where = search(), show_empty = FALSE) 
{
    ss <- setNames(lapply(where, function(x) {
        which_debugged(ls(x, all.names = TRUE))
    }), gsub("package:", "", where))
    ns <- unlist(sapply(gsub("package:", "", where), function(x) {
        if (inherits({
            n <- try(getNamespace(x), silent = TRUE)
        }, "try-error")) 
            NULL
        else x
    }))
    ss_ns <- setNames(lapply(ns, function(x) {
        objects <- ls(getNamespace(x), all.names = TRUE)
        which_debugged(objects, ns = x)
    }), ns)
    if (!show_empty) {
        ss <- ss[sapply(ss, length) > 0]
        ss_ns <- ss_ns[sapply(ss_ns, length) > 0]
    }
    for (i in names(ss)) ss_ns[[i]] <- setdiff(ss_ns[[i]], ss[[i]])
    list(env = ss, ns = ss_ns)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
