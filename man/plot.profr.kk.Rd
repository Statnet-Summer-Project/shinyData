\name{plot.profr.kk}
\alias{plot.profr.kk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.profr.kk(x, xlab = NULL, ..., minlabel = 0.1, angle = 0, xlim = range(x$start, x$end))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{minlabel}{
%%     ~~Describe \code{minlabel} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
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
function (x, xlab = NULL, ..., minlabel = 0.1, angle = 0, xlim = range(x$start, 
    x$end)) 
{
    plot(1, 1, xlim = xlim, ylim = range(x$level) + c(-0.5, 0.5), 
        type = "n", ..., xlab = xlab, ylab = "level")
    rect(x$start, x$level - 0.5, x$end, x$level + 0.5, ...)
    labels <- subset(x, time > max(time) * minlabel)
    if (nrow(labels) > 0) 
        text(labels$start, jitter(labels$level, factor = 2), 
            labels$f, pos = 4, srt = angle, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
