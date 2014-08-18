\name{fa}
\alias{fa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function fa..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fa(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{levels}{
%%     ~~Describe \code{levels} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{exclude}{
%%     ~~Describe \code{exclude} here~~
}
  \item{ordered}{
%%     ~~Describe \code{ordered} here~~
}
  \item{nmax}{
%%     ~~Describe \code{nmax} here~~
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
function (x = character(), levels, labels = levels, exclude = NA, 
    ordered = is.ordered(x), nmax = NA) 
{
    if (is.null(x)) 
        x <- character()
    nx <- names(x)
    if (missing(levels)) {
        y <- unique(x, nmax = nmax)
        ind <- sort.list(y)
        y <- as.character(y)
        levels <- unique(y[ind])
    }
    force(ordered)
    exclude <- as.vector(exclude, typeof(x))
    x <- as.character(x)
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    if (!is.null(nx)) 
        names(f) <- nx
    nl <- length(labels)
    nL <- length(levels)
    if (!any(nl == c(1L, nL))) 
        stop(gettextf("invalid 'labels'; length \%d should be 1 or \%d", 
            nl, nL), domain = NA)
    levels(f) <- if (nl == nL) 
        as.character(labels)
    else paste0(labels, seq_along(levels))
    class(f) <- c(if (ordered) "ordered", "factor")
    f
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
