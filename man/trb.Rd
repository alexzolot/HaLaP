\name{trb}
\alias{trb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function trb..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trb(x = NULL, max.lines = getOption("deparse.max.lines"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{max.lines}{
%%     ~~Describe \code{max.lines} here~~
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
function (x = NULL, max.lines = getOption("deparse.max.lines")) 
{
    if (is.null(x) && (exists(".Traceback", envir = baseenv()))) 
        x <- get(".Traceback", envir = baseenv())
    else if (is.numeric(x)) 
        x <- .Internal(traceback(x))
    n <- length(x)
    if (n == 0L) 
        cat(gettext("No traceback available"), "\n")
    else {
        for (i in 1L:n) {
            label <- paste0(n - i + 1L, ": ")
            m <- length(x[[i]])
            if (!is.null(srcref <- attr(x[[i]], "srcref"))) {
                srcfile <- attr(srcref, "srcfile")
                x[[i]][m] <- paste0(x[[i]][m], " at ", basename(srcfile$filename), 
                  "#", srcref[1L])
            }
            if (m > 1) 
                label <- c(label, rep(substr("          ", 1L, 
                  nchar(label, type = "w")), m - 1L))
            if (is.numeric(max.lines) && max.lines > 0L && max.lines < 
                m) {
                cat(paste0(label[1L:max.lines], x[[i]][1L:max.lines]), 
                  sep = "\n")
                cat(label[max.lines + 1L], " ...\n")
            }
            else cat(paste0(label, x[[i]]), sep = "\n")
        }
    }
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
