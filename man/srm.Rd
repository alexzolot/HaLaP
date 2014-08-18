\name{srm}
\alias{srm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function srm..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
srm(..., list = character(0), lib = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache"), lib.loc = Sys.getenv("R_LOCAL_LIB_LOC", unset = "."), remove = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{list}{
%%     ~~Describe \code{list} here~~
}
  \item{lib}{
%%     ~~Describe \code{lib} here~~
}
  \item{lib.loc}{
%%     ~~Describe \code{lib.loc} here~~
}
  \item{remove}{
%%     ~~Describe \code{remove} here~~
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
function (..., list = character(0), lib = Sys.getenv("R_LOCAL_CACHE", 
    unset = ".R_Cache"), lib.loc = Sys.getenv("R_LOCAL_LIB_LOC", 
    unset = "."), remove = TRUE) 
{
    if (class((.x <- substitute(lib))) == "name") 
        lib <- deparse(.x)
    else lib <- lib
    if (!(file.exists(lib.loc) && file.info(lib.loc)$isdir)) 
        stop(lib.loc, " is not an existing directory.", call. = FALSE, 
            domain = NA)
    path <- file.path(lib.loc, lib)
    if (file.exists(path) && !file.info(path)$isdir) 
        stop(path, " exists but is not a directory!", call. = FALSE, 
            domain = NA)
    if (m <- match(path, .pathAttributes(), nomatch = FALSE)) {
        e <- as.environment(m[1])
        if (!is.null(n <- attr(e, "readonly")) && n) 
            stop(path, " is attached as read only!", call. = FALSE, 
                domain = NA)
    }
    nam <- list
    if (!is.null(m <- match.call(expand.dots = FALSE)$...)) 
        nam <- c(nam, sapply(m, function(x) switch(class(x), 
            name = deparse(x), call = {
                o <- eval(x, envir = parent.frame(n = 4))
                if (!is.character(o)) stop("non-character name!", 
                  call. = FALSE, domain = NA)
                o
            }, character = x, stop("garbled call to 'Store'", 
                call. = FALSE, domain = NA))))
    if (length(nam) == 0) 
        return()
    nam <- drop(sort(unique(nam)))
    if (file.exists(path)) {
        if (!file.info(path)$isdir) 
            stop(path, " exists but is not a directory!", call. = FALSE, 
                domain = NA)
        fils <- .mostFiles(path)
        if (any(i <- grep("@\\.RData$", fils, invert = TRUE))) {
            warning(paste("Converting", path, "from old filename format to new"), 
                call. = FALSE, domain = NA)
            newF <- .enc(.dec(fils[i]))
            for (j in i) file.rename(file.path(path, fils[j]), 
                file.path(path, newF[j]))
        }
    }
    else dir.create(path)
    for (n in nam) {
        no <- !exists(n, inherits = FALSE, envir = parent.frame())
        comm <- if (no) 
            substitute({
                assign(N, get(N))
                save(list = N, file = FILE)
                rm(list = N)
            }, list(N = n, FILE = file.path(path, .enc(n))))
        else substitute({
            save(list = N, file = FILE)
        }, list(N = n, FILE = file.path(path, .enc(n))))
        eval.parent(comm)
    }
    pos <- if (any(m <- (.pathAttributes() == path))) {
        m <- which(m)[1]
        detach(pos = m)
        m
    }
    else 2
    .attach(path, pos = pos, warn = FALSE, readonly = FALSE)
    if (remove) {
        o <- intersect(eval.parent(quote(objects(all.names = TRUE))), 
            nam)
        if (length(o) > 0) 
            eval.parent(substitute(remove(list = O), list(O = o)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
