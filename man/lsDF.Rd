\name{lsDF}
\alias{lsDF}
\title{lsDF}
\usage{
lsDF(.all = FALSE, ...)
}
\description{
list of data.frames
}
\examples{
lsDF(FALSE); a= lsDF(TRUE); hee(a); if(nrow(a)>0)srt(a, ~  + class + ds - size)
}

