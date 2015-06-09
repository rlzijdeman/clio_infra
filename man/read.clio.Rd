\name{read.clio}
\alias{read.clio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.clio(file, shape = "wide", value = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The downloaded .xlsx file from Clio-infra
}
  \item{shape}{
The shape in which you want to download the file: "wide" for the original format (with years as columns), or "long", with years as rows
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data.frame
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Richard L. Zijdeman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Not run:
## install_github("rlzijdeman/clio_infra") # for latest version
## library(clio.infra)
## df <- read.clio(file = "./data/source/gdp/data/GDP Per Capita.xlsx",
##             shape = "long",
##           value = "GDPpC")



## The function is currently defined as
function (file, shape = "wide", value = NA) 
{
    clio.raw <- read.xlsx(file, sheetIndex = 1, startRow = 3, 
        encoding = "UTF-8")
    names(clio.raw) <- tolower(names(clio.raw))
    names(clio.raw) <- gsub("[.]{2}", ".", names(clio.raw))
    names(clio.raw) <- gsub("x", "year.", names(clio.raw))
    if (shape == "long") {
        clio.dt <- data.table(clio.raw)
        clio.dt.molten <- reshape2::melt(clio.dt, id.vars = c("code", 
            "continent.region.country"), measure.vars = 3:dim(clio.dt)[2], 
            variable.name = "year", value.name = value)
        clio.dt.molten[, `:=`(year, as.numeric(gsub("year.", 
            "", year)))]
    }
    else {
        if (shape == "wide") {
            return(data.table(clio.raw))
        }
        else {
            stop("Invalid shape specification. Choose 'wide' or 'long'.")
        }
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clio_infra }
