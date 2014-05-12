# install and load packages
library(xlsx)
library(reshape2)
library(data.table)
library(ggplot2)

# Wrapper function to easily read in data in CLIO-INFRA format
# reads in .xlsx, returns data.table (not data.frame(!))
# in case of shape = "long", you can provide the name of the value variable
read.clio <- function(file, shape = "wide", value = NA){
  clio.raw <- read.xlsx(file,
                        sheetName = "Sheet1",
                        startRow  = 3,
                        encoding  = "UTF-8")
  names(clio.raw) <- tolower(names(clio.raw))
  names(clio.raw) <- gsub("[.]{2}", ".", names(clio.raw)) # replace ".." by "."
  names(clio.raw) <- gsub("x",  "year.", names(clio.raw))
  if (shape == "long") {
    clio.dt <- data.table(clio.raw)
    clio.dt.molten <- reshape2::melt(clio.dt,
                           id.vars = c("code", "continent.region.country"),
                           measure.vars = 3:dim(clio.dt)[2],
                           variable.name = "year",
                           value.name = value)
    clio.dt.molten[, year := as.numeric(gsub("year.", "", year))]
  } else {
    if (shape == "wide") {
      return(data.table(clio.raw))
    } else {
      stop("Invalid shape specification. Choose 'wide' or 'long'.")
    }
  }
}