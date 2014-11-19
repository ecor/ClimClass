\name{climate}
\alias{climate}
\title{Climate normals}
\usage{
climate(series, first.yr = NULL, last.yr = NULL, max.perc.missing)
}
\arguments{
  \item{series}{the monthly series of temperature and
  precipitation.}

  \item{first.yr}{first year of the period over which
  climatology is calculated}

  \item{last.yr}{last year of the period over which
  climatology is calculated}

  \item{max.perc.missing}{maximum acceptable percentage of
  missing data in the averaging period from \code{first.yr}
  to \code{last.yr} (0-99).}
}
\value{
A data frame with climatic monthly values of:
precipitation, minimum and maximum temperatures (if
existing in series), mean temperature (either averaged from
existing values in series, or calculated by the function as
(Tn + Tx)/2), absolute minimum monthly temperature.
}
\description{
Creates climate mean monthly values from a monthly series
of temperature and precipitation.
}
\details{
\code{series} is a data frame with years, months,
temperature (and precipitation) values. Names in series
columns must include: year, month, Tn and Tx (minimum and
maximum temperatures, respectively) or, as an alternative,
Tm (mean temperatures).

If \code{first.yr} or \code{last.yr} are NULL (default),
the lowest and highest values in series are taken as the
period.
}
\examples{
data(Trent_climate)

# clima_81_10 is a list of data frames of the type series,
# each one referring to one station
# having climatic means of temperature and precipitation

clima_81_10<-lapply(lista_cli, FUN=climate, first.yr=1981, last.yr=2010, max.perc.missing=15)
}
\author{
Emanuele Eccel
}

