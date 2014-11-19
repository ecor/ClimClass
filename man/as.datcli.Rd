\name{as.datcli}
\alias{as.datcli}
\title{as.datcli}
\usage{
as.datcli(df, station = "T0009", MonthField = "month", PrecField = "P",
  MinTempField = "Tn", MaxTempField = "Tx", MeanTempField = "Tm",
  AbsMinTempField = "AbsTn", AbsMinTempOffset = 4,
  StationField = "station")
}
\arguments{
  \item{df}{data frame or a list}

  \item{station}{name of the considered station}

  \item{MonthField}{character string for month field in
  \code{df}. Default is \code{"month"}.}

  \item{PrecField}{character string for Mean Precipitation
  field in \code{df}. Default is \code{"P"}.}

  \item{MinTempField}{character string for Mean Daily
  Minimum Temperature field in \code{df}. Default is
  \code{"Tn"}.}

  \item{MaxTempField}{character string for Mean Daily
  Maximum Temperature field in \code{df}. Default is
  \code{"Tx"}.}

  \item{MeanTempField}{character string for Mean Daily
  Maximum Temperature field in \code{df}. Default is
  \code{"Tm"}.}

  \item{AbsMinTempField}{character string for Absolute
  Monthly Minimum Temperature field in \code{df}. Default
  is \code{"AbsTn"}.}

  \item{AbsMinTempOffset}{estimated offset between Average
  Min Temperature and Absolute Max Temperature.}

  \item{StationField}{character string for Station field in
  \code{df}. Default is \code{"station"}.}
}
\description{
Tranforms a Data Frame (see example dataset) into a data
frame format like 'datcli' in 'climatol' package
}
\examples{
library(climatol)

data(Trent_climate)
TrentinoClimateDf <- melt(clima_81_10,id=names(clima_81_10[[1]]))
names(TrentinoClimateDf)[names(TrentinoClimateDf)=="L1"] <- "station"


station <- "T0129"
datcli <- as.datcli(TrentinoClimateDf,station=station)
diagwl(datcli,est=station,alt=100,per="Period",mlab="en") ## plots a Walter-Lieth's climograph
}
\seealso{
\url{http://www.climatol.eu/},
\url{http://www.zoolex.org/walter.html}
}

