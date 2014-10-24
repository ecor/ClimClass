\name{Trent_climate} 
\docType{data} 
\alias{Trent_climate} 
\title{Data set of Trentino climate} 
\description{Data set for definition of climate of Trentino, Italy. It includes  monthly series of temperature and precipitation, and reference tables for definition of aridity and continentality / oceanicity. 
} 
\usage{data(Trent_climate)}
\format{
\code{lista_cli} a list of 40 data frames (one for each station), with monthly time series of precipitation and temperature (minimum and maximum).

\code{clima_81_10} a list (one table for each station) of 40 monthly climatic normals of precipitation and temperature (minimum, maximum, and mean) for the climatic period 1981 - 2010. It has been calculated by function \code{\link{climate}}.

\code{thornth_list} a "hyperlist" (list of lists of lists), one list of lists for each station. For every station, the first list (\code{Thornth._W._bal}) reports the monthly series of water balance quantities for the station, each in one data frame (see function thornthwaite for details). The second list (\code{quantiles}) reports the monthly quantiles for the same quantities.
 
\code{W_balance} is the first list (\code{W_balance}) in \code{thornth_list} organized according to stations. See Examples in function \code{\link{thornthwaite}} for its construction.

\code{quantiles} is the second list (\code{quantiles}) in \code{thornth_list} organized according to stations. See Examples in function \code{\link{thornthwaite}} for its construction.

\code{coord_elev} is a data frame of coordinates and elevation for each station in the data set. Fields are: station id, northing (degrees), easting (degrees), elevation (m).

\code{arid_ind_tables} is a list formed by six data frames. Used for reference in aridity indices assessment (see function \code{\link{arid}} and references for data sources).

\code{continental_ind_tables} is a list formed by three data frames. Used for reference in continentality / oceanicity indices assessment (see function \code{\link{contin}} and references for data sources).

}

\source{
Series like "Txxxx" were supplied by the Autonomous Province of Trento - Meteotrentino (I). Series like "FEMxx" were supplied by Fondazione Edmund Mach, San Michele all'Adige (I).} 
\references{} References to secondary sources.
\examples{} Examples of how to use the data object. Load or
display or plot, possibly.
\keyword{datasets}