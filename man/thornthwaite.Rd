\name{thornthwaite}
\alias{thornthwaite}
\title{Thornthwaite and Mather's water balance}
\usage{
thornthwaite(series, latitude, clim_norm = NULL, first.yr = NULL,
  last.yr = NULL, quant = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
  snow.init = 20, Tsnow = -1, TAW = 100, fr.sn.acc = 0.95,
  snow_melt_coeff = 1)
}
\arguments{
  \item{series}{the monthly series of temperature and
  precipitation.}

  \item{latitude}{latitude of the station in degrees.}

  \item{clim_norm}{climatic normals.}

  \item{first.yr}{first year of the period over which water
  balance is calculated. Default is \code{NULL}
  (calculations start with the first year of the series).}

  \item{last.yr}{last year of the period over which water
  balance is calculated. Default is \code{NULL}
  (calculations stop with the last year of the series).}

  \item{quant}{vector of quantiles for which water balance
  has to be assessed. Default is: min, 10th, 25th 50th,
  75th, 90th, max.}

  \item{snow.init}{initial water equivalent for snowpack
  (mm). Default is 20.}

  \item{Tsnow}{maximum temperature (monthly mean) for
  precipitation to be treated as snowfall. Default is -1
  degree C.}

  \item{TAW}{maximum (field capacity) for soil water
  retention, and initial soil water content (mm). Default
  is 100.}

  \item{fr.sn.acc}{fraction of snow that contributes to
  snowpack (0-1). 1 - fr.sn.acc is treated as liquid
  monthly precipitation Default is 0.95.}

  \item{snow_melt_coeff}{monthly coefficient(s) for
  snowmelt. Default is 1.}
}
\value{
A \code{thornthwaite} S3 object, consisting on a list of
two lists. The first (name: W_balance) is a list of data
frames containing the monthly series of all indices, the
second (name: quantiles) the relevant quantiles. See
details for meanings of single variables.
}
\description{
Calculates Thornthwaite and Mather's water balance from
monthly series of precipitation and temperature. Aimed at a
classification of a site's climate according to its water
balance features.
}
\details{
The algorithm for the calculation of water balance is
adapted from Thornthwaite, 1948; Thornthwaite and Mather,
1955; Thornthwaite and Mather, 1957.

\code{series} is a data frame with years, months,
temperature and precipitation values. Names in series
columns must include: year, month, Tn and Tx (minimum and
maximum temperatures, respectively) or, as an alternative,
Tm (mean temperatures), and P (mandatory).

\code{clim_norm} is a monthly data frame of climate
normals, with column names: "P", "Tn", "Tx", "Tm"
(precipitation, minimum, maximum and mean temperature,
respectively). It can be the output of function
\code{\link{climate}}. If \code{clim_norm} is not NULL, any
missing value in the monthly series is substituted by the
corresponding climatic value in \code{clim_norm}.

At any winter season, the maximum monthly snowpack height
is attained in the last month before "spring" conditions
(\code{Tm} >= \code{Tsnow}), even if a month with Tm <
Tsnow may occur later.

\code{snow_melt_coeff} is (are) the coefficient(s) for snow
melt fraction(s) at any month where the condition for
melting exists. If \code{snow_melt_coeff} = 1 (default),
all the melting occurs in the first month when \code{Tm >=
Tsnow}; if it is a vector, melting is spread over more than
one month. If the sum of coefficients is less than 1, the
residual melting occurs in one further month.

The output function is a list of two lists of data frames
(balance and quantile). In both lists, data frame (and
names) are the following (all variables in mm):

\code{Precipitation} (repeats input values);

\code{Et0} (potential evapotranspiration);

\code{Storage} (water stored in soil);

\code{Prec. - Evap.} (difference between precipitation and
potential evapotranspiration);

\code{Deficit} (difference between potential and real
evapotranspiration, due to water unavailability in soil);

\code{Surplus} (water surplus in soil, routed to runoff).

Please, refer to the quoted references for details.

This function requires the function \code{\link{daylength}}
(libr. \code{\link{geosphere}}).
}
\examples{
data(Trent_climate)


# lista_cli is a list of data frames of the type "series",
# each one referring to one station - see function "climate".
# clima_81_10 is a list of data frames having climatic means
# of temperature and precipitation, each one referring to one station.
# It can be the output of function "climate".
library(geosphere) # required for function daylength
thornt_lst<-NULL
lista_cli <- lista_cli[1:3] ## lista_cli is reduced to diminish elapsed time of execution!
for(k in 1 : length(lista_cli[1:3])) {
  thornt_lst[[k]]<-thornthwaite(series=lista_cli[[k]],
  clim_norm=clima_81_10[[k]],
  latitude = 46, first.yr=1981,
  last.yr=2010, snow_melt_coeff=c(0.5,0.5 )  )
}
names(thornt_lst)<-names(lista_cli)

# splits list into two lists
W_balance<-NULL; quantiles<-NULL
for(k in 1 : length(lista_cli))
{
  W_balance[[k]]<-thornt_lst[[k]]$W_balance
  quantiles[[k]]<-thornt_lst[[k]]$quantiles
 }
 names(W_balance)<-names(thornt_lst); names(quantiles)<-names(thornt_lst)
}
\author{
Giambattista Toller and Emanuele Eccel
}
\references{
Thornthwaite, C. W., 1948: An Approach toward a Rational
Classification of Climate. Geographical Review, Vol. 38,
No. 1(Jan.):55-94.

Thornthwaite, C. W., and Mather, J.R., 1955: The water
balance.  Publications in Climatology, Volume 8(1),
Laboratory of Climatology

Thornthwaite, C. W., and Mather, J.R., 1957: Instructions
and tables for computing potential evapotranspiration and
the water balance.  Publications in climatology, Volume
10(3), Laboratory of Climatology
}
\seealso{
\code{\link{climate}}, \code{\link{ExAtRa}},
\code{\link{plot.thornthwaite}}
}

