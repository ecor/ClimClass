\name{ExAtRa}
\alias{ExAtRa}
\title{Extra-Atmospheric Radiation}
\usage{
ExAtRa(DOY, latitude, Gsc = 0.082, unit = "mm", T = 12)
}
\arguments{
  \item{DOY}{day of the year.}

  \item{latitude}{latitude in degrees (negative for S
  emishpere).}

  \item{Gsc}{solar constant in MJ m-2 min-1 (default:
  0.0820).}

  \item{unit}{unit for solar radiation. Accepted values are
  "mm" and "MJ".}

  \item{T}{temperature in degrees C. Default is 12.}
}
\value{
The daily extra-atmospheric solar radiation energy,
espressed either in MJ or in mm of evaporated water.
}
\description{
Calculates Extra-Atmospheric Radiation. Called by function
\code{\link{arid}} for Thornthwaite's index.
}
\details{
If \code{unit} = "mm", the calculated value represents the
water height evaporated by solar radiation, calculated by
the latent heat for vaporization. Otherwise (\code{unit} =
"MJ") output is the solar radiation energy in MJ.
Temperature \code{T} is used only for the assessment of
latent heat of vaporization, when \code{unit} = "mm".
}
\examples{
data(Trent_climate)
# creates a vector with middle days for every month in a year
quinci   <- paste(15,"/",1:12,"/",2014,sep="")
posixlt  <- strptime(quinci, format="\%d/\%m/\%Y")
yDay     <- posixlt$yday+1  # field yday starts from 0
latitude<-46

# generates 12 values, one for each month
coeff_rad<- ExAtRa(DOY=yDay,latitude=latitude, unit="mm")
}
\author{
Emanuele Eccel
}
\seealso{
\code{\link{arid}}
}

