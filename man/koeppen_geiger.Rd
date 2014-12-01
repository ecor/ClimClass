\name{koeppen_geiger}
\alias{koeppen_geiger}
\title{Koeppen - Geiger's climate classification}
\usage{
koeppen_geiger(clim_norm, A_B_C_special_sub.classes = FALSE)
}
\arguments{
  \item{clim_norm}{average values (climate normals) for the
  desired period.}

  \item{A_B_C_special_sub.classes}{logical. Sets if
  calculations have to consider sub-classes based on rain
  features in climate types A, B, and C (see details).
  Default is \code{FALSE}.}
}
\value{
A one-line data frame reporting a resume of climatic
features useful for the classification, and one last field
(1 type - or "climate" - plus 1 or 2 sub-types) reporting
Koeppen - Geiger's climate classification. See details.
}
\description{
General climate classification after Koeppen - Geiger.
}
\details{
\code{clim_norm} is a monthly data frame of climate
normals, with column names: "P", "Tn", "Tx", "Tm"
(precipitation, minimum, maximum and mean temperature,
respectively). It can be the output of function
\code{\link{climate}}.

Koeppen - Geiger's classification is based on Trewartha and
Lyle, 1980. The function also holds for Southern emisphere,
except for the "Gange" sub-type ("Ag" and "Cg"). Type "H"
(highland climate) and sub-types "Bn" and "Cn" (where n
stands for Nebel) are never attributed, being based on a
qualitative description in the quoted reference.

Sub-type "w" (wet-and-dry) or "m" (monsoon) in climate "A"
is set according to the definition after Encyclopaedia
Britannica
(\url{http://www.britannica.com/EBchecked/topic/322068/Koppen-climate-classification}):
if P in the 4 driest months is less than 1/5 of the wettest
months and if both the 4 driest and wettest months are
split over non-contiguous seasons (either 2 months per
season or 1 and 3 months per season), then sub-type is "".

For climate "A", the letter "m" is attributed to the first
sub-type.

Climates "Cx" have P[May + June] >= 1.3 P [Aug. + Sept.] in
N emisphere, and P[Nov. + Dec.] >= 1.3 P[Febr. + March] in
S emisphere.

\code{A_B_C_special_sub.classes}, if \code{TRUE}, adds a
letter to the second sub-type of climates: "i" or "g"
(climate A), "w" or "s" (climate B), and "i", "g", or "x"
(climate C).

The returned data frame contains following fields:

\code{T_w.m} = temperature of the warmest month (degrees C)

\code{T_c.m} = temperature of the coldest month (degrees C)

\code{T_avg} = average temperature (degrees C)

\code{P_tot} = total precipitation depth (mm)

\code{P_wint} = precipitation depth in the 6 coldest
(winter) months (mm)

\code{P_summ} = precipitation depth in the 6 warmest
(summer) months (mm)

\code{P_d.m} = precipitation depth in the driest month (mm)

\code{P_d.m.summ} = precipitation depth in the driest month
of "summer" half of the year (mm)

\code{P_d.m.wint} = precipitation depth in the driest month
of "winter" half of the year (mm)

\code{T_4th_w.m} = temperature of the 4th warmest month
(degrees C)

\code{class} = climatic class, resulting from the merging
of "climate" (A to E) and sub-type(s)
}
\examples{
data(Trent_climate)
# clima_81_10 is a list of data frames having climatic means of temperature and precipitation as
# required by Koeppen - Geiger classification, each one referring to one station.
# It can be the output of function climate.
class_clim_l<-lapply(clima_81_10, FUN=koeppen_geiger, A_B_C_special_sub.classes=TRUE)
}
\author{
Emanuele Eccel
}
\references{
Trewartha, G.T. and Lyle, H.H., 1980: An Introduction to
Climate. MacGraw - Hill, 5th Ed. Appendix: Koeppen's
Classification of Climates.
}
\seealso{
\code{\link{climate}}
}

