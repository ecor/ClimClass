\name{ClimClass-package}
\alias{ClimClass-package}
\alias{ClimClass}
\docType{package}
\title{
Climate classifications and graphics
}
\description{
Classification of climate according to Koeppen - Geiger, of aridity indices, of continentality indices, and of water balance after Thornthwaite. Drawing climographs: Thornthwaite, Peguy, Bagnouls-Gaussen.
}
\details{
\tabular{ll}{
Package: \tab ClimClass\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-10-13\cr
License: \tab GPL (>= 2)\cr
}
The package collects several criteria for climate classification. The most general is Koeppen - Geiger's classification, as described in Trewartha (1980), implemented in function \code{\link{koeppen_geiger}}. Almost all sub-classes have been considered, with the only exception of those whose attribution is based on qualitative assessment of climatic features. 

A classic graphical visualization of temperature and precipitation, according to Bagnouls and Gaussen (1953), is provided by function \code{\link{bagn_gau}}. A similar, but more sophisticated representation of the same variable, is that of Walter - Lieth (Lieth et al., CD). This function is implemented in library \code{\link{climatol}}.

Function \code{\link{arid}} calculates a set of six annual aridity indices (Emberger, 1955; Lang, R., 1920; Rivas - Martinez, (website); and UNEP, 1997; De Martonne, 1925; Thornthwaite, 1948). For the latter two also a monthly index is calculated.

A set of four continentality indices is proposed by function \code{\link{contin}} (Gorczynski, L., 1920; Conrad, 1946; Gams, 1932; Rivas - Martinez, web page).

Thornthwaite's method for the assessment of soil water balance (Thornthwaite, 1948; Thornthwaite and Mather, 1955; Thornthwaite and Mather, 1957) makes use of monthly series to calculate the main quantities in water balance: evapotranspiration, soil water deficit, soil water surplus. From these series, quantiles are calculated for every month, to infer climatic features concerning soil water. Function \code{\link{thornthwaite}} provides such analysis, and function \code{\link{plot}} manages the plot of the quantiles of the relevant quantities.

The assessment of potential evapotranspiration by Thornthwaite and Mather's algorithm requires the estimation of extra-atmospheric radiation, which is calculated by function \code{\link{ExAtRa}}, based on the algorithm of Allen et al., 2005.

The data set included in the library is formed by monthly time series of temperature and precipitation from Trentino, Italy (courtesy of Autonomous Province of Trento - Meteotrentino, and of Fondazione Edmund Mach, San Michele all'Adige). Climatic normals are calculated, too (output of function \code{\link{climate}}). The output of function \code{\link{thornthwaite}} is present in the data set \code{\link{Trent_climate}}, as input for function \code{\link{plot}}. 

Reference tables for aridity and continentality indices are provided as lists, to rank the classifications on standard scales (\code{arid_ind_tables} and \code{contin_ind_tables}, respectively).

}
\author{
Emanuele Eccel, Emanuele Cordano, Giambattista Toller

Maintainer: Emanuele Eccel  <emanuele.eccel@fmach.it>

}
\references{
Allen, R.G., Walter, I.A., Elliott, R.L., Howell, T.A., Itenfisu, D., Jensen, M.E., and Snyder, R.L. (eds.), 2005: ASCE Standardized Reference Evapotranspiration Equation. 216 pp.

Bagnouls, F., and Gaussen, H., 1953: Saison seche et indice xerothermique. Docum. pour les Cartes des Prod. Veget. Serie: Generalite, 1 (1953), pp. 1-49.

Conrad, V. 1946: Usual formulas of continentality and their limits of validity. Transactions, American Geophysical Union, Volume 27, Issue 5, p. 663-664

De Martonne E., 1925: Traite de Geographie Physique: 3 tomes, Paris.

Emberger, L., 1955. Une classification biogeographique des climats. Receuil des travaux des laboratoires de botanique, geologie et zoologie de la faculte des sciences de l'universite de Montpellier (Serie Botanique), Fascicule 7, 3-43.

Gams, H., 1932. Die klimatische Begrenzung von Pflanzenarealen und die Verteilung der hygrischen Kontinentalitaet in den Alpen. Zeitschr. Ges. Erdkunde, Berlin. 

Gorczynski, L. (1920) : Sur le calcul du degre de continentalisme et son application dans la climatologie. Geografiska Annaler 2, 324-331.

Lang, R., 1920. Verwitterung und Bodenbildung als Einfuehrung in die Bodenkunde. Schweizerbart Science Publishers, Stuttgart

Lebourgeoise, F., 2010: Cours de bioclimatologie a l'usage des forestiers. Departement SIAFEE, UFR Forets, Arbres et Milieux Naturels. ENGREF, Nancy Cedex.

Lieth, H., Berlekamp, J., Fuest, S., and Riediger, S.: Walter-Lieth: Climate Diagram World Atlas, CD-Series I of Climate and Biosphere, 1st edit.

Michalet, R., and Souchier, B., 1991: Une approche synthetique biopedoclimatique del montagnes mediterraneennes: l'exemple du Maroc septemptrional. Thesis, Univ. J. Fourier, Grenoble, 273 pp

Rivas-Martinez: http://www.globalbioclimatics.org/

Rivas-Martinez - http://www.iao.florence.it/training/geomatics/BenSlimane/Marocco21_3_1_2.htm

Thornthwaite, C. W., 1948: An Approach toward a Rational Classification of Climate. Geographical Review, Vol. 38, No. 1(Jan.):55-94.

Thornthwaite, C. W., and Mather, J.R., 1955: The water balance.  Publications in Climatology, Volume 8(1), Laboratory of Climatology

Thornthwaite, C. W., and Mather, J.R., 1957: Instructions and tables for computing potential evapotranspiration and the water balance.  Publications in climatology, Volume 10(3), Laboratory of Climatology

Trewartha, G.T. and Lyle, H.H., 1980: An Introduction to Climate. MacGraw - Hill, 5th Ed. Appendix: Koeppen's Classification of Climates.

UNEP (United Nations Environment Programme), 1997. World atlas of desertification 2ED. UNEP, London

}


\keyword{"climate classification""}% __ONLY ONE__ keyword per line
\keyword{Koeppen - Geiger}
\keyword{Thornthwaite}
\keyword{aridity}
\keyword{continentality}
\keyword{Bagnouls - Gaussen}
\keyword{Peguy}

