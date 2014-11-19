\name{contin}
\alias{contin}
\title{Continentality indices}
\usage{
contin(clim_norm, latitude = NULL, elevation = NULL,
  Michalet_correction = FALSE, indices = 1:4)
}
\arguments{
  \item{clim_norm}{climatic normals}

  \item{latitude}{station latitude in degrees. Used in
  Gorczynski's and Conrad's classifications (indices 1 and
  2). Default is \code{NULL}.}

  \item{elevation}{station elevation in m. Used in Gams'
  classification (index 3). Default is \code{NULL}.}

  \item{Michalet_correction}{logic: if \code{TRUE},
  Michalet's correction is applied to index 3 (Gams).
  Default is \code{FALSE}.}

  \item{indices}{set of aridity indices to be listed.
  Default is all indices (1 to 4).}
}
\value{
A single-line data frame with the desired continentality
index(es).
}
\description{
Calculates climate continentality / oceanicity according to
several indices.
}
\details{
clim_norm is a monthly data frame of climate normals, with
column names: "P", "Tn", "Tx", "Tm" (precipitation,
minimum, maximum and mean temperature, respectively). It
can be the output of function \code{\link{climate}}.

\code{indices}' values are the following:

1: Gorczynski - K.G. (Gorczynski, L., 1920).

2: Conrad - K.C. (Conrad, 1946).

3: Gams - alpha. (Gams, H., 1932). For Michalet's
correction: Michalet and Souchier, 1991.

4: Rivas-Martinez - Ic. (Rivas - Martinez, web page).

A reference for the continentality / oceanicity degree is
given in the list object \code{continental_ind_tables} of
data set \code{\link{Trent_climate}}.

If Michalet's correction is applied to Gams' hygric
continentality index, the value of precipitation is
proportionally diminished for elevations below 900 m a.s.l.
See also Lebourgeoise, 2010.
}
\examples{
data(Trent_climate)


# clima_81_10 is a list of data frames having climatic means of temperature and precipitation as
# required by the aridity indices algorithms, each one referring to one station.
# It can be the output of function climate.

# creates a data frame with all the continentality indices for all stations in clima_81_10

latit<-coord_elev$North
elev<-coord_elev$Elevation

contin_I<-NULL
for(i in 1:length(clima_81_10)) {
  contin_I[[i]]<-contin(clima_81_10[[i]],
   latitude=latit[i],
   elevation=elev[i],
   Michalet_correction=TRUE)
}
names(contin_I)<-names(clima_81_10)
}
\author{
Emanuele Eccel
}
\references{
Conrad, V. 1946: Usual formulas of continentality and their
limits of validity. Transactions, American Geophysical
Union, Volume 27, Issue 5, p. 663-664.

Gams, H., 1932. Die klimatische Begrenzung von
Pflanzenarealen und die Verteilung der hygrischen
Kontinentalitaet in den Alpen. Zeitschr. Ges. Erdkunde,
Berlin.

Gorczynski, L. (1920) : Sur le calcul du degre de
continentalisme et son application dans la climatologie.
Geografiska Annaler 2, 324-331.

Lebourgeoise, F., 2010: Cours de bioclimatologie a l'usage
des forestiers. Departement SIAFEE, UFR Forets, Arbres et
Milieux Naturels. ENGREF, Nancy Cedex.

Michalet, R., and Souchier, B., 1991: Une approche
synthetique biopedoclimatique del montagnes
mediterraneennes: l'exemple du Maroc septemptrional.
Thesis, Univ. J. Fourier, Grenoble, 273 pp.

Rivas-Martinez: http://www.globalbioclimatics.org/.
}
\seealso{
\code{\link{climate}}
}

