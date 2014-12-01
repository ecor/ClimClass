\name{bagn_gau}
\alias{bagn_gau}
\title{Bagnouls - Gaussen graphs}
\usage{
  bagn_gau(clim_norm_sta, save_dir = NULL, format = NULL,
    main_title = NULL, st_name = NULL, trace_grid = TRUE,
    tick_step = 20, bar_width = 30, bar_col = "grey",
    trace_0.line = TRUE, ...)
}
\arguments{
  \item{clim_norm_sta}{data frame with climatic normals}

  \item{save_dir}{name of destination directory for graphs
  (if any).}

  \item{format}{graphical format of graphs; default is
  NULL.}

  \item{main_title}{main title for all charts; e.g., it may
  include references to station id. Default is
  \code{NULL}.}

  \item{st_name}{name to be included into graphs titles.
  Only for file output. Default is \code{NULL}.}

  \item{trace_grid}{logic. If \code{TRUE} (default) adds a
  grid.}

  \item{tick_step}{step for Y axis (precipitation). Default
  is 20 (mm)}

  \item{bar_width}{width of bars in the chart. Default is
  30.}

  \item{bar_col}{color of bars. Default is "grey".}

  \item{trace_0.line}{logic. If \code{TRUE} (default), a
  line at P = 0 and T = 0 is traced.}

  \item{...}{arguments to be passed to methods, such as
  graphical parameters (see \code{\link{par}}).}
}
\value{
  Bagnouls - Gaussen's charts of precipitation and
  temperature.
}
\description{
  Plots Bagnouls - Gaussen climatic charts of precipitation
  and temperature. Conventionally, in this chart the scale
  of precipitation has a double extension with respect to
  the scale of temperature (Bagnouls and Gaussen, 1953).
}
\details{
  \code{clim_norm_sta} can be e.g. one element of the
  output of function \code{\link{climate}}. See
  \code{examples}.

  If \code{format} is NULL (default), graphs are sent to
  the console. Otherwise, a file is produced and saved.
  \code{format} is used only if the graphs are to be sent
  to files. Values allowed are: "png", "jpeg", "tiff",
  "bmp".

  If one or more data are missing, the chart is not
  processed.

  Most graphic parameters for functions \code{\link{plot}},
  \code{\link{axis}}, and \code{\link{mtext}} are accepted.
}
\note{
  A conflict is generated if parameters already used by the
  function are passed (e.g. \code{col} - use
  \code{col.main}, \code{col.axis}, ..., instead).
}
\examples{
data(Trent_climate)
# clima_81_10 can be generated from monthly time series by function \\code{\\link{climate}}.
par(ask=TRUE)
for(sta in 1:length(clima_81_10)) {
  bagn_gau(clim_norm_sta= clima_81_10 [[sta]],
   main_title=paste(names(clima_81_10[sta]), "  1981-2010")
	, bar_width=40)
}
}
\author{
  Emanuele Eccel
}
\references{
  Bagnouls, F., and Gaussen, H., 1953: Saison seche et
  indice xerothermique. Docum. pour les Cartes des Prod.
  Veget. Serie: Generalite, 1 (1953), pp. 1-49
}
\seealso{
  \code{\link{climate}}
}

