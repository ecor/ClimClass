# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' as.datcli
#' 
#' @description Tranforms a Data Frame (see example dataset) into a data frame format like  'datcli' in 'climatol' package
#' 
#' @param df data frame or a list 
#' @param station name of the considered station
#' @param MonthField character string for month field in \code{df}. Default is \code{"month"}.
#' @param PrecField character string for Mean Precipitation field in \code{df}. Default is \code{"P"}.
#' @param MinTempField character string  for Mean Daily Minimum Temperature field in \code{df}. Default is \code{"Tn"}.
#' @param MaxTempField character string  for Mean Daily Maximum Temperature field in \code{df}. Default is \code{"Tx"}.
#' @param MeanTempField character string for Mean Daily Maximum Temperature field in \code{df}. Default is \code{"Tm"}.
#' @param AbsMinTempField character string for Absolute Monthly Minimum Temperature field in \code{df}. Default is \code{"AbsTn"}. 
#' @param AbsMinTempOffset estimated offset between Average Min Temperature and  Absolute Max Temperature.
#' @param StationField  character string for Station  field in \code{df}. Default is \code{"station"}.
#'    
#' @export
#' @seealso \url{http://www.climatol.eu/}, \url{http://www.zoolex.org/walter.html}
#' 
#' @examples 
#' 
#' 
#' library(climatol)
#' 
#'data(Trent_climate)
#'TrentinoClimateDf <- melt(clima_81_10,id=names(clima_81_10[[1]]))
#'names(TrentinoClimateDf)[names(TrentinoClimateDf)=="L1"] <- "station"
#' 
#' 
#' station <- "T0129"
#' datcli <- as.datcli(TrentinoClimateDf,station=station)
#' diagwl(datcli,est=station,alt=100,per="Period",mlab="en") ## plots a Walter-Lieth's climograph
#' 

as.datcli <- function(df,station="T0009",MonthField="month",
		     PrecField="P",
			 MinTempField="Tn",
			 MaxTempField="Tx",
			 MeanTempField="Tm",
			 AbsMinTempField="AbsTn",
			 AbsMinTempOffset=4,
			 StationField="station") {
	
		 	 
		 if (length(station)>1)	 {
			 
			 station <- station[1]
			 warning("Only fist station is considered!!!")
		 }
		 
		 if (is.data.frame(df)) {
		 	out <- df[df[,StationField]==station,]
		 } else if (is.list(df)) {
			 
			 out <- df[[station]]
		 }
		 if  (!(AbsMinTempField %in% names(out))) {
			 
			 warning("Missing information about absolute Monthly Minimum Temperature, somehow deduced!!!")
			 out[,AbsMinTempField] <- out[,MinTempField]-AbsMinTempOffset
		 }
		 
		 row.names(out) <- sprintf("2000-%02d-01",out[,MonthField])
		 
		 
		 out <- out[,c(PrecField,MaxTempField,MinTempField,AbsMinTempField)]
		 
		 sort <- sort(row.names(out))
		 
		 
		 out <- out[sort,]
		 
		 row.names(out) <- months(as.Date(row.names(out)),abbreviate=TRUE)
	
		 out <- t(as.matrix(out))
		 
	     return(out)	
	
	
	
}
