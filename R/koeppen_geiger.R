NULL
#' @description General climate classification after Koeppen - Geiger.
#' 
#' @param clim_norm average values (climate normals) for the desired period.
#' @param A_B_C_special_sub.classes logical. Sets if calculations have to consider sub-classes based on rain features in climate types A, B, and C  (see details). Default is FALSE.
#' 
#'
#' @title Koeppen - Geiger's climate classification
#' @author Emanuele Eccel
#'
#' @return A one-line data frame reporting a resume of climatic features useful for the classification, and one last field (1 type - or "climate" - plus 1 or 2 sub-types) reporting Koeppen - Geiger's climate classification.
#'
#' @details \code{clim_norm} is a monthly data frame of climate normals, with column names: "P", "Tn", "Tx", "Tm" (precipitation, minimum, maximum and mean temperature, respectively). It can be the output of function \code{\link{climate}}.
#' 
#' Koeppen - Geiger's classification is based on Trewartha and Lyle, 1980. The function also holds for Southern emisphere, except for the "Gange" sub-type ("Ag" and "Cg"). Type "H" (highland climate) and sub-types "Bn" and "Cn" (where n stands for Nebel) are never attributed, being based on a qualitative description in the quoted reference.
#' 
#' Sub-type "w" (wet-and-dry) or "m" (monsoon) in climate "A" is set according to the definition after Encyclopaedia Britannica (http://www.britannica.com/EBchecked/topic/322068/Koppen-climate-classification): if P in the 4 driest months is less than 1/5 of the wettest months and if both the 4 driest and wettest months are split over non-contiguous seasons (either 2 months for season or 1 and 3 months for season), then sub-type is "".
#' 
#' For climate "A", the letter "m" is attributed to the first sub-type.
#' 
#' Climates "Cx" have P[May + June] >= 1.3 P [Aug. + Sept.] in N emisphere, and P[Nov. + Dec.] >= 1.3 P[Febr. + March] in S emisphere.
#' 
#' \code{A_B_C_special_sub.classes}, if TRUE, adds a letter to the second sub-type of climates: "i" or "g" (climate A), "w" or "s" (climate B), and  "i", "g", or "x" (climate C).
#' 
#' Fields in the output data frame are:
#' 
#' T_w.m = temperature of the warmest month (degrees C)
#' 
#' T_c.m = temperature of the coldest month (degrees C)
#' 
#' T_avg = average temperature (degrees C)
#' 
#' P_tot = total precipitation depth (mm)
#' 
#' P_wint = precipitation depth in the 6 coldest (winter) months (mm)
#' 
#' P_summ = precipitation depth in the 6 warmest (summer) months (mm)
#' 
#' P_d.m = precipitation depth in the driest month (mm)
#' 
#' P_d.m.summ = precipitation depth in the driest month of "summer" half of the year (mm)
#' 
#' P_d.m.wint = precipitation depth in the driest month of "winter" half of the year (mm)
#' 
#' T_4th_w.m = temperature of the 4th warmest month (degrees C)
#' 
#' class = climatic class, resulting from the merging of "climate" (A to E) and sub-type(s)
#'
#' @export
#'
#' @references 
#' Trewartha, G.T. and Lyle, H.H., 1980: An Introduction to Climate. MacGraw - Hill, 5th Ed. Appendix: Koeppen's Classification of Climates.
#'
#' @examples
#' 
#' data(Trent_climate)
#' # clima_81_10 is a list of data frames having climatic means of temperature and precipitation as 
#' # required by Koeppen - Geiger classification, each one referring to one station. It can be the output 
#' # of function climate.
#' class_clim_l<-lapply(clima_81_10, FUN=koeppen_geiger, A_B_C_special_sub.classes=TRUE)
#'
#' @seealso \code{\link{climate}}


koeppen_geiger<-function(clim_norm, A_B_C_special_sub.classes =FALSE)
{
  # calculation of relevant indices for classification
  T_warm_month<-max(clim_norm$Tm)
  T_cold_month<-min(clim_norm$Tm)
  T_avg<-mean(clim_norm$Tm)
  P_tot<-sum(clim_norm$P)
  wint_months<-order(clim_norm$Tm)[1:6]
  summ_months<-order(clim_norm$Tm)[7:12]
  if(1 %in% wint_months | 2 %in% wint_months) # N emisphere
  {
    autumn_months<-9:11
    solst_months<-5:7
    late.spring_early.summer_months<-5:6
    late.summer_months<-8:9
  } else # S emisphere
  {
    autumn_months<-3:5
    solst_months<-c(11:12,1)
    late.spring_early.summer_months<-11:12
    late.summer_months<-2:3
  }
  
  P_wint.half<-sum(clim_norm$P[clim_norm$month %in% wint_months])
  P_summ.half<-sum(clim_norm$P[clim_norm$month %in% summ_months])
  P_driest_month<-min(clim_norm$P)
  P_driest_month_summ.half<-min(clim_norm$P[clim_norm$month %in% summ_months])
  P_driest_month_wint.half<-min(clim_norm$P[clim_norm$month %in% wint_months])
  P_wettest_month<-max(clim_norm$P)
  P_wettest_month_summ.half<-max(clim_norm$P[clim_norm$month %in% summ_months])
  P_wettest_month_wint.half<-max(clim_norm$P[clim_norm$month %in% wint_months])
  T_4th_warm_month<-sort(clim_norm$Tm, decreasing=TRUE)[4]
  
  matr_indices<-round(data.frame(T_warm_month, T_cold_month, T_avg, P_tot,
                                 P_wint.half,P_summ.half,P_driest_month, 
                                 P_driest_month_summ.half,P_driest_month_wint.half,  P_wettest_month,
                                 P_wettest_month_summ.half,P_wettest_month_wint.half,
                                 T_4th_warm_month),1)
  names(matr_indices)<-c("T_w.m", "T_c.m", "T_avg","P_tot","P_wint", "P_summ","P_d.m", "P_d.m.summ",  "P_d.m.wint",  "P_w.m","P_w.m.summ",  "P_w.m.wint", "T_4th_w.m")
  
  type<-NA
  thresh<-NULL
  sub.type_1 <-NULL
  sub.type_2 <-NULL
  
  # step 1 of classification - main types (+ 2 special classes for type B)
  if(T_cold_month >= 18) type <- "A"
  if(T_cold_month < 18 &  T_cold_month > -3 & T_warm_month >= 10) type <- "C"
  if(T_cold_month <= -3 & T_warm_month >= 10) type <- "D"
  if(T_warm_month < 10) type <- "E"
  if(P_summ.half >= 0.70*P_tot & P_tot < 20*T_avg + 280) 
  {type <- "B"
   rain.reg<-"summ"
   thresh<- 20*T_avg + 280} else     
     if(P_wint.half >= 0.70*P_tot & P_tot < 20*T_avg ) 
     {type <- "B"
      rain.reg<-"wint"
      thresh<- 20*T_avg} else     
        if(P_summ.half < 0.70*P_tot & P_wint.half < 0.70*P_tot & P_tot < 20*T_avg + 140) 
        {type <- "B"
         thresh<- 20*T_avg + 140}
  
  # step 2 of classification - types 1 and 2
  if(type == "A")
  {
    if(P_driest_month >= 60) sub.type_1<-"f"
    if(P_driest_month < 60) sub.type_1<-"w"
    if(P_driest_month < 60 & P_driest_month >= 100 - P_tot/25) sub.type_1<-"m" 
    
    if(order(clim_norm$P)[12] %in% autumn_months) sub.type_2 <- "w'"
    if(order(clim_norm$P)[1] %in% solst_months) sub.type_2 <- "s"
    # class w" attribution
    dry.4<-order(clim_norm$P)[1:4]
    wet.4<-order(clim_norm$P)[9:12]
    seas.1<-c(12,1,2); seas.2<-3:5; seas.3<-6:8; seas.4<-9:11
    if(sum(clim_norm$P[dry.4])*5 < sum(clim_norm$P[wet.4]) & 
         ((sum(dry.4 %in% c(seas.1, seas.3))==4 & sum(wet.4 %in% c(seas.2, seas.4))==4) |
            (sum(dry.4 %in% c(seas.2, seas.4))==4 & sum(wet.4 %in% c(seas.1, seas.3))==4))) sub.type_2 <- noquote("w\"")
    
    if(A_B_C_special_sub.classes ==TRUE)
    {
      if(T_warm_month - T_cold_month < 5) sub.type_2 <- paste(sub.type_2,"i" , sep="")
      if(1 %in% wint_months &  order(clim_norm$Tm)[12]<=6 & sum(order(clim_norm$P)[11:12] %in% 7:9)==2) sub.type_2 <- paste(sub.type_2,"g" , sep="")
    }
  }
  
  if(type == "B")
  {
    if(P_tot < thresh/2 ) sub.type_1<-"W" else sub.type_1<-"S"
    
    if(T_avg >= 18) sub.type_2 <- "h" else 
    {
      if(T_warm_month < 18) sub.type_2 <- "k'" else sub.type_2 <- "k"
    }
    
    if(A_B_C_special_sub.classes==TRUE) 
    {
      if(rain.reg == "summ") sub.type_2<-paste(sub.type_2,"w", sep="")
      if(rain.reg == "wint") sub.type_2<-paste(sub.type_2,"s", sep="")
    }
    
  }
  
  if(type == "C") 
  {
    if(P_driest_month_summ.half < 30 & P_driest_month_summ.half <  P_wettest_month_wint.half / 3) sub.type_1<-"s" else 
      if(P_driest_month_wint.half < P_wettest_month_summ.half / 10) sub.type_1<-"w" else sub.type_1<-"f"
    
    if(T_warm_month >= 22) sub.type_2 <- "a" else
    {
      if(T_4th_warm_month > 10) sub.type_2 <- "b" else sub.type_2 <- "c" 
    }
    if(A_B_C_special_sub.classes ==TRUE)
    {
      if(T_warm_month - T_cold_month < 5) sub.type_2 <- paste(sub.type_2,"i", sep="")
      if(1 %in% wint_months & order(clim_norm$Tm)[12]<=6 & sum(order(clim_norm$P)[11:12] %in% 7:9)==2) sub.type_2 <- paste(sub.type_2,"g", sep="") 
      if(order(clim_norm$P)[12] %in% late.spring_early.summer_months & sum(clim_norm$P[late.spring_early.summer_months]) >  sum(clim_norm$P[late.summer_months])*1.3) sub.type_2 <- paste(sub.type_2,"x", sep="")
    }
  }
  
  if(type == "D") 
  {
    if(P_driest_month_summ.half < 30 & P_driest_month_summ.half <  P_wettest_month_wint.half / 3) sub.type_1<-"s" else 
      if(P_driest_month_wint.half < P_wettest_month_summ.half / 10) sub.type_1<-"w" else sub.type_1<-"f"
    
    if(T_warm_month >= 22) sub.type_2 <- "a" else
    {
      if(T_4th_warm_month > 10) sub.type_2 <- "b" else sub.type_2 <- "c" 
    }
    if(T_cold_month < -38) sub.type_2 <- "d"
    
  }
  
  if(type == "E")
  {
    if(T_warm_month > 0) sub.type_1<- "T" else sub.type_1<-"F"
    sub.type_2<-NULL
  }
  
  matr_climate<-data.frame(paste(type,sub.type_1,sub.type_2, sep=""))
  names(matr_climate)<-"class"
  
  matr_climate<-data.frame(matr_indices, matr_climate)
  
  return(matr_climate)
  
}
