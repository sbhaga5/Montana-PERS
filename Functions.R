#Get period function
GetNPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g) - 1
  TempValue <- PV*R/(pmt*(1+R))
  NPER <- -log(1-TempValue,(1+R))
  if(is.infinite(NPER)) {NPER <- 100}
  if(is.nan(NPER)) {NPER <- 100}
  return(NPER)
}

PresentValue = function(rate, nper, pmt) {
  PV = (1 + rate)*pmt * (1 - (1 + rate) ^ (-nper)) / rate
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

ScenarioPlot <- function(Data, YAxisLabel){
  ggplot(Data, aes(x = FYE)) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
    geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
    labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
  #scale_linetype_manual(labels = '')
}

SimulationPlot <- function(Data, FYE, YAxisLabel){
  Data <- (as.data.frame(Data))
  Data <- cbind(FYE, Data)
  colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
  ggplot(Data, aes(x = Data[,1])) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) + 
    labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
}