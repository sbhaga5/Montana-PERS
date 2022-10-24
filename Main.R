source('Montana PERS Funding Model 2021.R')
source('Functions.R')

#Scenarios
#Scenarios for UAL, FR, etc
Scenario_Returns <- Scenario_UAL <- Scenario_FR <- Scenario_ER_Percentage <-
  Scenario_ER_InflAdj <- Scenario_Total_ER <- Scenario_AllIn_ER <- as.data.frame(FYE)

#Economic Scenarios
Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
for (i in 1:length(Scenarios)){
  NewData <- as.data.frame(RunModel(DeSimType = Scenarios[i]))
  Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
  Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
  Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
  Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
  Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
  Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
  Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
}

colnames(Scenario_Returns) <- colnames(Scenario_UAL) <- colnames(Scenario_FR) <- colnames(Scenario_ER_Percentage) <-
  colnames(Scenario_ER_InflAdj) <- colnames(Scenario_Total_ER) <- colnames(Scenario_AllIn_ER) <- c('FYE',Scenarios)


ScenarioPlot(Scenario_FR, 'Funded Ratio (MVA)')

#Simulations
start_time <- Sys.time()
#Set seed insures a consistency when simulations are run multiple times
set.seed((1234))
NumberofSimulations <- 100
#initialize the return simulations based on years and # of simulations
Returns_Sims <- UAL_Sims <- FR_Sims <- ER_Sims <- 
  matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)

#Run the simulations
for (i in 1:NumberofSimulations){
  NewData <- as.data.frame(RunModel(ReturnType = 'Stochastic', StoSimType = 'Assumed'))
  Returns_Sims[,i+1] <- NewData$ROA_MVA
  UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
  FR_Sims[,i+1] <- NewData$FR_MVA
  ER_Sims[,i+1] <- NewData$ER_Percentage
}

Simulations_Returns <- cbind(FYE,FYE,FYE)
Simulations_UAL <- cbind(FYE,FYE,FYE)
Simulations_FR <- cbind(FYE,FYE,FYE)
Simulations_ER <- cbind(FYE,FYE,FYE)

#Get the 25th, 50th, 75th percentile
for(i in 1:length(FYE)){
  Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
  Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
  Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
  Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
}

SimulationPlot(Simulations_FR, FYE)

end_time <- Sys.time()
print(end_time - start_time)