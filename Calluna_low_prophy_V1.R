
library(signal)
library(DAutilities)
library(Surrogate)
library(decisionSupport)
library(tidyverse)

setwd("C:/Users/ZEF/Desktop/Calluna_Folder") 
#avoid using 'new' in your titles. Imagine that we will look back at this long after your PhD for ideas, when we both have grey beards. It will no longer be 'new'
Calluna_low_prophy_V1 <- function(x, varnames){
  
  #Calculate the plant amount of whole production area
  original_plant_number <- production_area * plants_per_ha
  
  #Define risky months (May to August)                
  weather_arguments_for_infection <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  
  #Simulate the infection risk for each month under uncertainty
  #risk_per_month <- runif(4, 0, (infection_risk>=0)) * weather_arguments_for_infection 
  
  risk_per_month <- vv(infection_risk, var_CV, 12)* weather_arguments_for_infection
  risk_per_month <- sapply(risk_per_month, function(x) max(c(min(c(1,x)),0)))
  
  #Settings for factors in % so that they will not be higher or lower than 1
  #random values are selected out of pre-defined intervalls
  #Calculate risks for NORMAL prophylactic application
  fungus_possibility <- vv(fungus_possibility_N, var_CV, 12)* weather_arguments_for_infection
  fungus_possibility_N <- sapply(fungus_possibility, function(x) max(c(min(c(1,x)),0)))
  
  detection_factor <- vv(detection_factor_N, var_CV, 12)* weather_arguments_for_infection
  detection_factor_N <- sapply(detection_factor, function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor <- vv(disease_expansion_factor_N, var_CV, 12)* weather_arguments_for_infection
  disease_expansion_factor_N <- sapply(disease_expansion_factor, function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect <- vv(fungus_fight_effect_N, var_CV, 12)* weather_arguments_for_infection
  fungus_fight_effect_N <- sapply(fungus_fight_effect, function(x) max(c(min(c(1,x)),0)))
  
  #For REDUCED prophylactic application
  fungus_possibility <- vv(fungus_possibility_R, var_CV, 12)* weather_arguments_for_infection
  fungus_possibility_R <- sapply(fungus_possibility, function(x) max(c(min(c(1,x)),0)))
  
  detection_factor <- vv(detection_factor_R, var_CV, 12)* weather_arguments_for_infection
  detection_factor_R <- sapply(detection_factor, function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor <- vv(disease_expansion_factor_R, var_CV, 12)* weather_arguments_for_infection
  disease_expansion_factor_R <- sapply(disease_expansion_factor, function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect <- vv(fungus_fight_effect_R, var_CV, 12)* weather_arguments_for_infection
  fungus_fight_effect_R <- sapply(fungus_fight_effect, function(x) max(c(min(c(1,x)),0)))
  
  
  #Use function to split prophylactic application sum per year 
  #in accidentally selected application quantity per month
  
  need.random.integers <- function(a,b,n,k){
  #finds n random integers in range a:b which sum to k
    while(TRUE){
      x <- sample(1:(k - n*a),n-1, replace = TRUE)
      x <- sort(x)
      x <- c(x,k-n*a) - c(0,x)
      if(max(x) <= b-a) return(a+x)
    }
  }
  
  #For NORMAL prophylactic application (number_yearly_prophy_application_N)
  effect_application_N <- need.random.integers(0, round((number_yearly_prophy_application_N), digits = 0),
                                               4, round((number_yearly_prophy_application_N), digits = 0)) 
  effect_application_N <- effect_application_N * weather_arguments_for_infection
  
  
  #Define effect of normal prophylactic pesticide application (potential to reduce fungus onset)
  #Estimated effect of fungus reduce potential are interlinked with each number of application per month   
  effect_application_N <- effect_application_N %>%
  replace(., effect_application_N==1, effect_one_prophy_application) %>%
  replace(., effect_application_N==2, effect_two_prophy_application) %>%
  replace(., effect_application_N==3, effect_three_prophy_application) %>%
  replace(., effect_application_N==4, effect_four_prophy_application) %>%
  replace(., effect_application_N==5, effect_five_prophy_application) %>%
  replace(., effect_application_N==6, effect_six_prophy_application) %>%
  replace(., effect_application_N==7, effect_seven_prophy_application) %>%
  replace(., effect_application_N>=8, effect_eight_prophy_application)
  
  
  #For REDUCED prophylactic application (number_yearly_prophy_application_R)
  effect_application_R <- need.random.integers(0, round((number_yearly_prophy_application_R), digits = 0),
                                               4, round((number_yearly_prophy_application_R), digits = 0))
  effect_application_R <- effect_application_R * weather_arguments_for_infection
  
  #Define effect of reduced prophylactic pesticide application (potential to reduce fungus onset)
  effect_application_R <- effect_application_R %>%
    replace(., effect_application_R==1, effect_one_prophy_application) %>%
    replace(., effect_application_R==2, effect_two_prophy_application) %>%
    replace(., effect_application_R==3, effect_three_prophy_application) %>%
    replace(., effect_application_R==4, effect_four_prophy_application) %>%
    replace(., effect_application_R==5, effect_five_prophy_application) %>%
    replace(., effect_application_R==6, effect_six_prophy_application) %>%
    replace(., effect_application_R==7, effect_seven_prophy_application) %>%
    replace(., effect_application_R>=8, effect_eight_prophy_application)
  
  
  #Simulate different infection risks per month per quarter of the production system
  #to receive the overall number of all infected Callunas in the whole production area
  #Splitting the system into quarters allows realistic conditions (Variation of infection respectively to area)
  
  #Simulate the infection of plants
  infected_plant_number_N <- original_plant_number*risk_per_month
  not_infected_plants_after_prophy_N <- weather_arguments_for_infection
  symptomatic_plants_N <- weather_arguments_for_infection
  detected_plants_after_monitoring_N <- weather_arguments_for_infection
  healthy_plants_after_fungus_fight_N <- weather_arguments_for_infection
  getting_again_sick_plants_N <- weather_arguments_for_infection
  
  ####Simulate infection and plant losses for NORMAL prophylactic application####
  for (i in 2:length(infected_plant_number_N)){
    for (j in 2:length(not_infected_plants_after_prophy_N)) {
        for (k in 2:length(symptomatic_plants_N)) {
          for (l in 2:length(detected_plants_after_monitoring_N)) {
            for (m in 2:length(getting_again_sick_plants_N)) {
              for (n in 2:length(healthy_plants_after_fungus_fight_N)) {
  
  infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
    (original_plant_number - infected_plant_number_N[i-1])
  
  not_infected_plants_after_prophy_N[j] <- not_infected_plants_after_prophy_N[j-1] + effect_application_N[j]*
    (infected_plant_number_N[j] - not_infected_plants_after_prophy_N[j-1])
  
  still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
  
  symptomatic_plants_N[k] <- symptomatic_plants_N[k-1] + fungus_possibility_N[k]*
                             (still_infected_plants_N[k] - symptomatic_plants_N[k-1]) 
  
  detected_plants_after_monitoring_N[l] <- detected_plants_after_monitoring_N[l-1] + detection_factor_N[l]*
                                         (symptomatic_plants_N[l] - detected_plants_after_monitoring_N[l-1])
  
  symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
  
  getting_again_sick_plants_N[m] <- getting_again_sick_plants_N[m-1] + disease_expansion_factor_N[m]*
                                 (symptomatic_plants_after_monitoring_N[m] - getting_again_sick_plants_N[m-1])
  
  all_symptomatic_plants_N <- symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N
  
  healthy_plants_after_fungus_fight_N[n] <- healthy_plants_after_fungus_fight_N[n-1] + fungus_fight_effect_N[n]*
                                         (all_symptomatic_plants_N[n] - healthy_plants_after_fungus_fight_N[n-1])
  
  final_fungus_infected_plants_N <- all_symptomatic_plants_N[12] - healthy_plants_after_fungus_fight_N[12]
  
  direct_plant_losses_N <- detected_plants_after_monitoring_N[12]
  
  actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)}}}}}}
  
  
  
  #Simulate infection and plant losses for REDUCED prophylactic application####
  infected_plant_number_R <- original_plant_number * risk_per_month
  not_infected_plants_after_prophy_R <- weather_arguments_for_infection
  symptomatic_plants_R <- weather_arguments_for_infection
  detected_plants_after_monitoring_R <- weather_arguments_for_infection
  healthy_plants_after_fungus_fight_R <- weather_arguments_for_infection
  getting_again_sick_plants_R <- weather_arguments_for_infection
  
  ####Simulate infection and plant losses for REDUCED prophylactic application####
  for (i in 2:length(infected_plant_number_R)){
    for (j in 2:length(not_infected_plants_after_prophy_R)) {
      for (k in 2:length(symptomatic_plants_R)) {
        for (l in 2:length(detected_plants_after_monitoring_R)) {
          for (m in 2:length(getting_again_sick_plants_R)) {
            for (n in 2:length(healthy_plants_after_fungus_fight_R)) {
              
              infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
                (original_plant_number - infected_plant_number_R[i-1])
              
              not_infected_plants_after_prophy_R[j] <- not_infected_plants_after_prophy_R[j-1] + effect_application_R[j]*
                (infected_plant_number_R[j] - not_infected_plants_after_prophy_R[j-1])
              
              still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
              
              symptomatic_plants_R[k] <- symptomatic_plants_R[k-1] + fungus_possibility_R[k]*
                (still_infected_plants_R[k] - symptomatic_plants_R[k-1]) 
              
              detected_plants_after_monitoring_R[l] <- detected_plants_after_monitoring_R[l-1] + detection_factor_R[l]*
                (symptomatic_plants_R[l] - detected_plants_after_monitoring_R[l-1])
              
              symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
              
              getting_again_sick_plants_R[m] <- getting_again_sick_plants_R[m-1] + disease_expansion_factor_R[m]*
                (symptomatic_plants_after_monitoring_R[m] - getting_again_sick_plants_R[m-1])
              
              all_symptomatic_plants_R <- symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R
              
              healthy_plants_after_fungus_fight_R[n] <- healthy_plants_after_fungus_fight_R[n-1] + fungus_fight_effect_R[n]*
                (all_symptomatic_plants_R[n] - healthy_plants_after_fungus_fight_R[n-1])
              
              final_fungus_infected_plants_R <- all_symptomatic_plants_R[12] - healthy_plants_after_fungus_fight_R[12]
              
              direct_plant_losses_R <- detected_plants_after_monitoring_R[12]
              
              actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)}}}}}}
  
  
  ####Calculating benefits and outputs####
  
  benefits <- actual_saleable_Callunas_N
  costs <- actual_saleable_Callunas_R
  
  #cashflow and results 
  cashflow <- benefits - costs
  NPV <- DecisionSupport::discount(x= cashflow, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  
  return(list(NetPresentValue = NPV))
  
}



input_table <- "Calluna_new_prophy.csv"
legend_file <- "Calluna_new_prophy_Legend.csv"
results_folder <- "Results_new_prophy"
figures_folder <- "Figures_new_prophy"

make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}
make_variables(estimate_read_csv(input_table))
dir.create(figures_folder)

decisionSupport(input_table, #input file with estimates
                results_folder, #output folder
                write_table = TRUE, Calluna_low_prophy_V1, 10000,
                functionSyntax = "plainNames")


#produce EVPI tables
library(DAutilities)
outvars <- list("NetPresentValue")
legend_table <- read.csv(legend_file)

labels <- list("Net Present Value")

MC_file <- read.csv(paste(results_folder,"/mcSimulationResults.csv",sep = ""))


mc_EVPI <- MC_file[,-grep("cashflow",colnames(MC_file))]
mc_EVPI <- MC_file
empirical_EVPI(mc = mc_EVPI,"NetPresentValue",write_table = TRUE,fileformat="png",outfolder = figures_folder,
               p_spearman = 0.05, legend_table = legend_table,output_legend_table = legend_table)



#produce compound figures
for (variable_name in outvars)
  compound_figure(variable_name = variable_name,
                  MC_table = MC_file,
                  PLS_table = read.csv(paste(results_folder,"/",variable_name,"_pls_results.csv",sep = "")),
                  EVPI_table = read.csv(paste(figures_folder,"/EVPI_table_",variable_name,".csv",sep = "")),
                  cash_flow_vars = NULL, #paste("cashflow_",variable_name,sep=""),
                  nbreaks = 100,scaler = "auto",percentile_remove = c(.01,.99),
                  npls = 15,plsthreshold = 0.8,colorscheme = "quant_col",MCcolor = "negpos",fonttype = 'sans',
                  borderlines = FALSE,lwd = 2,
                  fileformat = "png",filename = paste(figures_folder,"/Combined_",variable_name,sep = ""),
                  legend_table = legend_table)
###########################################################################################################################