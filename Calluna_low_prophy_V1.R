library(decisionSupport)
library(DAutilities)
library(signal)
library(dplyr)
library(Surrogate)
library(MASS)
library(magrittr)

Calluna_low_prophy_V1 <- function(x, varnames){
  
  #### Calculate the number of plants in the whole production area ####
  original_plant_number <- production_area * plants_per_ha
  
  # Define risky months (May to August)                
  W <- weather_arguments_for_infection <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  
  # Define months where detection rate is increased (MONITORING PLAN before plant pots are placed apart) (May to June)
  MPD <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0) 
  
  # Simulate the infection risk for each month under uncertainty
  risk_per_month <- vv(infection_risk, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  #### Settings for factors in % so that they will not be higher or lower than 1 ####
  # random values are selected out of pre-defined intervalls
  
  #### Calculate risks and factors for NORMAL prophylactic application ####
  fungus_possibility_N <- vv(fungus_possibility_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))

  detection_factor_N <- vv(detection_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_N <- vv(disease_expansion_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_N <- vv(fungus_fight_effect_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  #### Calculate risks and factors for REDUCED prophylactic application ####
  fungus_possibility_R <- vv(fungus_possibility_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_R <- vv(detection_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_R <- vv(disease_expansion_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_R <- vv(fungus_fight_effect_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  #### Calculate risks and factors for MONITORING PLAN and NORMAL prophylactic application ####
  # detection_factor and fungus_fight_effect differ because of Monitoring plan and the
  # possibility of higher detection rate and more specific fungus fight management
  fungus_possibility_MPN <- vv(fungus_possibility_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_MPN <- vv(detection_factor_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_MPN <- vv(disease_expansion_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_MPN <- vv(fungus_fight_effect_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  #### Calculate risks and factors for MONITORING PLAN and REDUCED prophylactic application ####
  # detection_factor and fungus_fight_effect differ because of Monitoring plan and the
  # possibility of higher detection rate and more specific fungus fight management
  fungus_possibility_MPR <- vv(fungus_possibility_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_MPR <- vv(detection_factor_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_MPR <- vv(disease_expansion_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_MPR <- vv(fungus_fight_effect_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  #### NORMAL and REDUCED prophylactic pesticide application costs in Calluna production system per year ####
  costs_yearly_prophy_application_N <- round((number_yearly_prophy_application_N),digits=0) * 
    cost_one_prophy_application * production_area
  
  costs_yearly_prophy_application_R <- round((number_yearly_prophy_application_R),digits=0) * 
    cost_one_prophy_application * production_area
  
  #### NORMAL and REDUCED prophylactic application costs for MONITORING PLAN in Calluna production system per year ####
  costs_yearly_prophy_application_MPN <- round((number_yearly_prophy_application_N),digits=0) * 
    cost_one_prophy_application * production_area
  
  costs_yearly_prophy_application_MPR <- round((number_yearly_prophy_application_R),digits=0) * 
    cost_one_prophy_application * production_area
  
  # Use function to split prophylactic application sum per year in accidentally selected application quantity per month
  
  need.random.integers <- function(a,b,n,k){
    # Finds n random integers in range a:b which sum to k ### is this really working? Needs to be checked 
    while(TRUE){   # what does while(TRUE) mean? TRUE is always TRUE...
      x <- sample(1:(k - n*a),n-1, replace = TRUE)
      x <- sort(x)
      x <- c(x,k-n*a) - c(0,x)
      if(max(x) <= b-a) return(a+x)}}
  
  # For NORMAL prophylactic application
  effect_application_N <- need.random.integers(0, round((number_yearly_prophy_application_N), digits = 0),
                                               4, round((number_yearly_prophy_application_N), digits = 0))* W

  # Define effect of NORMAL prophylactic pesticide application (potential to reduce fungus onset)
  # Estimated effect of fungus reduce potential are interlinked with each number of application per month  
  
  effect_application_N <- effect_application_N %>%
    replace(., effect_application_N==1, effect_one_prophy_application) %>%
    replace(., effect_application_N==2, effect_two_prophy_application) %>%
    replace(., effect_application_N==3, effect_three_prophy_application) %>%
    replace(., effect_application_N==4, effect_four_prophy_application) %>%
    replace(., effect_application_N==5, effect_five_prophy_application) %>%
    replace(., effect_application_N==6, effect_six_prophy_application) %>%
    replace(., effect_application_N==7, effect_seven_prophy_application) %>%
    replace(., effect_application_N>=8, effect_eight_prophy_application)
  
  
  # For REDUCED prophylactic application
  effect_application_R <- need.random.integers(0, round((number_yearly_prophy_application_R), digits = 0),
                                               4, round((number_yearly_prophy_application_R), digits = 0))* W
  
  # Define effect of REDUCED prophylactic pesticide application (potential to reduce fungus onset)
  effect_application_R <- effect_application_R %>%
    replace(., effect_application_R==1, effect_one_prophy_application) %>%
    replace(., effect_application_R==2, effect_two_prophy_application) %>%
    replace(., effect_application_R==3, effect_three_prophy_application) %>%
    replace(., effect_application_R==4, effect_four_prophy_application) %>%
    replace(., effect_application_R==5, effect_five_prophy_application) %>%
    replace(., effect_application_R==6, effect_six_prophy_application) %>%
    replace(., effect_application_R==7, effect_seven_prophy_application) %>%
    replace(., effect_application_R>=8, effect_eight_prophy_application)
  
  # Define effect of NORMAL and REDUCED prophylactic pesticide application for MONITORING PLAN (potential to reduce fungus onset)
  effect_application_MPN <- effect_application_N
  effect_application_MPR <- effect_application_R

  # Simulate all probabilities of infection, effect of prophylactic applications, ####
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for NORMAL prophy
  
  infected_plant_number_N <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_N <- W
  symptomatic_plants_N <- W
  detected_plants_after_monitoring_N <- W
  healthy_plants_after_fungus_fight_N <- W
  getting_again_sick_plants_N <- W
  
  for (i in 2:length(infected_plant_number_N)){ 
    infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_N[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_N)) {
    not_infected_plants_after_prophy_N[j] <- not_infected_plants_after_prophy_N[j-1] + effect_application_N[j]*
      (infected_plant_number_N[j] - not_infected_plants_after_prophy_N[j-1])}
  
  still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
  
  for (k in 2:length(symptomatic_plants_N)) {
    symptomatic_plants_N[k] <- symptomatic_plants_N[k-1] + fungus_possibility_N[k]*
      (still_infected_plants_N[k] - symptomatic_plants_N[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_N)) {
    detected_plants_after_monitoring_N[l] <- detected_plants_after_monitoring_N[l-1] + detection_factor_N[l]*
      (symptomatic_plants_N[l] - detected_plants_after_monitoring_N[l-1])}
  
  symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
  
  for (m in 2:length(getting_again_sick_plants_N)) {
    getting_again_sick_plants_N[m] <- getting_again_sick_plants_N[m-1] + disease_expansion_factor_N[m]*
      (symptomatic_plants_after_monitoring_N[m] - getting_again_sick_plants_N[m-1])}
  
  all_symptomatic_plants_N <- symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N
  
  for (n in 2:length(healthy_plants_after_fungus_fight_N)) {
    healthy_plants_after_fungus_fight_N[n] <- healthy_plants_after_fungus_fight_N[n-1] + fungus_fight_effect_N[n]*
      (all_symptomatic_plants_N[n] - healthy_plants_after_fungus_fight_N[n-1])}
  
  final_fungus_infected_plants_N <- all_symptomatic_plants_N[12] - healthy_plants_after_fungus_fight_N[12]
  
  direct_plant_losses_N <- detected_plants_after_monitoring_N[12]
  
  actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)
  
  ####Simulate infection and plant losses for NORMAL prophylactic application
  #for (i in 2:length(infected_plant_number_N)){
  #  for (j in 2:length(not_infected_plants_after_prophy_N)) {
  #      for (k in 2:length(symptomatic_plants_N)) {
  #        for (l in 2:length(detected_plants_after_monitoring_N)) {
  #          for (m in 2:length(getting_again_sick_plants_N)) {
  #            for (n in 2:length(healthy_plants_after_fungus_fight_N)) {
  
  #infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
  #  (original_plant_number - infected_plant_number_N[i-1])
  
  #not_infected_plants_after_prophy_N[j] <- not_infected_plants_after_prophy_N[j-1] + effect_application_N[j]*
  #  (infected_plant_number_N[j] - not_infected_plants_after_prophy_N[j-1])
  
  #still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
  
  #symptomatic_plants_N[k] <- symptomatic_plants_N[k-1] + fungus_possibility_N[k]*
  #                           (still_infected_plants_N[k] - symptomatic_plants_N[k-1]) 
  
  #detected_plants_after_monitoring_N[l] <- detected_plants_after_monitoring_N[l-1] + detection_factor_N[l]*
  #                                       (symptomatic_plants_N[l] - detected_plants_after_monitoring_N[l-1])
  
  #symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
  
  #getting_again_sick_plants_N[m] <- getting_again_sick_plants_N[m-1] + disease_expansion_factor_N[m]*
  #                               (symptomatic_plants_after_monitoring_N[m] - getting_again_sick_plants_N[m-1])
  
  #all_symptomatic_plants_N <- symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N
  
  #healthy_plants_after_fungus_fight_N[n] <- healthy_plants_after_fungus_fight_N[n-1] + fungus_fight_effect_N[n]*
  #                                       (all_symptomatic_plants_N[n] - healthy_plants_after_fungus_fight_N[n-1])
  
  #final_fungus_infected_plants_N <- all_symptomatic_plants_N[12] - healthy_plants_after_fungus_fight_N[12]
  
  #direct_plant_losses_N <- detected_plants_after_monitoring_N[12]
  
  #actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)}}}}}}
  
  
  
  # Simulate all probabilities of infection, effect of prophylactic applications, ####
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for REDUCED prophy
  
  infected_plant_number_R <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_R <- W
  symptomatic_plants_R <- W
  detected_plants_after_monitoring_R <- W
  healthy_plants_after_fungus_fight_R <- W
  getting_again_sick_plants_R <- W
  
  for (i in 2:length(infected_plant_number_R)){
    infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_R[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_R)) {
    not_infected_plants_after_prophy_R[j] <- not_infected_plants_after_prophy_R[j-1] + effect_application_R[j]*
      (infected_plant_number_R[j] - not_infected_plants_after_prophy_R[j-1])}
  
  still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
  
  for (k in 2:length(symptomatic_plants_R)) {
    symptomatic_plants_R[k] <- symptomatic_plants_R[k-1] + fungus_possibility_R[k]*
      (still_infected_plants_R[k] - symptomatic_plants_R[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_R)) {
    detected_plants_after_monitoring_R[l] <- detected_plants_after_monitoring_R[l-1] + detection_factor_R[l]*
      (symptomatic_plants_R[l] - detected_plants_after_monitoring_R[l-1])}
  
  symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
  
  for (m in 2:length(getting_again_sick_plants_R)) {
    getting_again_sick_plants_R[m] <- getting_again_sick_plants_R[m-1] + disease_expansion_factor_R[m]*
      (symptomatic_plants_after_monitoring_R[m] - getting_again_sick_plants_R[m-1])}
  
  all_symptomatic_plants_R <- symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R
  
  for (n in 2:length(healthy_plants_after_fungus_fight_R)) {
    healthy_plants_after_fungus_fight_R[n] <- healthy_plants_after_fungus_fight_R[n-1] + fungus_fight_effect_R[n]*
      (all_symptomatic_plants_R[n] - healthy_plants_after_fungus_fight_R[n-1])}
  
  final_fungus_infected_plants_R <- all_symptomatic_plants_R[12] - healthy_plants_after_fungus_fight_R[12]
  
  direct_plant_losses_R <- detected_plants_after_monitoring_R[12]
  
  actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)
  
  # Simulate all probabilities of infection, effect of prophylactic applications, ####
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for NORMAL prophy and MONITORING PLAN
  
  infected_plant_number_MPN <- original_plant_number * risk_per_month
  
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_MPN <- W
  symptomatic_plants_MPN <- W
  detected_plants_after_monitoring_MPN <- MPD
  healthy_plants_after_fungus_fight_MPN <- W
  getting_again_sick_plants_MPN <- W
  
  for (i in 2:length(infected_plant_number_MPN)){ 
    infected_plant_number_MPN[i] <- infected_plant_number_MPN[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_MPN[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_MPN)) {
    not_infected_plants_after_prophy_MPN[j] <- not_infected_plants_after_prophy_MPN[j-1] + effect_application_MPN[j]*
      (infected_plant_number_MPN[j] - not_infected_plants_after_prophy_MPN[j-1])}
  
  still_infected_plants_MPN <- infected_plant_number_MPN - not_infected_plants_after_prophy_MPN
  
  for (k in 2:length(symptomatic_plants_MPN)) {
    symptomatic_plants_MPN[k] <- symptomatic_plants_MPN[k-1] + fungus_possibility_MPN[k]*
      (still_infected_plants_MPN[k] - symptomatic_plants_MPN[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_MPN)) {
    detected_plants_after_monitoring_MPN[l] <- detected_plants_after_monitoring_MPN[l-1] + detection_factor_MPN[l]*
      (symptomatic_plants_MPN[l] - detected_plants_after_monitoring_MPN[l-1])}
  
  symptomatic_plants_after_monitoring_MPN <- symptomatic_plants_MPN - detected_plants_after_monitoring_MPN
  
  for (m in 2:length(getting_again_sick_plants_MPN)) {
    getting_again_sick_plants_MPN[m] <- getting_again_sick_plants_MPN[m-1] + disease_expansion_factor_MPN[m]*
      (symptomatic_plants_after_monitoring_MPN[m] - getting_again_sick_plants_MPN[m-1])}
  
  all_symptomatic_plants_MPN <- symptomatic_plants_after_monitoring_MPN + getting_again_sick_plants_MPN
  
  for (n in 2:length(healthy_plants_after_fungus_fight_MPN)) {
    healthy_plants_after_fungus_fight_MPN[n] <- healthy_plants_after_fungus_fight_MPN[n-1] + fungus_fight_effect_MPN[n]*
      (all_symptomatic_plants_MPN[n] - healthy_plants_after_fungus_fight_MPN[n-1])}
  
  final_fungus_infected_plants_MPN <- all_symptomatic_plants_MPN[12] - healthy_plants_after_fungus_fight_MPN[12]
  
  direct_plant_losses_MPN <- detected_plants_after_monitoring_MPN[12]
  
  actual_saleable_Callunas_MPN <- original_plant_number - (final_fungus_infected_plants_MPN + direct_plant_losses_MPN)
  
  # Simulate all probabilities of infection, effect of prophylactic applications, ####
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for REDUCED prophy and MONITORING PLAN
  
  infected_plant_number_MPR <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_MPR <- W
  symptomatic_plants_MPR <- W
  detected_plants_after_monitoring_MPR <- MPD
  healthy_plants_after_fungus_fight_MPR <- W
  getting_again_sick_plants_MPR <- W
  
  for (i in 2:length(infected_plant_number_MPR)){ 
    infected_plant_number_MPR[i] <- infected_plant_number_MPR[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_MPR[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_MPR)) {
    not_infected_plants_after_prophy_MPR[j] <- not_infected_plants_after_prophy_MPR[j-1] + effect_application_MPR[j]*
      (infected_plant_number_MPR[j] - not_infected_plants_after_prophy_MPR[j-1])}
  
  still_infected_plants_MPR <- infected_plant_number_MPR - not_infected_plants_after_prophy_MPR
  
  for (k in 2:length(symptomatic_plants_MPR)) {
    symptomatic_plants_MPR[k] <- symptomatic_plants_MPR[k-1] + fungus_possibility_MPR[k]*
      (still_infected_plants_MPR[k] - symptomatic_plants_MPR[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_MPR)) {
    detected_plants_after_monitoring_MPR[l] <- detected_plants_after_monitoring_MPR[l-1] + detection_factor_MPR[l]*
      (symptomatic_plants_MPR[l] - detected_plants_after_monitoring_MPR[l-1])}
  
  symptomatic_plants_after_monitoring_MPR <- symptomatic_plants_MPR - detected_plants_after_monitoring_MPR
  
  for (m in 2:length(getting_again_sick_plants_MPR)) {
    getting_again_sick_plants_MPR[m] <- getting_again_sick_plants_MPR[m-1] + disease_expansion_factor_MPR[m]*
      (symptomatic_plants_after_monitoring_MPR[m] - getting_again_sick_plants_MPR[m-1])}
  
  all_symptomatic_plants_MPR <- symptomatic_plants_after_monitoring_MPR + getting_again_sick_plants_MPR
  
  for (n in 2:length(healthy_plants_after_fungus_fight_MPR)) {
    healthy_plants_after_fungus_fight_MPR[n] <- healthy_plants_after_fungus_fight_MPR[n-1] + fungus_fight_effect_MPR[n]*
      (all_symptomatic_plants_MPR[n] - healthy_plants_after_fungus_fight_MPR[n-1])}
  
  final_fungus_infected_plants_MPR <- all_symptomatic_plants_MPR[12] - healthy_plants_after_fungus_fight_MPR[12]
  
  direct_plant_losses_MPR <- detected_plants_after_monitoring_MPR[12]
  
  actual_saleable_Callunas_MPR <- original_plant_number - (final_fungus_infected_plants_MPR + direct_plant_losses_MPR)
  
  ####Simulate infection and plant losses for REDUCED prophylactic application
  #for (i in 2:length(infected_plant_number_R)){
  #  for (j in 2:length(not_infected_plants_after_prophy_R)) {
  #    for (k in 2:length(symptomatic_plants_R)) {
  #      for (l in 2:length(detected_plants_after_monitoring_R)) {
  #        for (m in 2:length(getting_again_sick_plants_R)) {
  #          for (n in 2:length(healthy_plants_after_fungus_fight_R)) {
  
  #            infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
  #              (original_plant_number - infected_plant_number_R[i-1])
  
  #            not_infected_plants_after_prophy_R[j] <- not_infected_plants_after_prophy_R[j-1] + effect_application_R[j]*
  #              (infected_plant_number_R[j] - not_infected_plants_after_prophy_R[j-1])
  
  #            still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
  
  #            symptomatic_plants_R[k] <- symptomatic_plants_R[k-1] + fungus_possibility_R[k]*
  #              (still_infected_plants_R[k] - symptomatic_plants_R[k-1]) 
  
  #            detected_plants_after_monitoring_R[l] <- detected_plants_after_monitoring_R[l-1] + detection_factor_R[l]*
  #              (symptomatic_plants_R[l] - detected_plants_after_monitoring_R[l-1])
  
  #            symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
  
  #            getting_again_sick_plants_R[m] <- getting_again_sick_plants_R[m-1] + disease_expansion_factor_R[m]*
  #              (symptomatic_plants_after_monitoring_R[m] - getting_again_sick_plants_R[m-1])
  
  #            all_symptomatic_plants_R <- symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R
  
  #            healthy_plants_after_fungus_fight_R[n] <- healthy_plants_after_fungus_fight_R[n-1] + fungus_fight_effect_R[n]*
  #              (all_symptomatic_plants_R[n] - healthy_plants_after_fungus_fight_R[n-1])
  
  #            final_fungus_infected_plants_R <- all_symptomatic_plants_R[12] - healthy_plants_after_fungus_fight_R[12]
  
  #            direct_plant_losses_R <- detected_plants_after_monitoring_R[12]
  
  #            actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)}}}}}}
  
  
  #### Calculation of Calluna monetary value for NORMAL prophy cultivation ####
  # Differ between saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_N <- actual_saleable_Callunas_N * value_of_saleable_Calluna
  value_of_sorted_out_plants_N <- direct_plant_losses_N * value_sorted_out_Calluna
  value_of_not_saleable_plants_N <- final_fungus_infected_plants_N * value_not_saleable_Calluna
  
  #### Calculation of Calluna monetary value for REDUCED prophy cultivation ####
  # Differ between saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_R <- actual_saleable_Callunas_R * value_of_saleable_more_sustainable_Calluna
  value_of_sorted_out_plants_R <- direct_plant_losses_R * value_sorted_out_Calluna
  value_of_not_saleable_plants_R <- final_fungus_infected_plants_R * value_not_saleable_Calluna
  
  #### Calculation of Calluna monetary value for NORMAL and REDUCED prophy with MONITORING PLAN cultivation ####
  # Differ between saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_MPN <- actual_saleable_Callunas_MPN * value_of_saleable_Calluna
  value_of_sorted_out_plants_MPN <- direct_plant_losses_MPN * value_sorted_out_Calluna
  value_of_not_saleable_plants_MPN <- final_fungus_infected_plants_MPN * value_not_saleable_Calluna
  
  value_saleable_plants_MPR <- actual_saleable_Callunas_MPR * value_of_saleable_more_sustainable_Calluna
  value_of_sorted_out_plants_MPR <- direct_plant_losses_MPR * value_sorted_out_Calluna
  value_of_not_saleable_plants_MPR <- final_fungus_infected_plants_MPR * value_not_saleable_Calluna
  
  #### List up yearly costs for whole production area for NORMAL prophy ####
  costs_per_year_N <- (value_of_one_new_plant * original_plant_number) +
    value_of_sorted_out_plants_N +
    value_of_not_saleable_plants_N +
    costs_water * production_area +
    costs_yearly_prophy_application_N +
    costs_normal_fertilizer * production_area +
   (costs_monitoring_per_ha_month * sum(W) * production_area) +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_N * production_area
  
  
  #### List up the benefits for each year for whole production area for NORMAL prophy ####
  benefits_per_year_N <- value_saleable_plants_N
  
  
  #### List up costs for each year for whole production area for REDUCED prophy ####
  # Optimizing in monitoring leads to higher costs per hectar because of more time and staff use
  # Threshold is defined from 1 - 3 ha production area
  costs_staff <- ifelse(production_area > threshold_big_area_more_staff, costs_staff + 
                          costs_more_staff, costs_staff)
  
  #### List up yearly costs for whole production area for REDUCED prophy ####
  costs_per_year_R <- (value_of_one_new_plant * original_plant_number) +
    value_of_sorted_out_plants_R +
    value_of_not_saleable_plants_R +
    costs_water * production_area +
    costs_yearly_prophy_application_R +
   (costs_normal_fertilizer + fertilizer_adjustment) * production_area +
   (costs_monitoring_per_ha_month * sum(W) * production_area) +
    additional_costs_more_monitoring_per_ha * production_area +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_R * production_area
  
  
  
  # List up the benefits for each year for whole production area for REDUCED prophy
  benefits_per_year_R <- value_saleable_plants_R
  
  #### List up yearly costs for whole production area for NORMAL prophy and MONITORING PLAN####
  # For MONITORING PLAN also Lab costs (costs_per_sample) must be mentioned
  costs_per_year_MPN <- (value_of_one_new_plant * original_plant_number) +
    value_of_sorted_out_plants_MPN +
    value_of_not_saleable_plants_MPN +
   (direct_plant_losses_MPN * amount_of_samples_MP * costs_per_sample_MP) +
    costs_water * production_area +
    costs_yearly_prophy_application_MPN +
    costs_normal_fertilizer * production_area +
   (costs_monitoring_plan_per_ha * production_area) +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_N * production_area
  
  # List up the benefits for each year for whole production area for NORMAL application
  # and MONITORING PLAN. Producers say they have potential resource savings due to MP
  benefits_per_year_MPN <- value_saleable_plants_MPN + (savings_due_to_MP * production_area)
    
  #### List up yearly costs for whole production area for REDUCED prophy and MONITORING PLAN####
  # For MONITORING PLAN also Lab costs (costs_per_sample) must be mentioned
  costs_per_year_MPR <- (value_of_one_new_plant * original_plant_number) +
    value_of_sorted_out_plants_MPR +
    value_of_not_saleable_plants_MPR +
   (direct_plant_losses_MPR * amount_of_samples_MP * costs_per_sample_MP) +
    costs_water * production_area +
    costs_yearly_prophy_application_MPR +
   (costs_normal_fertilizer + fertilizer_adjustment) * production_area +
   (costs_monitoring_plan_per_ha * production_area) +
    additional_costs_more_monitoring_per_ha * production_area +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_R * production_area
  
  # List up the benefits for each year for whole production area for REDUCED application
  # and MONITORING PLAN. Producers say they have potential resource savings due to MP 
  benefits_per_year_MPR <- value_saleable_plants_MPR + (savings_due_to_MP * production_area)
  
  #### Benefits and outputs ####
  
  # Define all benefits and costs for NORMAL PROPHY APPLICATION                         N
  # Define all benefits and costs for REDUCED PROPHY APPLICATION                        R
  # Define all benefits and costs for NORMAL PROPHY APPLICATION and MONITORING PLAN     MPN
  # Define all benefits and costs for REDUCED PROPHY APPLICATION and MONITORING PLAN    MPR
  #### Just change the letters to get the prefered output #### 
  
  benefits <- benefits_per_year_MPR
  costs <- costs_per_year_MPR
  
  #### cashflow and results #### 
  cashflow <- benefits - costs
  NPV <- decisionSupport::discount(x= cashflow, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  
  return(list(NetPresentValue = NPV))
  
}



input_table <- "Calluna_low_prophy.csv"
legend_file <- "Calluna_low_prophy_Legend.csv"
results_folder <- "Results_low_prophy"
figures_folder <- "Figures_low_prophy"

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
