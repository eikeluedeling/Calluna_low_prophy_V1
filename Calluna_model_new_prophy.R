
library(signal)
library(DAutilities)
library(Surrogate)
library(decisionSupport)
library(tidyverse)

#setwd("C:/Users/ZEF/Desktop/Calluna_Folder")

Calluna_model_new_prophy <- function(x, varnames){
  
  #Calculate the plant amount of whole production area
  original_plant_number <- production_area * plants_per_ha
  
  #Define risky months                
  weather_arguments_for_infection <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  
  #Simulate the infection risk for each month under uncertainty
  #risk_per_month <- runif(4, 0, (infection_risk>=0)) * weather_arguments_for_infection 
  
  risk_per_month <- vv(infection_risk, var_CV, 12)* weather_arguments_for_infection
  risk_per_month <- sapply(risk_per_month, function(x) max(c(min(c(1,x)),0)))
  
  #Settings for N risks in % so that they will not be higher 1 or lower 0
  #For NORMAL prophylactic application
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
  #in accidentally selected application amounts per months
  
  need.random.integers <- function(a,b,n,k){
    #finds n random integers in range a:b which sum to k
    while(TRUE){
      x <- sample(1:(k - n*a),n-1, replace = TRUE)
      x <- sort(x)
      x <- c(x,k-n*a) - c(0,x)
      if(max(x) <= b-a) return(a+x)
    }
  }
  
  #For normal application (number_yearly_prophy_application_N)
  effect_application_N <- need.random.integers(0, round((number_yearly_prophy_application_N), digits = 0),
                                               4, round((number_yearly_prophy_application_N), digits = 0)) 
  effect_application_N <- effect_application_N * weather_arguments_for_infection
  
  
  #Define effect of normal prophylactic pesticide application (potential to reduce fungus onset)
  #Estimated effect of fungus reduce potential are interlinked with each number of application per month   
  effect_application_N <- replace(effect_application_N, effect_application_N==1, effect_one_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==2, effect_two_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==3, effect_three_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==4, effect_four_prophy_application) 
  effect_application_N <- replace(effect_application_N, effect_application_N==5, effect_five_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==6, effect_six_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==7, effect_seven_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==8, effect_eight_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N>=8, effect_eight_prophy_application)
  
  #For reduced application (number_yearly_prophy_application_R)
  effect_application_R <- need.random.integers(0, round((number_yearly_prophy_application_R), digits = 0),
                                               4, round((number_yearly_prophy_application_R), digits = 0))
  effect_application_R <- effect_application_R * weather_arguments_for_infection
  
  #Define effect of reduced prophylactic pesticide application (potential to reduce fungus onset)
  effect_application_R <- replace(effect_application_R, effect_application_R==1, effect_one_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==2, effect_two_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==3, effect_three_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==4, effect_four_prophy_application)  
  effect_application_R <- replace(effect_application_R, effect_application_R==5, effect_five_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==6, effect_six_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==7, effect_seven_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==8, effect_eight_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R>=8, effect_eight_prophy_application)
  
  
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
  for(i in 2:length(infected_plant_number))
    {infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
                  (original_plant_number - infected_plant_number_N[i-1])
    
     not_infected_plants_after_prophy_N[i] <- not_infected_plants_after_prophy_N[i-1] + effect_application_N[i]*
      (infected_plant_number_N[i] - not_infected_plants_after_prophy_N[i-1])
    
     still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
     
     symptomatic_plants_N[i] <- symptomatic_plants_N[i-1] + fungus_possibility_N[i]* 
                             (still_infected_plants_N[i] - symptomatic_plants_N[i-1])
     
     detected_plants_after_monitoring_N[i] <- detected_plants_after_monitoring_N[i-1] + detection_factor_N[i]*
                                           (symptomatic_plants_N[i] - detected_plants_after_monitoring_N[i-1])
     
     symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
     
     
     getting_again_sick_plants_N[i] <- getting_again_sick_plants_N[i-1] + disease_expansion_factor_N[i]*
                                    (symptomatic_plants_after_monitoring_N[i] - getting_again_sick_plants_N[i-1])
     
     
     all_symptomatic_plants_N <- (symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N) 
     
     healthy_plants_after_fungus_fight_N[i] <- healthy_plants_after_fungus_fight_N[i-1] + fungus_fight_effect_N[i]*
                                            (all_symptomatic_plants_N[i] - healthy_plants_after_fungus_fight_N[i-1])
     
     ####Why I get negative values? Cause could be the distance between healthy and symptomatic plants####
     ####Zero for negative values is just a temporary solution
     final_fungus_infected_plants_N <- ifelse((all_symptomatic_plants_N - healthy_plants_after_fungus_fight_N)<0,
                                            0,(all_symptomatic_plants_N - healthy_plants_after_fungus_fight_N)) 
     
     direct_plant_losses_N <- detected_plants_after_monitoring_N
     
  }
  
  ####Simulate infection and plant losses for NORMAL prophylactic application####
  infected_plant_number_R <- original_plant_number * risk_per_month
  not_infected_plants_after_prophy_R <- weather_arguments_for_infection
  symptomatic_plants_R <- weather_arguments_for_infection
  detected_plants_after_monitoring_R <- weather_arguments_for_infection
  healthy_plants_after_fungus_fight_R <- weather_arguments_for_infection
  getting_again_sick_plants_R <- weather_arguments_for_infection
  
  ####Simulate infection and plant losses for REDUCED prophylactic application####
  for(i in 2:length(infected_plant_number_R))
  {infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
    (original_plant_number - infected_plant_number_R[i-1])
  
  not_infected_plants_after_prophy_R[i] <- not_infected_plants_after_prophy_R[i-1] + effect_application_R[i]*
    (infected_plant_number_R[i] - not_infected_plants_after_prophy_R[i-1])
  
  still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
  
  symptomatic_plants_R[i] <- symptomatic_plants_R[i-1] + fungus_possibility_R[i]* 
    (still_infected_plants_R[i] - symptomatic_plants_R[i-1])
  
  detected_plants_after_monitoring_R[i] <- detected_plants_after_monitoring_R[i-1] + detection_factor_R[i]*
    (symptomatic_plants_R[i] - detected_plants_after_monitoring_R[i-1])
  
  symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
  
  
  getting_again_sick_plants_R[i] <- getting_again_sick_plants_R[i-1] + disease_expansion_factor_R[i]*
    (symptomatic_plants_after_monitoring_R[i] - getting_again_sick_plants_R[i-1])
  
  
  all_symptomatic_plants_R <- (symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R) 
  
  healthy_plants_after_fungus_fight_R[i] <- healthy_plants_after_fungus_fight_R[i-1] + fungus_fight_effect_R[i]*
    (all_symptomatic_plants_R[i] - healthy_plants_after_fungus_fight_R[i-1])
  
  ####Same problem as above:####
  ####Why I get negative values? Cause could be the distance between healthy and symptomatic plants
  ####Zero for negative values is just a temporary solution
  #Is this now solved?
  final_fungus_infected_plants_R <- ifelse((all_symptomatic_plants_R - healthy_plants_after_fungus_fight_R)<0,
                                           0,(all_symptomatic_plants_R - healthy_plants_after_fungus_fight_R)) 
  
  direct_plant_losses_R <- detected_plants_after_monitoring_R
  
  }
  
  actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)
  
  actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)
  
  benefits <- actual_saleable_Callunas_N
  costs <- actual_saleable_Callunas_R
  
  #cashflow and results 
  cashflow <- benefits - costs
  NPV <- discount(cashflow, discount_rate = discount_rate, TRUE)
  
  
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
                write_table = TRUE, Calluna_model_new_prophy, 10000,
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
  
   ###symptomatic_plants[i] <- symptomatic_plants[i-1] + (infected_plant_number[6] - symptomatic_plants[6-1])*
   ###   vv(fungus_onset_possibility_low_risk, var_CV, 1)
     
     
   ###  sick_plants_after_prophy[i] <- sick_plants_after_prophy[i] + ((infected_plant_number[i] - symptomatic_plants[i-1]) *
   ###     vv(prophy_effect, var_CV, 1))
     
   ###  healthy_after_prophy <- symptomatic_plants - sick_plants_after_prophy
     
  # Calculating costs of different decisions
  # Normal and reduced prophylactic pesticide application (costs) in Calluna production system per year
  costs_normal_yearly_application <- round((number_yearly_prophy_application_N),digits=0) * 
       cost_one_prophy_application * production_area
     
  costs_reduced_yearly_application <- round((number_yearly_prophy_application_R),digits=0) * 
       cost_one_prophy_application * production_area
 

  ####Calculation Calluna worth for normal cultivation####
  #Differ between saleable, sorted out and not saleable plants after cultivation time
  saleable_Calluna_N <-  sum(original_plant_amount) - (direct_plant_losses_N[8] + final_fungus_infected_plants_N)
  
  value_saleable_plants_N <- saleable_Calluna_N * value_of_saleable_Calluna
  value_of_sorted_out_plants_N <- direct_plant_losses_N * value_sorted_out_Calluna
  value_of_not_saleable_plants_N <- final_fungus_infected_plants_N * value_not_saleable_Calluna

  ####Calculation Calluna worth for reduced prophy cultivation####
  #Differ between saleable, sorted out and not saleable plants after cultivation time
  saleable_Calluna_R <-  sum(original_plant_amount) - (direct_plant_losses_N[8] + final_fungus_infected_plants_N)
  
  value_saleable_plants_R <- saleable_Calluna_R * value_of_saleable_Calluna
  value_of_sorted_out_plants_R <- direct_plant_losses_R * value_sorted_out_Calluna
  value_of_not_saleable_plants_R <- final_fungus_infected_plants_R * value_not_saleable_Calluna
  
  ####List up the costs for each year for whole production area for normal application####
  costs_per_year_N <- (value_of_one_new_plant * original_plant_amount) +
    value_of_sorted_out_plants_N +
    value_of_not_saleable_plants_N +
    (costs_water * production_area) +
    (costs_normal_yearly_application) +
    (costs_normal_fertilizer * production_area) +
    (costs_monitoring_per_ha_month * sum(weather_arguments_for_infection) * production_area) +
    (costs_staff) +
    (environmental_impact_direct_fungus_fight_N * production_area) +
    (yearly_cost_of_direct_fungus_fight_N * production_area) +
    (factor_of_prophy_pollution * production_area * environmental_impact_prophy_application_fungus_N)
    
  
  #List up the benefits for each year for whole production area for normal application
  benefits_per_year_N <- value_saleable_plants_N
  
  
  ####List up costs for each year for whole production area for reduced application####
  #Optimizing in monitoring leads to higher costs per hectar because of more time use
  #Threshold is defined from 1 - 3 ha production area
  costs_staff <- ifelse(production_area > threshold_big_area_more_staff, costs_staff + 
                          costs_more_staff, costs_staff)
  
  
  costs_per_year_R <- (value_of_one_new_plant * original_plant_amount) +
    value_of_sorted_out_plants_R +
    value_of_not_saleable_plants_R +
    (costs_water * production_area) +
    (costs_reduced_yearly_application) +
    ((costs_normal_fertilizer + fertilizer_adjustment) * production_area) +
    (costs_monitoring_per_ha_month * sum(weather_arguments_for_infection) * production_area) +
    (additional_costs_more_monitoring_per_ha * production_area) +
    (costs_staff) +
    (environmental_impact_direct_fungus_fight_R * production_area) +
    (yearly_cost_of_direct_fungus_fight_R * production_area) +
    (factor_of_prophy_pollution * production_area * environmental_impact_prophy_application_fungus_R)
  
  
  
  #List up the benefits for each year for whole production area for reduced application
  benefits_per_year_R <- value_saleable_plants_R
  
  ####Events which occure at most one time in 10 years####
  #List up the possible benefits after 10 years for reduced application and more monitoring
  #Idea: Define future yield of sustainability after ten years (e.g. regarding to more customers and ecological benefits)
  
  resource_and_sustainability_benefits_after_10_years_R <-value_of_higher_acceptance_per_ha * production_area +
    value_less_health_risk_of_staff +
    value_future_production_safety * production_area +
    (1 + value_future_better_selling_per_plant) * value_saleable_plants_R +
    value_of_future_less_resistance_risk * production_area 

  
  #Define all benefits and costs for NORMAL APPLICATION
  #Define all benefits and costs for REDUCED APPLICATION 
  ####Just change N to R or vice versa#### 
  
  benefits <- benefits_per_year_R
  costs <- costs_per_year_R
  
  


=======

library(signal)
library(DAutilities)
library(Surrogate)
library(decisionSupport)
library(tidyverse)

setwd("C:/Users/ZEF/Desktop/Calluna_Folder")

Calluna_model_new_prophy <- function(x, varnames){
  
  #Calculate the plant amount of whole production area
  original_plant_number <- production_area * plants_per_ha
  
  #Define risky months                
  weather_arguments_for_infection <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  
  #Simulate the infection risk for each month under uncertainty
  #risk_per_month <- runif(4, 0, (infection_risk>=0)) * weather_arguments_for_infection 
  
  risk_per_month <- vv(infection_risk, var_CV, 12)* weather_arguments_for_infection
  risk_per_month <- sapply(risk_per_month, function(x) max(c(min(c(1,x)),0)))
  
  #Settings for N risks in % so that they will not be higher 1 or lower 0
  #For NORMAL prophylactic application
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
  #in accidentally selected application amounts per months
  
  need.random.integers <- function(a,b,n,k){
    #finds n random integers in range a:b which sum to k
    while(TRUE){
      x <- sample(1:(k - n*a),n-1, replace = TRUE)
      x <- sort(x)
      x <- c(x,k-n*a) - c(0,x)
      if(max(x) <= b-a) return(a+x)
    }
  }
  
  #For normal application (number_yearly_prophy_application_N)
  effect_application_N <- need.random.integers(0, round((number_yearly_prophy_application_N), digits = 0),
                                               4, round((number_yearly_prophy_application_N), digits = 0)) 
  effect_application_N <- effect_application_N * weather_arguments_for_infection
  
  
  #Define effect of normal prophylactic pesticide application (potential to reduce fungus onset)
  #Estimated effect of fungus reduce potential are interlinked with each number of application per month   
  effect_application_N <- replace(effect_application_N, effect_application_N==1, effect_one_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==2, effect_two_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==3, effect_three_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==4, effect_four_prophy_application) 
  effect_application_N <- replace(effect_application_N, effect_application_N==5, effect_five_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==6, effect_six_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==7, effect_seven_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N==8, effect_eight_prophy_application)
  effect_application_N <- replace(effect_application_N, effect_application_N>=8, effect_eight_prophy_application)
  
  #For reduced application (number_yearly_prophy_application_R)
  effect_application_R <- need.random.integers(0, round((number_yearly_prophy_application_R), digits = 0),
                                               4, round((number_yearly_prophy_application_R), digits = 0))
  effect_application_R <- effect_application_R * weather_arguments_for_infection
  
  #Define effect of reduced prophylactic pesticide application (potential to reduce fungus onset)
  effect_application_R <- replace(effect_application_R, effect_application_R==1, effect_one_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==2, effect_two_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==3, effect_three_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==4, effect_four_prophy_application)  
  effect_application_R <- replace(effect_application_R, effect_application_R==5, effect_five_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==6, effect_six_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==7, effect_seven_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R==8, effect_eight_prophy_application)
  effect_application_R <- replace(effect_application_R, effect_application_R>=8, effect_eight_prophy_application)
  
  
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
  for(i in 2:length(infected_plant_number)) #this variable gives an error, is not defined and is not in the input file
    {infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
                  (original_plant_number - infected_plant_number_N[i-1])
    
     not_infected_plants_after_prophy_N[i] <- not_infected_plants_after_prophy_N[i-1] + effect_application_N[i]*
      (infected_plant_number_N[i] - not_infected_plants_after_prophy_N[i-1])
    
     still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
     
     symptomatic_plants_N[i] <- symptomatic_plants_N[i-1] + fungus_possibility_N[i]* 
                             (still_infected_plants_N[i] - symptomatic_plants_N[i-1])
     
     detected_plants_after_monitoring_N[i] <- detected_plants_after_monitoring_N[i-1] + detection_factor_N[i]*
                                           (symptomatic_plants_N[i] - detected_plants_after_monitoring_N[i-1])
     
     symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
     
     
     getting_again_sick_plants_N[i] <- getting_again_sick_plants_N[i-1] + disease_expansion_factor_N[i]*
                                    (symptomatic_plants_after_monitoring_N[i] - getting_again_sick_plants_N[i-1])
     
     
     all_symptomatic_plants_N <- (symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N) 
     
     healthy_plants_after_fungus_fight_N[i] <- healthy_plants_after_fungus_fight_N[i-1] + fungus_fight_effect_N[i]*
                                            (all_symptomatic_plants_N[i] - healthy_plants_after_fungus_fight_N[i-1])
     
     ####Why I get negative values? Cause could be the distance between healthy and symptomatic plants####
     ####Zero for negative values is just a temporary solution
     final_fungus_infected_plants_N <- ifelse((all_symptomatic_plants_N - healthy_plants_after_fungus_fight_N)<0,
                                            0,(all_symptomatic_plants_N - healthy_plants_after_fungus_fight_N)) 
     
     direct_plant_losses_N <- detected_plants_after_monitoring_N
     
  }
  
  ####Simulate infection and plant losses for REDUCED prophylactic application####
  infected_plant_number_R <- original_plant_number * risk_per_month
  not_infected_plants_after_prophy_R <- weather_arguments_for_infection
  symptomatic_plants_R <- weather_arguments_for_infection
  detected_plants_after_monitoring_R <- weather_arguments_for_infection
  healthy_plants_after_fungus_fight_R <- weather_arguments_for_infection
  getting_again_sick_plants_R <- weather_arguments_for_infection
  
  ####Simulate infection and plant losses for REDUCED prophylactic application####
  for(i in 2:length(infected_plant_number_R))
  {infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
    (original_plant_number - infected_plant_number_R[i-1])
  
  not_infected_plants_after_prophy_R[i] <- not_infected_plants_after_prophy_R[i-1] + effect_application_R[i]*
    (infected_plant_number_R[i] - not_infected_plants_after_prophy_R[i-1])
  
  still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
  
  symptomatic_plants_R[i] <- symptomatic_plants_R[i-1] + fungus_possibility_R[i]* 
    (still_infected_plants_R[i] - symptomatic_plants_R[i-1])
  
  detected_plants_after_monitoring_R[i] <- detected_plants_after_monitoring_R[i-1] + detection_factor_R[i]*
    (symptomatic_plants_R[i] - detected_plants_after_monitoring_R[i-1])
  
  symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
  
  
  getting_again_sick_plants_R[i] <- getting_again_sick_plants_R[i-1] + disease_expansion_factor_R[i]*
    (symptomatic_plants_after_monitoring_R[i] - getting_again_sick_plants_R[i-1])
  
  
  all_symptomatic_plants_R <- (symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R) 
  
  healthy_plants_after_fungus_fight_R[i] <- healthy_plants_after_fungus_fight_R[i-1] + fungus_fight_effect_R[i]*
    (all_symptomatic_plants_R[i] - healthy_plants_after_fungus_fight_R[i-1])
  
  ####Same problem as above:####
  ####Why I get negative values? Cause could be the distance between healthy and symptomatic plants
  ####Zero for negative values is just a temporary solution
  final_fungus_infected_plants_R <- ifelse((all_symptomatic_plants_R - healthy_plants_after_fungus_fight_R)<0,
                                           0,(all_symptomatic_plants_R - healthy_plants_after_fungus_fight_R)) 
  
  direct_plant_losses_R <- detected_plants_after_monitoring_R
  
  }
  
  actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)
  
  actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)
  
  benefits <- actual_saleable_Callunas_N
  costs <- actual_saleable_Callunas_R
  
  #cashflow and results 
  cashflow <- benefits - costs
  NPV <- discount(cashflow, discount_rate = discount_rate, TRUE)
  
  
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
                write_table = TRUE, Calluna_model_new_prophy, 10000,
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
                                  
   ##################The following subjects of the code are the residuals of the old code##############################
                               
   ###symptomatic_plants[i] <- symptomatic_plants[i-1] + (infected_plant_number[6] - symptomatic_plants[6-1])*
   ###   vv(fungus_onset_possibility_low_risk, var_CV, 1)
     
     
   ###  sick_plants_after_prophy[i] <- sick_plants_after_prophy[i] + ((infected_plant_number[i] - symptomatic_plants[i-1]) *
   ###     vv(prophy_effect, var_CV, 1))
     
   ###  healthy_after_prophy <- symptomatic_plants - sick_plants_after_prophy
     
  # Calculating costs of different decisions
  # Normal and reduced prophylactic pesticide application (costs) in Calluna production system per year
  costs_normal_yearly_application <- round((number_yearly_prophy_application_N),digits=0) * 
       cost_one_prophy_application * production_area
     
  costs_reduced_yearly_application <- round((number_yearly_prophy_application_R),digits=0) * 
       cost_one_prophy_application * production_area
 

  ####Calculation Calluna worth for normal cultivation####
  #Differ between saleable, sorted out and not saleable plants after cultivation time
  saleable_Calluna_N <-  sum(original_plant_amount) - (direct_plant_losses_N[8] + final_fungus_infected_plants_N)
  
  value_saleable_plants_N <- saleable_Calluna_N * value_of_saleable_Calluna
  value_of_sorted_out_plants_N <- direct_plant_losses_N * value_sorted_out_Calluna
  value_of_not_saleable_plants_N <- final_fungus_infected_plants_N * value_not_saleable_Calluna

  ####Calculation Calluna worth for reduced prophy cultivation####
  #Differ between saleable, sorted out and not saleable plants after cultivation time
  saleable_Calluna_R <-  sum(original_plant_amount) - (direct_plant_losses_N[8] + final_fungus_infected_plants_N)
  
  value_saleable_plants_R <- saleable_Calluna_R * value_of_saleable_Calluna
  value_of_sorted_out_plants_R <- direct_plant_losses_R * value_sorted_out_Calluna
  value_of_not_saleable_plants_R <- final_fungus_infected_plants_R * value_not_saleable_Calluna
  
  ####List up the costs for each year for whole production area for normal application####
  costs_per_year_N <- (value_of_one_new_plant * original_plant_amount) +
    value_of_sorted_out_plants_N +
    value_of_not_saleable_plants_N +
    (costs_water * production_area) +
    (costs_normal_yearly_application) +
    (costs_normal_fertilizer * production_area) +
    (costs_monitoring_per_ha_month * sum(weather_arguments_for_infection) * production_area) +
    (costs_staff) +
    (environmental_impact_direct_fungus_fight_N * production_area) +
    (yearly_cost_of_direct_fungus_fight_N * production_area) +
    (factor_of_prophy_pollution * production_area * environmental_impact_prophy_application_fungus_N)
    
  
  #List up the benefits for each year for whole production area for normal application
  benefits_per_year_N <- value_saleable_plants_N
  
  
  ####List up costs for each year for whole production area for reduced application####
  #Optimizing in monitoring leads to higher costs per hectar because of more time use
  #Threshold is defined from 1 - 3 ha production area
  costs_staff <- ifelse(production_area > threshold_big_area_more_staff, costs_staff + 
                          costs_more_staff, costs_staff)
  
  
  costs_per_year_R <- (value_of_one_new_plant * original_plant_amount) +
    value_of_sorted_out_plants_R +
    value_of_not_saleable_plants_R +
    (costs_water * production_area) +
    (costs_reduced_yearly_application) +
    ((costs_normal_fertilizer + fertilizer_adjustment) * production_area) +
    (costs_monitoring_per_ha_month * sum(weather_arguments_for_infection) * production_area) +
    (additional_costs_more_monitoring_per_ha * production_area) +
    (costs_staff) +
    (environmental_impact_direct_fungus_fight_R * production_area) +
    (yearly_cost_of_direct_fungus_fight_R * production_area) +
    (factor_of_prophy_pollution * production_area * environmental_impact_prophy_application_fungus_R)
  
  
  
  #List up the benefits for each year for whole production area for reduced application
  benefits_per_year_R <- value_saleable_plants_R
  
  ####Events which occure at most one time in 10 years####
  #List up the possible benefits after 10 years for reduced application and more monitoring
  #Idea: Define future yield of sustainability after ten years (e.g. regarding to more customers and ecological benefits)
  
  resource_and_sustainability_benefits_after_10_years_R <-value_of_higher_acceptance_per_ha * production_area +
    value_less_health_risk_of_staff +
    value_future_production_safety * production_area +
    (1 + value_future_better_selling_per_plant) * value_saleable_plants_R +
    value_of_future_less_resistance_risk * production_area 

  
  #Define all benefits and costs for NORMAL APPLICATION
  #Define all benefits and costs for REDUCED APPLICATION 
  ####Just change N to R or vice versa#### 
  
  benefits <- benefits_per_year_R
  costs <- costs_per_year_R
