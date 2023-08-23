#---------------------------------------------------------------------#
# Script: The Economic Costs of the War in Donbas for the Affected    #
#         Ukrainian Regions                                           #
#                                                                     #
# Authors: Frantisek Masek                                            #
#          Renan Serenini                                             #
#                                                                     #
# Short abstract: We use the Synthetic Difference-in-differences      #
#                 estimator to measure the economic effects of the    # 
#                 war in Donbas on Luhansk and Donetsk oblasts        #
#                 between the years 2014 and 2019.                    #
#                                                                     # 
# Last update: 12/08/2023                                             #
#---------------------------------------------------------------------#


#---------------------------------------------------------------------#
# Preparation --------------------------------------------------------
#---------------------------------------------------------------------#


# Clear working environment #
rm(list=ls(all=TRUE))

# Load packages #
library(readxl)
library(synthdid)
library(plm)

#Load data#
Ukraine<-as.data.frame(read_excel("Ukraine_data.xlsx"))

#List of variables of interest
variables= c("Income", "GRP", "Unemployment", "Investments")


#Custom function#

# It applies the estimation from synthdid package and return a list with results
# @param dataset is a dataframe
# @param vars is a list of the variables to be used as dependent variable, once at a time
# @param method is the package options for the estimator: sdid, scm and did
# returns a nested list that includes: a vector with the point estimates, a vector with the standard errors, and a list with the resulting objects

estimation <- function(data,vars, method= "sdid"){
  
  #Empty vectors to store results
  tau <- c() #vector with point estimates
  std_error <- c() #vector with standard errors
  tau_list <- list() #list with resulting objects
  
  #loop to run one estimation for each variable
  for (variable in vars) {
    
    #Slice data and store as panel
    data_subset <- na.omit(data[c("Region", "Year", variable, "Treated")])
    setup = panel.matrices(data_subset)
    
    #Run SDID and get standard errors (values differ from the paper, as it uses randomization)
    if (method == "sdid"){
      tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    } else if (method == "scm"){
      tau.hat = sc_estimate(setup$Y, setup$N0, setup$T0)
    } else if (method == "did"){
      tau.hat = did_estimate(setup$Y, setup$N0, setup$T0)
    }
    
    se = sqrt(vcov(tau.hat, method='placebo'))
    
    #Append results
    tau <- c(tau,tau.hat) 
    std_error <- c(std_error,se) 
    tau_list <- append(tau_list,list(tau.hat)) 
  }
  
  return( list(tau,std_error,tau_list))
}

#---------------------------------------------------------------------#
# Table 1 - The ATE and s.e. for each variable of interest------------
#---------------------------------------------------------------------#


results_sdid=estimation(Ukraine, variables , method = "sdid")

Table1 <- rbind(results_sdid[[1]], results_sdid[[2]])
colnames(Table1) <- variables
row.names(Table1) <- c("Estimate", "Standard error");Table1


#---------------------------------------------------------------------#
# Figures 1 and 2 - SDID plots ---------------------------------------
#---------------------------------------------------------------------#

tau_list_sdid <- results_sdid[[3]]
plot_list <- list() #list to store plots

#Define plot settings and store plots in a list
for (i in 1:4) {
  plot_list[[i]] <- plot(tau_list_sdid[[i]], treated.name = 'Donbas',
                        control.name = 'Doppelganger', effect.curvature=-.4,
                        line.width = 2.5,point.size = 9,trajectory.linetype = 6,
                        trajectory.alpha=0.7, effect.alpha=0.7, diagram.alpha=1,
                        onset.alpha=1) + theme(legend.position="bottom", 
                        legend.direction='horizontal',legend.key=element_blank(),
                        legend.background=element_blank(),
                        legend.text = element_text(size=30),
                        legend.key.size = unit(2, 'cm'),
                        strip.background=element_blank(),
                        strip.text.x = element_blank())+
                        theme(axis.text.x = element_text(size = 30), 
                        axis.text.y = element_text(size = 30))
}

#Save each plot as a pdf file
for (i in 1:4) {
  filename <- paste(variables[i], "_plot.pdf", sep = "")
  file.remove(filename)
  pdf(filename, width = 8, height = 12, paper = "a4r")
  print(plot_list[[i]])
  dev.off()
}

#---------------------------------------------------------------------#
# Figures 3 and 4 - Region by region differences ---------------------
#---------------------------------------------------------------------#

plot_list_2 <- list() #list to store plots
#Define plot settings and store plots in a list
for (i in 1:4) {
  plot_list_2[[i]] <- synthdid_units_plot(tau_list_sdid[[i]])+
    theme(legend.background=element_blank(), legend.title = element_blank(),
          legend.direction='horizontal', legend.position="bottom",
          strip.background=element_blank(), legend.text = element_text(size=30),
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 30))
  
}

#Save each plot as a pdf file
for (i in 1:4) {
  filename <- paste(variables[i], "_plot_regions.pdf", sep = "")
  file.remove(filename)
  pdf(filename, width = 8, height = 12, paper = "a4r")
  print(plot_list_2[[i]])
  dev.off()
}

#---------------------------------------------------------------------#
# Table 2 - ATE and s.e. with different estimators -------------------
#---------------------------------------------------------------------#

## SDID
Table2_sdid <- Table1
row.names(Table2_sdid) <- c("Estimate_sdid", "Standard error_sdid")

## SCM
results_scm=estimation(Ukraine, variables , method = "scm")

Table2_scm <- rbind(results_scm[[1]], results_scm[[2]])
colnames(Table2_scm) <- variables
row.names(Table2_scm) <- c("Estimate_scm", "Standard error_scm")

## DID
results_did=estimation(Ukraine, variables , method = "did")

Table2_did <- rbind(results_did[[1]], results_did[[2]])
colnames(Table2_did) <- variables
row.names(Table2_did) <- c("Estimate_did", "Standard error_did")

#Print entire Table 2
Table2 <- rbind(Table2_sdid,Table2_scm, Table2_did);Table2


#---------------------------------------------------------------------#
# Table 3 - ATE for each region separately----------------------------
#---------------------------------------------------------------------#

## ONLY LUHANSK
Luhansk_only <- Ukraine[Ukraine$Region !="Donetsk",]
results_luhansk <- estimation(Luhansk_only, variables , method = "sdid")


#Table3_Luhansk
Table3_Luhansk <- rbind(results_luhansk[[1]], results_luhansk[[2]])
colnames(Table3_Luhansk) <- variables
row.names(Table3_Luhansk) <- c("Estimate_luhansk", "Standard error_luhansk")

## ONLY DONETSK
Donetsk_only <- Ukraine[Ukraine$Region !="Luhansk",]
results_donetsk <- estimation(Donetsk_only, variables , method = "sdid")


#Table3_donetsk
Table3_donetsk <- rbind(results_donetsk[[1]], results_donetsk[[2]])
colnames(Table3_donetsk) <- variables
row.names(Table3_donetsk) <- c("Estimate_donetsk", "Standard error_donetsk")


# Entire Table 3
Table3 <- rbind(Table3_Luhansk, Table3_donetsk); Table3

#---------------------------------------------------------------------#
# Table 4 - Direct and indirect effects--------------------------------
#---------------------------------------------------------------------#

#Treated units
treated <- unique(Ukraine[ Ukraine$Treated==1,1])
#Units that may have received a spillover effect
spill <- unique(Ukraine[(Ukraine$Spillover>0 & Ukraine$Treated==0),1])

#Initially we run the SDID estimator considering just "pure controls" in the
#donor pool, i.e., units that are not susceptible to receive a spillover

Ukraine_pure <- subset(Ukraine, !(Region %in% spill))

results_spillover <- estimation (Ukraine_pure, variables)

#Vectors to store results
direct <- c()
direct_se <- c()
indirect <- c()
indirect_se <- c()

# Routine:
# -For each variable, get the vector of unit and time weights.
# -Units in 'spill' or 'treated' receive fixed weights. Append this vector to the previous one.
# -Include time and unit weights in the dataframe and compute final weights (time weights*unit weights).
# -Run wls

fixed <- array(c(1/2,1/2,1/3,1/3,1/3), dim = c(5,1)) #assign fixed weights for treated
row.names(fixed) <- c(treated,spill)                 #Name rows for merge

post <- array(rep(1/6,6), dim=c(6,1)) #Fixed weights for post-treatment years
row.names(post) <- seq(2014,2019,1)   #Name for merge

for (i in 1:4) {
  #subset according to variable of interest
  Ukraine_1 <- na.omit(Ukraine[c("Region", "Year", variables[i], "Treated", "Spillover")])
  
  #Unit weights
  u_weights <- synthdid_controls(results_spillover[[3]][[i]], 
                                 weight.type = 'omega', mass = 1.1)
  
  unit_weights <- rbind(u_weights,fixed)    #Append vectors
  colnames(unit_weights) <- "Unit_weights"  #Rename column
  #Include Unit weights in dataframe
  Ukraine_2 <- merge(Ukraine_1, data.frame(Region=rownames(unit_weights), unit_weights), by="Region")
  
  
  #Time weights
  t_weights <- synthdid_controls(results_spillover[[3]][[i]], 
                                 weight.type = 'lambda',  mass = 1)
  time_weights <- rbind(t_weights, post)
  colnames(time_weights) <- "Time_weights"
  Ukraine_3 <- merge(Ukraine_2, data.frame(Year=rownames(time_weights), time_weights), by="Year", all = TRUE)
  Ukraine_3$Final_weights <- Ukraine_3$Time_weights*Ukraine_3$Unit_weights
  Ukraine_3$Final_weights[is.na(Ukraine_3$Final_weights)] <- 0 #fill NA
  
  #Run wls
  reg_wls_twfe <- lm(get(variables[i]) ~ Treated + Spillover + Region + factor(Year), data = Ukraine_3, weights = Ukraine_3$Final_weights)
  
  #Append results
  direct <- c(direct,reg_wls_twfe$coefficients[2]) 
  direct_se <- c(direct_se,summary(reg_wls_twfe)$coefficients[, "Std. Error"][2])
  indirect <- c(indirect,reg_wls_twfe$coefficients[3]) 
  indirect_se <- c(indirect_se,summary(reg_wls_twfe)$coefficients[, "Std. Error"][3])
  
}

Table4 <- rbind(direct, direct_se, indirect, indirect_se)
colnames(Table4) <- variables
row.names(Table4) <- c("Direct", "Standard error", "Indirect", "Standard error")
Table4
