# top of code -------------------------------------------------------------

# Simulation 
# Started Coding 07/06/23
# By: Samara Nehemiah
# updated: 03/10/2024


library(dplyr)
library(tidyverse)
library(matlab)
library(msm)
library(foreach)
library(doParallel)
library(parallel)




rm(list=ls(all=TRUE))





# Read in starting data ---------------------------------------------------

{
 #change
  setwd('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode')
  w_age<-read.csv("Simulation/w_age.csv", header=F) #weight at age
  ssbw_age<-read.csv("Simulation/ssbw_age.csv", header=F) #ssb weight at age
  rw_age<-read.csv("Simulation/rw_age.csv", header=F) #rivard weight at age, probably do not need
  mat_age<-read.csv("Simulation/mat_age.csv", header=F) #female mature at age
  M<-read.csv("Simulation/m_age.csv", header=F) #full year natural mortality at age
  sex<-read.csv("Simulation/sex_age.csv", header=F) #prop female at age
  ageerr<-read.csv("Simulation/ageerr.csv", header=F) #aging error matrix
  
  #paramater estimates of each value from SB model
  #std<-read.csv('C:/Users/Samara/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Spatial Models/Base_FINAL_021924/scaa-stripedbass.std', sep="", header=T)
  #std<-read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Spatial Models/Base_FINALv2_040124/scaa-stripedbass.std', sep="", header=T)
  #std<-read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Spatial Models/Base_FINALv3_060524/scaa-stripedbass.std', sep="", header=T)
  std<-read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Spatial Models/Final Spatial Models/Final_ageingerror/scaa-stripedbass.std', sep="", header=T)
  #View(std)
  
  
  #est_occ<-subset(std,index>300)
  est_occ<-std[grep("log_prop", std$name), ]
  #head(est_occ)
  
  #restructure so ssb is 30 years
  ssbw_age<-ssbw_age[7:36,]
  row.names(ssbw_age) <- 1:nrow(ssbw_age)
  w_age<-w_age[7:36,]
  row.names(w_age) <- 1:nrow(w_age)
  rw_age<-rw_age[7:36,]
  row.names(rw_age) <- 1:nrow(rw_age)

}

# Simulate True Pop -------------------------------------------------------


{ #start code to simulate population
  #set.seed(12345)
  
  
  fage<-1
  lage<-15 #plus group
  
  cb<-1
  ac<-2
  
  
  #ESS_C<-50
  #ESS_I<-25
  
  #setting starting parameters
  
  #initializing recruitment
  
  start_log_R0<-subset(std, name=="log_R",value)#log mean values from par file
  start_log_R0<-start_log_R0[['value']] #converting to vector
  
  #initializing F
  #FEQ
  #start_log_F0<-std$value[grep("Feq", std$name)]#llog values from par file
  #start_log_F0[2]<-log(0.5) #fixing cb to a less dramatic feq
  start_log_F0<-c(log(0.2),log(0.2))
  #start_log_a_sf1<-std$value[grep("a_sf1",std$name)] #log values for afsel
  #start_log_a_sf2<-std$value[grep("a_sf2",std$name)] #log values for afsel
  start_log_a_sf1<-log(1)
  start_log_a_sf2<-log(4)
  #Average f
   # F_cb1<-log(0.1)
   # F_cb2<-log(0.1)
   # F_ac1<-log(0.1)
   # F_ac2<-log(0.1)
  F_cb1<-std$value[grep("Fcb1",std$name)]#log F values, in cb ts=1
  F_cb1<-mean(F_cb1[9:36])#average of years 1995-2017, no moratorium
  F_cb2<-std$value[grep("Fcb2",std$name)]#llog values from par file
  F_cb2<-mean(F_cb2[9:36])
    start_F_cb<-c(F_cb1,F_cb2)
  F_ac1<-std$value[grep("Fac1",std$name)]#log F values, in cb ts=1
  F_ac1<-mean(F_ac1[9:36])#average of years 1995-2017, no moratorium
  F_ac2<-std$value[grep("Fac2",std$name)]#llog values from par file
  F_ac2<-mean(F_ac2[9:36])
   start_F_ac<-c(F_ac1,F_ac2)


  
  #estimation model
  start_fryear<-1 #first year of data with region only
  start_lryear<-20 #last year of data with region only
  start_fsyear<-21 #first year of data with stock data
  start_lsyear<-30 #lasy eyar of data with stock info
  
  
  #beverton-holt relationship
    #not using beverton-hold 1/19/24
  #start_alpha_bay<-log(114389.9707)
  #start_beta_bay<-log(0.001479825)
  #start_alpha_coast<-log(110000)
  #start_beta_coast<-log(0.0025)
  
  #start_log_R1<-log(600000) #mean recruitment Chesapeake bay
  #start_log_R2<-log(600000) #mean recruitment Atlantic coast
  nsurv<-4
  survey.names<-c("CB1","CB2","AC1","AC2")
  
  
  
  tstep<-2
  tstep.names<-c("Jan-Jun", "Jul-Dec")
  
  tblock<-2 #fisheryselectivity timeblocks
  tblock.names<-c("1", "16")
  
  #pulling fsel params from year 1990-2017, will set each at 10 year time blocks here
  #starting fsel params
  
  #fixing for simplicity here
  start_fsel_cb_a<-log(1)
  start_fsel_cb_b<-log(4)
  start_fsel_cb_c<-log(1)
  start_fsel_cb_d<-log(10)

  # start_fsel_cb_a<-log(2)
  # start_fsel_cb_b<-log(4)
  # start_fsel_cb_c<-log(1)
  # start_fsel_cb_d<-log(6)
  # 
  start_fsel_ac_a<-log(1)
  start_fsel_ac_b<-log(4)
  # start_fsel_ac_a<-log(2)
  # start_fsel_ac_b<-log(3)
  
  #double logistic params for CB
  # start_fsel_cb_a<-std$value[grep("sf1_cb",std$name)]
  # start_fsel_cb_a<-array(start_fsel_cb_a,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_cb_a<-(start_fsel_cb_a[2,4])
  # start_fsel_cb_b<-std$value[grep("sf2_cb",std$name)]
  # start_fsel_cb_b<-array(start_fsel_cb_b,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_cb_b<-(start_fsel_cb_b[2,4])
  # start_fsel_cb_c<-std$value[grep("sf3_cb",std$name)]
  # start_fsel_cb_c<-array(start_fsel_cb_c,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_cb_c<-(start_fsel_cb_c[2,4])
  # start_fsel_cb_d<-std$value[grep("sf4_cb",std$name)]
  # start_fsel_cb_d<-array(start_fsel_cb_d,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_cb_d<-start_fsel_cb_d[2,4]
  # # #   #Atl Coast
  # start_fsel_ac_a<-std$value[grep("sf1_ac",std$name)]
  # start_fsel_ac_a<-array(start_fsel_ac_a,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_ac_a<-start_fsel_ac_a[2,3]
  # start_fsel_ac_b<-std$value[grep("sf2_ac",std$name)]
  # start_fsel_ac_b<-array(start_fsel_ac_b,dim=c(tstep,4),dimnames=list(tstep.names,seq(1,4)))
  # start_fsel_ac_b<-start_fsel_ac_b[2,3]


  #catchability
  
  log_q<-std$value[grep("log_q_nj", std$name)] #same q for all surveys, here using NJBT q
  log_q_age1<-std$value[grep("log_q_age1n", std$name)] #starting parameter for age 1
  log_q_yoy<-std$value[grep("log_q_yoy_bay", std$name)] #starting parameter for yoy
  
  #fisheries independent surveys
    #ches bay (pulled from chesmmap survey)
  # start_ssel_cb_a<-std$value[grep("log_ssf1_cm", std$name)]#log values
  # start_ssel_cb_a<-start_ssel_cb_a[1]
  # start_ssel_cb_b<-std$value[grep("log_ssf2_cm", std$name)]
  # start_ssel_cb_b<-start_ssel_cb_b[1]

  #starting with simple logistic
   start_ssel_cb_a<-log(1)
   start_ssel_cb_b<-log(4)
  
  #atl coast (pulled from CTLIST)
  # start_ssel_ac_a<-std$value[grep("log_ssf1_ct", std$name)]#log values
  # start_ssel_ac_a<-start_ssel_ac_a[1]
  # start_ssel_ac_b<-std$value[grep("log_ssf2_ct", std$name)]
  # start_ssel_ac_b<-start_ssel_ac_b[1]
  # start_ssel_ac_c<-std$value[grep("log_ssf3_ct", std$name)]
  # start_ssel_ac_c<-start_ssel_ac_c[1]
  # start_ssel_ac_d<-std$value[grep("log_ssf4_ct", std$name)]
  # start_ssel_ac_d<-start_ssel_ac_d[1]

   start_ssel_ac_a<-log(1)#log values
   start_ssel_ac_b<-log(4)
   start_ssel_ac_c<-log(1)
   start_ssel_ac_d<-log(10)
  
  
  
  #Occupancy Probability
  start_log_prop_bay<-std$value[grep("log_prop_bay", std$name)] #occupancy in the AC for the Bay stock
  start_log_prop_coast<-std$value[grep("log_prop_coast", std$name)] #occupancy in the AC for the Coast stock
  
  start_log_prop_bay1<-start_log_prop_bay[1:14] #ages 2-15
  start_log_prop_bay2<-start_log_prop_bay[15:28] #ages 2-15, timestep 2
  start_log_prop_coast1<-start_log_prop_coast[1:14] #ages 2-15
  start_log_prop_coast2<-start_log_prop_coast[15:28] #ages 2-15, timestep 2
  

  #THE MODEL
  fmyr<-1
  lmyr<-30
  fage<-fage
  lage<-lage
  mnyrs<-30 #number of years to simulate daa
  stock<-2 #number of stocks (1-CB, 2=AC)
  tstep<-2 #number of timesteps(1-Jan-Jun, 2-Jul-DEC)
  region<-2 #number of regions (1-CB, 2=AC)
  
  log_R<-start_log_R0 #mean recruitment for ches bay and atlantic coast
  log_F0<-start_log_F0 #mean log F for both regions

  log_F1<-start_F_cb #mean log F for the CB regions
  log_F2<-start_F_ac #mean log F for the AC region
  
  #stock recruit params
  #alpha<-c(exp(start_alpha_bay),exp(start_alpha_coast))
  #beta<-c(exp(start_beta_bay),exp(start_beta_coast))

  #assigning array names
  year.names<-seq(1,30,1)
  age.names<-c("age1", "age2","age3",'age4',"age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")
  eq_age.names<-c("age2","age3",'age4',"age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")
  
  stock.names<-c("Chesapeake Bay stock","Atlantic Coast stock")
  #tstep.names<-c("Jan-Jun", "Jul-Dec")
  region.names<-c("Chesapeake Bay region","Atlantic Coast region")

  #array(nrow,ncol,number of dimensions)
  #dim=c(stock,region,yrs,ts,age)
  Nt<-array(0,dim=c(stock,mnyrs,tstep,lage),dimnames = list(stock.names,year.names,tstep.names,age.names))#tot stock abundance
  N<-array(0, dim = c(stock,region,mnyrs,tstep,lage), dimnames = list(stock.names,region.names,year.names,tstep.names,age.names)) #Number of fish in the Chesapeake Bay Spatial Region
  N_eq<-array(0,dim=c(stock,lage), dimnames=list(stock.names,age.names))
  N_dev<-array(0,dim=c(stock,lage), dimnames=list(stock.names,age.names))
  Z<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names))
  Fmort<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names))
  afsel<-array(0, dim=c(stock,lage),dimnames=list(stock.names,age.names))
  Feq<-array(0, dim=c(stock,lage),dimnames=list(stock.names,age.names))
  recruit<-array(0, dim = c(stock,mnyrs), dimnames = list(stock.names,year.names)) #Number of fish in the Chesapeake Bay Spatial Region
  
  SSB<-array(0,dim=c(stock,mnyrs), dimnames=list(stock.names,year.names))
  Bmass<-array(0, dim = c(stock,region,mnyrs,tstep), dimnames = list(stock.names,region.names,year.names,tstep.names))

  #true values
  true_Ntot<-numeric(mnyrs)
  true_Ntot_stock<-array(0,dim=c(stock,mnyrs),dimnames=list(stock.names,year.names))
  true_biomass<-numeric(mnyrs)
  true_ssb<-numeric(mnyrs)
  true_f<-array(0,dim=c(mnyrs,lage), dimnames=list(year.names,age.names))
  true_f_year<-numeric(mnyrs)
  true_spr<-array(0,dim=c(stock), dimnames=list(stock.names))
  
  #catch info
  est_C<-array(0,dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names))
  est_C_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames = list(stock.names,region.names,tstep.names,year.names,age.names))
  est_region_C<-array(0,dim=c(region,tstep,mnyrs),dimnames=list(region.names,tstep.names,year.names))
  est_reg_C_age<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames = list(region.names,tstep.names,year.names,age.names))
  est_Cp_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames = list(stock.names,region.names,tstep.names,year.names,age.names))

#fisheries independent
  #ches bay
  est_I_age_regcb<-array(0,dim=c(stock,mnyrs,tstep,lage), dimnames=list(stock.names,year.names,tstep.names,age.names)) #abundance of each catch at age for the survey in the cb region in both timesteps
  est_Ip_regcb<-array(0,dim=c(stock,mnyrs,tstep,lage), dimnames=list(stock.names,year.names,tstep.names,age.names)) #estimated proportions at age for the survey in the cb region for both timesteps
  est_I_regcb<-array(0,dim=c(stock,mnyrs,tstep), dimnames=list(stock.names,year.names,tstep.names)) #index of abundance for survey in the cb region in both timesteps
  #atl coast
  est_I_age_regac<-array(0,dim=c(stock,mnyrs,tstep,lage), dimnames=list(stock.names,year.names,tstep.names,age.names)) #abundance of each catch at age for the survey in the ac region in both timesteps
  est_Ip_regac<-array(0,dim=c(stock,mnyrs,tstep,lage), dimnames=list(stock.names,year.names,tstep.names,age.names)) #estimated proportions at age for the survey in the ac region for both timesteps
  est_I_regac<-array(0,dim=c(stock,mnyrs,tstep), dimnames=list(stock.names,year.names,tstep.names)) #index of abundance for survey in the ac region in both timesteps

  #age1 and yoy surveys for each region, these are conducted in timestep 2
  est_I_age1<-array(0,dim=c(stock,region,mnyrs), dimnames=list(stock.names,region.names,year.names))
  est_I_age1_reg<-array(0,dim=c(region,mnyrs), dimnames=list(region.names,year.names))
  est_I_yoy<-array(0,dim=c(stock,region,mnyrs), dimnames=list(stock.names,region.names,year.names))
  est_I_yoy_reg<-array(0,dim=c(region,mnyrs), dimnames=list(region.names,year.names))
  
  #calculating rdevs for each stock
  #log_Rdevs<-array(rnorm(mnyrs,0,0), dim=c(stock, mnyrs), dimnames=list(stock.names, year.names))
  #log_Fdevs<-array(rnorm(mnyrs,0,0.1), dim=c(region,tstep, mnyrs), dimnames=list(region.names, tstep.names,year.names))
  
  cb_Rdevs<-std$value[grep("recruit_cb", std$name)] #reading in the annual recruitment everyyear for cb
  cb_Rdevs<-cb_Rdevs[9:36] #using values from 1990-2017
  cb_Rdevs_sd<-sqrt(sum((log_R[1]-cb_Rdevs)^2)/length(cb_Rdevs)) #calculating the variance of recruitment
  ac_Rdevs<-std$value[grep("recruit_ac", std$name)]#reading in the annual recruitment everyyear for ac
  ac_Rdevs<-ac_Rdevs[9:36] #using values from 1990-2017
  ac_Rdevs_sd<-sqrt(sum((log_R[2]-ac_Rdevs)^2)/length(ac_Rdevs))#calculating the variance of recruitment
  log_Rdevs<-array(0, dim=c(stock, mnyrs), dimnames=list(stock.names, year.names)) #setting up array to store values
  log_Rdevs[1,]<-rnorm(mnyrs,0,cb_Rdevs_sd) #setting cv rdevs
  log_Rdevs[2,]<-rnorm(mnyrs,0,ac_Rdevs_sd) #setting ac rdevs
  
  
  #calculating fdevs for each region
  cb1_Fdevs<-std$value[grep("Fcb1", std$name)] #reading in the annual F every year for cb, ts1
  cb1_Fdevs<-cb1_Fdevs[9:36]
  cb1_Fdevs_sd<-sqrt(sum((F_cb1-cb1_Fdevs)^2)/length(cb1_Fdevs)) #calculating the variance of F from 1990-2017
  cb2_Fdevs<-std$value[grep("Fcb2", std$name)] #reading in the annual F every year for cb, ts1
  cb2_Fdevs<-cb2_Fdevs[9:36]
  cb2_Fdevs_sd<-sqrt(sum((F_cb2-cb2_Fdevs)^2)/length(cb2_Fdevs)) #calculating the variance of recruitment

  ac1_Fdevs<-std$value[grep("Fac1", std$name)] #reading in the annual F every year for cb, ts1
  ac1_Fdevs<-ac1_Fdevs[9:36]
  ac1_Fdevs_sd<-sqrt(sum((F_ac1-ac1_Fdevs)^2)/length(ac1_Fdevs)) #calculating the variance of F from 1990-2017
  ac2_Fdevs<-std$value[grep("Fac2", std$name)] #reading in the annual F every year for cb, ts1
  ac2_Fdevs<-ac2_Fdevs[9:36]
  ac2_Fdevs_sd<-sqrt(sum((F_ac2-ac2_Fdevs)^2)/length(ac2_Fdevs)) #calculating the variance of recruitment

   log_Fdevs<-array(0, dim=c(region,tstep,mnyrs), dimnames=list(region.names,tstep.names,year.names)) #setting up array to store values
   log_Fdevs[cb,1,]<-rnorm(mnyrs,0,cb1_Fdevs_sd) #setting cv log_Fdevs tstep1
   log_Fdevs[cb,2,]<-rnorm(mnyrs,0,cb2_Fdevs_sd) #setting cb log_Fdevs tstep2
   log_Fdevs[ac,1,]<-rnorm(mnyrs,0,ac1_Fdevs_sd) #setting ac log_Fdevs tstep1
   log_Fdevs[ac,2,]<-rnorm(mnyrs,0,ac2_Fdevs_sd) #setting ac log_Fdevs tstep2

  #fisheries selectivity
  sf1_cb<-exp(start_fsel_cb_a) #slope of fsel ascending ([,2 = using from 1995 block])
  sf2_cb<-exp(start_fsel_cb_b) #50% sel fsel ascending
  sf3_cb<-exp(start_fsel_cb_c) #slope descending
  sf4_cb<-exp(start_fsel_cb_d) #50% sel of descending
  sf1_ac<-exp(start_fsel_ac_a) #slope  for ac
  sf2_ac<-exp(start_fsel_ac_b) #50% sel for ac
  
  
  #survey selectivity
  ssf1_cb<-exp(start_ssel_cb_a) #slope of ssel ascending for cb surveys, timestep 1 and 2
  ssf2_cb<-exp(start_ssel_cb_b) #50% sel of ascending for cb surveys, timestep 1 and 2
  #ssf3_cb<-exp(start_ssel_cb_c) #slope of descending ssel for cb surveys, timestep 1 and 2
  #ssf4_cb<-exp(start_ssel_cb_d) #50% sel of descending of ssel for cb surveys, timestep 1 and 2
  ssf1_ac<-exp(start_ssel_ac_a) #slope of ssel ascending for ac surveys, tstep1 and 2
  ssf2_ac<-exp(start_ssel_ac_b) #50% sel of ascending for ac surveys, tstep 1 and 2
  ssf3_ac<-exp(start_ssel_ac_c) #slope of ssel descending for ac surveys, tstep 1 and 2
  ssf4_ac<-exp(start_ssel_ac_d) #50% of sel of descending for ac surveys, tstep 1 and 2
  
  prop<-array(0,dim=c(stock,tstep,region,lage),dimnames=list(stock.names,tstep.names,region.names,age.names)) #for the atlantic coast region!
  
  fsel<-array(0,dim = c(region,mnyrs,tstep,lage),dimnames=list(region.names,year.names,tstep.names,age.names))
  ssel_cb<-array(0, dim=c(tstep,mnyrs,lage),dimnames=list(tstep.names,year.names,age.names))
  ssel_ac<-array(0, dim=c(tstep,mnyrs,lage),dimnames=list(tstep.names,year.names,age.names))
  
  est_I_age<-array(0,dim = c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names,age.names))
  est_Ip_age<-array(0,dim = c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names,age.names))

  #below code is to initialize
  prop_bay1<-ifelse(exp(start_log_prop_bay1)>=0.99,0.99,ifelse(exp(start_log_prop_bay1)<=0.01,0.01,exp(start_log_prop_bay1)))
  prop_bay2<-ifelse(exp(start_log_prop_bay2)>=0.99,0.99,ifelse(exp(start_log_prop_bay2)<=0.01,0.01,exp(start_log_prop_bay2)))
  prop_coast1<-ifelse(exp(start_log_prop_coast1)>=0.99,0.99,ifelse(exp(start_log_prop_coast1)<=0.01,0.01,exp(start_log_prop_coast1)))
  prop_coast2<-ifelse(exp(start_log_prop_coast2)>=0.99,0.99,ifelse(exp(start_log_prop_coast2)<=0.01,0.01,exp(start_log_prop_coast2)))
  #prop_bay1<-exp(start_log_prop_bay1)
  #prop_bay2<-exp(start_log_prop_bay2)
  #prop_coast1<-exp(start_log_prop_coast1)
  #prop_coast2<-exp(start_log_prop_coast2)
  


#setting occupancy probabilities
  for(ts in 1:tstep){
    prop[cb,ts,cb,1]=1 #cb  stock in region 1, age 1, timestep 1 and 1
    prop[cb,ts,ac,1]=0 #cb  stock in region 2, age 1, timestep 1 an d2
    prop[ac,ts,cb,1]=0 #ac  stock in region 1, age 1, timestep 1 an d2
    prop[ac,ts,ac,1]=1 #ac  stock in region 2, age 1, timestep 1 an d2
  }
  
  #starting parameters for occupancy prob
  prop[cb,1,cb,1:15]<-c(1,1-prop_bay1) #proportion of bay stock, in ts=1, in the bayregion 
  prop[cb,1,ac,2:15]<-prop_bay1#proportion of bay stock, in ts=1, in the coast region 
  prop[cb,2,ac,2:15]<-prop_bay2 #proportion of bay in the coast, ts 2
  prop[cb,2,cb,1:15]<-c(1,1-prop_bay2) #proportion of bay in the bay, ts 2
  
  #prop[cb,2,ac,1:15]
  #plot(prop[cb,2,ac,],ylim=c(0,1),pch=16)
  
  prop[ac,1,cb,1:15]<-c(0,1-prop_coast1) #proportion of coast stock, in ts=1, in the bay region 
  prop[ac,1,ac,2:15]<-prop_coast1#proportion of coast stock, in ts=1, in the coast region 
  prop[ac,2,ac,2:15]<-prop_coast2 #proportion of coast in the coast, ts 2
  prop[ac,2,cb,1:15]<-c(0,1-prop_coast2) #proportion of coast in the bay, ts 2
  
  log_occ_prob_sd_bay<-std$std.dev[grep("log_prop_bay", std$name)]
  log_occ_prob_sd_coast<-std$std.dev[grep("log_prop_coast", std$name)]
  
  
  #Rdevs<-exp(log_Rdevs)

  for(ts in 1:tstep){
      for(a in fage:lage){
        #removed timeblock
        #fsel[cb,1:30,ts,a]<-(1/(1+exp(-sf1_cb[ts]*(a-sf2_cb[ts]))))*(1/(1+exp(-sf3_cb[ts]*(sf4_cb[ts]-a))))
        fsel[cb,1:30,ts,a]<-(1/(1+exp(-sf1_cb*(a-sf2_cb))))*(1/(1+exp(-sf3_cb*(sf4_cb-a))))
        #fsel[cb,1:30,ts,a]<-1/(1+exp(-sf1_cb*(a-sf2_cb))) 
        fsel[ac,1:30,ts,a]<-1/(1+exp(-sf1_ac*(a-sf2_ac))) 
    }#close age loop
    #fsel[cb,1:30,ts,]<-fsel[cb,1:30,ts,]/max(fsel[cb,1:30,ts,])
  }#close tstep loop

    #plot(fsel[ac,1,1,],pch=16)
    #points(fsel[ac,1,1,],pch=16,col="red")
    #points(fsel[cb,21,1,],pch=16,col="blue")
    
  for(ts in 1:tstep){
    for(a in fage:lage){
      #ssel_cb[ts,1:mnyrs,a]<-(1/(1+exp(-ssf1_cb[ts]*(a-ssf2_cb[ts]))))#*(1/(1+exp(-ssf3_cb[ts]*(ssf4_cb[ts]-a))))
      #ssel_ac[ts,1:mnyrs,a]<-(1/(1+exp(-ssf1_ac[ts]*(a-ssf2_ac[ts]))))*(1/(1+exp(-ssf3_ac[ts]*(ssf4_ac[ts]-a))))
  
      #not varying for each time-step
      ssel_cb[ts,1:mnyrs,a]<-(1/(1+exp(-ssf1_cb*(a-ssf2_cb))))
      ssel_ac[ts,1:mnyrs,a]<-(1/(1+exp(-ssf1_ac*(a-ssf2_ac))))*(1/(1+exp(-ssf3_ac*(ssf4_ac-a))))
      
    }
  } #close tstep loop
  
    #plot(ssel_cb[1,1,],pch=16)
    #points(ssel_cb[2,2,],pch=16,col="red")
    #plot(ssel_ac[1,1,],pch=16)
    #points(ssel_ac[2,2,],pch=16,col="red")
  
  q<-exp(log_q)
  q_age1<-exp(log_q_age1)
  q_yoy<-exp(log_q_yoy)
  
    #F in all years
  for(ts in 1:tstep){
    for(y in 1:mnyrs){
      for(a in fage:lage){
        Fmort[cb,ts,y,a]<-fsel[cb,y,ts,a]*exp(log_F1[ts]+log_Fdevs[1,ts,y])# are Fdev and Rdevs supposed to be an input (from output of the model)?
        Fmort[ac,ts,y,a]<-fsel[ac,y,ts,a]*exp(log_F2[ts]+log_Fdevs[2,ts,y]) #coast
      } #close age loop
    }#close year loop
    }#close tstep loop
  

  #Total mortality for each spatial region, time step, age, years 
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
            for(a in fage:lage){
            Z[r,ts,y,a]<-Fmort[r,ts,y,a]+(0.5*M[1,a])
          }#close age loop
        }#close time step loop
      }#clopse year loop
    }#close stock
    

  
# Calc Abundance ----------------------------------------------------------

 
  
  
  #Caclulate Nt and N at age in the first year and first timestep
  for(s in 1:stock){
    recruit[s,1]<-exp(log_R[s])
    Nt[s,fmyr,1,1]<-recruit[s,1] #setting recruitment in the first year, first timestep, first age
    }
  
  # plot(recruit[ac,],pch=16,col="skyblue2")
  # points(recruit[cb,],pch=16,col="coral2")
  
  a_sf1<-exp(start_log_a_sf1)
  a_sf2<-exp(start_log_a_sf2)
  
  for(s in 1:stock){
    for(a in fage:lage){
      afsel[s,a]<-1/(1+exp(-a_sf1*(a-a_sf2)))
      Feq[s,a]<-exp(start_log_F0[s])*afsel[s,a]
    }
  }
  
  
  #Nt in the first year
  for(s in 1:stock){
    for(r in 1:region){
      for(a in 2:lage){
        #***note F dimensions are not the same as NT dimensions
        #*first calculating equilibrium abundance in the first year
        N_eq[s,a]<-Nt[s,fmyr,1,a-1]*exp(-(M[1,a-1]+Feq[s,a-1]))#first year, age =a, 1=ts1
        #N_dev[s,a]<-abs(rnorm(1,1,0.5)) #then calculating abundance deviation in the first year
        
        N_dev[s,a]<- 1#set N_dev to 1 to start calculations to make sure everything is running right
        #N_dev[s,a]<-rlnorm(1,0,0.5) #then calculating abundance deviation in the first year
        
        Nt[s,fmyr,1,a]<-N_eq[s,a]*N_dev[s,a]  #then plugging in abundance in the first year as the equillibrium abundance times the abundance deviations
        
      } #close age loop
      #Nt[s,fmyr,1,lage]<-Nt[s,fmyr,1,lage]+Nt[s,fmyr,1,lage]*exp(-(M[1,lage]+Fmort[r,1,fmyr,lage])) #calculation of the plus group in the first year
      Nt[s,fmyr,1,lage]<-Nt[s,fmyr,1,lage]/(1-exp(-(M[1,lage]+Feq[s,lage]))) #calculation of the plus group in the first year
      
    }#close region loop
  }#close stock loop
   
  #now put each stock in the correct spatial region
  for(s in 1:stock){
    for(r in 1:region){
      for(a in 1:lage){
        N[s,r,fmyr,1,a]<-Nt[s,fmyr,1,a]*prop[s,1,r,a]
      }#close age loop
    }#close region loop
  }#close stock loop
  #plot(Nt[1,1,1:30,2,7])
  
  
  #Calculate Nt and N in the first year and second time step
  for(a in 1:lage){
    Nt[cb,fmyr,2,a]<- N[cb,cb,fmyr,1,a]*exp(-Z[cb,1,fmyr,a])+N[cb,ac,fmyr,1,a]*exp(-Z[ac,1,fmyr,a]) #calculating total Bay stock abundance as the population that survived in the coast and the bay
    Nt[ac,fmyr,2,a]<- N[ac,cb,fmyr,1,a]*exp(-Z[cb,1,fmyr,a])+N[ac,ac,fmyr,1,a]*exp(-Z[ac,1,fmyr,a]) #calculating total Bay stock abundance as the population that survived in the coast and the bay
  }
  
    
  #range(Nt)
  for(s in 1:stock){
    for(r in 1:region){
      for(a in 1:lage){
      #put each stock in the correct spatial region
      N[s,r,fmyr,2,a]<-Nt[s,fmyr,2,a]*prop[s,2,r,a]
      }
    }
  }
  
  #range(N)
  #Nt[1, ,1,2,5] #view abundance for each stock i each region in year 1, timstep 2
  
  
  # calculate biomass and spawning stock biomass in the first year
  for(s in 1:stock){
    for(r in 1:region){
      for (ts in 1:tstep){
        Bmass[s,r,1,ts]<-sum(N[s,r,1,ts,]*rw_age[1,])/1000
        SSB[s,1]<-sum(Nt[s,1,1,]*sex*mat_age*ssbw_age[1,])/1000 #abundance * prop female * maturity * weight
        }#close tstep
    }#close region
  }# close stock
  
  #range(Nt)
  #Bmass[1:stock,1:region,1,1:2] #estiamtes of biomass in the first year
  #SSB[1:stock,1,1:2] #estiamtes of ssb in the first year
  #Nt[1:2,1,1:2,fage:lage]
  

  #fill out abundance for the rest of the timeseries
  #recruitment from Beverton Holt stock recruitment relationship
  #removed timestep loop and just wrote everything out timestep by timestep
  for(y in 2:mnyrs){
    #for(ts in 1:tstep){
    t1=1 #time step 1
    t2=2 #time step 2
    for(s in 1:stock){
      recruit[s,y]<-exp(log_R[s]+log_Rdevs[s,y])
      #recruit[s,y]<-(alpha[s]*SSB[s,y-1])/(1+SSB[s,y-1]*beta[s])*Rdevs[s,y] #recruit in the next year based on bev-holt

      Nt[s,y,t1,fage]<-recruit[s,y] #setting recruitment for each stock in each year (first timestep) = recruit
      for(a in 2:lage){
        Nt[s,y,t1,a]<-N[s,cb,y-1,t2,a-1]*exp(-Z[cb,t2,y-1,a-1])+N[s,ac,y-1,t2,a-1]*exp(-Z[ac,t2,y-1,a-1]) #calc tot stock abundance atthe pop for a stock that survived in both spatail regions in the previous time step (ts 2, last yr)
      } #close age loop
      #calc the plus group
      Nt[s,y,t1,lage]<-Nt[s,y,t1,lage]+N[s,cb,y-1,t2,lage]*exp(-Z[cb,t2,y-1,lage])+N[s,ac,y-1,t2,lage]*exp(-Z[ac,t2,y-1,lage])
      #print(Nt[s,y,,a])
      #calculate SSB for the first time step
      SSB[s,y]<-sum(Nt[s,y,t1,]*sex[1,]*mat_age[1,]*ssbw_age[y,])/1000 # in metric tons, SSB should come from the y-1 in the

      #put fish in each of the regions
      for(r in 1:region){
        N[s,r,y,t1,]<-Nt[s,y,t1,]*prop[s,t1,r,]
      }# close region loop
      
      
      #calculate abundance for all ages in the second timestep
      
      Nt[s,y,t2,fage]<-Nt[s,y,t1,fage]*exp(-Z[s,t1,y,fage]) #abundance for first age in time step 2 = survival of fage from timestep1, Z is by region but using trhe same as stock since they are in their respective regions
      for(a in 2:lage){ 
        Nt[s,y,t2,a]<-N[s,cb,y,t1,a]*exp(-Z[cb,t1,y,a])+N[s,ac,y,t1,a]*exp(-Z[ac,t1,y,a]) #calc tot stock abundance at the pop for a stock that survived in both spatail regions in the previous time step (ts 2, last yr)
        #if(a==fage){
        # print(Nt[1:2,y,ts2,a])
        #}
      } #close age loop

      #Bmass[s,y,t2]<-sum(Nt[s,y,t2,]*w_age[y,])/1000 #calculate biomass in metric tons
      
      for(r in 1:region){
        N[s,r,y,t2,]<-Nt[s,y,t2,]*prop[s,t2,r,]
        #if(a==fage){
        # print(N[1:2,1:2,y,ts,a])
        #}
      }# close region loop
    }#close stock
  }#close year
  
  #plot(N[1,,1,2])
  #range(Nt)
  for(y in 1:mnyrs){
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          Bmass[s,r,y,ts]<-sum(N[s,r,y,ts,]*rw_age[y,])/1000
        }
      }
    }
  }
  
  
  
  #fill in true values
  for(y in 1:mnyrs){
      true_Ntot[y]<-sum(Nt[1,y,1,])+sum(Nt[2,y,1,]) #summing total stock in time-step 1
      true_ssb[y]<-sum(SSB[,y]) #summing across stocks
      true_biomass[y]<-sum(Bmass[,1,y,1])+sum(Bmass[,2,y,1]) #biomass in jan 1, adding region 1 to region, sum is summing over stocks
  }
  for(y in 1:mnyrs){
    for(s in 1:stock){
      true_Ntot_stock[s,y]<-sum(Nt[s,y,1,]) #summing total abundance for each stock in time-step 1
    }
  }
  
#****change true_f to be year and ages
 for(y in 1:mnyrs){
   for(s in 1:stock){
    for(r in 1:2){
      for(a in 1:lage){
        true_f[y,a]<-true_f[y,a]+
          (Fmort[r,1,y,a]*(N[s,r,y,1,a])/(sum(N[1,,y,1,a])+sum(N[2,,y,1,a])))+
          (Fmort[r,2,y,a]*(N[s,r,y,2,a])/(sum(N[1,,y,2,a])+sum(N[2,,y,2,a]))) #denominator needs to sum over stock &region
       }
     }
   }
 }
  for(y in 1:mnyrs){
    true_f_year[y]<-sum(true_f[y,])
  }


  # calc SPR  ------------------------------------------------------
  #SPR Is an equillibrium model
  
  years<-100 #number to run population out
  
  Nf<-array(0,dim=c(stock,region,tstep,years,lage))
  Nf_stock<-array(0,dim=c(stock,tstep,years,lage)) #hold eq calculation for total stock abundance
  
  #F_0<-rray(0,dim=c(region,tstep,lage))
  F_eq<-array(0,dim=c(region,tstep,lage))
  Fa<-array(0,dim=c(lage))
  V<-array(0,dim=c(region,tstep,lage))
  Fstar<-array(0,dim=c(region,tstep,lage))
  Z_eq<-array(0,dim=c(region,tstep,lage)) #array to hold total mortality
  
  
  for(r in 1:region){
    for(ts in 1:tstep){
      for(a in fage:lage){
        F_eq[r,ts,a]<-Fmort[r,ts,30,a] #setting F_eq to fishing mortality in the last year of the time-series
      } #close age loop
    }#close tstep loop
  } #close region
  
  
  # #calculate Fa which is the sum of all F values for each age
  # for(a in fage:lage){
  #   Fa[a]<-Reduce("+",F_eq[,,a]) #Reduce adds all values of array 
  # }
  for(r in 1:region){
    for(ts in 1:tstep){
      for(a in fage:lage){
           Fa[a]<-Fa[a]+F_eq[r,ts,a] #Reduce adds all values of array 
      } #close age loop
    }#close tstep loop
  } #close region
  
  #calculate V
  
  for(r in 1:region){
    for(ts in 1:tstep){
      for(a in fage:lage){
        V[r,ts,a]<-F_eq[r,ts,a]/max(Fa)
      } #close age loop
    }#close tstep loop
  } #close region
  
  R<-vector(length=stock)
  for(s in 1:stock){
    R[s]<-exp(log_R[s])/sum(exp(log_R))
    #R[s]<-recruit[s,30]/sum(recruit[,30]) #sum last year of recruits
  }

  SSBf<-array(0,dim=c(stock,151))
  SPR_1<-array(0,dim=c(stock,151))
  # SSBf<-array(0,dim=c(stock))
  # SPR_1<-array(0,dim=c(stock))
  
  G_spr<-0
  
  for(G in 0:150){ #looping through G's 
    #G=0.2
    G_spr=0.01*G  #Fishing mort SPR is 0.01*f
    #G_spr=G  #Fishing mort SPR is 0.01*f
    
    for(r in 1:region){
      for(ts in 1:tstep){
        for(a in fage:lage){
          Fstar[r,ts,a]<-G_spr*V[r,ts,a] #G is a scalar, G=0 is the unfished condition
        } #close age loop
      }#close tstep loop
    } #close region
    
    
    #set timesteps
    ts1<-1
    ts2<-2
    
    
    #population dynamics
    
    #calculate total mortality based on Fstar calc
    for(r in 1:region){
      for(ts in 1:tstep){
        for(a in fage:lage){
          Z_eq[r,ts,a]<-Fstar[r,ts,a]+(0.5*M[1,a])
        }#close age loop
      }# close tstep loop
    }#close region loop
    
    
    #Dynamics in Year 1 
    #dynamics in ts 1
    for(s in 1:stock){
      #setting recruitment for all years
      Nf_stock[s,ts1,1,fage]<-R[s]
    }  
    #   #age 2-15+
    #   for(a in 2:lage){
    #     req<-s
    #     Nf_stock[s,ts1,1,a]<-Nf_stock[s,ts1,1,a-1]*exp(-Z_eq[req,ts1,a-1])   #ts=1, year=1
    #   } #close age loop
    #   Nf_stock[s,ts1,1,lage]<-Nf_stock[s,ts1,1,lage]/(1-exp(-(Z_eq[req,ts1,lage])))
    # }#close stock
    
    
    # #put fish in their respective regions
    # for(s in 1:stock){
    #   for(r in 1:region){
    #     for(a in 1:lage){
    #       Nf[s,r,ts1,1,a]<-Nf_stock[s,ts1,1,a]*prop[s,ts1,r,a] #1 =first year
    #     }#close age loop
    #   }#close region loop
    # }#close stock loop
    
    
    
    # #dynamics in ts 2
    # #for(r in 1:region){
    # for(a in 1:lage){
    #   Nf_stock[1,ts2,1,a]<- Nf[1,1,ts1,1,a]*exp(-Z_eq[1,ts1,a])+Nf[1,2,ts1,1,a]*exp(-Z_eq[2,ts1,a]) ##this equation is suspicious , ts=2,year=1  
    #   Nf_stock[2,ts2,1,a]<- Nf[2,1,ts1,1,a]*exp(-Z_eq[1,ts1,a])+Nf[2,2,ts1,1,a]*exp(-Z_eq[2,ts1,a]) ##this equation is suspicious , ts=2,year=1  
    # } #close age loop
    # #}#close region
    # 
    # #put fish in their repsective regions
    # for(s in 1:stock){
    #   for(r in 1:region){
    #     for(a in 1:lage){
    #       Nf[s,r,ts2,1,a]<-Nf_stock[s,ts2,1,a]*prop[s,ts2,r,a] #year=1
    #     }#close age loop
    #   }#close region loop
    # }#close stock loop
    # 
    
    #Dynamics for years 2 on
    #dynamics ts1 
    
    for(y in 2:years){
      for(s in 1:stock){
        #set recruitment
        Nf_stock[s,ts1,y,fage]<-R[s]  
        for(a in 2:lage){
          #add each stock in each region to get total stock abundance
          Nf_stock[s,ts1,y,a]<- Nf[s,1,ts2,y-1,a-1]*exp(-Z_eq[1,ts2,a-1])+Nf[s,2,ts2,y-1,a-1]*exp(-Z_eq[2,ts2,a-1])
        } #close age loop
        Nf_stock[s,ts1,y,lage]<-Nf_stock[s,ts1,y,lage]+Nf[s,1,ts2,y-1,lage]*exp(-Z_eq[1,ts2,lage])+Nf[s,2,ts2,y-1,lage]*exp(-Z_eq[2,ts2,lage])
        
        #put fish in their region for time-step1
        for(r in 1:region){
          Nf[s,r,ts1,y,]<-Nf_stock[s,ts1,y,]*prop[s,ts1,r,]
        } #close region loop
        
        #fage in second timestep
        #Nf_stock[s,ts2,y,fage]<-Nf_stock[s,ts1,y,fage]*exp(-Z_eq[s,ts1,fage]) #here setting Z region =stock since
        #second timestep, age 2
        for(a in fage:lage){
          Nf_stock[s,ts2,y,a]<-Nf[s,1,ts1,y,a]*exp(-Z_eq[1,ts1,a])+Nf[s,2,ts1,y,a]*exp(-Z_eq[2,ts1,a])
        } #close age loop
        
        for(r in 1:region){
          #put fish in their regions
          Nf[s,r,ts2,y,]<-Nf_stock[s,ts2,y,]*prop[s,ts2,r,]
        }# close region loop
      } #close stock loop
    }#close year loop
    
    SSBf[1:2,G]<-0 #initialize SSBf
    #SSBf[s]<-0 #initialize SSBf
    #for(s in 1:stock){
    for(r in 1:region){
        SSBf[1,G]<-SSBf[1,G]+sum(Nf[1,r,ts1,100,]*(sex*rw_age[30,]*mat_age)) #reduce sums all components of array, summing for year 75 right now
        SSBf[2,G]<-SSBf[2,G]+sum(Nf[2,r,ts1,100,]*(sex*rw_age[30,]*mat_age)) #reduce sums all components of array, summing for year 75 right now
    }
    #}
    # for(r in 1:region){
    #   #for(ts in 1:tstep){
    #   SSBf[1]<-SSBf[1]+Reduce("+",Nf[1,r,ts1,100,]*(sex*rw_age[30,]*mat_age)) #reduce sums all components of array, summing for year 75 right now
    #   SSBf[2]<-SSBf[2]+Reduce("+",Nf[2,r,ts1,100,]*(sex*rw_age[30,]*mat_age)) #reduce sums all components of array, summing for year 75 right now
    #   #}
    # }
    
    for(s in 1:stock){
      SPR_1[s,G]<-SSBf[s,G]/SSBf[s,1]
      #SPR_1[s]<-SSBf[s]
    }
  } #close G loop 
  
  #plot(SSBf[1,]/max(SSBf[1,]))
  
  slope<-0
  
  #create function that estimates G that gives you 40% SPR
  find_G_40 <- function(SPR) {
    G <- 1
    while (SPR[G] > 0.40 && G <= 150) {
      G <- G + 1
    }
    # Check if G exceeds the bounds
    if (G > 150) {
      stop("SPR does not reach 0.40 within the range of G = 0 to 150")
    }
    # Linear interpolation to find F_35
    slope <- (SPR[G] - SPR[G - 1]) / 0.01
    G_40 <- ((0.40 - SPR[G]) + slope * G * 0.01) / slope
    return(G_40)
  }
  
  # now plug in values to function 
  find_G_40(SPR_1[1,]);find_G_40(SPR_1[2,])
  #^ this is G value?
  
  #plug in get to get the fishing mortality rate that estimates 40% spr
  
  true_G<-array(0)
  true_G<-find_G_40(SPR_1[1,])*R[1]+find_G_40(SPR_1[2,])*R[2]
  
  #true_G[1]<-find_G_40(SPR_1[1,])
  #true_G[2]<-find_G_40(SPR_1[2,])
  
  Fbar<-array(0,dim=c(region,tstep,lage))
  Fbar_stock<-array(0,dim=c(stock,lage))
  F_spr<-0
  #F_spr<-array(0,dim=c(stock))
  #for(s in 1:stock){ 
    for(r in 1:region){
      for(ts in 1:tstep){
        for(a in fage:lage){
          Fbar[r,ts,a]<-true_G*V[r,ts,a]#
        } #close age loop
      } #close tstep loop
     }#close region loop
  #} #close stock loop
  
  #calculate f40% for each stock at age
  for(s in 1:stock){ 
    for(r in 1:region){
      for(ts in 1:tstep){
        for(a in fage:lage){
          Fbar_stock[s,a]<-Fbar_stock[s,a]+Fbar[r,ts,a]*prop[s,ts,r,a] 
        } #close age loop
      } #close tstep loop
    }#close region loop
  }#close stock loop
  
  
  #pick one reference age, for single reference point
  for(s in 1:stock){ 
      F_spr<-F_spr+Fbar_stock[s,8]*R[s]#Nf_stock[s,ts1,100,8]/sum(Nf_stock[,ts1,100,8]) *R[s]
  } #close stock loop
  
  
  #setting true values of 
  true_spr[1]<-Fbar_stock[1,8]
  true_spr[2]<-Fbar_stock[2,8]
  
  true_F_spr_faa<-F_spr

  
  # calc fishery catch ------------------------------------------------------

  
 
  
  for(y in 1:mnyrs){
    for(ts in 1:tstep){
      for(r in 1:region){
        #est_region_C[r,ts,y]=0
        for(s in 1:stock){
          est_C_age[s,r,ts,y,]=((Fmort[r,ts,y,]/Z[r,ts,y,])*(1-exp(-Z[r,ts,y,])))*N[s,r,y,ts,]
          est_C[s,r,ts,y]=sum(est_C_age[s,r,ts,y,])
        } #close stock loop
        est_region_C[r,ts,y]=sum(est_C[,r,ts,y])
      }#close region loop
    }#close tstep loop
  }#close year loop
  #print(est_C_age)      
  #print(est_C)  
  #est_region_C


  #Calculate proportions at age
  
  for(y in 1:mnyrs){
    for(ts in 1:tstep){
      for(r in 1:region){
        for(s in 1:stock){
          for(a in fage:lage){
            est_Cp_age[s,r,ts,y,a]=est_C_age[s,r,ts,y,a]/est_C[s,r,ts,y] #estimate catch proportions at age
          }#close age loop
        } #close stock loop
      }#close reigon loop
    }#close tstep loop
  }#close yr loop
  
  #range(est_Cp_age)
    
  
  

  # calc survey catch -------------------------------------------------------

  
  
  for(y in 1:mnyrs){
    for(s in 1:stock){
      for(ts in 1:tstep){
        est_I_age_regcb[s,y,ts,]<-q*(N[s,cb,y,ts,]*ssel_cb[ts,y,])
        est_I_age_regac[s,y,ts,]<-q*(N[s,ac,y,ts,]*ssel_ac[ts,y,])
      } #close tstep loop
    } #close stock loop
  } #stock yr loop
  
  for(y in 1:mnyrs){
    for(ts in 1:tstep){
      for(s in 1:stock){
        est_I_regcb[s,y,ts]<-sum(est_I_age_regcb[s,y,ts,fage:lage])
        est_I_regac[s,y,ts]<-sum(est_I_age_regac[s,y,ts,fage:lage])
      } #clsoe stock loop
    }#close tstep loop
  }#close yr loop
  
  for(y in 1:mnyrs){
    for(s in 1:stock){
      for(ts in 1:tstep){
        for(a in fage:lage){
          est_Ip_regcb[s,y,ts,a]<-est_I_age_regcb[s,y,ts,a]/est_I_regcb[s,y,ts]
          est_Ip_regac[s,y,ts,a]<-est_I_age_regac[s,y,ts,a]/est_I_regac[s,y,ts]
        }#close age loop
      } #close tstep loop
    } #close stock loop
  } #close yr loop

  # print rowsums to see if they sum to 1
  # for(s in 1:stock){
  #   for(ts in 1:tstep){
  #     print(rowSums(est_Ip_regcb[s,,ts,]))
  #     print(rowSums(est_Ip_regac[s,,ts,]))
  #   }
  # }


  #calculate age 1 and yoy surveys
  for(y in 1:mnyrs){
    for(s in 1:stock){
      for(r in 1:region){
        est_I_age1[s,r,y]<-q_age1*(N[s,r,y,2,fage])
      } #close region loop
    } #close stock loop
  } #stock yr loop


  for(y in 1:mnyrs-1){
    for(s in 1:stock){
      for(r in 1:region){
        est_I_yoy[s,r,y]<-q_yoy*(N[s,r,y+1,2,fage])
      } #close region loop
    } #close stock loop
  } #stock yr loop
  #est_I_yoy<--
  ##return()
  #plot(est_I_yoy[cb,cb,])

  } #end simulating population



 

 # simulating true datasets -----------------------------------------------------

   
   
  { #start simdat
    
  ###### catch data sets
  #***models 1-2, Fleets-as-area
  totcat_fleet<-array(0,dim=c(region,mnyrs),dimnames = list(region.names,year.names))
  cat_age_fleet<-array(0,dim=c(region,mnyrs,lage),dimnames = list(region.names,year.names,age.names))
  cat_prop_age_fleet<-array(0,dim=c(region,mnyrs,lage),dimnames = list(region.names,year.names,age.names))

  #***models 3-4, without stock structure
  totcat_stock<-array(0,dim=c(stock,region,tstep,mnyrs), dimnames=list(stock.names,region.names,tstep.names,year.names)) #setting up array for total catch data sets
  totcat_reg<-array(0,dim=c(region,tstep,mnyrs), dimnames=list(region.names,tstep.names,year.names)) #setting up array for total catch without stock strcture for each region
  cat_prop_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
  cat_prop_age_stock<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets, that are disaggregated by stock
  cat_age_reg<-array(0, dim=c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names,age.names))#total catch at age data sets that are aggregate by stock
  cat_prop_age_reg<-array(0, dim=c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names,age.names))#catch PAA data sets that are aggregate by stock
  cat_prop_stock_err<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names))
  
  ##### fish.ind. surveys
  #1+surveys
  tot_ioa<-array(0,dim=c(stock,region,tstep,mnyrs),dimnames=list(stock.names,region.names,tstep.names,year.names))#indices of abundance for each of the 4 surveys
  tot_ioa_stock<-array(0,dim=c(stock,region,tstep,mnyrs),dimnames=list(stock.names,region.names,tstep.names,year.names))#indices of abundance for each of the 4 surveys
  tot_ioa_reg<-array(0,dim=c(region,tstep,mnyrs),dimnames=list(region.names,tstep.names,year.names))#indices of abundance for each of the 4 surveys, without stock composition
  ioa_age_reg<-array(0,dim=c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names)) #hold the total catch at age for each fim survey, agg acros regions
  ioa_prop_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage),dimnames=list(stock.names,region.names,tstep.names,year.names,age.names))#indices of abundance proportions at age
  ioa_prop_age_stock<-array(0,dim=c(stock,region,tstep,mnyrs,lage),dimnames=list(stock.names,region.names,tstep.names,year.names,age.names))#indices of abundance proportions at age
  ioa_prop_age_reg<-array(0,dim=c(region,tstep,mnyrs,lage),dimnames=list(region.names,tstep.names,year.names,age.names))#indices of abundance proportions at age
  ioa_prop_age_fleet<-array(0,dim=c(nsurv,mnyrs,lage),dimnames = list(survey.names,year.names,age.names))
  
  #age 1 survs
  tot_age1<-array(0,dim=c(stock,region,mnyrs),dimnames = list(stock.names,region.names,year.names)) #indices of abundance for each of the age-1 surveys
  tot_age1_reg<-array(0,dim=c(region,mnyrs),dimnames = list(region.names,year.names)) #indices of abundance for each of the age-1 surveys

  #yoy survs
  tot_yoy<-array(NA,dim=c(stock,region,mnyrs),dimnames = list(stock.names,region.names,year.names)) #indices of abundance for each of the age-1 surveys
  tot_yoy_reg<-array(NA,dim=c(region,mnyrs),dimnames = list(region.names,year.names)) #indices of abundance for each of the age-1 surveys

  occ_prob_dat<-array(0,dim=c(stock,tstep,lage),dimnames=list(stock.names,tstep.names,age.names)) 
  occ_prob_sd<-array(0,dim=c(stock,tstep,lage),dimnames=list(stock.names,tstep.names,age.names))
  
  

  # 
  # Generate true sets
  # 

  
  
  #aggregate starting parameters
  
  start_log_R_faa<-mean(start_log_R0)
  start_log_Feq_faa<-mean(start_log_F0) #feq in the first year
  start_log_F_faa<-c(mean(start_F_cb), mean(start_F_ac)) #starting F for each fleet
  
  
  #generate total catch data, here it's equal to the simulated catch
  for(s in 1:stock){
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          totcat_stock[s,r,ts,y]<- est_C[s,r,ts,y]##total catch
        }
      }
    }
  }
  #generate data without stock structure
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        totcat_reg[r,ts,y]<-sum(totcat_stock[,r,ts,y]) #summing across stocks for each region
      } #close yr loop
    } #close tstep loop
  } #close region loop
  
  #generate date for fleets-as-areas, no time-step, no stock
  for(r in 1:region){
    for(y in 1:mnyrs){
      totcat_fleet[r,y]<-sum(totcat_reg[r,,y]) #summing to get total catch across the year
    }
  }
  
  for(s in 1:stock){
    for(r in 1:region){
      for(y in 1:mnyrs){
        for(ts in 1:tstep){
          #calculate the proportion at age 
          cat_prop_age_stock[s,r,ts,y,]<-est_Cp_age[s,r,ts,y,] #catch prop ages for each stock is the same as the estimated generate
          #cat_prop_age_stock[s,r,ts,y,]<-cat_prop_age[s,r,ts,y,]/sum(cat_prop_age[s,r,ts,y,])
        }#close time step loop
      }#close year loop
    }#close region loop
  } #close stock loop
  
  #aggregate over population over stock to get catch at age over regions
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        for(a in fage:lage){
          cat_age_reg[r,ts,y,a]<-sum(est_C_age[,r,ts,y,a])#estimate catch at age, summed over stock
        }#close age loop
      } #close yr loop
    }#close tstep loop
  }#close region loop

  #aggregate catch at age, annual estiamtes for eachfleet (or region)
  for(r in 1:region){
    for(y in 1:mnyrs){
      for(a in fage:lage){
        cat_age_fleet[r,y,a]<-sum(cat_age_reg[r,,y,a])
      }
    }
  }
  
  #develop CAA without stock structure
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        #for(a in fage:lage){
        cat_prop_age_reg[r,ts,y,]<-cat_age_reg[r,ts,y,]/totcat_reg[r,ts,y] 
      } #close yr loop
    } #close tstep loop
  } #close region loop
  
  #develop CAA annually for FAA model
  for(r in 1:region){
      for(y in 1:mnyrs){
        #for(a in fage:lage){
        cat_prop_age_fleet[r,y,]<-cat_age_fleet[r,y,]/totcat_fleet[r,y] 
      } #close yr loop
  } #close region loop
  
  
  #generate total index of abundance data
  for(s in 1:stock){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        #setting the total ioa for the stock composition equal to the estimated in each region
        tot_ioa_stock[s,ac,ts,y]<- est_I_regac[s,y,ts]# #total index  
        tot_ioa_stock[s,cb,ts,y]<- est_I_regcb[s,y,ts]# total index w
      } #close year loop
    }#close tstep loop
  }#close stock loop
  
  #aggregate IOA for each region
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        tot_ioa_reg[r,ts,y]<-sum(tot_ioa_stock[,r,ts,y]) #summing across stocks for each region
      } #close yr loop
    } #close tstep loop
  } #close region loop
  

  #generate proportions at age for fisheries independent surveys
  for(s in 1:stock){
    for(y in 1:mnyrs){
      for(ts in 1:tstep){
        #set up  proportions at age
        #no error to start calculations, equal to the proportions estimated in the simualting population
        ioa_prop_age_stock[s,ac,ts,y,]<-est_Ip_regac[s,y,ts,]
        ioa_prop_age_stock[s,cb,ts,y,]<-est_Ip_regcb[s,y,ts,]
      }
    }
  } 

  #generate ioa proportions at age for each region
  for(ts in 1:tstep){
    for(y in 1:mnyrs){
      for(a in fage:lage){
        #first calculate catch for each age for each survey
        ioa_age_reg[cb,ts,y,a]<-sum(est_I_age_regcb[,y,ts,a]) #estimated were calculated separately for each region
        ioa_age_reg[ac,ts,y,a]<-sum(est_I_age_regac[,y,ts,a])
        }#close age loop
      } #close yr loop
    } #close tstep loop

  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:mnyrs){
        for(a in fage:lage){
          ioa_prop_age_reg[r,ts,y,a]<-ioa_age_reg[r,ts,y,a]/tot_ioa_reg[r,ts,y] ###*divid total ioa at age for region /total ioa for the year
        }#close age loop
      }#close year loop
    }#close tstep loop
  }#close region loop

  
  #generate age 1 indices of abundance
  for(s in 1:stock){
    for(r in 1:region){
      for(y in 1:mnyrs){
        tot_age1[s,r,y]<- est_I_age1[s,r,y] #total index for the age 1 index
        tot_yoy[s,r,y]<- est_I_yoy[s,r,y] #total index for the yoy survey
      } #close year loop
      tot_age1[is.na(tot_age1)] <- -99 #missing values = -99
      tot_yoy[is.na(tot_yoy)] <- -99 #missing values =-99
      tot_yoy[s,r,lmyr]=-99 #not estimating yoy in the first year because calculated from the year prior
    }#close region loop
  }#close stock loop

  for(r in 1:region){
    for(y in 1:mnyrs){
      tot_age1_reg[r,y]<-sum(tot_age1[,r,y])
      tot_yoy_reg[r,y]<-sum(tot_yoy[,r,y])
    }#close region loop
    tot_yoy_reg[r,lmyr]=-99 #not estimating yoy in the first year because calculated from the year prior
  }#close yr loop

  ## generate occupancy probabilities
  for(s in 1:stock){
      for(ts in 1:tstep){
          for(a in fage:lage){
            occ_prob_dat[s,ts,a]<- prop[s,ts,ac,a] 
          }#close age loop
        occ_prob_dat[s,ts,]<-ifelse(occ_prob_dat[s,ts,]>=0.99,0.99,ifelse(occ_prob_dat[s,ts,]<=0.01,0.01,occ_prob_dat[s,ts,]))
        #occ_prob_dat[s,ac,ts,]<-1-occ_prob_dat[s,cb,ts,]
      }#close tstep loop
  }#close stock loop
  
  #log_occ_prob_sd_bay<-std$std.dev[grep("log_prop_bay", std$name)]
  #log_occ_prob_sd_coast<-std$std.dev[grep("log_prop_coast", std$name)]
   log_occ_prob_dat<-ifelse(occ_prob_dat==0,log(occ_prob_dat+0.01),log(occ_prob_dat)) #take the log of the values so that they are input on the log scale as the model requires
   
   #filling in occupancy sd
   occ_prob_sd[1,1,2:15]<-log_occ_prob_sd_bay[1:14] #ches bay stock, time step 1
   occ_prob_sd[1,2,2:15]<-log_occ_prob_sd_bay[15:28] #ches bay stock, time step 2
   occ_prob_sd[2,1,2:15]<-log_occ_prob_sd_coast[1:14] #Atlantic coast stock, time step 1
   occ_prob_sd[2,2,2:15]<-log_occ_prob_sd_coast[15:28] #Atlantic coast stock, time step 2   

   
   
   #things for the data sets
   
    switch_pen_prop_on<-1 # use occupancy prior in likelihood penalty
    switch_pen_prop_off<-2 #do not use occupancy prior in likelihood
    use_age_err_yes<-1 #apply aging error matrix in estimation model
    use_age_err_no<-2 #apply identity matrix in estimation model
  
  
  } #close sim dat
  










# Setting up directory and folders to store boot runs ---------------------

{

  setwd("C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial")
  
  ### writing a dat file wihtout comments so you can find the lines you need to replace to fill in simulated data
  filename1<-"scaa-stripedbass-faa"
  filename2<-"scaa-stripedbass-faa"
  filename3<-"scaa-stripedbass-se3"
  filename4<-"scaa-stripedbass-se3"
  filename5<-"scaa-stripedbass-se5"
  filename6<-"scaa-stripedbass-se6"
  
  
  
  #number of bootstrap replicates
  boot1=1         #number assigned to first run 
  bootN=500 	  #number assigned to last run 
  nboot=length(boot1:bootN)
  #set the seed for the R random number generator
  #set.seed(12345)  
  
  
  dat1<-scan(file=paste(filename1,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  write(dat1, file="countlines-faa1.dat") #countlines.dat used only for development, does not include blank lines or comments
  
  dat2<-scan(file=paste(filename2,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  write(dat2, file="countlines-faa2.dat") #countlines.dat used only for development, does not include blank lines or comments
  
  dat3<-scan(file=paste(filename3,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  write(dat3, file="countlines-se3.dat") #countlines.dat used only for development, does not include blank lines or comments
  
  dat4<-scan(file=paste(filename4,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  write(dat4, file="countlines-se4.dat") #countlines.dat used only for development, does not include blank lines or comments
  
  # dat5<-scan(file=paste(filename5,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  # write(dat5, file="countlines-se5.dat") #countlines.dat used only for development, does not include blank lines or comments
  # 
  dat6<-scan(file=paste(filename6,'.dat', sep=""), what="character", sep="&", blank.lines.skip=T,na.strings="$", comment.char="#")
  write(dat6, file="countlines-se6.dat") #countlines.dat used only for development, does not include blank lines or comments
  
  
  #switches applied during admb execution, list with space in between
  admb.switch='-est -nox'
  
  #folder name for bootstrap results
  #boot.folder='BootRuns'
  
  
  sim.dir<-getwd()
  
  #creating an admb file for each run (From AMY EXAMPLE CODE)
  
  #FAA 1
  dat.tmp.1<-dat1
  bamexe1=paste(filename1,'.exe',sep='')
  bamsource1=paste(sim.dir,'/',bamexe1,sep='')
  #bootout=paste(sim.dir,'/', boot.folder, sep='')
  
  process.dir1=paste(sim.dir,"/",as.character("faamodel1"),sep="") #created for each iteration
  #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  dir.create(process.dir1, showWarnings = FALSE)
  
  order<-seq(1,bootN) #number of bootstraps
  boot<-paste("boot",order,sep="")
  
  #FAA 2
  dat.tmp.2<-dat2
  bamexe2=paste(filename2,'.exe',sep='')
  bamsource2=paste(sim.dir,'/',bamexe2,sep='')
  #bootout=paste(sim.dir,'/', boot.folder, sep='')
  
  process.dir2=paste(sim.dir,"/",as.character("faamodel2"),sep="") #created for each iteration
  #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  dir.create(process.dir2, showWarnings = FALSE)
  
  #SE 3
  dat.tmp.3<-dat3
  bamexe3=paste(filename3,'.exe',sep='')
  bamsource3=paste(sim.dir,'/',bamexe3,sep='')
  #bootout=paste(sim.dir,'/', boot.folder, sep='')
  
  process.dir3=paste(sim.dir,"/",as.character("semodel3"),sep="") #created for each iteration
  #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  dir.create(process.dir3, showWarnings = FALSE)
  
  #SE 4
  dat.tmp.4<-dat4
  bamexe4=paste(filename4,'.exe',sep='')
  bamsource4=paste(sim.dir,'/',bamexe4,sep='')
  #bootout=paste(sim.dir,'/', boot.folder, sep='')
  
  process.dir4=paste(sim.dir,"/",as.character("semodel4"),sep="") #created for each iteration
  #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  dir.create(process.dir4, showWarnings = FALSE)
  
  #SE 5
  # dat.tmp.5<-dat5
  # bamexe5=paste(filename5,'.exe',sep='')
  # bamsource5=paste(sim.dir,'/',bamexe5,sep='')
  # #bootout=paste(sim.dir,'/', boot.folder, sep='')
  # 
  # process.dir5=paste(sim.dir,"/",as.character("semodel5"),sep="") #created for each iteration
  # #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  # dir.create(process.dir5, showWarnings = FALSE)
  
  #SE 6
  dat.tmp.6<-dat6
  bamexe6=paste(filename6,'.exe',sep='')
  bamsource6=paste(sim.dir,'/',bamexe6,sep='')
  #bootout=paste(sim.dir,'/', boot.folder, sep='')
  
  process.dir6=paste(sim.dir,"/",as.character("semodel6"),sep="") #created for each iteration
  #dir.create(process.dir, showWarnings = FALSE,overwrite=TRUE) #this doesn't work for some reason
  dir.create(process.dir6, showWarnings = FALSE)
  
  
  #remove previously saved output
  unlink("faamodel1/sim_F_results.txt")
  unlink("faamodel1/sim_results.txt")
  unlink("faamodel2/sim_F_results.txt")
  unlink("faamodel2/sim_results.txt")
  unlink("semodel3/sim_F_results.txt")
  unlink("semodel3/sim_results.txt")
  unlink("semodel4/sim_F_results.txt")
  unlink("semodel4/sim_results.txt")
  # unlink("semodel5/sim_F_results.txt")
  # unlink("semodel5/sim_results.txt")
  unlink("semodel6/sim_F_results.txt")
  unlink("semodel6/sim_results.txt")
  
  
  unlink("semodel3/sim_F40_results.txt")
  unlink("semodel4/sim_F40_results.txt")
  
  
}
  
  

##### Setting up NUmber of boot straps
  
  
  # order<-seq(1,bootN) #number of bootstraps
  # boot<-paste("boot",order,sep="")
# 
#   totCores=detectCores()
#   numCores <- totCores
#   cl <- makeCluster(numCores-1)
#   registerDoParallel(cl)
  
  
  
#foreach(iboot=boot1:bootN) %dopar% {
  for(iboot in boot1:bootN){
    
  
  # adding error into true datsets ---------------------

  {
    #catch data
    dat_stock_cat<-array(0,dim=c(stock,region,tstep,mnyrs), dimnames=list(stock.names,region.names,tstep.names,year.names)) #setting up array for total catch data sets
    dat_stock_cat_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_stock_cat_age_err<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_stock_prop_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    #cat_stock_err<-array(0, dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names))#sd =1
    cat_stock_err<-array(rnorm(mnyrs,0,0.2), dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names))#sd =1
    dat_cat_weight_stock<-array(0, dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names))#sd =1
    
    dat_region_cat<-array(0,dim=c(region,tstep,mnyrs), dimnames=list(region.names,tstep.names,year.names)) #setting up array for total catch data sets
    dat_region_cat_age<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_region_cat_age_err<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_region_prop_age<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets

    dat_faa_cat<-array(0,dim=c(region,mnyrs), dimnames=list(region.names,year.names)) #setting up array for total catch data sets
    dat_faa_cat_age<-array(0,dim=c(region,mnyrs,lage), dimnames=list(region.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_faa_prop_age<-array(0,dim=c(region,mnyrs,lage), dimnames=list(region.names,year.names,age.names)) #setting up array for catch proportions at age datasets

    ESS_F<-100

    tot_cat_yr<-array(0, dim=c(mnyrs),dimnames=list(year.names))
    #adding error to catch data
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            dat_stock_cat[s,r,ts,y]<-totcat_stock[s,r,ts,y]*exp(cat_stock_err[s,r,ts,y])
           }
        }
      }
    }
    # #calculating total annual catch
    # for(y in 1:mnyrs){
    #   tot_cat_yr<-sum(dat_stock_cat[,1,1,y])+sum(dat_stock_cat[,2,1,y])+sum(dat_stock_cat[,1,2,y])+sum(dat_stock_cat[,2,2,y])
    # }

    #aggregate catch data for regions/timestep (SE 3 and 4)
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          dat_region_cat[r,ts,y]<-sum(dat_stock_cat[,r,ts,y])
        }
      }
    }
    #sum(dat_stock_cat[,2,1,24]);dat_region_cat[2,1,24]
    
    #caluclating the proportion of each stock/region for each timestep
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){  
            dat_cat_weight_stock[s,r,ts,y]<-dat_stock_cat[s,r,ts,y]/(sum(dat_region_cat[r,,y])) #calculating the weight of total catch, so that the ESS is summed to the total number of sampled fish in each year
            #in the denominator, the sum is summing across stock, for timestep 1 and timestep 2. so then the ESS will add up to the total for the year
          }
        }
      }
    }
    #dat_cat_weight_stock[,,,1]
    
    #aggregate catch data for FAA models (FAA 1 and 2)
    for(r in 1:region){
        for(y in 1:mnyrs){
          dat_faa_cat[r,y]<-sum(dat_region_cat[r,,y])
        }
      }
    #dat_stock_cat[,1,,10];dat_faa_cat[1,10]
    
    #generate error for catch-at-age
    #hold_aging_err
    #dat_cat_prop_age_stock<-array(0,dim=c(stock,region,tstep,mnyrs,lage),dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #array to store data for catch at age proportions with error
    hold_age_err_stock<-array(0,dim=c(lage),dimnames=list(age.names)) #array to store data for catch at age proportions with error
    stock_err<-array(0,dim=c(lage),dimnames=list(age.names)) #array to store data for catch at age proportions with error
    dat_cat_prop_age_stock_err<-array(0,dim=c(stock,region,tstep,mnyrs,lage),dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #array to store data for catch at age proportions with error
    
    aging_error_matrix <- as.matrix(ageerr)
    #aging_error_matrix<-diag(15) #this is the identity matrix
    
    
    #weight ESS population
    ESS_fish<-array(0,dim=c(stock,region,tstep,mnyrs),dimnames=list(stock.names,region.names,tstep.names,year.names)) #array to store data for catch at age proportions with error
    
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            ESS_fish[s,r,ts,y]<-ESS_F*dat_cat_weight_stock[s,r,ts,y]
            dat_stock_cat_age[s,r,ts,y,]<-rmultinom(1,ESS_fish[s,r,ts,y],cat_prop_age_stock[s,r,ts,y,])
            hold_age_err_stock<-dat_stock_cat_age[s,r,ts,y,]
            stock_err<- aging_error_matrix %*% hold_age_err_stock
            dat_stock_cat_age_err[s,r,ts,y,]<-stock_err
            #dat_stock_prop_age[s,r,ts,y,]<-dat_stock_cat_age_err[s,r,ts,y,]/sum(dat_stock_cat_age_err[s,r,ts,y,])
          }
        }
      }
    }
    
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            for(a in fage:lage){
              if(sum(dat_stock_cat_age_err[s,r,ts,y,])==0){
                dat_stock_prop_age[s,r,ts,y,a]<--99#if no observed fish for any age, set whole year/timestep =0
              }
              else{
                dat_stock_prop_age[s,r,ts,y,a]<-dat_stock_cat_age_err[s,r,ts,y,a]/sum(dat_stock_cat_age_err[s,r,ts,y,])
              }
            } #close age loop
          } #close year loop
        } # close tstep
      } # closeregion loop
    } #close stock loop
    
    #View(dat_stock_prop_age[2,1,1,,])
    #dat_stock_cat_age_err[2,1,1,,]
    #rowSums(dat_stock_prop_age[2,1,2,,])
    #dat_stock_cat_age_err[2,1,1,1,]/sum(dat_stock_cat_age_err[2,1,1,1,])
    
    #aggregate catch-at-age data for regions/timestep (SE 3 and 4)
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          for(a in fage:lage){
            dat_region_cat_age[r,ts,y,a]<-sum(dat_stock_cat_age[,r,ts,y,a])
            dat_region_cat_age_err[r,ts,y,a]<-sum(dat_stock_cat_age_err[,r,ts,y,a])
          } #close age loop
         }#close year loop
      }#close tstep loop
    } #close region loop
    
    #calculate catch proportions at age without stock component
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          for(a in fage:lage){
            dat_region_prop_age[r,ts,y,a]<-dat_region_cat_age_err[r,ts,y,a]/sum(dat_region_cat_age_err[r,ts,y,])
          } #close age loop
        } #close year loop
      } #close tstep loop
    } #close region loop
    
    #dat_stock_cat_age_err[,1,1,5,];dat_region_cat_age_err[1,1,5,]
    #round(dat_region_prop_age[1,,1,],3);round(cat_prop_age_reg[1,,1,],3)
    
    #aggregate catch-at-age data for FAA (FAA 1 and 2)
    for(r in 1:region){
      for(y in 1:mnyrs){
        for(a in fage:lage){
          dat_faa_cat_age[r,y,a]<-sum(dat_region_cat_age_err[r,,y,a])
            #sum(dat_stock_cat_age_err[1,r,,y,a])+sum(dat_stock_cat_age_err[2,r,,y,a])
        }
      }
    }
    
    #calc FAA proportions-at-age
    for(r in 1:region){
      for(y in 1:mnyrs){
        for(a in fage:lage){
          dat_faa_prop_age[r,y,a]<-dat_faa_cat_age[r,y,a]/sum(dat_faa_cat_age[r,y,])
        }
      }
    }
    #dat_region_cat_age_err[1,,10,];dat_faa_cat_age[1,10,]

        #Survey Data
    dat_stock_tot_ioa<-array(0,dim=c(stock,region,tstep,mnyrs), dimnames=list(stock.names,region.names,tstep.names,year.names)) #setting up array for total catch data sets
    dat_stock_ioa_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_stock_ioa_age_err<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_stock_ioa_prop_age<-array(0,dim=c(stock,region,tstep,mnyrs,lage), dimnames=list(stock.names,region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    #tot_ioa_stock_err<-array(0, dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names)) #set to 1 to start calculations
    tot_ioa_stock_err<-array(rnorm(mnyrs,0,0.4), dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names)) #set to 1 to start calculations
    dat_ioa_weight_stock<-array(0, dim=c(stock,region,tstep,mnyrs), dimnames = list(stock.names,region.names,tstep.names,year.names)) #set to 1 to start calculations
    
    
    dat_region_tot_ioa<-array(0,dim=c(region,tstep,mnyrs), dimnames=list(region.names,tstep.names,year.names)) #setting up array for total catch data sets
    dat_region_ioa_age<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_region_ioa_age_err<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_region_ioa_prop_age<-array(0,dim=c(region,tstep,mnyrs,lage), dimnames=list(region.names,tstep.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    
    nsurv<-4
    survey.names<-c("CB1","CB2","OC1","OC2")
    dat_faa_tot_ioa<-array(0,dim=c(nsurv,mnyrs), dimnames=list(survey.names,year.names)) #setting up array for total catch data sets
    dat_faa_ioa_age<-array(0,dim=c(nsurv,mnyrs,lage), dimnames=list(survey.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    dat_faa_ioa_prop_age<-array(0,dim=c(nsurv,mnyrs,lage), dimnames=list(survey.names,year.names,age.names)) #setting up array for catch proportions at age datasets
    
    ESS_S<-100
    
    #adding error to catch data
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            dat_stock_tot_ioa[s,r,ts,y]<-tot_ioa_stock[s,r,ts,y]*exp(tot_ioa_stock_err[s,r,ts,y])
          }
        }
      }
    }
    
    #aggregate catch data for regions/timestep (SE 3 and 4)
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          dat_region_tot_ioa[r,ts,y]<-sum(dat_stock_tot_ioa[,r,ts,y])
        }
      }
    }
    #dat_stock_tot_ioa[,2,2,10];dat_region_tot_ioa[2,2,10]
    
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
           dat_ioa_weight_stock[s,r,ts,y]<-dat_stock_tot_ioa[s,r,ts,y]/(sum(dat_region_tot_ioa[r,,y])) #calculating the weight of total ioa, so that the ESS is summed to the total number of sampled fish in each year
          }
        }
      }
    }
    

    #aggregate ioa data for FAA models (FAA 1 and 2)
    for(y in 1:mnyrs){
      dat_faa_tot_ioa[1,y]<-dat_region_tot_ioa[1,1,y]
      dat_faa_tot_ioa[2,y]<-dat_region_tot_ioa[1,2,y]
      dat_faa_tot_ioa[3,y]<-dat_region_tot_ioa[2,1,y]
      dat_faa_tot_ioa[4,y]<-dat_region_tot_ioa[2,2,y]
   }
    
    #generate error for ioa prop-at-age
    #hold_aging_err
    hold_age_err_stock<-array(0,dim=c(lage),dimnames=list(age.names)) #array to store data for catch at age proportions with error
    stock_err<-array(0,dim=c(lage),dimnames=list(age.names)) #array to store data for catch at age proportions with error

    #aging_error_matrix <- as.matrix(ageerr)
    #aging_error_matrix<-diag(15) #this is the identity matrix
    
    ESS_surv<-array(0,dim=c(stock,region,tstep,mnyrs),dimnames=list(stock.names,region.names,tstep.names,year.names)) #array to store data for catch at age proportions with error
    
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            ESS_surv[s,r,ts,y]<-ESS_S*dat_ioa_weight_stock[s,r,ts,y]

            dat_stock_ioa_age[s,r,ts,y,]<-rmultinom(1,ESS_surv[s,r,ts,y],ioa_prop_age_stock[s,r,ts,y,])
            hold_age_err_stock<-dat_stock_ioa_age[s,r,ts,y,]
            stock_err<- aging_error_matrix %*% hold_age_err_stock
            dat_stock_ioa_age_err[s,r,ts,y,]<-stock_err
          }
        }
      }
    }
    
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          for(y in 1:mnyrs){
            for(a in fage:lage){
              if(sum(dat_stock_ioa_age_err[s,r,ts,y,])==0){
                dat_stock_ioa_prop_age[s,r,ts,y,a]<- -99 #if no observed fish for any age, set whole year/timestep =0
              }
              else{
                dat_stock_ioa_prop_age[s,r,ts,y,a]<-dat_stock_ioa_age_err[s,r,ts,y,a]/sum(dat_stock_ioa_age_err[s,r,ts,y,])
              }
            } #close age loop
          } #close year loop
        } #close tstep loop
      } #close region loop
    } #close stock loop
    
    #remove any NAN values from being divide by 0 (if there are no fish from a stock in a region during a time-step/yr)
    #dat_stock_ioa_prop_age[is.nan(dat_stock_ioa_prop_age)] <- 0
    
    
    #round(dat_stock_ioa_prop_age[1,1,1,5,],3);round(ioa_prop_age_stock[1,1,1,5,],3)
    
    #aggregate ioa-at-age data for regions/timestep (SE 3 and 4)
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          for(a in fage:lage){
            dat_region_ioa_age[r,ts,y,a]<-sum(dat_stock_ioa_age[,r,ts,y,a])
            dat_region_ioa_age_err[r,ts,y,a]<-sum(dat_stock_ioa_age_err[,r,ts,y,a])
          }
        }
      }
    }
    
    #aggregate ioa-at-age data for regions/timestep (SE 3 and 4)
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          for(a in fage:lage){
            dat_region_ioa_prop_age[r,ts,y,a]<-dat_region_ioa_age_err[r,ts,y,a]/sum(dat_region_ioa_age_err[r,ts,y,])
          }
        }
      }
    }
    #dat_stock_ioa_age_err[,2,1,5,];dat_region_ioa_age_err[2,1,5,]
    
    #aggregate catch-at-age data for FAA (FAA 1 and 2)
      for(y in 1:mnyrs){
        for(a in fage:lage){
          dat_faa_ioa_age[1,y,a]<-dat_region_ioa_age_err[1,1,y,a]
          dat_faa_ioa_age[2,y,a]<-dat_region_ioa_age_err[1,2,y,a]
          dat_faa_ioa_age[3,y,a]<-dat_region_ioa_age_err[2,1,y,a]
          dat_faa_ioa_age[4,y,a]<-dat_region_ioa_age_err[2,2,y,a]
        }
      }
    
    #aggregate catch-at-age data for FAA (FAA 1 and 2)
    for(y in 1:mnyrs){
      for(n in 1:nsurv){
        for(a in fage:lage){
          dat_faa_ioa_prop_age[n,y,a]<-dat_faa_ioa_age[n,y,a]/sum(dat_faa_ioa_age[n,y,])
        }
      }
    }
    
    
    #age 1 surveys
    nsurv1<-2
    age1.names<-c("CB","AC")
    
    dat_ioa_age1_stock<-array(0,dim=c(stock,region,mnyrs),dimnames=list(stock.names,region.names,year.names))
    #tot_age1_err<-array(0, dim=c(stock,region,mnyrs), dimnames = list(stock.names,region.names,year.names))
    tot_age1_err<-array(rnorm(mnyrs,0,0.5), dim=c(stock,region,mnyrs), dimnames = list(stock.names,region.names,year.names))

    dat_ioa_age1_region<-array(0,dim=c(region,mnyrs),dimnames=list(region.names,year.names))
    dat_ioa_age1_faa<-array(0,dim=c(nsurv1,mnyrs),dimnames=list(age1.names,year.names))
    
    for(s in 1:stock){
      for(r in 1:region){
        for(y in 1:mnyrs){
          dat_ioa_age1_stock[s,r,y]<-tot_age1[s,r,y]*exp(tot_age1_err[s,r,y])# index for the age 1 index
        }
      }
    }
    #dat_ioa_age1_stock[,2,]
    #aggregate over stocks (SE 3 and SE4)
    for(r in 1:region){
        for(y in 1:mnyrs){
          dat_ioa_age1_region[r,y]<-sum(tot_age1[,r,y])# index for the age 1 index
      }
    }
    #dat_ioa_age1_stock[,1,5]; dat_ioa_age1_region[1,5]
    #aggregate over regions for the FAA models (1 and 2)
    for(y in 1:mnyrs){
      dat_ioa_age1_faa[1,y]<-dat_ioa_age1_region[1,y]
      dat_ioa_age1_faa[2,y]<-dat_ioa_age1_region[2,y]
    }
    
    #yoy surveys
    nsruvyoy<-2
    yoy.names<-c("CB","AC")
    
    dat_ioa_yoy_stock<-array(0,dim=c(stock,region,mnyrs),dimnames=list(stock.names,region.names,year.names))
    tot_yoy_err<-array(rnorm(mnyrs,0,0.5), dim=c(stock,region,mnyrs), dimnames = list(stock.names,region.names,year.names))
    #tot_yoy_err<-array(0, dim=c(stock,region,mnyrs), dimnames = list(stock.names,region.names,year.names))
    dat_ioa_yoy_region<-array(0,dim=c(region,mnyrs),dimnames=list(region.names,year.names))
    dat_ioa_yoy_faa<-array(0,dim=c(nsruvyoy,mnyrs),dimnames=list(yoy.names,year.names))
    

    tot_yoy[s,r,y]<- est_I_yoy[s,r,y] #total index for the yoy survey
    for(s in 1:stock){
      for(r in 1:region){
        for(y in 1:mnyrs){
          dat_ioa_yoy_stock[s,r,y]<- tot_yoy[s,r,y]*exp(tot_yoy_err[s,r,y])# index for the age 1 index
        }
      }
    }
    #dat_ioa_yoy_stock[,1,]
    #aggregate over stocks (SE 3 and SE4)
    for(r in 1:region){
      for(y in 1:mnyrs){
        dat_ioa_yoy_region[r,y]<-sum(dat_ioa_yoy_stock[,r,y])# index for the age 1 index
      }
    }
    dat_ioa_yoy_region[1,30]<--99
    dat_ioa_yoy_region[2,30]<--99
    
      #aggregate over regions for the FAA models (1 and 2)
      for(y in 1:mnyrs){
        dat_ioa_yoy_faa[1,y]<-dat_ioa_yoy_region[1,y]
        dat_ioa_yoy_faa[2,y]<-dat_ioa_yoy_region[2,y]
      }
    
    cat_cv<-rep(0.2,mnyrs)
    ioa_cv_1<-rep(0.4,mnyrs)
    ioa_cv_2<-rep(0.4,mnyrs)
    age1_cv<-rep(0.5,mnyrs)
    yoy_cv<-rep(0.5,mnyrs)
    
    
    #setting ESS for each estimation model
    avg_ESS<-array(0, dim=c(stock,region,tstep))
    ESS_F_se_dat<-array(0, dim=c(region,tstep))
    
    #calculate the average ESS across all years
      for(s in 1:stock){
        for(r in 1:region){
          for(ts in 1:tstep){
               avg_ESS[s,r,ts]<-mean(ESS_fish[s,r,ts,])
           }
        }
      }

    ESS_F_se_dat[1,1]<-round(sum(avg_ESS[,1,1])) #region 1, timestep 1
    ESS_F_se_dat[1,2]<-round(sum(avg_ESS[,1,2])) #region 1, timestep 2
    ESS_F_se_dat[2,1]<-round(sum(avg_ESS[,2,1])) #region 2, timestep 1
    ESS_F_se_dat[2,2]<-round(sum(avg_ESS[,2,2])) #region 2, timestep 2
    
    ESS_F_faa_dat<-rep(ESS_F,2)
    
    
    #now ess for the surveys
    avg_sESS<-array(0, dim=c(stock,region,tstep))
    ESS_S_se_dat<-array(0, dim=c(region,tstep))
    
    #calculate the average ESS across all years
    for(s in 1:stock){
      for(r in 1:region){
        for(ts in 1:tstep){
          avg_sESS[s,r,ts]<-mean(ESS_surv[s,r,ts,])
        }
      }
    }
    
    ESS_S_se_dat[1,1]<-round(sum(avg_sESS[,1,1])) #region 1, timestep 1
    ESS_S_se_dat[1,2]<-round(sum(avg_sESS[,1,2])) #region 1, timestep 2
    ESS_S_se_dat[2,1]<-round(sum(avg_sESS[,2,1])) #region 2, timestep 1
    ESS_S_se_dat[2,2]<-round(sum(avg_sESS[,2,2])) #region 2, timestep 2
    
    
    ESS_S_faa_dat<-c(ESS_S_se_dat[1,1],ESS_S_se_dat[1,2],ESS_S_se_dat[2,1],ESS_S_se_dat[2,2])
    
    
  }
      


  

  
  
  # FAA Model 1 ---------------------
  
  #
  #
  # Fleets-as-areas Model, 1 - no aging error
  #
  #
  
  filename.dat1=paste(filename1,'.dat',sep='')	
  
  
  setwd(process.dir1)
  
  # filling in the data ------------------------------------------------
  
  #fill in starting parameters
  {  #start filling in dataset
    dat.tmp.1[16]<-as.character(paste(start_log_R_faa,collapse="\t")) #starting parameter for log_recruitment
    dat.tmp.1[17]<-as.character(paste(start_log_F_faa,collapse="\t")) #starting parameters for F in both regions
    dat.tmp.1[18]<-as.character(paste(start_log_Feq_faa,collapse="\t")) #starting parameters for log_Feq
    
    dat.tmp.1[19]<-as.character(paste(rep(start_fsel_ac_a,1),collapse="\t")) #start log_sf1_ac
    dat.tmp.1[20]<-as.character(paste(rep(start_fsel_ac_b,1),collapse="\t")) #start log_sf2_ac
    dat.tmp.1[21]<-as.character(paste(rep(start_fsel_cb_a,1),collapse="\t")) #start log_sf1_cb
    dat.tmp.1[22]<-as.character(paste(rep(start_fsel_cb_b,1),collapse="\t")) #start log_sf2_cb
    dat.tmp.1[23]<-as.character(paste(rep(start_fsel_cb_c,1),collapse="\t")) #start log_sf3_cb
    dat.tmp.1[24]<-as.character(paste(rep(start_fsel_cb_d,1),collapse="\t")) #start log_sf4_cb
    
    dat.tmp.1[25]<-as.character(paste(rep(start_ssel_ac_a,1),collapse="\t")) #start log_ssf1_ac
    dat.tmp.1[26]<-as.character(paste(rep(start_ssel_ac_b,1),collapse="\t")) #start log_ssf2_ac
    dat.tmp.1[27]<-as.character(paste(rep(start_ssel_ac_c,1),collapse="\t")) #start log_ssf2_ac
    dat.tmp.1[28]<-as.character(paste(rep(start_ssel_ac_d,1),collapse="\t")) #start log_ssf2_ac
    
    dat.tmp.1[29]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
    dat.tmp.1[30]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
    dat.tmp.1[31]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
    
    dat.tmp.1[32]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
    dat.tmp.1[33]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
    
    #fill in total catch
    dat.tmp.1[34]<-as.character(paste(dat_faa_cat[1,],collapse="\t")) #line 23 is where total catch starts, timestep 1, region1
    dat.tmp.1[35]<-as.character(paste(dat_faa_cat[2,],collapse="\t")) #line 24, time step 2, region 1
    
    #fill in catch PAA
    linenum=36 #starting line number of comp matrix
    for(r in 1:region){
      for(y in 1:mnyrs){
        dat.tmp.1[linenum]<-as.character(paste(dat_faa_prop_age[r,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
        linenum=linenum+1
        #print(dat_faa_prop_age[r,y,])
      } #close year looop
    }# close region loop
    #rowSums(dat_faa_prop_age[2,,])
    
    #fill in catch CV
    #cat_cv<-rep(0.2,mnyrs)
    dat.tmp.1[96]<-as.character(paste(cat_cv,collapse="\t")) #line 96 is where cv for fisheries starts
    dat.tmp.1[97]<-as.character(paste(cat_cv,collapse="\t")) #fleet 2
    
    
    # fill in survey data
    linenum=98
    for(n in 1:nsurv){
      dat.tmp.1[linenum]<-as.character(paste(dat_faa_tot_ioa[n,],collapse="\t")) #using the same survey info as SE model, just assuming it represents annual trends
      linenum=linenum+1
    }
    
    #fill in survey PAA
    
    linenum=102  #starting line number of age comp matrix for surveys
    for(n in 1:nsurv){
      for(y in 1:mnyrs){
        dat.tmp.1[linenum]<-as.character(paste(dat_faa_ioa_prop_age[n,y,],collapse="\t"))
        linenum=linenum+1
      } #close year looop
    }# close survey loop
    #rowSums(dat_faa_ioa_prop_age[4,,])
    #dat_ioa_prop_age_reg[2,2,1,];ioa_prop_age_reg[2,2,1,]
    
    #fill in catch CV
    #ioa_cv<-rep(0.1,mnyrs)
    dat.tmp.1[222]<-as.character(paste(ioa_cv_1,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.1[223]<-as.character(paste(ioa_cv_1,collapse="\t")) #region 1, timestep 2
    dat.tmp.1[224]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 1
    dat.tmp.1[225]<-as.character(paste(ioa_cv_1,collapse="\t")) #region 2, timestep 2
    
    
    # fill in age 1 IOA and CV
    dat.tmp.1[226]<-as.character(paste(dat_ioa_age1_faa[1,],collapse="\t")) #using same surveys as SE, just assuming they represent annual trends
    dat.tmp.1[227]<-as.character(paste(dat_ioa_age1_faa[2,],collapse="\t"))
    #pasted as CB age 1 survey, AC age 1 survey
    #age1_cv<-rep(0.1,mnyrs)
    dat.tmp.1[228]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.1[229]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
    
    dat.tmp.1[230]<-as.character(paste(dat_ioa_yoy_faa[1,],collapse="\t")) #using same surveys as SE
    dat.tmp.1[231]<-as.character(paste(dat_ioa_yoy_faa[2,],collapse="\t"))
    
    #yoy_cv<-rep(0.1,mnyrs)
    dat.tmp.1[232]<-as.character(paste(yoy_cv,collapse="\t")) #l
    dat.tmp.1[233]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
    
    dat.tmp.1[341]<-as.character(paste(use_age_err_no,collpase="\t"))
    
    #fill in EFFECTIVE SAMPLE SIZE
    dat.tmp.1[372]<-as.character(paste(ESS_F_faa_dat[1],collapse="\t")) #
    dat.tmp.1[373]<-as.character(paste(ESS_F_faa_dat[2],collapse="\t")) #ESS for region 2 fishery (coast), space in dat file, can't get rid of so line nums are not in sync
    dat.tmp.1[374]<-as.character(paste(ESS_S_faa_dat[1],collapse="\t")) #ESS for survey in region 1 (bay)
    dat.tmp.1[375]<-as.character(paste(ESS_S_faa_dat[2],collapse="\t")) #ESS for survey in region 2 (bay)
    dat.tmp.1[376]<-as.character(paste(ESS_S_faa_dat[3],collapse="\t")) #ESS for survey in region 2 (bay)
    dat.tmp.1[377]<-as.character(paste(ESS_S_faa_dat[4],collapse="\t")) #ESS for survey in region 2 (bay)
    
    
    dat.tmp.1[378]<-as.character(paste(iboot),collapse='\t') #simulation number
    #dat.tmp.2[460]<-as.character(paste(1,collapse='\t')) #simulation number
    
    #inputting true values
    dat.tmp.1[379]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.1[380]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
    dat.tmp.1[381]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
    #dat.tmp.1[382]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
    dat.tmp.1[382]<-as.character(paste(true_F_spr_faa,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
    linenum=383
    for(y in 1:mnyrs){
      dat.tmp.1[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
      linenum=linenum+1
    }        
    #dat.tmp.1[383]<-as.character(paste('12345',collapse="\t")) # test number
  } #stop editing dataset
  
  # Run ADMB ----------------------------------------------------------------
  
  
  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  write(file=filename.dat1, dat.tmp.1)
  run.command=paste(filename1, admb.switch, '-ind', filename.dat1, sep=" ")
  bamboot1=paste(process.dir1,'/',basename(filename1),'.exe',sep='')
  file.copy(bamsource1, bamboot1, overwrite=TRUE)
  #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamrun1=paste(basename(filename1),'.exe',sep='')
  run.command1=paste(bamrun1, admb.switch, '-ind', filename.dat1, sep=" ")
  shell(run.command1) #start admb code
  
  
  #dev.off()
  
  
  
  #unlink(process.dir1, recursive=T)
  
#}





















# FAA Model 2 ---------------------


  #
  #
  # Fleets-as-areas Model, 2
  #
  #


  filename.dat2=paste(filename2,'.dat',sep='')	
  
  
  setwd(process.dir2)

  
  # filling in the data ------------------------------------------------
  
  #fill in starting parameters
  {  #start filling in dataset
    dat.tmp.2[16]<-as.character(paste(start_log_R_faa,collapse="\t")) #starting parameter for log_recruitment
    dat.tmp.2[17]<-as.character(paste(start_log_F_faa,collapse="\t")) #starting parameters for F in both regions
    dat.tmp.2[18]<-as.character(paste(start_log_Feq_faa,collapse="\t")) #starting parameters for log_Feq
    
    dat.tmp.2[19]<-as.character(paste(rep(start_fsel_ac_a,1),collapse="\t")) #start log_sf1_ac
    dat.tmp.2[20]<-as.character(paste(rep(start_fsel_ac_b,1),collapse="\t")) #start log_sf2_ac
    dat.tmp.2[21]<-as.character(paste(rep(start_fsel_cb_a,1),collapse="\t")) #start log_sf1_cb
    dat.tmp.2[22]<-as.character(paste(rep(start_fsel_cb_b,1),collapse="\t")) #start log_sf2_cb
    dat.tmp.2[23]<-as.character(paste(rep(start_fsel_cb_c,1),collapse="\t")) #start log_sf3_cb
    dat.tmp.2[24]<-as.character(paste(rep(start_fsel_cb_d,1),collapse="\t")) #start log_sf4_cb
    
    dat.tmp.2[25]<-as.character(paste(rep(start_ssel_ac_a,1),collapse="\t")) #start log_ssf1_ac
    dat.tmp.2[26]<-as.character(paste(rep(start_ssel_ac_b,1),collapse="\t")) #start log_ssf2_ac
    dat.tmp.2[27]<-as.character(paste(rep(start_ssel_ac_c,1),collapse="\t")) #start log_ssf2_ac
    dat.tmp.2[28]<-as.character(paste(rep(start_ssel_ac_d,1),collapse="\t")) #start log_ssf2_ac
    
    dat.tmp.2[29]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
    dat.tmp.2[30]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
    dat.tmp.2[31]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
    
    dat.tmp.2[32]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
    dat.tmp.2[33]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
    
    #fill in total catch
    dat.tmp.2[34]<-as.character(paste(dat_faa_cat[1,],collapse="\t")) #line 23 is where total catch starts, timestep 1, region1
    dat.tmp.2[35]<-as.character(paste(dat_faa_cat[2,],collapse="\t")) #line 24, time step 2, region 1
    
    #fill in catch PAA
    linenum=36 #starting line number of comp matrix
    for(r in 1:region){
      for(y in 1:mnyrs){
        dat.tmp.2[linenum]<-as.character(paste(dat_faa_prop_age[r,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
        linenum=linenum+1
      } #close year looop
    }# close region loop
    #rowSums(dat_faa_prop_age[1,,])
    
    #fill in catch CV
    #cat_cv<-rep(0.2,mnyrs)
    dat.tmp.2[96]<-as.character(paste(cat_cv,collapse="\t")) #line 96 is where cv for fisheries starts
    dat.tmp.2[97]<-as.character(paste(cat_cv,collapse="\t")) #fleet 2
    
    
    # fill in survey data
    linenum=98
    for(n in 1:nsurv){
      dat.tmp.2[linenum]<-as.character(paste(dat_faa_tot_ioa[n,],collapse="\t")) #using the same survey info as SE model, just assuming it represents annual trends
      linenum=linenum+1
    }
    
    #fill in survey PAA
    
    linenum=102  #starting line number of age comp matrix for surveys
    for(n in 1:nsurv){
      for(y in 1:mnyrs){
        dat.tmp.2[linenum]<-as.character(paste(dat_faa_ioa_prop_age[n,y,],collapse="\t"))
        linenum=linenum+1
      } #close year looop
    }# close survey loop
    #rowSums(dat_ioa_prop_age_fleet[4,,])
    #dat_ioa_prop_age_reg[2,2,1,];ioa_prop_age_reg[2,2,1,]
    
    #fill in catch CV
    #ioa_cv<-rep(0.1,mnyrs)
    dat.tmp.2[222]<-as.character(paste(ioa_cv_1,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.2[223]<-as.character(paste(ioa_cv_1,collapse="\t")) #region 1, timestep 2
    dat.tmp.2[224]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 1
    dat.tmp.2[225]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 2
    
    
    # fill in age 1 IOA and CV
    dat.tmp.2[226]<-as.character(paste(dat_ioa_age1_faa[1,],collapse="\t")) #using same surveys as SE, just assuming they represent annual trends
    dat.tmp.2[227]<-as.character(paste(dat_ioa_age1_faa[2,],collapse="\t"))
    #pasted as CB age 1 survey, AC age 1 survey
    #age1_cv<-rep(0.1,mnyrs)
    dat.tmp.2[228]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.2[229]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
    
    dat.tmp.2[230]<-as.character(paste(dat_ioa_yoy_faa[1,],collapse="\t")) #using same surveys as SE
    dat.tmp.2[231]<-as.character(paste(dat_ioa_yoy_faa[2,],collapse="\t"))
    
    #yoy_cv<-rep(0.1,mnyrs)
    dat.tmp.2[232]<-as.character(paste(yoy_cv,collapse="\t")) #l
    dat.tmp.2[233]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
    
    dat.tmp.2[341]<-as.character(paste(use_age_err_yes,collpase="\t"))
    
    #fill in EFFECTIVE SAMPLE SIZE
    dat.tmp.2[372]<-as.character(paste(ESS_F_faa_dat[1],collapse="\t")) #
    dat.tmp.2[373]<-as.character(paste(ESS_F_faa_dat[2],collapse="\t")) #ESS for region 2 fishery (coast), space in dat file, can't get rid of so line nums are not in sync
    dat.tmp.2[374]<-as.character(paste(ESS_S_faa_dat[1],collapse="\t")) #ESS for survey in region 1 (bay)
    dat.tmp.2[375]<-as.character(paste(ESS_S_faa_dat[2],collapse="\t")) #ESS for survey in region 2 (bay)
    dat.tmp.2[376]<-as.character(paste(ESS_S_faa_dat[3],collapse="\t")) #ESS for survey in region 2 (bay)
    dat.tmp.2[377]<-as.character(paste(ESS_S_faa_dat[4],collapse="\t")) #ESS for survey in region 2 (bay)
    
    
    dat.tmp.2[378]<-as.character(paste(iboot),collapse='\t') #simulation number
    #dat.tmp.2[460]<-as.character(paste(1,collapse='\t')) #simulation number
    
    #inputting true values
    dat.tmp.2[379]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.2[380]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
    dat.tmp.2[381]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
    #dat.tmp.2[382]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
    dat.tmp.2[382]<-as.character(paste(true_F_spr_faa,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
    linenum=383
    for(y in 1:mnyrs){
      dat.tmp.2[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
      linenum=linenum+1
    }    
 
    #dat.tmp.1[383]<-as.character(paste('12345',collapse="\t")) # test number
  } #stop editing dataset
  
  
  # Run ADMB ----------------------------------------------------------------
  
  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  write(file=filename.dat2, dat.tmp.2)
  run.command2=paste(filename2, admb.switch, '-ind', filename.dat2, sep=" ")
  #bamboot=paste(process.dir,'/',basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamboot2=paste(process.dir2,'/',basename(filename2),'.exe',sep='')
  file.copy(bamsource2, bamboot2, overwrite=TRUE)
  #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamrun2=paste(basename(filename2),'.exe',sep='')
  run.command2=paste(bamrun2, admb.switch, '-ind', filename.dat2, sep=" ")
  shell(run.command2) #start admb code
  
  
  #dev.off()
  
 
  
  #unlink(process.dir2, recursive=T)
  
  #}

















# SE Model 3 ---------------------

# #
# Spatially Explicit Model, #3 - SE with SCALE AGES
# #


  filename.dat3=paste(filename3,'.dat',sep='')	
  
  
  setwd(process.dir3)
  
  
  # filling in the data ------------------------------------------------
  
  #fill in starting parameters
  {  #start filling in dataset
    dat.tmp.3[21]<-as.character(paste(start_log_R0,collapse="\t")) #starting parameter for log_recruitment
    dat.tmp.3[22]<-as.character(paste(start_F_cb,collapse="\t")) #starting parameters for F in both regions
    dat.tmp.3[23]<-as.character(paste(start_F_ac,collapse="\t"))#starting parameters for F in both regions
    dat.tmp.3[24]<-as.character(paste(start_log_F0,collapse="\t")) #starting parameters for log_Feq
    
    dat.tmp.3[25]<-as.character(paste(rep(start_fsel_ac_a,2),collapse="\t")) #start log_sf1_ac
    dat.tmp.3[26]<-as.character(paste(rep(start_fsel_ac_b,2),collapse="\t")) #start log_sf2_ac
    dat.tmp.3[27]<-as.character(paste(rep(start_fsel_cb_a,2),collapse="\t")) #start log_sf1_cb
    dat.tmp.3[28]<-as.character(paste(rep(start_fsel_cb_b,2),collapse="\t")) #start log_sf2_cb
    dat.tmp.3[29]<-as.character(paste(rep(start_fsel_cb_c,2),collapse="\t")) #start log_sf3_cb
    dat.tmp.3[30]<-as.character(paste(rep(start_fsel_cb_d,2),collapse="\t")) #start log_sf4_cb
    
    dat.tmp.3[31]<-as.character(paste(rep(start_ssel_ac_a,2),collapse="\t")) #start log_ssf1_ac
    dat.tmp.3[32]<-as.character(paste(rep(start_ssel_ac_b,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.3[33]<-as.character(paste(rep(start_ssel_ac_c,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.3[34]<-as.character(paste(rep(start_ssel_ac_d,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.3[35]<-as.character(paste(rep(start_ssel_cb_a,2),collapse="\t")) #start log_ssf1_cb
    dat.tmp.3[36]<-as.character(paste(rep(start_ssel_cb_b,2),collapse="\t")) #start log_ssf2_cb
    
    dat.tmp.3[37]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
    dat.tmp.3[38]<- as.character(paste(log_q,collapse="\t"))#start_log_q_bay
    dat.tmp.3[39]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
    dat.tmp.3[40]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_bay
    dat.tmp.3[41]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_coast
    dat.tmp.3[42]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
    
    dat.tmp.3[43]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
    dat.tmp.3[44]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
    
    
    #fill in total catch
    dat.tmp.3[45]<-as.character(paste(dat_region_cat[1,1,],collapse="\t")) #line 23 is where total catch starts, timestep 1, region1
    dat.tmp.3[46]<-as.character(paste(dat_region_cat[1,2,],collapse="\t")) #line 24, time step 2, region 1
    dat.tmp.3[47]<-as.character(paste(dat_region_cat[2,1,],collapse="\t")) #line 27, timestep 1, region 2
    dat.tmp.3[48]<-as.character(paste(dat_region_cat[2,2,],collapse="\t")) #line 29, timestep 2, region 2
    
    
    #fill in catch PAA
    linenum=49  #starting line number of comp matrix
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          #dat_cat_prop_age_err[r,ts,y,]<-dat_cat_prop_age_err[r,ts,y,]/sum(dat_cat_prop_age_err[r,ts,y,]) #with aging error
          dat.tmp.3[linenum]<-as.character(paste(dat_region_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
          linenum=linenum+1
          #print(dat_region_prop_age[r,ts,y,])
        } #close year looop
      } #close tstep loop
    }# close region loop
    #cat_prop_age_reg[2,1,,]
    

    #fill in catch CV
    #cat_cv<-rep(0.2,mnyrs)
    dat.tmp.3[169]<-as.character(paste(cat_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.3[170]<-as.character(paste(cat_cv,collapse="\t")) #region 1, timestep 2
    dat.tmp.3[171]<-as.character(paste(cat_cv,collapse="\t")) #region 2, timestep 1
    dat.tmp.3[172]<-as.character(paste(cat_cv,collapse="\t")) #region 2, timestep 2
    
    
    # fill in survey data
    linenum=173  #starting line number of index of abundance
    for(r in 1:region){
      for(ts in 1:tstep){
        #print(tot_ioa_reg[r,ts,])
        dat.tmp.3[linenum]<-as.character(paste(dat_region_tot_ioa[r,ts,],collapse="\t"))
        linenum=linenum+1
      } #close tstep loop
    }# close region loop
    
    
    
    #fill in survey PAA

    linenum=177 #starting line number of comp matrix
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          #dat_ioa_prop_age_err[r,ts,y,]<-dat_ioa_prop_age_err[r,ts,y,]/sum(dat_ioa_prop_age_err[r,ts,y,]) #with aging error
          dat.tmp.3[linenum]<-as.character(paste(dat_region_ioa_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
          linenum=linenum+1
        } #close year looop
      } #close tstep loop
    }# close region loop
    #cat_prop_age_reg[2,1,,]
    
    #fill in survey CV
    #ioa_cv<-rep(0.1,mnyrs)
    dat.tmp.3[297]<-as.character(paste(ioa_cv_1,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.3[298]<-as.character(paste(ioa_cv_1,collapse="\t")) #region 1, timestep 2
    dat.tmp.3[299]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 1
    dat.tmp.3[300]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 2
    
    
    # fill in age 1 IOA and CV
    linenum=301 #starting line number of age comp matrix for surveys
    for(r in 1:region){
      dat.tmp.3[linenum]<-as.character(paste(dat_ioa_age1_region[r,],collapse="\t"))
      linenum=linenum+1
    }# close region loop
    #age1_cv<-rep(0.1,mnyrs)
    dat.tmp.3[303]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.3[304]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
    
    linenum=305
    for(r in 1:region){
      dat.tmp.3[linenum]<-as.character(paste(dat_ioa_yoy_region[r,],collapse="\t"))
      linenum=linenum+1
    }# close region loop
    
    #yoy_cv<-rep(0.1,mnyrs)
    dat.tmp.3[307]<-as.character(paste(yoy_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.3[308]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
    
    #fill in occupancy probabilities
    linenum=416 #where occupancy probabilities start
    for(s in 1:stock){
      for(t in 1:tstep){
        dat.tmp.3[linenum]<-as.character(paste(log_occ_prob_dat[s,t,2:lage],collapse="\t")) #model inputs for the probability in the AC region
        #print(dat.tmp[linenum])
        linenum=linenum+1
      }#close timestep loop
    }#close stock loop

    #sd for occupancy probability, needs to be edited, not sure how to do this
    linenum=420
    for(s in 1:stock){
      for(t in 1:tstep){
        dat.tmp.3[linenum]<-as.character(paste(occ_prob_sd[s,t,2:lage],collapse="\t"))
        linenum=linenum+1
      }#close timestep loop
    }#close stock loop
    
    dat.tmp.3[424]<-as.character(paste(use_age_err_no,collapse="\t"))
    
    #fill in EFFECTIVE SAMPLE SIZE
    dat.tmp.3[455]<-as.character(paste(ESS_F_se_dat[1,],collapse="\t")) #line 431 starts ess. This is EFF for region 1 fishery (bay) 
    dat.tmp.3[456]<-as.character(paste(ESS_F_se_dat[2,],collapse="\t")) #ESS for region 2 fishery (coast)
    dat.tmp.3[457]<-as.character(paste(ESS_S_se_dat[1,1],collapse="\t")) #ESS for survey in region 1 (bay) in timestep 1
    dat.tmp.3[458]<-as.character(paste(ESS_S_se_dat[1,2],collapse="\t")) #ESS for survey in region 1 (bay) n timestep 2
    dat.tmp.3[459]<-as.character(paste(ESS_S_se_dat[2,1],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
    dat.tmp.3[460]<-as.character(paste(ESS_S_se_dat[2,2],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
    
    #fill in likelihood switches
    dat.tmp.3[461]<-as.character(paste(switch_pen_prop_on,collapse="\t")) #if swtich = 0, will not use penalty in likelihood that quantifies the proportion of each stock in each region based on literature
    # if switch = 1, will use the penalty in likelihood (kneebone et al. 2012)
    
    
    dat.tmp.3[462]<-as.character(paste(iboot),collapse='\t') #simulation number
    #dat.tmp[460]<-as.character(paste(1,collapse='\t')) #simulation number
    
    #inputting true values
    dat.tmp.3[463]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.3[464]<-as.character(paste(true_Ntot_stock[1,],collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.3[465]<-as.character(paste(true_Ntot_stock[2,],collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.3[466]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
    dat.tmp.3[467]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
    #dat.tmp.3[464]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
    dat.tmp.3[468]<-as.character(paste(true_spr,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
    dat.tmp.3[469]<-as.character(paste(true_F_spr_faa,collapse="\t")) 
    
    linenum=470
    for(y in 1:mnyrs){
      dat.tmp.3[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
      linenum=linenum+1
    }
    
    dat.tmp.3[500]<-as.character(paste('12345',collapse="\t")) #this is true_f (total f for each stock)
    
  } #stop editing dataset
  
  
  
  
  # Run ADMB ----------------------------------------------------------------
  
  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  write(file=filename.dat3, dat.tmp.3)
  run.command3=paste(filename3, admb.switch, '-ind', filename.dat3, sep=" ")
  #bamboot=paste(process.dir,'/',basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamboot3=paste(process.dir3,'/',basename(filename3),'.exe',sep='')
  file.copy(bamsource3, bamboot3, overwrite=TRUE)
  #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamrun3=paste(basename(filename3),'.exe',sep='')
  run.command3=paste(bamrun3, admb.switch, '-ind', filename.dat3, sep=" ")
  shell(run.command3) #start admb code
  
  
  #dev.off()
  
  #}
  
  #unlink(process.dir3, recursive=T)
  
#} #end nboot






















# SE Model 4 ---------------------



  # #
  # Spatially Explicit Model, 4
  # #

  filename.dat4=paste(filename4,'.dat',sep='')	
  
  
  setwd(process.dir4)


  # filling in the data ------------------------------------------------
  
  #fill in starting parameters
  {  #start filling in dataset
    dat.tmp.4[21]<-as.character(paste(start_log_R0,collapse="\t")) #starting parameter for log_recruitment
    dat.tmp.4[22]<-as.character(paste(start_F_cb,collapse="\t")) #starting parameters for F in both regions
    dat.tmp.4[23]<-as.character(paste(start_F_ac,collapse="\t"))#starting parameters for F in both regions
    dat.tmp.4[24]<-as.character(paste(start_log_F0,collapse="\t")) #starting parameters for log_Feq
    
    dat.tmp.4[25]<-as.character(paste(rep(start_fsel_ac_a,2),collapse="\t")) #start log_sf1_ac
    dat.tmp.4[26]<-as.character(paste(rep(start_fsel_ac_b,2),collapse="\t")) #start log_sf2_ac
    dat.tmp.4[27]<-as.character(paste(rep(start_fsel_cb_a,2),collapse="\t")) #start log_sf1_cb
    dat.tmp.4[28]<-as.character(paste(rep(start_fsel_cb_b,2),collapse="\t")) #start log_sf2_cb
    dat.tmp.4[29]<-as.character(paste(rep(start_fsel_cb_c,2),collapse="\t")) #start log_sf3_cb
    dat.tmp.4[30]<-as.character(paste(rep(start_fsel_cb_d,2),collapse="\t")) #start log_sf4_cb
    
    dat.tmp.4[31]<-as.character(paste(rep(start_ssel_ac_a,2),collapse="\t")) #start log_ssf1_ac
    dat.tmp.4[32]<-as.character(paste(rep(start_ssel_ac_b,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.4[33]<-as.character(paste(rep(start_ssel_ac_c,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.4[34]<-as.character(paste(rep(start_ssel_ac_d,2),collapse="\t")) #start log_ssf2_ac
    dat.tmp.4[35]<-as.character(paste(rep(start_ssel_cb_a,2),collapse="\t")) #start log_ssf1_cb
    dat.tmp.4[36]<-as.character(paste(rep(start_ssel_cb_b,2),collapse="\t")) #start log_ssf2_cb
    
    dat.tmp.4[37]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
    dat.tmp.4[38]<- as.character(paste(log_q,collapse="\t"))#start_log_q_bay
    dat.tmp.4[39]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
    dat.tmp.4[40]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_bay
    dat.tmp.4[41]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_coast
    dat.tmp.4[42]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
    
    dat.tmp.4[43]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
    dat.tmp.4[44]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
    
    
    #fill in total catch
    dat.tmp.4[45]<-as.character(paste(dat_region_cat[1,1,],collapse="\t")) #line 23 is where total catch starts, timestep 1, region1
    dat.tmp.4[46]<-as.character(paste(dat_region_cat[1,2,],collapse="\t")) #line 24, time step 2, region 1
    dat.tmp.4[47]<-as.character(paste(dat_region_cat[2,1,],collapse="\t")) #line 27, timestep 1, region 2
    dat.tmp.4[48]<-as.character(paste(dat_region_cat[2,2,],collapse="\t")) #line 29, timestep 2, region 2
    
    
    #fill in catch PAA
    linenum=49  #starting line number of comp matrix
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          #dat_cat_prop_age_err[r,ts,y,]<-dat_cat_prop_age_err[r,ts,y,]/sum(dat_cat_prop_age_err[r,ts,y,]) #with aging error
          dat.tmp.4[linenum]<-as.character(paste(dat_region_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
          linenum=linenum+1
        } #close year looop
      } #close tstep loop
    }# close region loop
    #cat_prop_age_reg[2,1,,]
    
    
    #fill in catch CV
    #cat_cv<-rep(0.2,mnyrs)
    dat.tmp.4[169]<-as.character(paste(cat_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.4[170]<-as.character(paste(cat_cv,collapse="\t")) #region 1, timestep 2
    dat.tmp.4[171]<-as.character(paste(cat_cv,collapse="\t")) #region 2, timestep 1
    dat.tmp.4[172]<-as.character(paste(cat_cv,collapse="\t")) #region 2, timestep 2
    
    
    # fill in survey data
    linenum=173  #starting line number of index of abundance
    for(r in 1:region){
      for(ts in 1:tstep){
        #print(tot_ioa_reg[r,ts,])
        dat.tmp.4[linenum]<-as.character(paste(dat_region_tot_ioa[r,ts,],collapse="\t"))
        linenum=linenum+1
      } #close tstep loop
    }# close region loop
    
    
    
    #fill in survey PAA
    
    linenum=177 #starting line number of comp matrix
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 1:mnyrs){
          #dat_ioa_prop_age_err[r,ts,y,]<-dat_ioa_prop_age_err[r,ts,y,]/sum(dat_ioa_prop_age_err[r,ts,y,]) #with aging error
          dat.tmp.4[linenum]<-as.character(paste(dat_region_ioa_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
          linenum=linenum+1
        } #close year looop
      } #close tstep loop
    }# close region loop
    #cat_prop_age_reg[2,1,,]
    
    #fill in catch CV
    #ioa_cv<-rep(0.1,mnyrs)
    dat.tmp.4[297]<-as.character(paste(ioa_cv_1,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.4[298]<-as.character(paste(ioa_cv_1,collapse="\t")) #region 1, timestep 2
    dat.tmp.4[299]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 1
    dat.tmp.4[300]<-as.character(paste(ioa_cv_2,collapse="\t")) #region 2, timestep 2
    
    
    # fill in age 1 IOA and CV
    linenum=301 #starting line number of age comp matrix for surveys
    for(r in 1:region){
      dat.tmp.4[linenum]<-as.character(paste(dat_ioa_age1_region[r,],collapse="\t"))
      linenum=linenum+1
    }# close region loop
    #age1_cv<-rep(0.1,mnyrs)
    dat.tmp.4[303]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.4[304]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
    
    linenum=305
    for(r in 1:region){
      dat.tmp.4[linenum]<-as.character(paste(dat_ioa_yoy_region[r,],collapse="\t"))
      linenum=linenum+1
    }# close region loop
    
    #yoy_cv<-rep(0.1,mnyrs)
    dat.tmp.4[307]<-as.character(paste(yoy_cv,collapse="\t")) #line 148 is where cv for fisheries starts
    dat.tmp.4[308]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
    
    #fill in occupancy probabilities
    linenum=416 #where occupancy probabilities start
    for(s in 1:stock){
      for(t in 1:tstep){
        dat.tmp.4[linenum]<-as.character(paste(log_occ_prob_dat[s,t,2:lage],collapse="\t")) #model inputs for the probability in the AC region
        #print(dat.tmp[linenum])
        linenum=linenum+1
      }#close timestep loop
    }#close stock loop
    
    #sd for occupancy probability, needs to be edited, not sure how to do this
    linenum=420
    for(s in 1:stock){
      for(t in 1:tstep){
        dat.tmp.4[linenum]<-as.character(paste(occ_prob_sd[s,t,2:lage],collapse="\t"))
        linenum=linenum+1
      }#close timestep loop
    }#close stock loop
    
    dat.tmp.4[424]<-as.character(paste(use_age_err_yes,collapse="\t"))
    
    #fill in EFFECTIVE SAMPLE SIZE
    dat.tmp.4[455]<-as.character(paste(ESS_F_se_dat[1,],collapse="\t")) #line 431 starts ess. This is EFF for region 1 fishery (bay) 
    dat.tmp.4[456]<-as.character(paste(ESS_F_se_dat[2,],collapse="\t")) #ESS for region 2 fishery (coast)
    dat.tmp.4[457]<-as.character(paste(ESS_S_se_dat[1,1],collapse="\t")) #ESS for survey in region 1 (bay) in timestep 1
    dat.tmp.4[458]<-as.character(paste(ESS_S_se_dat[1,2],collapse="\t")) #ESS for survey in region 1 (bay) n timestep 2
    dat.tmp.4[459]<-as.character(paste(ESS_S_se_dat[2,1],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
    dat.tmp.4[460]<-as.character(paste(ESS_S_se_dat[2,2],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
    
    #fill in likelihood switches
    dat.tmp.4[461]<-as.character(paste(switch_pen_prop_on,collapse="\t")) #if swtich = 0, will not use penalty in likelihood that quantifies the proportion of each stock in each region based on literature
    # if switch = 1, will use the penalty in likelihood (kneebone et al. 2012)
    
    
    dat.tmp.4[462]<-as.character(paste(iboot),collapse='\t') #simulation number
    #dat.tmp[460]<-as.character(paste(1,collapse='\t')) #simulation number
    
    #inputting true values
    dat.tmp.4[463]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.4[464]<-as.character(paste(true_Ntot_stock[1,],collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.4[465]<-as.character(paste(true_Ntot_stock[2,],collapse="\t")) #this is true_ntot (total N for each stock)
    dat.tmp.4[466]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
    dat.tmp.4[467]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
    #dat.tmp.4[464]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
    dat.tmp.4[468]<-as.character(paste(true_spr,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
    dat.tmp.4[469]<-as.character(paste(true_F_spr_faa,collapse="\t")) 
    
    linenum=470
    for(y in 1:mnyrs){
      dat.tmp.4[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
      linenum=linenum+1
    }    
    
    dat.tmp.4[500]<-as.character(paste('12345',collapse="\t")) #this is true_f (total f for each stock)
    
  } #stop editing dataset  

  
  # Run ADMB ----------------------------------------------------------------

  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  write(file=filename.dat4, dat.tmp.4)
  run.command4=paste(filename4, admb.switch, '-ind', filename.dat4, sep=" ")
  #bamboot=paste(process.dir,'/',basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamboot4=paste(process.dir4,'/',basename(filename4),'.exe',sep='')
  file.copy(bamsource4, bamboot4, overwrite=TRUE)
  #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamrun4=paste(basename(filename4),'.exe',sep='')
  run.command4=paste(bamrun4, admb.switch, '-ind', filename.dat4, sep=" ")
  shell(run.command4) #start admb code
  
  
  #dev.off()

#}

  #unlink(process.dir, recursive=T)
  
  #} #end nboot


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # SE Model 5 ---------------------
  
  
  # 
  # # #
  # # Spatially Explicit Model, 5
  # # #
  # 
  # filename.dat5=paste(filename5,'.dat',sep='')
  # 
  # 
  # setwd(process.dir5)
  # 
  # 
  # # filling in the data ------------------------------------------------
  # 
  # #fill in starting parameters
  # {  #start filling in dataset
  #   
  #   #current first year with stock data is year 1, and year 30 for the fim surveys
  #   dat.tmp.5[3]<-as.character(paste(1,collapse="\t"))#start dat year for data with stock data (i.e. last 10)
  #   dat.tmp.5[4]<-as.character(paste(30,collapse="\t"))#start dat year for data with stock data (i.e. last 10)
  #   
  #   dat.tmp.5[23]<-as.character(paste(start_log_R0,collapse="\t")) #starting parameter for log_recruitment
  #   dat.tmp.5[24]<-as.character(paste(start_F_cb,collapse="\t")) #starting parameters for F in both regions
  #   dat.tmp.5[25]<-as.character(paste(start_F_ac,collapse="\t"))#starting parameters for F in both regions
  #   dat.tmp.5[26]<-as.character(paste(start_log_F0,collapse="\t")) #starting parameters for log_Feq
  #   
  #   dat.tmp.5[27]<-as.character(paste(rep(start_fsel_ac_a,2),collapse="\t")) #start log_sf1_ac
  #   dat.tmp.5[28]<-as.character(paste(rep(start_fsel_ac_b,2),collapse="\t")) #start log_sf2_ac
  #   dat.tmp.5[29]<-as.character(paste(rep(start_fsel_cb_a,2),collapse="\t")) #start log_sf1_cb
  #   dat.tmp.5[30]<-as.character(paste(rep(start_fsel_cb_b,2),collapse="\t")) #start log_sf2_cb
  #   dat.tmp.5[31]<-as.character(paste(rep(start_fsel_cb_c,2),collapse="\t")) #start log_sf3_cb
  #   dat.tmp.5[32]<-as.character(paste(rep(start_fsel_cb_d,2),collapse="\t")) #start log_sf4_cb
  #   
  #   
  #   dat.tmp.5[33]<-as.character(paste(rep(start_ssel_ac_a,2),collapse="\t")) #start log_ssf1_ac
  #   dat.tmp.5[34]<-as.character(paste(rep(start_ssel_ac_b,2),collapse="\t")) #start log_ssf2_ac
  #   dat.tmp.5[35]<-as.character(paste(rep(start_ssel_ac_c,2),collapse="\t")) #start log_ssf2_ac
  #   dat.tmp.5[36]<-as.character(paste(rep(start_ssel_ac_d,2),collapse="\t")) #start log_ssf2_ac
  #   dat.tmp.5[37]<-as.character(paste(rep(start_ssel_cb_a,2),collapse="\t")) #start log_ssf1_cb
  #   dat.tmp.5[38]<-as.character(paste(rep(start_ssel_cb_b,2),collapse="\t")) #start log_ssf2_cb
  #   
  #   dat.tmp.5[39]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
  #   dat.tmp.5[40]<- as.character(paste(log_q,collapse="\t"))#start_log_q_bay
  #   dat.tmp.5[41]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
  #   dat.tmp.5[42]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_bay
  #   dat.tmp.5[43]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_coast
  #   dat.tmp.5[44]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
  #   
  #   dat.tmp.5[45]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
  #   dat.tmp.5[46]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
  #   
  #   
  #   #fill in total catch
  #   linenum=47
  #   for(s in 1:stock){
  #     for(r in 1:region){
  #       for(ts in 1:tstep){
  #         dat.tmp.5[linenum]<-as.character(paste(dat_stock_cat[s,r,ts,],collapse="\t")) 
  #         linenum<-linenum+1
  #       }
  #     }
  #   }
  #   #dat.tmp.5[47]<-as.character(paste(totcat_stock[1,1,1,]*exp(cat_stock_err[1,1,1,]),collapse="\t")) # tot cat stock 1, region 1, timestep 1,
  #   #dat.tmp.5[48]<-as.character(paste(totcat_stock[1,1,2,]*exp(cat_stock_err[1,1,2,]),collapse="\t")) # tot cat stock 1, region 1, timestep 2,
  #   #dat.tmp.5[49]<-as.character(paste(totcat_stock[1,2,1,]*exp(cat_stock_err[1,2,1,]),collapse="\t")) # tot cat stock 1, region 2, timestep 1,
  #   #dat.tmp.5[50]<-as.character(paste(totcat_stock[1,2,2,]*exp(cat_stock_err[1,2,2,]),collapse="\t")) # tot cat stock 1, region 1, timestep 2,
  #   #dat.tmp.5[51]<-as.character(paste(totcat_stock[2,1,1,]*exp(cat_stock_err[2,1,1,]),collapse="\t")) # tot cat stock 2, region 1, timestep 1,
  #   ##dat.tmp.5[52]<-as.character(paste(totcat_stock[2,1,2,]*exp(cat_stock_err[2,1,2,]),collapse="\t")) # tot cat stock 2, region 1, timestep 2,
  #   #dat.tmp.5[53]<-as.character(paste(totcat_stock[2,2,1,]*exp(cat_stock_err[2,2,1,]),collapse="\t")) # tot cat stock 2, region 2, timestep 1,
  #   #dat.tmp.5[54]<-as.character(paste(totcat_stock[2,2,2,]*exp(cat_stock_err[2,2,2,]),collapse="\t")) # tot cat stock 2, region 1, timestep 2,
  #   
  # 
  #   #fill in catch PAA
  #   linenum=55  #starting line number of comp matrix
  #   for(s in 1:stock){
  #     for(r in 1:region){
  #       for(ts in 1:tstep){
  #         for(y in 1:mnyrs){
  #           #dat_cat_prop_age_stock[s,r,ts,y,]<-rmultinom(1,500,cat_prop_age_stock[s,r,ts,y,])#/totcat_reg[r,ts,y]
  #           #dat_cat_prop_age_stock[s,r,ts,y,]<-dat_cat_prop_age_stock[s,r,ts,y,]/sum(dat_cat_prop_age_stock[s,r,ts,y,])
  #           dat.tmp.5[linenum]<-as.character(paste(dat_stock_prop_age[s,r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
  #           #print(rmultinom(1,1000,cat_prop_age_reg[r,ts,y,]))
  #           linenum=linenum+1
  #         } #close year looop
  #       } #close tstep loop
  #     }# close region loop
  #     #cat_prop_age_reg[2,1,,]
  #   }
  #   
  #   #rowSums(dat_cat_prop_age_reg[2,1,,])
  #   #plot(cat_prop_age_reg[1,1,10,],pch=16,ylim=c(0,0.5))
  #   #points(dat_cat_prop_age_reg[1,1,10,],pch=16,col="red")
  #   #sum(cat_prop_age_reg[1,1,1,])
  #   #View(cat_prop_age_reg[1,1,,])
  #   
  #   #fill in catch CV
  #   #cat_cv<-rep(0.2,mnyrs)
  #   dat.tmp.5[295]<-as.character(paste(cat_cv,collapse="\t")) #stock 1line 148 is where cv for fisheries starts
  #   dat.tmp.5[296]<-as.character(paste(cat_cv,collapse="\t")) #stock 1region 1, timestep 2
  #   dat.tmp.5[297]<-as.character(paste(cat_cv,collapse="\t")) #stock 1 region 2, timestep 1
  #   dat.tmp.5[298]<-as.character(paste(cat_cv,collapse="\t")) #stock 1 region 2, timestep 2
  #   dat.tmp.5[299]<-as.character(paste(cat_cv,collapse="\t")) #stock 2 line 148 is where cv for fisheries starts
  #   dat.tmp.5[300]<-as.character(paste(cat_cv,collapse="\t")) #stock 2region 1, timestep 2
  #   dat.tmp.5[301]<-as.character(paste(cat_cv,collapse="\t")) #stock 2region 2, timestep 1
  #   dat.tmp.5[302]<-as.character(paste(cat_cv,collapse="\t")) #stock 2region 2, timestep 2
  #   
  #   
  #   # fill in survey data
  #   linenum=303  #starting line number of index of abundance
  #   for(r in 1:region){
  #     for(s in 1:stock){
  #       for(ts in 1:tstep){
  #         #print(tot_ioa_reg[r,ts,])
  #         dat.tmp.5[linenum]<-as.character(paste(dat_stock_tot_ioa[s,r,ts,],collapse="\t"))
  #         linenum=linenum+1
  #       }
  #     } #close tstep loop
  #   }# close region loop
  #   
  #   
  #   #fill in survey PAA
  #   linenum=311  #starting line number of age comp matrix for surveys
  #   for(r in 1:region){
  #     for(s in 1:stock){
  #       for(ts in 1:tstep){
  #         for(y in 1:mnyrs){
  #           #dat_ioa_prop_age_stock[s,r,ts,y,]<-rmultinom(1,500,ioa_prop_age_stock[s,r,ts,y,])#generating dataset with multinomial error from ioa proprs at age
  #           #dat_ioa_prop_age_stock[s,r,ts,y,]<-dat_ioa_prop_age_stock[s,r,ts,y,]/sum(dat_ioa_prop_age_stock[s,r,ts,y,])
  #           dat.tmp.5[linenum]<-as.character(paste(dat_stock_ioa_prop_age[s,r,ts,y,],collapse="\t"))
  #           linenum=linenum+1
  #         }
  #       } #close year looop
  #     } #close tstep loop
  #   }# close region loop
  #   #rowSums(ioa_prop_age_stock[1,1,1,1:30,])
  #   #dat_ioa_prop_age_reg[2,2,1,];ioa_prop_age_reg[2,2,1,]
  #   
  #   #fill in catch CV
  #   #ioa_cv<-rep(0.1,mnyrs)
  #   dat.tmp.5[551]<-as.character(paste(ioa_cv,collapse="\t")) #where cv for indices starts
  #   dat.tmp.5[552]<-as.character(paste(ioa_cv,collapse="\t")) #region 1, stock 1,timestep 2
  #   dat.tmp.5[553]<-as.character(paste(ioa_cv,collapse="\t")) #region 1, stock 2,timestep 1
  #   dat.tmp.5[554]<-as.character(paste(ioa_cv,collapse="\t")) #region 1, stock 2,timestep 2
  #   dat.tmp.5[555]<-as.character(paste(ioa_cv,collapse="\t")) #region 2, stock 1, timestep 1,
  #   dat.tmp.5[556]<-as.character(paste(ioa_cv,collapse="\t")) #region 2, stock 2, timestep 2
  #   dat.tmp.5[557]<-as.character(paste(ioa_cv,collapse="\t")) #region 2,stock 1, timestep 1
  #   dat.tmp.5[558]<-as.character(paste(ioa_cv,collapse="\t")) #region 2, stock 1, timestep 2
  #   
  #   
  #   # fill in age 1 IOA and CV
  #   linenum=559 #starting line number of age comp matrix for surveys
  #   for(r in 1:region){
  #     dat.tmp.5[linenum]<-as.character(paste(dat_ioa_age1_region[r,],collapse="\t")) #using region here because age 1 only has one stock in each region
  #     linenum=linenum+1
  #   }# close region loop
  #   #age1_cv<-rep(0.1,mnyrs)
  #   dat.tmp.5[561]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
  #   dat.tmp.5[562]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
  #   
  #   linenum=563
  #   for(r in 1:region){
  #     dat.tmp.5[linenum]<-as.character(paste(dat_ioa_yoy_region[r,],collapse="\t")) #sing region here because recruitment only happens to one stock in each reigon
  #     linenum=linenum+1
  #   }# close region loop
  #   
  #   #yoy_cv<-rep(0.1,mnyrs)
  #   dat.tmp.5[565]<-as.character(paste(yoy_cv,collapse="\t")) #line 148 is where cv for fisheries starts
  #   dat.tmp.5[566]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
  #   
  #   #fill in occupancy probabilities
  #   linenum=674 #where occupancy probabilities start
  #   for(s in 1:stock){
  #     for(t in 1:tstep){
  #       dat.tmp.5[linenum]<-as.character(paste(log_occ_prob_dat[s,t,2:lage],collapse="\t")) #model inputs for the probability in the AC region
  #       #print(dat.tmp[linenum])
  #       linenum=linenum+1
  #     }#close timestep loop
  #   }#close stock loop
  #   #log_occ_prob_dat[1,1,2:lage]
  #   #dat.tmp[411]
  #   
  #   #sd for occupancy probability, needs to be edited, not sure how to do this
  #   linenum=678
  #   for(s in 1:stock){
  #     for(t in 1:tstep){
  #       dat.tmp.5[linenum]<-as.character(paste(occ_prob_sd[s,t,2:lage],collapse="\t"))
  #       linenum=linenum+1
  #     }#close timestep loop
  #   }#close stock loop
  #   
  #   dat.tmp.5[682]<-as.character(paste(use_age_err_yes,collapse="\t"))
  #   
  #   ESS_F<-c(500,500)
  #   ESS_C<-c(500,500)
  #   #fill in EFFECTIVE SAMPLE SIZE
  #   dat.tmp.5[713]<-as.character(paste(ESS_F[1],collapse="\t")) #line 431 starts ess. This is EFF for region 1 fishery (bay) 
  #   dat.tmp.5[714]<-as.character(paste(ESS_F[2],collapse="\t")) #ESS for region 2 fishery (coast)
  #   dat.tmp.5[715]<-as.character(paste(ESS_S[1],collapse="\t")) #ESS for survey in region 1 (bay)
  #   dat.tmp.5[716]<-as.character(paste(ESS_S[2],collapse="\t")) #ESS for survey in region 2 (bay)
  #   
  #   #fill in likelihood switches
  #   dat.tmp.5[717]<-as.character(paste(switch_pen_prop,collapse="\t")) #if swtich = 0, will not use penalty in likelihood that quantifies the proportion of each stock in each region based on literature
  #   # if switch = 1, will use the penalty in likelihood (kneebone et al. 2012)
  #   
  #   
  #   dat.tmp.5[718]<-as.character(paste(iboot),collapse='\t') #simulation number
  #   #dat.tmp.5[718]<-as.character(paste(1,collapse='\t')) #simulation number
  #   
  #   #inputting true values
  #   dat.tmp.5[719]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
  #   dat.tmp.5[720]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
  #   dat.tmp.5[721]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
  #   #dat.tmp.5[722]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
  #   
  #   linenum=722
  #   for(y in 1:mnyrs){
  #     dat.tmp.5[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
  #     linenum=linenum+1
  #   }
  #   
  # } #stop editing dataset
  
  # Run ADMB ----------------------------------------------------------------
  
  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  # write(file=filename.dat5, dat.tmp.5)
  # run.command5=paste(filename5, admb.switch, '-ind', filename.dat5, sep=" ")
  # #bamboot=paste(process.dir,'/',basename(filename),'-',as.character(iboot),'.exe',sep='')
  # bamboot5=paste(process.dir5,'/',basename(filename5),'.exe',sep='')
  # file.copy(bamsource5, bamboot5, overwrite=TRUE)
  # #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  # bamrun5=paste(basename(filename5),'.exe',sep='')
  # run.command5=paste(bamrun5, admb.switch, '-ind', filename.dat5, sep=" ")
  # shell(run.command5) #start admb code
  
  
  #dev.off()
  
  #}
  
  #unlink(process.dir, recursive=T)
  
  
  
  
  
  
  
  
  # SE Model 6 ---------------------
  
  
  # 
  # # #
  # # Spatially Explicit Model, 5
  # # #
  # 
  filename.dat6=paste(filename6,'.dat',sep='')
  
  
  setwd(process.dir6)
  
  
  
  # filling in the data ------------------------------------------------

#fill in starting parameters
{  #start filling in dataset
  
  #current first year with stock data is year 1, and year 30 for the fim surveys
  dat.tmp.6[1]<-as.character(paste(1,collapse="\t"))#start dat year for all data
  dat.tmp.6[2]<-as.character(paste(30,collapse="\t"))#last dat year for all data
  
  dat.tmp.6[3]<-as.character(paste(start_fryear,collapse="\t"))#start dat year for data with stock data (i.e. first 20)
  dat.tmp.6[4]<-as.character(paste(start_lryear,collapse="\t"))#start dat year for data with stock data (i.e. first 20)
  
  dat.tmp.6[5]<-as.character(paste(start_fsyear,collapse="\t"))#start dat year for data with stock data (i.e. last 10)
  dat.tmp.6[6]<-as.character(paste(start_lsyear,collapse="\t"))#start dat year for data with stock data (i.e. last 10)
  
  dat.tmp.6[25]<-as.character(paste(start_log_R0,collapse="\t")) #starting parameter for log_recruitment
  dat.tmp.6[26]<-as.character(paste(start_F_cb,collapse="\t")) #starting parameters for F in both regions
  dat.tmp.6[27]<-as.character(paste(start_F_ac,collapse="\t"))#starting parameters for F in both regions
  dat.tmp.6[28]<-as.character(paste(start_log_F0,collapse="\t")) #starting parameters for log_Feq
  
  dat.tmp.6[29]<-as.character(paste(rep(start_fsel_ac_a,2),collapse="\t")) #start log_sf1_ac
  dat.tmp.6[30]<-as.character(paste(rep(start_fsel_ac_b,2),collapse="\t")) #start log_sf2_ac
  dat.tmp.6[31]<-as.character(paste(rep(start_fsel_cb_a,2),collapse="\t")) #start log_sf1_cb
  dat.tmp.6[32]<-as.character(paste(rep(start_fsel_cb_b,2),collapse="\t")) #start log_sf2_cb
  dat.tmp.6[33]<-as.character(paste(rep(start_fsel_cb_c,2),collapse="\t")) #start log_sf3_cb
  dat.tmp.6[34]<-as.character(paste(rep(start_fsel_cb_d,2),collapse="\t")) #start log_sf4_cb
  
  
  dat.tmp.6[35]<-as.character(paste(rep(start_ssel_ac_a,2),collapse="\t")) #start log_ssf1_ac
  dat.tmp.6[36]<-as.character(paste(rep(start_ssel_ac_b,2),collapse="\t")) #start log_ssf2_ac
  dat.tmp.6[37]<-as.character(paste(rep(start_ssel_ac_c,2),collapse="\t")) #start log_ssf2_ac
  dat.tmp.6[38]<-as.character(paste(rep(start_ssel_ac_d,2),collapse="\t")) #start log_ssf2_ac
  dat.tmp.6[39]<-as.character(paste(rep(start_ssel_cb_a,2),collapse="\t")) #start log_ssf1_cb
  dat.tmp.6[40]<-as.character(paste(rep(start_ssel_cb_b,2),collapse="\t")) #start log_ssf2_cb
  
  dat.tmp.6[41]<- as.character(paste(log_q,collapse="\t"))#start_log_q_coast
  dat.tmp.6[42]<- as.character(paste(log_q,collapse="\t"))#start_log_q_bay
  dat.tmp.6[43]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_cooast
  dat.tmp.6[44]<- as.character(paste(log_q_age1,collapse="\t")) #start_log_qage1_bay
  dat.tmp.6[45]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_coast
  dat.tmp.6[46]<- as.character(paste(log_q_yoy,collapse="\t")) #start_log_qyoy_bay
  
  dat.tmp.6[47]<- as.character(paste(start_log_a_sf1,collapse="\t")) #starting param for slope of afsel
  dat.tmp.6[48]<- as.character(paste(start_log_a_sf2,collapse="\t")) #starting param for age at 50%sel for afsel
  
  ### 
  #
  # Fisheries info
  #
  ###
  
  #first data frmo year 1 -20, no stock data
  
  #fill in total catch, without 
  #linenum=49
  dat.tmp.6[49]<-as.character(paste(dat_region_cat[1,1,1:20],collapse="\t")) #line 23 is where total catch starts, timestep 1, region1
  dat.tmp.6[50]<-as.character(paste(dat_region_cat[1,2,1:20],collapse="\t")) #line 24, time step 2, region 1
  dat.tmp.6[51]<-as.character(paste(dat_region_cat[2,1,1:20],collapse="\t")) #line 27, timestep 1, region 2
  dat.tmp.6[52]<-as.character(paste(dat_region_cat[2,2,1:20],collapse="\t")) #line 29, timestep 2, region 2


  #fill in catch PAA
  linenum=53  #starting line number of comp matrix
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:20){ #only for first 20 years
        dat.tmp.6[linenum]<-as.character(paste(dat_region_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
        linenum=linenum+1
      } #close year looop
    } #close tstep loop
  }# close region loop
  
  #rowSums(dat_cat_prop_age_reg[2,1,,])
  #plot(cat_prop_age_reg[1,1,10,],pch=16,ylim=c(0,0.5))
  #points(dat_cat_prop_age_reg[1,1,10,],pch=16,col="red")
  #sum(cat_prop_age_reg[1,1,1,])
  #View(cat_prop_age_reg[1,1,,])
  
  #fill in catch CV
  dat.tmp.6[133]<-as.character(paste(cat_cv[1:20],collapse="\t")) # region 1, time-step 1
  dat.tmp.6[134]<-as.character(paste(cat_cv[1:20],collapse="\t")) # #region 1, time-step 2
  dat.tmp.6[135]<-as.character(paste(cat_cv[1:20],collapse="\t")) # region 2, time-step 1
  dat.tmp.6[136]<-as.character(paste(cat_cv[1:20],collapse="\t")) # region 2, time-step 2
 
  
  
  #now fisheries data with stock data
  linenum=137
  for(s in 1:stock){
    for(r in 1:region){
      for(ts in 1:tstep){
        dat.tmp.6[linenum]<-as.character(paste(dat_stock_cat[s,r,ts,21:30],collapse="\t")) 
        linenum<-linenum+1
      }
    }
  }
  
  #fill in catch PAA
  linenum=145  #starting line number of comp matrix
  for(s in 1:stock){
    for(r in 1:region){
      for(ts in 1:tstep){
        for(y in 21:30){
          dat.tmp.6[linenum]<-as.character(paste(dat_stock_prop_age[s,r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
          linenum=linenum+1
        } #close year looop
      } #close tstep loop
    }# close region loop
    #cat_prop_age_reg[2,1,,]
  }
  
  #fill in catch CV
  dat.tmp.6[225]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 1line 148 is where cv for fisheries starts
  dat.tmp.6[226]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 1region 1, timestep 2
  dat.tmp.6[227]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 1 region 2, timestep 1
  dat.tmp.6[228]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 1 region 2, timestep 2
  dat.tmp.6[229]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 2 line 148 is where cv for fisheries starts
  dat.tmp.6[230]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 2region 1, timestep 2
  dat.tmp.6[231]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 2region 2, timestep 1
  dat.tmp.6[232]<-as.character(paste(cat_cv[21:30],collapse="\t")) #stock 2region 2, timestep 2
  
  
  
  ###
  #
  # Survey Data
  #
  ###
  
  #first, years without stock data
  linenum=233  #starting line number of index of abundance
  for(r in 1:region){
    for(ts in 1:tstep){
      #print(tot_ioa_reg[r,ts,])
      dat.tmp.6[linenum]<-as.character(paste(dat_region_tot_ioa[r,ts,1:20],collapse="\t"))
      linenum=linenum+1
    } #close tstep loop
  }# close region loop
  
  
  # fill in survey data with stock data
  linenum=237  #starting line number of index of abundance
  for(r in 1:region){
    for(s in 1:stock){
      for(ts in 1:tstep){
        #print(tot_ioa_reg[r,ts,])
        dat.tmp.6[linenum]<-as.character(paste(dat_stock_tot_ioa[s,r,ts,21:30],collapse="\t"))
        linenum=linenum+1
      }#close tstep loop
    } #close stock loop
  }# close region loop
  
  
  
  #fill in survey PAA wihtout stock data
  
  linenum=245 #starting line number of comp matrix
  for(r in 1:region){
    for(ts in 1:tstep){
      for(y in 1:20){
        dat.tmp.6[linenum]<-as.character(paste(dat_region_ioa_prop_age[r,ts,y,],collapse="\t"))# N = number of vectors you want, in this case number of ages
        linenum=linenum+1
      } #close year looop
    } #close tstep loop
  }# close region loop

  #fill in survey PAA with stock data
  
  linenum=325  #starting line number of age comp matrix for surveys
  for(r in 1:region){
    for(s in 1:stock){
      for(ts in 1:tstep){
        for(y in 21:30){
          dat.tmp.6[linenum]<-as.character(paste(dat_stock_ioa_prop_age[s,r,ts,y,],collapse="\t"))
          linenum=linenum+1
        }
      } #close year looop
    } #close tstep loop
  }# close region loop
  #rowSums(ioa_prop_age_stock[1,1,1,1:30,])
  
  
  #fill in catch CV
  dat.tmp.6[405]<-as.character(paste(ioa_cv_1[1:20],collapse="\t")) #line 148 is where cv for fisheries starts
  dat.tmp.6[406]<-as.character(paste(ioa_cv_1[1:20],collapse="\t")) #region 1, timestep 2
  dat.tmp.6[407]<-as.character(paste(ioa_cv_2[1:20],collapse="\t")) #region 2, timestep 1
  dat.tmp.6[408]<-as.character(paste(ioa_cv_2[1:20],collapse="\t")) #region 2, timestep 2
  
  

#fill in catch CV
  #ioa_cv<-rep(0.1,mnyrs)
  dat.tmp.6[409]<-as.character(paste(ioa_cv_1[21:30],collapse="\t")) #where cv for indices starts
  dat.tmp.6[410]<-as.character(paste(ioa_cv_1[21:30],collapse="\t")) #region 1, stock 1,timestep 2
  dat.tmp.6[411]<-as.character(paste(ioa_cv_1[21:30],collapse="\t")) #region 1, stock 2,timestep 1
  dat.tmp.6[412]<-as.character(paste(ioa_cv_1[21:30],collapse="\t")) #region 1, stock 2,timestep 2
  dat.tmp.6[413]<-as.character(paste(ioa_cv_2[21:30],collapse="\t")) #region 2, stock 1, timestep 1,
  dat.tmp.6[414]<-as.character(paste(ioa_cv_2[21:30],collapse="\t")) #region 2, stock 2, timestep 2
  dat.tmp.6[415]<-as.character(paste(ioa_cv_2[21:30],collapse="\t")) #region 2,stock 1, timestep 1
  dat.tmp.6[416]<-as.character(paste(ioa_cv_2[21:30],collapse="\t")) #region 2, stock 1, timestep 2
  
  
  
  # fill in age 1 IOA and CV
  linenum=417 #starting line number of age comp matrix for surveys
  for(r in 1:region){
    dat.tmp.6[linenum]<-as.character(paste(dat_ioa_age1_region[r,],collapse="\t")) #using region here because age 1 only has one stock in each region
    linenum=linenum+1
  }# close region loop
  #age1_cv<-rep(0.1,mnyrs)
  dat.tmp.6[419]<-as.character(paste(age1_cv,collapse="\t")) #line 148 is where cv for fisheries starts
  dat.tmp.6[420]<-as.character(paste(age1_cv,collapse="\t")) #region 1, timestep 2
  
  linenum=421
  for(r in 1:region){
    dat.tmp.6[linenum]<-as.character(paste(dat_ioa_yoy_region[r,],collapse="\t")) #sing region here because recruitment only happens to one stock in each reigon
    linenum=linenum+1
  }# close region loop
  
  #yoy_cv<-rep(0.1,mnyrs)
  dat.tmp.6[423]<-as.character(paste(yoy_cv,collapse="\t")) #line 148 is where cv for fisheries starts
  dat.tmp.6[424]<-as.character(paste(yoy_cv,collapse="\t")) #region 1, timestep 2
  
  #fill in occupancy probabilities
  linenum=532 #where occupancy probabilities start
  for(s in 1:stock){
    for(t in 1:tstep){
      dat.tmp.6[linenum]<-as.character(paste(log_occ_prob_dat[s,t,2:lage],collapse="\t")) #model inputs for the probability in the AC region
      #print(dat.tmp[linenum])
      linenum=linenum+1
    }#close timestep loop
  }#close stock loop
  #log_occ_prob_dat[1,1,2:lage]
  #dat.tmp[411]
  
  #sd for occupancy probability, needs to be edited, not sure how to do this
  linenum=536
  for(s in 1:stock){
    for(t in 1:tstep){
      dat.tmp.6[linenum]<-as.character(paste(occ_prob_sd[s,t,2:lage],collapse="\t"))
      linenum=linenum+1
    }#close timestep loop
  }#close stock loop
  
  dat.tmp.6[540]<-as.character(paste(use_age_err_yes,collapse="\t"))
  
  #ESS_F<-as.numeric(c(500.0,500.0))
  #ESS_C<-as.numeric(c(500.0,500.0))
  #fill in EFFECTIVE SAMPLE SIZE
 
  dat.tmp.6[571]<-as.character(paste(ESS_F_se_dat[1,],collapse="\t")) #line 431 starts ess. This is EFF for region 1 fishery (bay) 
  dat.tmp.6[572]<-as.character(paste(ESS_F_se_dat[2,],collapse="\t")) #ESS for region 2 fishery (coast)
  dat.tmp.6[573]<-as.character(paste(ESS_S_se_dat[1,1],collapse="\t")) #ESS for survey in region 1 (bay) in timestep 1
  dat.tmp.6[574]<-as.character(paste(ESS_S_se_dat[1,2],collapse="\t")) #ESS for survey in region 1 (bay) n timestep 2
  dat.tmp.6[575]<-as.character(paste(ESS_S_se_dat[2,1],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
  dat.tmp.6[576]<-as.character(paste(ESS_S_se_dat[2,2],collapse="\t")) #ESS for survey in region 2 (coast) in timestep 1
  
  
  #fill in likelihood switches
  #dat.tmp.6[575]<-as.character(paste(switch_pen_prop_on,collapse="\t")) #if swtich = 0, will not use penalty in likelihood that quantifies the proportion of each stock in each region based on literature
  dat.tmp.6[577]<-as.character(paste(switch_pen_prop_off,collapse="\t")) #if swtich = 0, will not use penalty in likelihood that quantifies the proportion of each stock in each region based on literature
  
    # if switch = 1, will use the penalty in likelihood (kneebone et al. 2012)
  
  
  #dat.tmp.6[576]<-as.character(paste(iboot),collapse='\t') #simulation number
  dat.tmp.6[578]<-as.character(paste(iboot),collapse='\t') #simulation number
  
  #dat.tmp.6[576]<-as.character(paste(1,collapse='\t')) #simulation number
  
  #inputting true values
  dat.tmp.6[579]<-as.character(paste(true_Ntot,collapse="\t")) #this is true_ntot (total N for each stock)
  dat.tmp.6[580]<-as.character(paste(true_Ntot_stock[1,],collapse="\t")) #this is true_ntot (total N for each stock)
  dat.tmp.6[581]<-as.character(paste(true_Ntot_stock[2,],collapse="\t")) #this is true_ntot (total N for each stock)
  dat.tmp.6[582]<-as.character(paste(true_biomass,collapse="\t")) #this is true_biomass (total biomass for each stock)
  dat.tmp.6[583]<-as.character(paste(true_ssb,collapse="\t")) #this is true_ssb (total ssb for each stock)
  #dat.tmp.6[722]<-as.character(paste(true_f_year,collapse="\t")) #this is true_f (total f for each stock)
  dat.tmp.6[584]<-as.character(paste(true_spr,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
  dat.tmp.6[585]<-as.character(paste(true_F_spr_faa,collapse="\t")) #this is true_f that yields f40% for faa, weighted for each stock
  
  linenum=586
  for(y in 1:mnyrs){
    dat.tmp.6[linenum]<-as.character(paste(true_f[y,],collapse="\t")) #this is true_f (total f for each stock)
    linenum=linenum+1
  }
  
  dat.tmp.6[616]<-as.character(paste(12345,collapse="\t"))
  
} #stop editing dataset
  
  
  # Run ADMB ----------------------------------------------------------------
  
  #######Run admb. -ind switch changes the name of the data input file each bootstrap iteration
  write(file=filename.dat6, dat.tmp.6)
  run.command6=paste(filename6, admb.switch, '-ind', filename.dat6, sep=" ")
  #bamboot=paste(process.dir,'/',basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamboot6=paste(process.dir6,'/',basename(filename6),'.exe',sep='')
  file.copy(bamsource6, bamboot6, overwrite=TRUE)
  #bamrun=paste(basename(filename),'-',as.character(iboot),'.exe',sep='')
  bamrun6=paste(basename(filename6),'.exe',sep='')
  run.command6=paste(bamrun6, admb.switch, '-ind', filename.dat6, sep=" ")
  shell(run.command6) #start admb code
  
  #dev.off()
  
  #}
  
  #unlink(process.dir, recursive=T)
  
  } #end nboot
  
  
#stopCluster(cl)
  
  
  
  
  
  
  
  
  
  
  
  
  

# Evaluating Output -------------------------------------------------------
library('ggplot2')
library('ggbreak')
library('dplyr')
  
output_faa1<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/faamodel1/sim_results.txt',header=F, sep="")
output_faa2<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/faamodel2/sim_results.txt',header=F, sep="")
output_se3<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel3/sim_results.txt',header=F, sep="")
output_se4<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel4/sim_results.txt',header=F, sep="")
#output_se5<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel5/sim_results.txt',header=F, sep="")
output_se6<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel6/sim_results.txt',header=F, sep="")

#output_se3<-output_se3[2,]

#View(output_faa2)
output_faa1$model<-"FAA1"
colnames(output_faa1) <- c("simnum","year","F8est","Fre","estNtot","Nre",'estb','Bre','estssb','SSBre','estF40','F40RE' ,"estR", "Rre", "obj","model")

output_faa2$model<-"FAA2"
colnames(output_faa2) <- c("simnum","year","F8est","Fre","estNtot","Nre",'estb','Bre','estssb','SSBre','estF40','F40RE' ,"estR", "Rre","obj","model")

output_se3$model<-"SE1" #SE3 is is spatially explicit without aging error
colnames(output_se3) <- c("simnum","year","F8est","Fre","estNtot","Nre","estNtotCB","NreCB","estNtotAC","NreAC",'estb','Bre','estssb','SSBre','estF40','F40RE', 'estF40_cb','F40RE_cb','estF40_ac','F40RE_ac',"estRcb","Rcb_RE","estRac","Rac_RE","obj","model")

output_se4$model<-"SE2" #SE4 is spatially explicit with aging error
colnames(output_se4) <- c("simnum","year","F8est","Fre","estNtot","Nre","estNtotCB","NreCB","estNtotAC","NreAC",'estb','Bre','estssb','SSBre','estF40','F40RE', 'estF40_cb','F40RE_cb','estF40_ac','F40RE_ac',"estRcb","Rcb_RE","estRac","Rac_RE","obj","model")

#output_se5$model<-"SE5" #SE5 is spatially explicit with stock in data 
output_se6$model<-"SE3" #SE5 is spatially explicit with stock in data 
colnames(output_se6) <- c("simnum","year","F8est","Fre","estNtot","Nre","estNtotCB","NreCB","estNtotAC","NreAC",'estb','Bre','estssb','SSBre','estF40','F40RE', 'estF40_cb','F40RE_cb','estF40_ac','F40RE_ac',"estRcb","Rcb_RE","estRac","Rac_RE","obj","model")
#unique(output_se6$simnum)

# ggplot(data=output_se4)+
#   geom_boxplot(aes(x=(year),y=F40RE_cb,group=year))+
#   geom_hline(yintercept=0,linetype="dashed",col="red")
# 


#View(output_se3)

#output<-rbind(output_faa1,output_faa2,output_se3,output_se4,output_se6)
output<-bind_rows(output_faa1,output_faa2,output_se3,output_se4,output_se6)
#output$year<-rep(seq(1,30),10)
#output<-output_se5
#output<-output[-10,]



#colnames(output) <- c("simnum","year","F8est","Fre","estNtot","Nre",'estb','Bre','estssb','SSBre',"obj","model")
#output$model<-c(rep("FAA2",50),rep("SE2",50))
output<-subset(output,obj<0.01)
#range(output$obj)



#start pdf

#pdf("C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_500_081224.pdf",width=10.5, height=8)



coln<-c("simnum","model", "Nre","year")
Noutput<-output[, (colnames(output) %in% coln)]
write.csv(Noutput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/Abundance-021625.csv")
#Noutput<-output_faa2[, (colnames(output_faa2) %in% coln)]

#Noutput<-Noutput %>% pivot_longer(cols = Nre1:Nre30, names_to = "Year", values_to = "RE")
#numeric_values <- as.numeric(gsub("Nre", "", Noutput$Year))
# Replace the original column with numeric values
# Assuming your dataset is a data frame named 'your_data' and the column is named 'age_column'
#Noutput$Year <- numeric_values
#View(Noutput)
# define the summary function
N_summary <- Noutput %>%
  group_by(model,year) %>%
  summarise(
    y_min = quantile(Nre, 0.05),
    lower = quantile(Nre, 0.25),
    middle = quantile(Nre, 0.5),
    upper = quantile(Nre, 0.75),
    y_max = quantile(Nre, 0.95)
  )

# Create the custom boxplot
ggplot(N_summary, aes(x = year, group = year)) +
  geom_boxplot(
    aes(
      ymin = y_min,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = y_max
    ),
    stat = "identity"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  ggtitle("Total Abundance RE") +
  facet_wrap(~model) +
  theme_bw() +
  theme(legend.position = "none")

 
mod<-c("FAA1","FAA2","SE1","SE2","SE3")
for(i in 1:5){
  Noutput$Mre[Noutput$model==mod[i]]<-median(Noutput$Nre[Noutput$model==mod[i] & Noutput$year==30])
  print(unique(Noutput$Mre[Noutput$model==mod[i]]))
}
#print(unique(Noutput$Mre))
#View(output)

ggplot()+
  geom_boxplot(data=output, aes(x=(year),y=NreCB,group=year))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  #geom_line(data=Noutput,aes(x=year,y=Nre,col=as.factor(simnum)))+
  ggtitle("Total CB Abundance RE")+facet_wrap(~model,drop=TRUE)+
  theme_bw()+theme(legend.position="none")#+
  #facet_grid(~ gender) 

ggplot()+
  geom_boxplot(data=output, aes(x=(year),y=NreAC,group=year))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  #geom_line(data=Noutput,aes(x=year,y=Nre,col=as.factor(simnum)))+
  ggtitle("Total AC Abundance RE")+facet_wrap(~model)+
  theme_bw()+theme(legend.position="none")#+



colssb<-c("simnum", "model", "SSBre","year")
SSBoutput<-output[, (colnames(output) %in% colssb)]
write.csv(SSBoutput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/SSB-021625.csv")



ggplot()+
  #geom_line(data=SSBoutput,aes(x=year,y=SSBre,col=as.factor(simnum)))+
  geom_boxplot(data=SSBoutput, aes(x=(year),y=SSBre,group=year))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
    ggtitle("Total SSB RE")+facet_wrap(~model)+
  theme_bw()+theme(legend.position="none")

for(i in 1:5){
  SSBoutput$Mre[SSBoutput$model==mod[i]]<-median(SSBoutput$SSBre[SSBoutput$model==mod[i] & SSBoutput$year==30])
  print(unique(SSBoutput$Mre[SSBoutput$model==mod[i]]))
}



colbio<-c("simnum", "model", "Bre","year")
Boutput<-output[, (colnames(output) %in% colbio)]
write.csv(Boutput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/Biomass-021625.csv")

ggplot()+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  #geom_line(data=Boutput,aes(x=year,y=Bre,col=as.factor(simnum)))+
  geom_boxplot(data=Boutput, aes(x=(year),y=Bre,group=year))+
    ggtitle("Total Biomass RE")+facet_wrap(~model)+
  theme_bw()+theme(legend.position="none")

for(i in 1:5){
  Boutput$Mre[Boutput$model==mod[i]]<-median(Boutput$Bre[Boutput$model==mod[i] & Boutput$year==30])
  print(unique(Boutput$Mre[Boutput$model==mod[i]]))
}




colf<-c("simnum","model", "Fre","year")
Foutput<-output[, (colnames(output) %in% colf)]
write.csv(Foutput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/Fage8-021625.csv")

ggplot()+
  #geom_line(data=Foutput,aes(x=year,y=Fre,col=as.factor(simnum)))+
  geom_boxplot(data=Foutput, aes(x=year,y=Fre,group=year))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  ggtitle("F Age 8 RE")+
  #scale_x_continuous(name="Year")+
  facet_wrap(~model)+#,scales='free')+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),strip.text.x = element_text(size = 12), #striptext =facet wrap
        legend.position="right", plot.title=element_text(size=12, face="bold", hjust=0.5))+theme(legend.position="none")



for(i in 1:5){
  Foutput$Mre[Foutput$model==mod[i]]<-median(Foutput$Fre[Foutput$model==mod[i] & Foutput$year==30])
  print(unique(Foutput$Mre[Foutput$model==mod[i]]))
}




colfspr<-c("simnum","model", "F40RE","year")
Fsproutput<-output[, (colnames(output) %in% colfspr)]
write.csv(Fsproutput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/F40spr-021625.csv")

ggplot()+
  #geom_line(data=Foutput,aes(x=year,y=Fre,col=as.factor(simnum)))+
  geom_boxplot(data=Fsproutput, aes(x=model,y=F40RE,group=model))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  ggtitle("F 40% Spr RE")+
  #scale_x_continuous(name="Year")+
  #facet_wrap(~model)+#,scales='free')+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),strip.text.x = element_text(size = 12), #striptext =facet wrap
        legend.position="right", plot.title=element_text(size=12, face="bold", hjust=0.5))+theme(legend.position="none")



for(i in 1:5){
  Fsproutput$Mre[Fsproutput$model==mod[i]]<-median(Fsproutput$F40RE[Fsproutput$model==mod[i] & Fsproutput$year==30])
  print(unique(Fsproutput$Mre[Fsproutput$model==mod[i]]))
}


#colnames(output)

colr<-c("simnum","model", "Rre", "estRcb","Rcb_RE","estRac","Rac_RE","year")
Routput<-output[, (colnames(output) %in% colr)]
for(i in 1:5){
  Routput$MreR[Routput$model==mod[i]]<-median(Routput$Rre[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$MreR[Routput$model==mod[i]]))
}


for(i in 1:5){
  Routput$Mreac[Routput$model==mod[i]]<-median(Routput$Rac_RE[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$Mreac[Routput$model==mod[i]]))
}

for(i in 1:5){
  Routput$Mrecb[Routput$model==mod[i]]<-median(Routput$Rcb_RE[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$Mrecb[Routput$model==mod[i]]))
}


write.csv(Routput, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/R-021625.csv")

ggplot()+
  #geom_line(data=Foutput,aes(x=year,y=Fre,col=as.factor(simnum)))+
  geom_boxplot(data=Routput, aes(x=model,y=Rac_RE,group=model))+
  geom_hline(yintercept=0,linetype="dashed",col="red")+
  ggtitle("F 40% Spr RE")+
  #scale_x_continuous(name="Year")+
  #facet_wrap(~model)+#,scales='free')+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),strip.text.x = element_text(size = 12), #striptext =facet wrap
        legend.position="right", plot.title=element_text(size=12, face="bold", hjust=0.5))+theme(legend.position="none")

for(i in 1:5){
  Routput$MreR[Routput$model==mod[i]]<-median(Routput$Rre[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$MreR[Routput$model==mod[i]]))
}


for(i in 1:5){
  Routput$Mreac[Routput$model==mod[i]]<-median(Routput$Rac_RE[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$Mreac[Routput$model==mod[i]]))
}

for(i in 1:5){
  Routput$Mrecb[Routput$model==mod[i]]<-median(Routput$Rcb_RE[Routput$model==mod[i] & Routput$year==30])
  print(unique(Routput$Mrecb[Routput$model==mod[i]]))
}


col2<-c("simnum","model", "Nre","NreCB", "NreAC","Fre","F40RE","F40RE_cb","F40RE_ac","year")
output2<-output[, (colnames(output) %in% col2)]
write.csv(output2, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/fulloutput-021625.csv")




#colterm<-c("simnum","model", "Nre","NreCB", "NreAC","Fre","F40RE","F40RE_cb","F40RE_ac","Rre","Rac_RE","Rcb_RE","year")
colterm<-c("simnum","model", "Nre","NreCB", "NreAC","Fre","F40RE","F40RE_cb","F40RE_ac","year")
#colterm<-c("simnum","model", "Nre","Fre","SSBre","year")
terminal<-output2[, (colnames(output2) %in% colterm)]
#terminal<-terminal %>% pivot_longer(cols = c("Nre","Fre","SSBre"), names_to = "Type", values_to = "RE")
terminal<-terminal %>% pivot_longer(cols = c("Nre","NreCB", "NreAC","Fre","F40RE","F40RE_cb","F40RE_ac"), names_to = "Type", values_to = "RE")
#View(terminal)
abr_term<-subset(terminal,year == 30)


val<-c("Nre","NreCB", "NreAC","Fre","F40RE","F40RE_ac","F40RE_cb")

for(i in 1:5) {
  for(y in 1:length(val)){
       abr_term$Mre[abr_term$model==mod[i] & abr_term$Type==val[y]]<-median(abr_term$RE[abr_term$model==mod[i] & abr_term$Type==val[y]])
    }
  }
write.csv(abr_term, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/terminalyr-021625.csv")


#View(abr_term)
ggplot(data=abr_term, aes(x=Type,y=RE,fill=model))+
  geom_boxplot(na.rm = FALSE,position = position_dodge2(preserve = 'single'))+
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("Terminal Year RE")+
  theme_bw()+ #scale_y_break(c(3.5,5))+
  #scale_y_continuous(limits=c(-1.5,6.5))+
  #scale_y_break(c(3.5,5))+
  #scale_x_discrete(drop=FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),strip.text.x = element_text(size = 12), #striptext =facet wrap
        legend.position="right", plot.title=element_text(size=12, face="bold", hjust=0.5))#+

# col2<-c("simnum","model", "Nre","NreCB", "NreAC","Fre","F40RE","F40RE_cb","F40RE_ac","year")
# output2<-output[, (colnames(output) %in% col2)]
# write.csv(output2, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/fulloutput-021625.csv")




colobj<-c("simnum", "obj","model","year")
objoutput<-output[, (colnames(output) %in% colobj)]
#objoutput<-objoutput %>% pivot_longer(cols = obj, values_to = "obj")

#plot(objoutput)
ggplot(data=objoutput)+
  geom_point( aes(x=simnum,y=obj))+
  facet_wrap(~model)+theme_bw()
  






# F at age output ----------------------------

estfage_faa1<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/faamodel1/sim_F_results.txt',header=F, sep="")
estfage_faa2<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/faamodel2/sim_F_results.txt',header=F, sep="")
estfage_se3<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel3/sim_F_results.txt',header=F, sep="")
estfage_se4<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel4/sim_F_results.txt',header=F, sep="")
#estfage_se5<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel5/sim_F_results.txt',header=F, sep="")
estfage_se5<- read.csv('C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/semodel6/sim_F_results.txt',header=F, sep="")
#head(estfage_faa1)


estfage_faa1$model<-"FAA1"
estfage_faa2$model<-"FAA2"
estfage_se3$model<-"SE1" #SE3 is is spatially explicit without aging error
estfage_se4$model<-"SE2" #SE4 is spatially explicit with aging error
estfage_se5$model<-"SE3" #SE5 is spatially explicit with stock in data 

output_f<-rbind(estfage_faa1,estfage_faa2,estfage_se3,estfage_se4,estfage_se5)
#output_f<-estfage_se4



Fage<-paste("Fest",seq(1,15,by=1),sep="") #est F
FageRE<-paste("Fre",seq(1,15,by=1),sep="")
colnames(output_f) <- c("simnum","Year",Fage,FageRE,"obj","model")
#View(output_f)


# ggplot(data=subset(output_f))+
#   geom_boxplot( aes(x=Year,y=Fre7,group=Year))+
#   geom_hline(yintercept=0,linetype="dashed")+
#   facet_wrap(~model)+theme_bw()+ggtitle("RE F age 7")


Fre<-paste("Fre",seq(1,15),sep="")
col_fest<-c("simnum","model", "Year",Fre)
output_F_2<-output_f[, (colnames(output_f) %in% col_fest)]
output_F_2<-output_F_2 %>% pivot_longer(cols = Fre1:Fre15, names_to = "Age", values_to = "RE")

numeric_values <- as.numeric(gsub("Fre", "", output_F_2$Age))
# Replace the original column with numeric values
# Assuming your dataset is a data frame named 'your_data' and the column is named 'age_column'
output_F_2$Age <- numeric_values
#output_F_2
output_F_2<-subset(output_F_2, Year==30)


ggplot()+
  geom_boxplot(data=output_F_2, aes(x=Age,y=RE,fill=model,group=Age))+
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("F at age RE")+
  theme_bw()+facet_wrap(~model)+#,scales='free')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), axis.title.x=element_text(size=12),
        #axis.text.x = element_blank(),axis.ticks.x=element_blank(),#striptext =facet_wrap,
        legend.position="right", plot.title=element_text(size=12, face="bold", hjust=0.5))

write.csv(output_F_2, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/fullFatage-021625.csv")


#dev.off()






#calculating RSMRE

#how many models met convergence criteria?
convergence<-c(length(unique(output$simnum[output$model=="FAA1"])),length(unique(output$simnum[output$model=="FAA2"])),length(unique(output$simnum[output$model=="SE1"])),length(unique(output$simnum[output$model=="SE2"])),length(unique(output$simnum[output$model=="SE3"])))
# print("Number of Models Converged for all data generating scenario:")
# print(c("FAA1",length(unique(output$simnum[output$model=="FAA1"]))))
# print(c("FAA2",length(unique(output$simnum[output$model=="FAA2"]))))
# print(c("SE3", length(unique(output$simnum[output$model=="SE3"]))))
# print(c("SE4", length(unique(output$simnum[output$model=="SE4"]))))
# print(c("SE5", length(unique(output$simnum[output$model=="SE5"]))))



N_rmsre<-vector(length=5)
model<-c("FAA1","FAA2","SE1","SE2","SE3")
for(i in 1:5){
  N_rmsre[i]<-sqrt(sum(output$Nre[output$model==model[i]]^2)/length(output$Nre[output$model==model[i]]))
}
print(N_rmsre)
F_rmsre<-vector(length=5)
for(i in 1:5){
  F_rmsre[i]<-sqrt(sum(output$Fre[output$model==model[i]]^2)/length(output$Fre[output$model==model[i]]))
}
print(F_rmsre)
SSB_rmsre<-vector(length=5)
for(i in 1:5){
  SSB_rmsre[i]<-sqrt(sum(output$SSBre[output$model==model[i]]^2)/length(output$SSBre[output$model==model[i]]))
}
print(SSB_rmsre)
F40_rmsre<-vector(length=5)
for(i in 1:5){
  F40_rmsre[i]<-sqrt(sum(output$F40RE[output$model==model[i] & output$year==30]^2)/length(output$F40RE[output$model==model[i]& output$year==30]))
}
print(F40_rmsre)
#head(output)

#print(unique(Noutput$Mre))
MRE_N<-vector(length=5)
for(i in 1:5){
  MRE_N[i]<-unique(Noutput$Mre[Noutput$model==model[i]])
}
MRE_N

MRE_F<-vector(length=5)
for(i in 1:5){
  MRE_F[i]<-unique(Foutput$Mre[Foutput$model==model[i]])
}
MRE_F
  
MRE_SSB<-vector(length=5)
for(i in 1:5){
  MRE_SSB[i]<-unique(SSBoutput$Mre[SSBoutput$model==model[i]])
}
MRE_SSB

MRE_F40<-vector(length=5)
for(i in 1:5){
  MRE_F40[i]<-unique(Fsproutput$Mre[Fsproutput$model==model[i]])
}
MRE_F40



new_rmsre<-cbind(model,convergence,MRE_N,N_rmsre,MRE_SSB,SSB_rmsre,MRE_F,F_rmsre,MRE_F40,F40_rmsre)
colnames(new_rmsre)<-c("Model","Total","MRE N","N RMSRE","MRE SSB","SSB RMSRE","MRE F", "F RMSRE", "MRE F40","F40 RMSRE")

write.csv(new_rmsre, "C:/Users/Anyone/Documents/PhD - UMCES/NCBO Project/Striped-Bass-SCAA/Rcode/Simulation/Sim_spatial/Final_output/FinalRMSRE-021625.csv")

