//Program Name/Description: Statistical catch at age model for Striped bass
//Written By: Samara Nehemiah
//Date Created: 01/01/2024
//Last Modified By: SN 
//Last Modified: 04/30/2024
// Fleets as areas simulation model
DATA_SECTION

  init_int fdyear  //first year for data
  init_int ldyear  //last year for data

  init_int fmyear  //first year for model
  init_int lmyear  //last year for model

  init_int fage  //first age
  init_int lage  //last age

  init_int nyrs //number of years in survey
 
  init_int nsurv //number of fisheries independent surveys 
  init_int fleet //number of fleets (coast and bay)

  init_int age1surv //number of age 1 survey
  init_int yoysurv  //number of yoy survey

  init_int tblock //number of time blocks for fishing selectivity
  init_vector ftyear(1,tblock) //first year of each time block

  init_int lbound //lower bound of log_fs
  init_int ubound //upper bound of log_fs

  //starting parameters
  init_number start_log_R //starting parameter for log recruitment
  init_vector start_log_F(1,fleet) //starting parameters for mean log f for each region
  init_number start_log_Feq //starting parameters for F in the first year

  init_number start_log_sf1_ac //starting parameter for slope of selectivity of ac fishery
  init_number start_log_sf2_ac //starting parameter for age at 50% sel of ac fishery
  init_number start_log_sf1_cb //starting parameter for slope of selectivity of cb fishery
  init_number start_log_sf2_cb //starting parameter for age at 50% sel of cb fishery
  init_number start_log_sf3_cb //starting parameter for slope of selectivity of cb fishery
  init_number start_log_sf4_cb //starting parameter for age at 50% sel of cb fishery

  init_number start_log_ssf1 //starting parameter for coast survey selectivity slope
  init_number start_log_ssf2 //starting parameter for coast survey selectivity age at 50% sel
  init_number start_log_ssf3 //starting parameter for coast survey sel, slope of descending limb
  init_number start_log_ssf4 //starting parameter for caost survey sel, 50%sel of descending limb

  init_number start_log_q //starting parameter for catchability of 1+ survey
  init_number start_log_q_age1 //starting parameter for catchinility of age 1 survey
  init_number start_log_q_yoy  //starting parameter for catchability of the you survey

  init_number start_log_a_sf1 //starting params for slope of afsel
  init_number start_log_a_sf2 //starting param for age at 50% of afsel

  //data inputs
  init_matrix obs_C(1,fleet,fdyear,ldyear)             //observed total catch for each year
  init_3darray obs_Cp(1,fleet,fdyear,ldyear,fage,lage) //observed proportions at age in the catch
  init_matrix obs_CV(1,fleet,fdyear,ldyear)           //coefficient of variation

  //1+ fishery independent surveys
  init_matrix obs_I(1,nsurv,fdyear,ldyear)
  init_3darray obs_Ip(1,nsurv,fdyear,ldyear,fage,lage)  //observed proportions at age in the survey index of abudance
  init_matrix obs_I_CV(1,nsurv,fdyear,ldyear)  //observed CV for IOA

  //age 1 and yoy indicies and cvs
  init_matrix obs_I_age1(1,age1surv,fdyear,ldyear)        //observed indices of abudnace for age 1 surveys
  init_matrix obs_I_age1_CV(1,age1surv,fdyear,ldyear)     //observed CV for age 1 surveys
  init_matrix obs_I_yoy(1,yoysurv,fdyear,ldyear)          //observed yoy indices of abudance
  init_matrix obs_I_yoy_CV(1,yoysurv,fdyear,ldyear)       //observed CV for yoy indices

  //model assumptions
  init_matrix w_age(fdyear,ldyear,fage,lage) //average weight at age, in kG
  init_matrix ssbw_age(fdyear,ldyear,fage,lage) //adjustment of rivard weight to match the time of spawning
  init_matrix rw_age(fdyear,ldyear,fage,lage) //rivard weight at age
  init_vector m_age(fage,lage) //maturity at age
  init_vector M(fage,lage) //assumed natural mortality for all ages
  init_vector sex(fage,lage) //sex proportions at age

  //aging error matrix
  init_number use_age_err //1, use aging err matrix, 2, use identify matrix
  init_matrix age_err_a(fage,lage,fage,lage) //aging error ages 1-15
  init_matrix age_err_a_id(fage,lage,fage,lage) //identify matrix for age 1-15+
  
  init_vector ESS_C(1,fleet)//effective sample size for catch at age
  init_vector ESS_I(1,nsurv) //effective sample size for the survey index at age

  init_int sim_num //simulation number for data generation

  //true parameters
  init_vector true_Ntot(fmyear,lmyear)
  init_vector true_biomass(fmyear,lmyear) //storing true values of jan 1 biomass from simulation
  init_vector true_SSB(fmyear,lmyear) //storing true value of jan 1 SSB from simulatin
  //init_vector true_F_year(fmyear,lmyear) //storing true values ofJan 1 F
  init_number true_F_spr
  init_matrix true_F(fmyear,lmyear,fage,lage)

  init_number test // EOF test number

  matrix C_var(1,fleet,fmyear,lmyear)  //log-scale variance of catch
  matrix I_var(1,nsurv,fmyear,lmyear) //log-scale variance of index of abundance
  matrix I_var_age1(1,age1surv,fmyear,lmyear)//log-scale variance of index of abundance, age 1 surv
  matrix I_var_yoy(1,yoysurv,fmyear,lmyear) //log-scale varience of yoy ioa

  matrix mod_age_err(fage,lage,fage,lage)

  int a // looping variable for age
  int y // looping variable for year
  int s // looping variable for survey
  int f // looping variable for fleet
  int o //looping variable for age 1 survey
  int r // looping variable for yoy survey
  int t // looping variable for time block
  number d //constant for calculation of proportions at age variance
  int G //looping variable for spr model

  int ftbyr //variable for the first year of time block
  int ftlyr //variable for the last year of time block

 LOCAL_CALCS
  if(test!=12345)
  {
    //Write error message and output data to the screen
    cout << "Error in model inputs!" << endl;
    cout << "data years" << endl << fdyear << endl << ldyear << endl;
    cout << "model years" << endl << fmyear << endl << lmyear << endl;
    cout << "ages" << endl << fage << endl << lage << endl;
    cout << "obs catch" << endl << obs_C << endl;
    cout << "obs prop at age in catch" << endl << obs_Cp << endl;
    cout << "obs index" << endl << obs_I << endl;
    cout << "obs prop at age in survey" << endl << obs_Ip << endl;
    cout << "weight at age" << endl << w_age << endl;
    cout << "maturity at age" << endl << m_age << endl;
    cout << "M" << endl << M << endl;
    cout << "use age err" << endl << use_age_err << endl;
    cout << "age err" << endl << age_err_a << endl;
    cout << "id mat" << endl << age_err_a_id << endl;
    //cout << "C_sd" << endl << C_sd << endl;
    cout << "ESS_C" << endl << ESS_C << endl;
    //cout << "I_sd" << endl << I_sd << endl;
    cout << "ESS_I" << endl << ESS_I << endl;
    cout << "test" << endl << test << endl;
    exit(1);  //exit the program
  }
  //cout << I_var << endl;
  //convert SDs to variances so that it only has to be done once
  for(f=1;f<=fleet;f++)
  {
    C_var(f)=square(obs_CV(f)(fmyear,lmyear));//calculate variance from standard deviations
  }
  for(s=1;s<=nsurv;s++)
  {
    I_var(s)=square(obs_I_CV(s)(fmyear,lmyear));
  }
  for(o=1;o<=age1surv;o++)
  {
    I_var_age1(o)=square(obs_I_age1_CV(o)(fmyear,lmyear));
  }
  for(r=1;r<=yoysurv;r++)
  {
    I_var_yoy(r)=square(obs_I_yoy_CV(r)(fmyear,lmyear));
  }

  if(use_age_err==1)
  {
    mod_age_err=age_err_a;
      }
  else
  {    
    mod_age_err=age_err_a_id;
   }
  /*
  //cout << I_var << endl;
  cout << "data years" << endl << fdyear << endl << ldyear << endl;
  cout << "model years" << endl << fmyear << endl << lmyear << endl;
  cout << "ages" << endl << fage << endl << lage << endl;
  cout << "obs catch" << endl << obs_C << endl;
  cout << "obs prop at age in catch" << endl << obs_Cp << endl;
  cout << "obs index" << endl << obs_I << endl;
  cout << "obs prop at age in survey" << endl << obs_Ip << endl;
  cout << "start log sf1 ac" << endl << start_log_sf1_ac << endl; 
  cout << "start log sf2 ac" << endl <<  start_log_sf2_ac << endl; //starting parameter for age at 50% sel of ac fishery
  cout << "start log sf2 cb" << endl << start_log_sf1_cb << endl; //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf2 cb" << endl << start_log_sf2_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log sf3 cb" << endl << start_log_sf3_cb << endl; //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf4 cb" << endl << start_log_sf4_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log ssf1" << endl << start_log_ssf1 << endl; //starting parameter for coast survey selectivity slope
  cout << "start log ssf2" << endl << start_log_ssf2 << endl; //starting parameter for coast survey selectivity age at 50% sel
  cout << "start ssf3" << endl << start_log_ssf3 << endl; //starting parameter for coast survey sel, slope of descending limb
  cout << "start ssf4" << endl << start_log_ssf4 << endl;
  cout << "start log q" << endl << start_log_q << endl; //starting parameter for catchability of 1+ survey
  cout << "start log q age 1" << endl << start_log_q_age1 << endl; //starting parameter for catchinility of age 1 survey
  cout << " start log q yoy" << endl << start_log_q_yoy << endl;
  exit(1);
  */
 END_CALCS


PARAMETER_SECTION
  //The parameter section is where we specify the parameters to be estimated (init)
  //and any other values that will depend on the estimated parameters (i.e., variables)
  
  //parameters to determine numbers at age in first year
  //init_number log_N0(-1) //log of mean abundance at age in the first year
  //init_bounded_dev_vector log_N0_devs(fage+1,lage,-10,10)//deviations of abudnace at age for the first year

  //***********************
  // fishery selectivity
  //***********************

  init_bounded_vector log_sf1_ac(1,tblock,-2,2,1) // slope of the logistic function for fsel in the chesapeake bay
  vector sf1_ac(1,tblock) //for the slope of logistic function for fishery selectivity
  init_bounded_vector log_sf2_ac(1,tblock,0,5,1) // 50% selectivity param  of the logistic function for fsel in the chesapeake bay
  vector sf2_ac(1,tblock) //arctanget transformation for the 50% selectivity param of logistic function for fishery selectivity 

  //double logistic parameters for fishery in the chesapeake bay
  init_bounded_vector log_sf1_cb(1,tblock,-2,2,1)
  vector sf1_cb(1,tblock)
  init_bounded_vector log_sf2_cb(1,tblock,0,5,1)
  vector sf2_cb(1,tblock)
  init_bounded_vector log_sf3_cb(1,tblock,-2,2,1)
  vector sf3_cb(1,tblock)
  init_bounded_vector log_sf4_cb(1,tblock,0,5,1)
  vector sf4_cb(1,tblock)


  //*******************
  //survey selectivity
  //*******************
  init_bounded_vector log_ssf1(1,nsurv,-2,2,1) //log of the slope for  surveys
  vector ssf1(1,nsurv) //slope of the  survey
  init_bounded_vector log_ssf2(1,nsurv,0,5,1) //log of the 50% selected for the  survey
  vector ssf2(1,nsurv) //50% sel for the  survey
  init_bounded_vector log_ssf3(1,nsurv,-2,2,1) //log of the slope for   survey
  vector ssf3(1,nsurv) //slope of   survey
  init_bounded_vector log_ssf4(1,nsurv,0,5,1) //log of the 50% sel for   survey
  vector ssf4(1,nsurv) //50% sel for   survey

  init_bounded_number log_a_sf1(-2,2,1) // log of the slope of afsel
  init_bounded_number log_a_sf2(0,5,1) // lof of the age at 50% sel of afsel
  number a_sf1 //starting params for slope of afsel
  number a_sf2 //starting param for age at 50% of afsel
    
  init_vector log_q(1,nsurv) //survey catchability
  init_vector log_q_age1(1,age1surv) //age 1 survey catchability
  init_vector log_q_yoy(1,yoysurv) //yoy survey catchability

  //Recruitment
  init_bounded_number log_R(0,25,1) //mean of log recruitment
  init_bounded_dev_vector log_Rdevs(fmyear,lmyear,-10,10,1)// bounded by log(-10) and log(10)
  
  //Fishing mortality
  init_bounded_number log_Feq(-5,2,1)
  init_bounded_vector log_F(1,fleet,-5,2,1) //mean of log F for model timeseries
  init_bounded_dev_vector log_F1devs(fmyear,lmyear,-15,15,1)//F devs for fleet 1 (CB);
  init_bounded_dev_vector log_F2devs(fmyear,lmyear,-15,15,1)//F devs for fleet 2 (AC); 
  matrix est_F(fmyear,lmyear,fage,lage) //total F for combined fleets in Jan 1
  vector est_F_year(fmyear,lmyear)
  vector Feq(fage,lage) //F in equillibrium in the first year
 
  //set up what we are calculating for population model
  matrix N(fmyear,lmyear,fage,lage) //abundance at age
  matrix Z(fmyear,lmyear,fage,lage) //total mortality rate at age
  3darray F(1,fleet,fmyear,lmyear,fage,lage) //fishing mortality rate at age
  vector est_Ntot(fmyear,lmyear) //total Jan 1 abundance to compare to simulation

  3darray fsel(1,fleet,fmyear,lmyear,fage,lage) //fishery selectivity at age
  3darray log_fsel(1,fleet,fmyear,lmyear,fage,lage) //log of fsel
  vector afsel(fage,lage)//average fsel for each fleet it he first year
  vector log_afsel(fage,lage) //log of afsel

  matrix ssel(1,nsurv,fage,lage) //survey selectivity at age
  matrix log_ssel(1,nsurv,fage,lage) //log of survey selectivity at age
  
  vector q(1,nsurv)//survey catchability group a, age 1-15
  vector q_age1(1,age1surv)//age 1 survey catchability
  vector q_yoy(1,yoysurv) //yoy survey catchability
  
  //observation model
  matrix est_C(1,fleet,fmyear,lmyear) //estimated total catch
  3darray est_C_age(1,fleet,fmyear,lmyear,fage,lage) //estimated catch at age
  3darray est_C_age_err(1,fleet,fmyear,lmyear,fage,lage) //estimated catch at age
  3darray est_Cp(1,fleet,fmyear,lmyear,fage,lage) //estimated proportions at age in the catch
  3darray sigma2_Cp(1,fleet,fmyear,lmyear,fage,lage) //variance of estimate proportions at age in the catch
    
  matrix est_I(1,nsurv,fmyear,lmyear) //estimated aggregate index of abundance
  3darray est_I_age(1,nsurv,fmyear,lmyear,fage,lage) //estimated catch at age, index
  3darray est_I_age_err(1,nsurv,fmyear,lmyear,fage,lage) //estimated catch at age, index
  3darray est_Ip(1,nsurv,fmyear,lmyear,fage,lage) //estimated proportions at age in the index catch
  3darray sigma2_Ip(1,nsurv,fmyear,lmyear,fage,lage) //variance of estimate proportions at age in the survey
 
  matrix est_I_age1(1,age1surv,fmyear,lmyear) //estimated ioa for age 1 survey
  matrix est_I_yoy(1,yoysurv,fmyear,lmyear) //estimated ioa for yoy surv
  
  //matrix rw(fmyear,lmyear,fage,lage) //estimate rivard weight
  //matrix SSB_w(fmyear,lmyear,fage,lage) //estimated SSBweight at age, adjusted from rivard weight

  vector B(fmyear,lmyear) //estimated biomass for each year
  vector est_biomass(fmyear,lmyear)//estimated jan 1 biomass
  vector SSB(fmyear,lmyear)  //estimated spawning stock biomass for each year
  vector est_SSB(fmyear,lmyear) //jan 1 ssb
  vector J_w(fmyear,lmyear) //estimated january 1 biomass at age

  //params for spr model
  number years
  number G_spr
  number R_eq//equillibrium recruitment
  number F_spr //f value that gives you 40% spr
  matrix Nf(1,100,fage,lage)//abundance per recruit at age, for spr model
  vector SSBf(0,150) //holds SSB across f conditions
  vector SPR(0,150) //holds SPR value for different f conditions
  matrix F_eq(1,fleet,fage,lage)
  vector Fa(fage,lage)
  matrix Fstar(1,fleet,fage,lage)
  matrix V(1,fleet,fage,lage)
  number F_40 //F that yields spr40%
  vector Zf(fage,lage) //lifetime mortlaity in equillibrium
  number slope
  number est_G_40
  matrix F_bar(1,fleet,fage,lage) //hold f values after you find G
  number est_F_spr //holds values of F that achieve 40%

  //***********************
  //Likelihood components
  //***********************
  
  vector sig2_f(1,fleet) //variance
  vector sig2_I(1,nsurv)
  
  //likelihoods functions
  vector Lcatch(1,fleet)
  vector Lcatchagecomp(1,fleet)
  vector Lindex(1,nsurv) //age 1-15 surveys
  vector Lindexagecomp(1,nsurv) //age 1-15 surveys
  vector Lage1index(1,age1surv)
  vector Lyoyindex(1,yoysurv)
  number Lfsel
  number Lssel //survey selectivity
  //number pen_N0_dev //penalty for deviations of N0
  number pen_f2sel // penalty for fsel in coast of  tblock1 deviating from tblock2
  number pen_F //penalty for F
  number pen_rdev
  number pen_fdev

  objective_function_value neg_LL  //value we are going to minimize

 LOCAL_CALCS
  //set starting values for parameters
  //log_N0=log(200000.);//mean abudance
  log_R=start_log_R; //mean of log recruitment
  //cout << log_R << endl << endl;
  //exit(1);
  
  for(f=1;f<=fleet;f++)
  {
    log_F(f)=start_log_F(f); //mean of log F
    //log_Feq(f)=start_log_Feq(f);//mean fishing mortality in equilibrium
  }
  log_Feq=start_log_Feq;//mean fishing mortality in equilibrium

  log_a_sf1=start_log_a_sf1;
  log_a_sf2=start_log_a_sf2;
  
  //*******************
  //starting parameters for selectivity
  //*******************
  log_sf1_ac=start_log_sf1_ac;//slope of logistic for fsl
  log_sf2_ac=start_log_sf2_ac;//fsel 50% selected
  log_sf1_cb=start_log_sf1_cb;//slope of double logistic for fsl
  log_sf2_cb=start_log_sf2_cb;//fsel 50% selected
  log_sf3_cb=start_log_sf3_cb;//slope of descending double logistic for fsl
  log_sf4_cb=start_log_sf4_cb;//fsel 50% selected of descending slope

  //cout << log_sf1_ac << endl;
  //cout << log_sf2_ac << endl;
  //exit(1);

  for(s=1;s<=nsurv;s++)
  {
    log_ssf1(s)=start_log_ssf1;
    log_ssf2(s)=start_log_ssf2;
    log_ssf3(s)=start_log_ssf3;
    log_ssf4(s)=start_log_ssf4;
  }
  
  log_q=start_log_q;
  log_q_age1=start_log_q_age1;
  log_q_yoy=start_log_q_yoy;
  //cout << "log q" << endl << log_q << endl;
  //cout << "log q age1" << endl << log_q_age1 << endl;
  //cout << "log q yoy" << endl << log_q_yoy << endl;
  //exit(1);
  years=100;
  /*
  //test starting parameters
  cout << "start log R" << endl <<  start_log_R << endl; //starting parameter for log recruitment
  cout << "start log F" << endl << start_log_F << endl; //starting parameters for mean log f for each region
  cout << "start log Feq" << endl <<  start_log_Feq << endl; //starting parameters for F in the first year
  cout << "start afsel" << endl << log_a_sf1 << endl;
  cout << "Sstart afsel 2" << endl << log_a_sf2 << endl;
  //cout << "feq age" << endl <<  Feq << endl; //feq for each age
  cout << "start log sf1 ac" << endl <<  start_log_sf1_ac <<  endl; //starting parameter for slope of selectivity of ac fishery
  cout << "start log sf2 ac" << endl <<  start_log_sf2_ac << endl;  //starting parameter for age at 50% sel of ac fishery
  cout << "start log sf1 cb" << endl << start_log_sf1_cb << endl; //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf2 cb" << endl << start_log_sf2_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log sf3 cb" << endl << start_log_sf3_cb << endl;  //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf4 cb" << endl << start_log_sf4_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log ssf1 " << endl << start_log_ssf1 << endl; //starting parameter for coast survey selectivity slope
  cout << "start log ssf2 " << endl << start_log_ssf2 << endl; //starting parameter for coast survey selectivity age at 50% sel
  cout << "start log ssf3 " << endl << start_log_ssf3 << endl; //starting parameter for coast survey sel, slope of descending limb
  cout << "start log ssf4 " << endl << start_log_ssf4 << endl; //starting parameter for caost survey sel, 50%sel of descending limb
  cout << "start log q " << endl <<  start_log_q << endl;  //starting parameter for q for 1+ survey in coast
  cout << "start log qage1 " << endl << start_log_q_age1 << endl; //starting parameter for q for age 1 survey in coast
  cout << "start log qyoy " << endl << start_log_q_yoy << endl;  //starting parameter for q for yoy survey in bay
  cout << "aging error matrix" << endl << mod_age_err << endl;
  cout << " obs c" << endl <<  obs_C << endl;             //observed total catch for each year
  cout << " obscp " << endl << obs_Cp << endl; //observed proportions at age in the catch
  cout << "obs I " << endl << obs_I << endl;
  cout << "obs ip" << endl << obs_Ip << endl;  //observed proportions at age in the survey index of abudance
  cout << "obs i cv" << endl << obs_I_CV << endl;  //observed CV for IOA
  exit(1);
  */
  
 END_CALCS

PROCEDURE_SECTION
//In the procedure section we specify the model and the likelihood.

  calculate_mortality();

  //  cout << "After calculate_mortality" <<endl;

  calculate_N_at_age();

 // cout << "After calculate_N_at_age" <<endl;

  calculate_fishery_catch();

  //  cout << "After calculate_fisher_catch" <<endl;

  calculate_survey_catch();

  //  cout << "After calculate_survey_catch" <<endl;
  
  evaluate_likelihood();  

 // cout << "After calculate_likelihood" <<endl;

  calculate_B_SSB();

  // cout << "after calculate B SSB" << endl;

  //calculate_SPR();


FUNCTION calculate_mortality

  //*******************************
  // Calculate Fishery Selectivity
  //******************************

  for(t=1;t<=tblock;t++)
  {
    sf1_ac(t)=mfexp(log_sf1_ac(t));
    sf2_ac(t)=mfexp(log_sf2_ac(t));
  
    sf1_cb(t)=mfexp(log_sf1_cb(t));
    sf2_cb(t)=mfexp(log_sf2_cb(t));
    sf3_cb(t)=mfexp(log_sf3_cb(t));
    sf4_cb(t)=mfexp(log_sf4_cb(t));
  }
  //cout << sf2_ac << endl;
  //cout << sf2_cb << endl;
  //exit(1);


  for(t=1;t<=tblock;t++)
  {
    ftbyr=ftyear(t);
    if(t==tblock)
    {
      ftlyr=lmyear;
    }
    else
    {
      ftlyr=ftyear(t+1)-1;
    }
    //cout << "First year timeblock" << ftbyr << endl;
    //cout << "Last Year timeblock" << ftlyr << endl;
    
  
    f=1;//fill fsel for chesapeake bay
    for(y=ftbyr;y<=ftlyr;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        fsel(f,y,a)=(1./(1.+mfexp(-sf1_cb(t)*(double(a)-sf2_cb(t)))))*
                  (1./(1.+mfexp(-sf3_cb(t)*(sf4_cb(t)- double(a)))));
      }//close age loop
        //fsel(r,ts,y)/=max(fsel(r,ts,y));
    } //close year loop for fsel
    f=2;//fill fsel for atlantic coast
    for(y=ftbyr;y<=ftlyr;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        fsel(f,y,a)=1./(1.+mfexp(-sf1_ac(t)*(double(a)-sf2_ac(t))));
      }//close age loop
        //fsel(r,ts,y)/=max(fsel(r,ts,y));
    } //close year loop for fsel
  }//close time block

  //cout << "fsel " << endl << fsel <<endl;
  //exit(1);

  for(y=fmyear;y<=lmyear;y++)
  {
    for(f=1;f<=fleet;f++)
    {
      log_fsel(f,y)=log(fsel(f,y));
    }
  }

  //cout << "fsel" << fsel << endl;
  //exit(1);
  
  //*******************************
  // Calculate Survey Selectivity
  //******************************

  
  //set starting parameters
  
  ssf1=mfexp(log_ssf1);
  ssf2=mfexp(log_ssf2);
  ssf3=mfexp(log_ssf3);
  ssf4=mfexp(log_ssf4);

  //cout << ssf1 << endl << ssf2 << endl << ssf3 << endl << ssf4 << endl;

  for(s=1;s<=nsurv;s++)//looping over surveys and age
  {
    for(a=fage;a<=lage;a++)
    {
      ssel(s,a)=(1./(1.+mfexp(-ssf1(s)*(double(a)-ssf2(s)))))*
                  (1./(1.+mfexp(-ssf3(s)*(ssf4(s)- double(a)))));;//double logistic equation
     }//close age loop for fssel
   }//close survey loop
  //cout << ssel << endl;
  //exit(1);
  
  //calculate log of ssel
  for(s=1;s<=nsurv;s++)//looping over surveys
  {
    log_ssel(s)=log(ssel(s));
  }
  
  //cout << "ssel " << endl << ssel << endl;
  //exit(1);
  
  //calculate catchability
  q=mfexp(log_q);
  q_age1=mfexp(log_q_age1);
  q_yoy=mfexp(log_q_yoy);

  //cout << "q" << endl << q << endl;
  //cout << "qage1" << endl << q_age1 << endl;
  //cout << "qyoy" << endl << q_yoy << endl;
  //exit(1);

  //calculate F in the first year 
  a_sf1=mfexp(log_a_sf1);
  a_sf2=mfexp(log_a_sf2);
  
  //for(f=1;f<=fleet;f++)
  //{
    for(a=fage;a<=lage;a++)
    {
      afsel(a)=1./(1.+mfexp(-a_sf1*(double(a)-a_sf2)));//setting afsel to a logistic curve with slope of 1 and 50% sel at 4
    }
  //}
  //afsel(1)(fage,lage)/=max(afsel(1));
  //cout << afsel << endl;
  //exit(1);

  //for(f=1;f<=fleet;f++)
  //{
    for(a=fage;a<=lage;a++)
    {
      Feq(a)=afsel(a)*mfexp(log_Feq);
    }
  //}
  //cout << "log feq" << log_Feq << endl;
  //cout << "Feq " << Feq << endl;
  //exit(1);

  //calculate the fishing mortality rate (F) for each year and age
  for(y=fmyear;y<=lmyear;y++)
  {
      F(1,y)=fsel(1,y)*mfexp(log_F(1)+log_F1devs(y)); //fleet 1 = Chesapeake Bay
      F(2,y)=fsel(2,y)*mfexp(log_F(2)+log_F2devs(y)); //fleet 2 = Atlantic Coast
  }
  
  //cout << "F" << endl << F <<endl << endl;
  //cout << "logf 1" << endl << log_F(1) << endl;
  //cout << "logf 2" << endl << log_F(2) << endl;
  //cout << "fsel 1" << endl << fsel(1) << endl;
  //cout << "fsel 2" << endl << fsel(2) << endl;

  //calculate the total mortality rate for each year and age
  for(y=fmyear;y<=lmyear;y++)
  {
    Z(y)=M;
      for(f=1;f<=fleet;f++)
      {
        Z(y)+=F(f,y);
      }// close fleet loop
  }//close  year loop
  //cout << "Z" << endl << Z << endl << endl;
  //exit(1);

FUNCTION calculate_N_at_age
  //fill in abundance at age in the first row of the N-at-age matrix
  //fill in recuitment for the first year
  N(fmyear,fage)=mfexp(log_R+log_Rdevs(fmyear));
  //calculate N at age in the first year, assuming that population is at equillibrium
  for(a=fage+1;a<=lage;a++)
  {
    N(fmyear,a)=N(fmyear,a-1)*mfexp(-(M(a-1)+Feq(a-1))); //adding total mortality in the first year
   //N(fmyear,a)=exp(log_N0+log_N0_devs(a));
  }//close age loop
  //including plus group in first year
  N(fmyear,lage)/=1.-mfexp(-(M(lage)+Feq(lage)));
  //now include N0 deviations in first year of equilibrium
  //N(fmyear)(fage+1,lage)=elem_prod(N(fmyear)(fage+1,lage),exp(log_N0_devs(fage+1,lage)));

  //fill in the rest of the rows of the N-at-age matrix by applying exponential mortality
  for(y=fmyear+1;y<=lmyear;y++)
  {
    //calculate recruitment
    N(y,fage)=mfexp(log_R+log_Rdevs(y));
    //cout << "recruit" << endl << N(y,fage) << endl;
    //calculate abundance for the rest of the age classes
    for(a=fage+1;a<=lage;a++)
    {
      N(y,a)=N(y-1,a-1)*mfexp(-Z(y-1,a-1));
    }
    //last age is a plus group
    N(y,lage)+=N(y-1,lage)*mfexp(-Z(y-1,lage));  
  }//close year loop

  //cout << N(fmyear) << endl;
  //cout << N << endl;
  //exit(1);

 //calc total abundance
  est_Ntot=0.0;//initialize
  for(y=fmyear;y<=lmyear;y++)
  {
    est_Ntot(y)+=sum(N(y));
  }//close year loop

  
FUNCTION calculate_B_SSB
  for(y=fmyear;y<=lmyear;y++)
  {
    B(y)=N(y)*rw_age(y)/1000; //January 1 Biomass; using rivard weight
    SSB(y)=N(y)*elem_prod(sex,elem_prod(ssbw_age(y),m_age))/1000; //Female Spawning Stock biomass
  }
  // * is the dot product, which multiplies the values and then sums it
  //set to equal est_biomass and est_ssb to compare to the true values
  est_biomass=B;
  est_SSB=SSB;
  //these are already the total so just compared to the same

FUNCTION calculate_fishery_catch
  //calculate fishery catch at age using the Baranov catch equation C=(F/Z)*(1-exp(-Z))*N
  for(f=1;f<=fleet;f++)
  {
    //est_C(f)=0.0;
    for(y=fmyear;y<=lmyear;y++)
    {
      est_C_age(f,y)=elem_prod(elem_prod(elem_div(F(f,y)(fage,lage),Z(y)(fage,lage)),1.-mfexp(-Z(y)(fage,lage))),N(y)(fage,lage));
      est_C_age_err(f,y)=mod_age_err*est_C_age(f,y);
    }//close year loop for catch at age
   //calculate total catch
    est_C(f)=rowsum(est_C_age_err(f));
   //calculate proportions at age in the catch
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        est_Cp(f,y,a)=est_C_age_err(f,y,a)/est_C(f,y);
      }//close age loop in est_cp
    }//close year loop in est cp

  //calculate variance of expected proportion for catch at age (Fournier - multifanciel)
    for(y=fmyear;y<=lmyear;y++)
    {
      //if(y<ftyear(2) && f==2)
      //{
      //  d=0.05;
      //}
      //else
      //{
        d=0.1;
      //}
      for(a=fage;a<=lage;a++)
      {
        sigma2_Cp(f,y,a)=((1.-est_Cp(f,y,a))*est_Cp(f,y,a)+(d/double(lage-fage+1)))/ESS_C(f);
      }//close age loop for sigma2cp
    }//close year loop for sigma2cp
  }//close fleet loop for 
  /*
  for(f=1;f<=fleet;f++)
  {
    cout << "f= " << f << endl;
    cout << "ESS= " << ESS_C(f) << endl;
    for(y=fmyear;y<=lmyear;y++)
    {
      //for(a=fage;a<=lage;a++)
      //{
      //cout << "c age" << endl;
      //cout << est_C_age << endl;
      //cout << "est c age err" << endl;  
      //cout << est_C_age_err << endl;
      //cout << "sigmaCP" << endl;
      cout <<  "y= " << y  << endl;
      cout << sigma2_Cp(f,y) << endl;
     // }
    }
  }
  exit(1);
  */              

  //calculate est_f to compare to simulation
  est_F=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    for(a=fage;a<=lage;a++)
    {
      //for(f=1;f<=fleet;f++)
      //{
        est_F(y,a)+=F(1,y,a)+F(2,y,a);//adding fleet 1 (CB) to fleet 2 (Ocean)
      //}//close age loop
    }//close fleet
  }//clsoe year loop
  
  //for(y=fmyear;y<=lmyear;y++)
  //{
  //  est_F_year(y)=sum(est_F(y));
  //}
  //cout << "est f" << endl << est_F << endl;
  //cout << "true f" << endl << true_F << endl;
  //exit(1);

FUNCTION calculate_survey_catch
  
  //calculate estimated survey catch at age for each year
  for(s=1;s<=nsurv;s++)
  {
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        est_I_age(s,y,a)=q(s)*(N(y,a)*ssel(s,a));
      }//close age loop
      est_I_age_err(s,y)=mod_age_err*est_I_age(s,y);
    }//closing year loop
  }//closing survey loop
  
  
  for(s=1;s<=nsurv;s++)
  {
    for(y=fmyear;y<=lmyear;y++)
    {
     //calculate total survey catch at age for each year
      est_I(s,y)=sum(est_I_age_err(s,y)(fage,lage));
    }
  //calculate proportions at age in the survey catch
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        est_Ip(s,y,a)=est_I_age_err(s,y,a)/est_I(s,y);
      }
    }//close year loop
  

    //cout << "est_I" << endl << est_I << endl << "ssel" << endl <<  ssel << endl << "N" << endl << N << endl <<  endl;
  //calculate variance of expected proportion for index at age
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        sigma2_Ip(s,y,a)=((1.-est_Ip(s,y,a))*est_Ip(s,y,a)+0.1/double(lage-fage+1))/ESS_I(s);
      }//close age loop
    }//close year loop
   }//closing survey loop
  /*
  for(s=1;s<=nsurv;s++)
    {
      cout << "s=" << s << endl;
      //cout << "ssel= " << endl << ssel(s) << endl;
     cout << "estI" << endl;
     cout  << est_I(s) << endl;
    }
   exit(1);
  */
  //cout << "tot surv age " << endl << est_I_age << endl;
  //cout << "tot surv cat " << endl << est_I << endl;
  //exit(1);


  ///*********************************************
  ///            YOY and age 1 survey
  ///********************************************
  //age 1 survey
  for(o=1;o<=age1surv;o++)
  {
    for(y=fmyear;y<=lmyear;y++)
    {
      est_I_age1(o,y)=q_age1(o)*N(y,1);
    }
  }
  //yoy survey
  for(r=1;r<=yoysurv;r++)
  {
    for(y=fmyear;y<=lmyear-1;y++)
    {
      est_I_yoy(r,y)=q_yoy(r)*N(y+1,1);
    }
  }
  //cout << age1surv << endl;
  //cout << yoysurv << endl;
  //exit(1);
  //  cout << "catch" << endl;
  //cout << "q age 1"  << endl << q_age1 << endl;
  //for(y=fmyear;y<=lmyear;y++)
  //{
  //  cout << "N age 1" << endl << N(y,fage) << endl;
  // }
  //cout << "est I age1" << endl << est_I_age1 << endl << endl;
  //cout << "est I yoy" << endl << est_I_yoy << endl << endl;
  //cout << "obs I yoy" << endl << obs_I_yoy << endl << endl;
  //exit(1);

FUNCTION evaluate_likelihood

  //lognormal likelihood for total catch

  for(f=1;f<=fleet;f++)
  {
    Lcatch(f)=lognorm_negLL(obs_C(f),est_C(f),C_var(f),fmyear,lmyear);
    Lcatchagecomp(f)=multinom_negLL(obs_Cp(f),est_Cp(f),sigma2_Cp(f),fage,lage,fmyear,lmyear);//multinomial for proportions
  }
  
  //lognormal likelkihood for indices of abundance
  for(s=1;s<=nsurv;s++)
  {
    Lindex(s)=lognorm_negLL(obs_I(s),est_I(s),I_var(s),fmyear,lmyear);
    Lindexagecomp(s)=multinom_negLL(obs_Ip(s),est_Ip(s),sigma2_Ip(s),fage,lage,fmyear,lmyear);
  }
  //cout << Lage1index << endl;
  //exit(1);
    
  for(o=1;o<=age1surv;o++)
  {
    Lage1index(o)=lognorm_negLL(obs_I_age1(o),est_I_age1(o),I_var_age1(o),fmyear,lmyear);
    //cout << "obs_I_age1" << "o= " << " " << o << endl << obs_I_age1(o) << endl;
    //cout << "est_I_age1" << endl << est_I_age1(o) << endl;
    //cout << "I_var_age1" << endl << I_var_age1(o) << endl;
  }
  //cout << "Lage1index" << endl <<  Lage1index << endl;
  //exit(1);


  for(r=1;r<=yoysurv;r++)
  {
    Lyoyindex(r)=lognormyoy_negLL(obs_I_yoy(r),est_I_yoy(r),I_var_yoy(r),fmyear,lmyear-1);
  }
  //cout << "Lyoyindex" << endl <<  Lyoyindex << endl;
  //exit(1);


  Lfsel=0;
  //for(f=1;f<=fleet;f++)
  //{
  //  for(t=fmyear;t<=tblock;t++)
  //  {
  //    for(a=fage;a<=lage-3;a++)
  //    {
  //      Lfsel+=0.5*square(log_fsel(f,t,a+3)-3.*log_fsel(f,t,a+2)+3.*log_fsel(f,t,a+1)-log_fsel(f,t,a))/0.1; //smoothing penalty from Linton and Bence 2011
  //    }//close age loop
  //  }//close timeblock loop
  //  Lfsel+=0.01*norm2(log_fs(f));
  //}//close gleet loop

  Lssel=0;
  /*
  for(s=1;s<=nsurv;s++)
  {
    for(a=fage;a<=lage-3;a++)
    {
      Lssel+=0.5*square(log_ssel(s,a+3)-3.*log_ssel(s,a+2)+3.*log_ssel(s,a+1)-log_ssel(s,a))/0.1;
    }
    Lssel+=0.01*norm2(log_ss(s));
  }
  */

  pen_f2sel=0.;//slope of fish selectivity
  //pen_f2sel=0.5*norm2((log_fs(2,1)-mean(log_fs(2,1))))/0.25;
  
  if(!last_phase())
  {
    pen_F=0.5*square(sum(log_F)-log(0.1));
  }  

  pen_rdev=0.;
  pen_rdev+=norm2(log_Rdevs);//recruitment deviation penalty; assumes logscale variable of mean recruiment has SD 1 and mean of 0

  pen_fdev=0.;
  for(y=fmyear+1;y<=lmyear;y++)
   {
     pen_fdev+=1./square(0.5)*square(log_F1devs(y)-log_F1devs(y-1)); //penalizing the model for differences in F from one year to the next year    //pen_fdev+=1./square(0.5)*square(log_F2devs(y)-log_F2devs(y-1)); //penalizing the model for differences in F from one year to the next year
     pen_fdev+=1./square(0.5)*square(log_F2devs(y)-log_F2devs(y-1)); //penalizing the model for differences in F from one year to the next year    //pen_fdev+=1./square(0.5)*square(log_F2devs(y)-log_F2devs(y-1)); //penalizing the model for differences in F from one year to the next year
     //square 0.5 suggests that the penalty should have a SD of 0.5 on a normal distribution
  }
  
  //add all the components of the negative log likelihood together
  neg_LL=sum(Lcatch)+sum(Lcatchagecomp)+sum(Lindex)+sum(Lindexagecomp)+sum(Lage1index)+sum(Lyoyindex)+Lfsel+Lssel+pen_f2sel+pen_F+pen_rdev+pen_fdev; 
  /*
 cout << "neg_LL " << endl << neg_LL << " = " << sum(Lcatch) << " + " << sum(Lcatchagecomp) << " + " << sum(Lindexagecomp) << " + " << sum(Lage1index) << " + " << sum(Lyoyindex) <<
      " + " << Lfsel << " + " << Lssel << " + " << pen_F << " + " << pen_rdev << " + " << pen_fdev << endl <<endl;
 exit(1);
  */
  
FUNCTION dvariable lognorm_negLL(data_vector obsI, named_dvar_vector estI, dvector Ivar, int fmyear, int lmyear)
  dvariable negLL;
  negLL=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    if(obsI(y)!=-99)
    {
      negLL+=square((log(obsI(y)+0.01)-log(estI(y)+0.01)))/(2.*Ivar(y));
    }
  }
  //negLL=norm2(elem_div(log(obsI(fmyear,lmyear))-log(estI),(2.*Ivar)));
  return(negLL);
  
FUNCTION dvariable lognorm_negLL(dvector obsI, dvar_vector estI, dvector Ivar, data_int fmyear, data_int lmyear)
  dvariable negLL;
  negLL=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    if(obsI(y)!=-99)
    {
      negLL+=square((log(obsI(y)+0.01)-log(estI(y)+0.01)))/(2.*Ivar(y));
    }
  }
  //negLL=norm2(elem_div(log(obsI(fmyear,lmyear))-log(estI),(2.*Ivar)));
  return(negLL);

FUNCTION dvariable lognormyoy_negLL(dvector obsI, dvar_vector estI, dvector Ivar, data_int fmyear, data_int lmyear)
  dvariable negLL;
  negLL=0.0;
  for(y=fmyear;y<=lmyear-1;y++)
  {
    if(obsI(y)!=-99)
    {
      negLL+=square((log(obsI(y)+0.01)-log(estI(y)+0.01)))/(2.*Ivar(y));
    }
  }
  //negLL=norm2(elem_div(log(obsI(fmyear,lmyear))-log(estI),(2.*Ivar)));
  return(negLL);
  
FUNCTION dvariable lognormyoy_negLL(dvector obsI, dvar_vector estI, dvector Ivar, data_int fmyear, int lmyear)
  dvariable negLL;
  negLL=0.0;
  for(y=fmyear;y<=lmyear-1;y++)
  {
    if(obsI(y)!=-99)
    {
      negLL+=square((log(obsI(y)+0.01)-log(estI(y)+0.01)))/(2.*Ivar(y));
    }
  }
  //negLL=norm2(elem_div(log(obsI(fmyear,lmyear))-log(estI),(2.*Ivar)));
  return(negLL);

FUNCTION dvariable multinom_negLL(data_matrix obsP, named_dvar_matrix estP, named_dvar_matrix sigma2, int fage, int lage, int fyear, int lyear)
  dvariable negLL; //declaring new variable called negLL
  negLL=0.0;
  for(y=fyear;y<=lyear;y++)
  {
    for(a=fage;a<=lage;a++)
    {
      //L4+=-ESS_C*obs_Ip(y)*log(est_Ip(y)+0.001);
      if(obsP(y,a)!=-99)//runs likelihood if value is not equal to -99, accounting for missing values
      {
        negLL+=-log(mfexp(.5*(-square(obsP(y,a)-estP(y,a))/sigma2(y,a)))+0.01);
      }
    }
  }
  return(negLL);
  
FUNCTION dvariable multinom_negLL(dmatrix obsP, dvar_matrix estP, dvar_matrix sigma2, int fage, int lage, int fyear, int lyear)
  dvariable negLL; //declaring new variable called negLL
  negLL=0.0;
  for(y=fyear;y<=lyear;y++)
  {
    for(a=fage;a<=lage;a++)
    {
      //L4+=-ESS_C*obs_Ip(y)*log(est_Ip(y)+0.001);
      if(obsP(y,a)!=-99)//runs likelihood if value is not equal to -99, accounting for missing values
      {
        negLL+=-log(mfexp(.5*(-square(obsP(y,a)-estP(y,a))/sigma2(y,a)))+0.01);
      }
    }
  }
  return(negLL);

FUNCTION dvariable multinom_negLL(dmatrix obsP, dvar_matrix estP, dvar_matrix sigma2, data_int fage, data_int lage, int fyear, int lyear)
  dvariable negLL; //declaring new variable called negLL
  negLL=0.0;
  for(y=fyear;y<=lyear;y++)
  {
    for(a=fage;a<=lage;a++)
    {
      //L4+=-ESS_C*obs_Ip(y)*log(est_Ip(y)+0.001);
      if(obsP(y,a)!=-99)//runs likelihood if value is not equal to -99, accounting for missing values
      {
        negLL+=-log(mfexp(.5*(-square(obsP(y,a)-estP(y,a))/sigma2(y,a)))+0.01);
      }
    }
  }
  return(negLL);

FUNCTION dvariable multinom_negLL(data_matrix obsP, named_dvar_matrix estP, named_dvar_matrix sigma2, data_int fage, data_int lage, int fyear, int lyear)
  dvariable negLL; //declaring new variable called negLL
  negLL=0.0;
  for(y=fyear;y<=lyear;y++)
  {
    for(a=fage;a<=lage;a++)
    {
      //L4+=-ESS_C*obs_Ip(y)*log(est_Ip(y)+0.001);
      if(obsP(y,a)!=-99)//runs likelihood if value is not equal to -99, accounting for missing values
      {
        negLL+=-log(mfexp(.5*(-square(obsP(y,a)-estP(y,a))/sigma2(y,a)))+0.01);
      }
    }
  }
  return(negLL);



REPORT_SECTION
  
//The report section is used to write output to the standard output "filename.rep"
  report << "observed catch" << endl << obs_C << endl;
  report << "estimated catch" << endl << est_C << endl;
  report << "observed index" << endl << obs_I << endl;
  report << "estimated index" << endl << est_I << endl;
  report << "fishery selectivity" << endl << fsel << endl;
  report << "survey selectivity" << endl << ssel << endl;
  report << "abundance" << endl << N << endl;
  report << "fishing mortality" << endl << F << endl;
  //report << "biomass" << endl << B << endl;
  //report << "SSB" << endl << SSB << endl;
  /*
   ofstream catout("catch.txt");
  {
    catout << "fleet year logobs logpred" << endl;
    for(f=1;f<=fleet;f++)
    {
      for(y=fmyear;y<=lmyear;y++)
      {
        catout << f << " " << y << " " << log(obs_C(f,y)+0.00001) << " " << log(est_C(f,y)+0.00001) << endl;
      }
    }
  }
  
    ofstream ioaout("ioa.txt");
    {
          ioaout << "survey year obsioa predioa" << endl;
          for(s=1;s<=nsurv;s++)
          {
            for(y=fmyear;y<=lmyear;y++)
             {
                ioaout <<  s << " " << y << " " << log(obs_I(s,y)+0.00001) << " " << log(est_I(s,y)+0.00001) << endl;
             }//close year loop
           }//close survey loop
          
     }//close ofstream
        

   ofstream obscaa("ocaa.txt");//obs fishery catch at age
   {
     obscaa << "fleet year age obscaa estcaa standardresid" << endl;
     for(f=1;f<=fleet;f++)
     {
       for(y=fmyear;y<=lmyear;y++)
       {
         for(a=fage;a<=lage;a++)
         {
           obscaa << f << " " << y << " " << a << " " <<  log(obs_Cp(f,y,a)+0.001) << " " << log(est_Cp(f,y,a)+0.001) << " " <<  (obs_Cp(f,y,a)-est_Cp(f,y,a))/sqrt(C_var(f,y)) << endl;
         }
       }       
     }
   }

   ofstream obssaa("osaa.txt");
   {
     obssaa << "survey year age obssaa estsaa standardresid" << endl;
     for(s=1;s<=nsurv;s++)
     {
       for(y=fmyear;y<=lmyear;y++)
       {
         for(a=fage;a<=lage;a++)
         {
           if(obs_Ip(s,y,a)!=-99) obssaa << s  << " " << y << " " << a << " " <<  log(obs_Ip(s,y,a)+0.001) << " " <<  log(est_Ip(s,y,a)+0.001) << " " << (obs_Ip(s,y,a)-est_Ip(s,y,a))/sqrt(I_var(s,y)) << endl;
         }
       }
     }
     
   }//close ofstream

   ofstream fselout("fsel.txt");//fsel at age
   {
     fselout << "fleet year age fsel" << endl;
     for(f=1;f<=fleet;f++)
     {
       for(y=fmyear;y<=lmyear;y++)
       {
         for(a=fage;a<=lage;a++)
         {
           fselout << f  << " " << y << " " << a << " " << log(fsel(f,y,a)+0.001) << endl;
         }
       }
     }
   }

   ofstream sselout("ssel.txt");//survey proportions at age
   {
     sselout << "survey age ssel" << endl;
     for(s=1;s<=nsurv;s++)
     {
       for(a=fage;a<=lage;a++)
       {
         sselout << s << " " << a << " " << log(ssel(s,a)+0.001) << endl;
       }
     }
   }//close ofstream
   
   ofstream obsage1("age1.txt");
   {
     obsage1 << "survey year  obsyoy estyoy " << endl;
     for(o=1;o<=age1surv;o++)
     {
       for(y=fmyear;y<=lmyear;y++)
       {
           if(obs_I_age1(o,y)!=-99) obsage1 << o  << " " << y << " " <<  log(obs_I_age1(o,y)+0.001) << " " <<  log(est_I_age1(o,y)+0.001) << endl;
       }
     }
   }

   ofstream obsyoy("yoy.txt");
   {
     obsyoy << "survey year  obsyoy estyoy " << endl;
     for(r=1;r<=yoysurv;r++)
     {
       for(y=fmyear;y<=lmyear-1;y++)
       {
           if(obs_I_yoy(r,y)!=-99) obsyoy << r  << " " << y << " " <<  log(obs_I_yoy(r,y)+0.001) << " " <<  log(est_I_yoy(r,y)+0.001) << endl;
       }
     }
   }

   ofstream pop("pop.txt");// pop across years
   {
      pop << "year age  pop " << endl; 
      for(y=fmyear;y<=lmyear;y++)
      {
          for(a=fage;a<=lage;a++)
          {
           pop << " " << y << " " << " " << a << " " << N(y,a) << endl;
          }
      }
   }
  
   ofstream mort("f.txt");// pop across years
   {
      mort << "fleet year age  f " << endl; 
      for(f=1;f<=fleet;f++)
      {
           for(y=fmyear;y<=lmyear;y++)
           {
                  for(a=fage;a<=lage;a++)
                  {
                  mort << " " << f << " " << y << " " << " " << a << " " << F(f,y,a) << endl;
                  }
           }
      }
   }

   ofstream bio("bio.txt");// pop across years
   {
      bio << "year age  bio " << endl; 
      for(y=fmyear;y<=lmyear;y++)
      {
          for(a=fage;a<=lage;a++)
          {
           bio << " " << y << " " << " " << a << " " << B(y) << endl;
          }
      }
   }
   ofstream ssb("ssb.txt");// pop across years
   {
      ssb << "year estssb " << endl; 
      for(y=fmyear;y<=lmyear;y++)
      {
         ssb << " " << y << " " << SSB(y) << endl;
      }
   }
  */
FINAL_SECTION

  //CALCULATE SPR
  //fishing mortality in equillibrium = to last year of time series
  for(f=1;f<=fleet;f++)
  {
    for(a=fage;a<=lage;a++)
    {
      F_eq(f,a)=F(f,30,a);//fsel for ocean region,last year
    }//close age loop
  }//close fleet loop

  //calculate Fa, sum of all F for each age
  Fa=0.;//initialize
  for(f=1;f<=fleet;f++)
  {
    for(a=fage;a<=lage;a++)
    {
      Fa(a)+=F_eq(f,a);
    }//close age loop
  }//close gleet loop

  //calculate V, which is proporational F
  for(f=1;f<=fleet;f++)
  {
    for(a=fage;a<=lage;a++)
    {
      V(f,a)=F_eq(f,a)/max(Fa);
    }//close age loop
  }//close gleet loop

  R_eq=1.; //setting recruitment equal to one since there is only one stock

  SSBf=0.; //initializ
  
  for(G=0;G<=150;G++)  //looping through f's 
  {
    G_spr=0.01*double(G);  //Fishing mort SPR is 0.01*f
    //calculate Fstar, which calls on G

    for(f=1;f<=fleet;f++)
    {
      for(a=fage;a<=lage;a++)
      {
        Fstar(f,a)=G_spr*V(f,a); //G is a scalar so that g=0 is the unfished condition
      }//close age loop
    } //close fleet loop

    //calculate population dynmaics

    //equillibrium mortality
    Zf=0.; //setting Z = 0 to start
    for(f=1;f<=fleet;f++)
    {
      for(a=fage;a<=lage;a++)
      {
        Zf(a)+=Fstar(f,a)+M(a);
      } //close age loop
    }//close fleet loop

    //dynamics in year 1
    Nf(fmyear,fage)=R_eq; //setting recruitment in the first year
    for(a=2;a<=lage;a++)  //looping through the ages
    {
      Nf(fmyear,a)=Nf(fmyear,a-1)*mfexp(-Zf(a-1));   //equillibrium population dynamics
    }//close age loop
    Nf(fmyear,lage)/=1-mfexp(-Zf(lage));

    for(y=2;y<=years;y++)
    {
      Nf(y,fage)=R_eq; //recryutnebt is constant
      for(a=2;a<=lage;a++)
      {
        Nf(y,a)=Nf(y-1,a-1)*mfexp(-Zf(a-1)); 
      }//close age loop
    } //close yr loop
    //cout<<Nf<<endl;
    SSBf(G)=Nf(100)*elem_prod(sex,(elem_prod(rw_age(30),m_age))); //rw_age only used in the last year, pick year 75 to be same across estimation models
    SPR(G)=SSBf(G)/SSBf(0);
  }  
  //cout << SPR << endl;

  //Find F associated with appropriate reference point (F35%)
  G=0;
  while(SPR(G)>0.4 && G<150)
  {
    G++;
    //cout<<double(f)*0.01<< " " << SPR(f)<< endl;
  }
    slope=(SPR(G)-SPR(G-1))/0.01;
    est_G_40=((0.4-SPR(G))+slope*double(G)*.01)/slope; //this is the estimated g value


  //plug g back in to get f values
    for(f=1;f<=fleet;f++)
    {
      for(a=fage;a<=lage;a++)
      {
        F_bar(f,a)=est_G_40*V(f,a);//*(Nf(100,a)/sum(Nf(100))); //use the G value
      }//close age loop
    } //close fleet loop

    //sum across fleets/age to get F_spr
    est_F_spr=F_bar(1,8)+F_bar(2,8);//add fbars at age 8 to compare to other values

  
  // REPORT OUT
  ofstream ofs("sim_results.txt",ios::app);
  {
    for(y=fmyear;y<=lmyear;y++)
    {
     ofs << " " << sim_num << " "  << y << " " << est_F(y,8) << " " << (est_F(y,8)-true_F(y,8))/true_F(y,8) << " " <<
     est_Ntot(y) << " " << (est_Ntot(y)-true_Ntot(y))/true_Ntot(y) << " " <<
     est_biomass(y) << " " << (est_biomass(y)-true_biomass(y))/true_biomass(y) << " " <<
     est_SSB(y) << " " << (est_SSB(y)-true_SSB(y))/true_SSB(y) << " " <<
     est_F_spr << " " << (est_F_spr-true_F_spr)/true_F_spr << "  " << 
     log_R << " " << (log_R-start_log_R)/log_R << " " << objective_function_value::pobjfun->gmax<<endl;
     }
  }
  ofstream ofsf("sim_F_results.txt",ios::app);
  {
    for(y=fmyear;y<=lmyear;y++)
    {
     ofsf << " " << sim_num << " " << y << " " << est_F(y) << " " << elem_div((est_F(y)-true_F(y)),true_F(y)) << " " << objective_function_value::pobjfun->gmax<<endl;
    }
  }
RUNTIME_SECTION

  maximum_function_evaluations 20000, 20000, 20000 //change the maximum number of iterations for each phase
  
  

  
  


