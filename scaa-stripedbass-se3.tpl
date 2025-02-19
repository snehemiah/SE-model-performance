//Program Name/Description:
//Simulation Code of Spatially explicit statistical catch at age model 
//Written By: Samara Nehemiah
//Date Created: 10/30/2023
//Last Modified By: Samara
//Last Modified:3/25/2024



TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(800);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(1000000);
  arrmblsize=900000;

DATA_SECTION


  init_int fdyear  //first year for data
  init_int ldyear  //last year for data

  init_int fmyear  //first year for model
  init_int lmyear  //last year for model

  init_int fage  //first age
  init_int lage  //last age

  init_int nyrs //number of years in survey

  init_int fleet //number of fleets (coast and bay)
  init_int region //number of spatial regions
  init_int tstep //number of time steps in a year (jan-jun and jul-dec)
  init_int stock //number of stocks we are modeling (Ches Bay and Other)
  init_int nsurv //number of surveys, separated by time step = 9
  
  init_int age1surv_coast //number of age 1 survey in FAA model
  init_int age1surv_bay
  //init_int nage1 //number of age 1 in spatial model
  init_int yoysurv_coast //number of yoy survey in the coast
  init_int yoysurv_bay //number of yoy surveys in the bay

  init_int tblock //number of time blocks for fishing selectivity
  init_vector ftyear(1,tblock) //first year of each time block

  init_number lbound //lower bound of log_fs for cb
  init_number ubound //upper bound of log_fs for cb

  //starting parameters
  init_vector start_log_R(1,stock) //starting parameter for log recruitment
  init_matrix start_log_F(1,region,1,tstep) //starting parameters for mean log f for each region
  init_vector start_log_Feq(1,region) //starting parameters for F in the first year

  init_vector start_log_sf1_ac(1,tstep) //starting parameter for slope of selectivity of ac fishery
  init_vector start_log_sf2_ac(1,tstep) //starting parameter for age at 50% sel of ac fishery
  init_vector start_log_sf1_cb(1,tstep) //starting parameter for slope of selectivity of cb fishery
  init_vector start_log_sf2_cb(1,tstep) //starting parameter for age at 50% sel of cb fishery
  init_vector start_log_sf3_cb(1,tstep) //starting parameter for slope of selectivity of cb fishery
  init_vector start_log_sf4_cb(1,tstep) //starting parameter for age at 50% sel of cb fishery

  init_vector start_log_ssf1_coast(1,tstep) //starting parameter for coast survey selectivity slope
  init_vector start_log_ssf2_coast(1,tstep) //starting parameter for coast survey selectivity age at 50% sel
  init_vector start_log_ssf3_coast(1,tstep) //starting parameter for coast survey sel, slope of descending limb
  init_vector start_log_ssf4_coast(1,tstep) //starting parameter for caost survey sel, 50%sel of descending limb
  init_vector start_log_ssf1_bay(1,tstep) //starting parameter for bay survey selectivity slope
  init_vector start_log_ssf2_bay(1,tstep) //starting parameter for bay survey selectivity age at 50% sel

  init_number start_log_q_coast //starting parameter for q for 1+ survey in coast
  init_number start_log_q_bay //starting parameter for q for 1+ survey in bay
  init_number start_log_qage1_coast //starting parameter for q for age 1 survey in coast
  init_number start_log_qage1_bay //starting parameter for q for age 1 survey in bay
  init_number start_log_qyoy_coast //starting parameter for q for yoy survey in coast
  init_number start_log_qyoy_bay //starting parameter for q for yoy survey in bay

  init_number start_log_a_sf1 //starting params for slope of afsel
  init_number start_log_a_sf2 //starting param for age at 50% of afsel
  
  //data inputs
  init_3darray obs_C(1,region,1,tstep,fdyear,ldyear)             //observed total catch for each year
  init_4darray obs_Cp(1,region,1,tstep,fdyear,ldyear,fage,lage) //observed proportions at age in the catch
  init_3darray obs_CV(1,region,1,tstep,fdyear,ldyear)           //coefficient of variation

   //spatial observed index of abundance
  init_matrix obs_I_bay(1,tstep,fdyear,ldyear)
  init_matrix obs_I_coast(1,tstep,fdyear,ldyear)
    
  //spatial observed proportions at age for the survey
  init_3darray obs_Ip_bay(1,tstep,fdyear,ldyear,fage,lage)
  init_3darray obs_Ip_coast(1,tstep,fdyear,ldyear,fage,lage)
  
  //spatial observed CV for the survey
  init_matrix obs_I_CV_bay(1,tstep,fdyear,ldyear)//using same CV for both timesteps
  init_matrix obs_I_CV_coast(1,tstep,fdyear,ldyear)//using same CV for both timesteps

  init_vector obs_I_age1_bay(fdyear,ldyear) //observed indices of abudnace for age 1 newyork
  init_vector obs_I_age1_coast(fdyear,ldyear)
  init_vector obs_I_age1_CV_bay(fdyear,ldyear) //observed CV for age 1 surveys
  init_vector obs_I_age1_CV_coast(fdyear,ldyear) //observed CV for age 1 surveys

  init_vector obs_I_yoy_bay(fdyear,ldyear) //observed yoy indices of abundance for bay
  init_vector obs_I_yoy_coast(fdyear,ldyear)//observed yoy indices of abudance for the coast
  init_vector obs_yoy_CV_bay(fdyear,ldyear) //observed yoy indices of abundance for bay
  init_vector obs_yoy_CV_coast(fdyear,ldyear)//observed yoy indices of abudance for the coast

  init_matrix w_age(fdyear,ldyear,fage,lage) //average weight at age, in kG
  init_matrix ssbw_age(fdyear,ldyear,fage,lage) //adjustment of rivard weight to match the time of spawning
  init_matrix rw_age(fdyear,ldyear,fage,lage) //rivard weight at age
  init_vector m_age(fage,lage) //maturity at age
  init_vector M(fage,lage) //assumed natural mortality for all ages
  init_vector sex(fage,lage) //sex proportions at age

  init_matrix prop_bay(1,tstep,fage+1,lage)//log proportion at age for chesapeake bay stock in the Atlantic Coast
  init_matrix prop_coast(1,tstep,fage+1,lage)//log proportion at age for atlantic coast  stock in the Atlantic Coast

  init_matrix log_sd_bay(1,tstep,fage+1,lage) //bay log sd for occupancy probabilities
  init_matrix log_sd_coast(1,tstep,fage+1,lage) //coast log sd for occupancy probabilties

  //aging error matrix
  init_number use_age_err //1, use aging err matrix, 2, use identify matrix

  init_matrix age_err_a(fage,lage,fage,lage) //aging error ages 1-15
  init_matrix age_err_a_id(fage,lage,fage,lage) //identify matrix for age 1-15+
  
  //init_number C_sd //log-scale Standard Deviation for total catch
  init_matrix ESS_C(1,region,1,tstep)//effective sample size for catch at age
  //init_number I_sd //log-scale SD for index of abundance

  //effective sample size for the survey index at age
  init_vector ESS_I_coast(1,tstep)
  init_vector ESS_I_bay(1,tstep)

  //switchest for likelihood
  init_int use_pen_prop

  init_int sim_num //simulation number

  init_vector true_Ntot(fmyear,lmyear) //storing the true values of Jan 1 abundance
  init_matrix true_Ntot_stock(1,stock,fmyear,lmyear)//storing true values of stock specific abundance
  init_vector true_biomass(fmyear,lmyear) //storing the true values of Jan1 biomas
  init_vector true_SSB(fmyear,lmyear) //storing the true values of Jan 1 SSB
  //init_vector true_F_year(fmyear,lmyear) //storing true values for total F every year
  init_vector true_F_spr(1,stock) //true F that acheives 40% spr for each stock
  init_number true_annual_F
  init_matrix true_F(fmyear,lmyear,fage,lage) //stroing true values of Jan 1 F for each age
  

  init_number test // EOF test number

  3darray C_var(1,region,1,tstep,fmyear,lmyear)  //log-scale variance of catch
  
  //spatial variance
  matrix I_var_bay(1,tstep,fmyear,lmyear) //log-scale variance of index of abundancefor ches bay
  matrix I_var_coast(1,tstep,fmyear,lmyear) //log scale variance of atl coast

  vector I_var_age1_bay(fmyear,lmyear)//log-scale variance of index of abundance, age 1 surv, FAA
  vector I_var_age1_coast(fmyear,lmyear)//log-scale variance of index of abundance, age 1 surv, FAA
 
  vector I_var_yoy_bay(fmyear,lmyear)
  vector I_var_yoy_coast(fmyear,lmyear) //log-scale varience of yoy ioa for the caost

  matrix mod_age_err_a(fage,lage,fage,lage)

  int a // looping variable for age
  int y // looping variable for year
  int s // looping variable for survey
  int r // looping variable for region(previously fleet)
  int o //looping variable for age 1 survey
  int z // looping variable for yoy survey
  int t // looping variable for time block (fishing reg)
  int t2 // variable for calculation of abundance across time steps
  int ts // loooping variable for time step (jan-jun; jul-dec)
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
    cout << "num years" << endl << nyrs << endl;
    cout << "fleet" << endl << fleet << endl;
    cout << "region" << endl << region << endl;
    cout << "tstep" << endl << tstep << endl;
    cout << "stock" << endl << stock << endl;
    cout << "nsurv" << endl << nsurv << endl;
    cout << "age1surv_coast" << endl <<  age1surv_coast << endl;
    cout << "age1surv_bay" << endl <<  age1surv_bay << endl;
    //out << "nage1" << endl << nage1 << endl;
    cout << "yoy surv coast"  << endl << yoysurv_coast << endl;
    cout << "yoysurv bay" << endl << yoysurv_bay << endl;
    cout << "tblock" << endl << tblock << endl;
    cout << "first tblock yr" << endl << ftyear << endl;
    cout << "obs catch" << endl << obs_C << endl;
    cout << "obs prop at age in catch" << endl << obs_Cp << endl;
    cout << "obs cv" << endl << obs_CV << endl;
    cout << "obs I coast" << endl << obs_I_coast << endl;
    cout << "obs I bay" << endl << obs_I_bay << endl;
    cout << "obs prop coast surv" << endl << obs_Ip_coast << endl;
    cout << "obs prop bay surv" << endl << obs_Ip_bay << endl;
    cout << "obs cv coast" << endl << obs_I_CV_coast << endl;
    cout << "obs cv bay" << endl << obs_I_CV_bay << endl;
    cout << " obs age 1 bay" << endl <<obs_I_age1_bay << endl;
    cout << " obs age 1 coast" << endl <<obs_I_age1_coast << endl;
    cout << "obs age 1 cv bay " << obs_I_age1_CV_bay << endl;
    cout << "obs age 1 cv coast" << obs_I_age1_CV_coast << endl;
    cout << "obs I yoy coast " << endl <<  obs_I_yoy_coast << endl;
    cout << "obs I yoy bay " << endl <<  obs_I_yoy_bay << endl;
    cout << "obs I yoy cv coast" << endl << obs_yoy_CV_coast << endl;
    cout << "obs I yoy cv bay " << endl <<  obs_yoy_CV_bay << endl;
    cout << "avg weight" <<  w_age << endl;
    cout << "ssbw" << endl <<ssbw_age << endl; //adjustment of rivard weight to match the time of spawning
    cout << "rivard weight" << endl << rw_age << endl; //rivard weight at age
    cout << "mat at age" << endl <<  m_age << endl;
    cout << "M " << endl << M << endl;
    cout << "sex prop" << endl <<  sex << endl;
    cout << "prop cb" << endl << prop_bay << endl;
    cout << "prop ac" << endl << prop_coast << endl;
    cout << "age err a" << endl << age_err_a << endl;
    cout << "test" << endl << test << endl;
    exit(1);  //exit the program
  }
  //cout << I_var << endl;
  //convert SDs to variances so that it only has to be done once
  for(r=1;r<=region;r++)
  {
    for(t=1;t<=tstep;t++)
    {
      //cout << r << endl;
      //cout << t << endl;
      //cout << fmyear << endl;
      //cout << lmyear << endl;
      //cout << C_var(r,t) << endl;
      C_var(r,t)=square(obs_CV(r,t)(fmyear,lmyear));//calculate variance from standard deviations
      //cout << obs_CV(r,t)(fmyear,lmyear) << endl;
      //cout << C_var(r,t) << endl;
    }//close tstep
  }//close region 
  //cout << "end C_var loop"  << endl;
  
  //convert spatial SD to variance
  //I_var_coast(t)=square(obs_I_CV_coast(1,fmyear,lmyear));
  //I_var_bay=square(obs_I_CV_bay(1,tstep,fmyear,lmyear));
  //I_var_des=square(obs_I_CV_des(fmyear,lmyear));
  //I_var_nj=square(obs_I_CV_nj(fmyear,lmyear));
  //I_var_ny=square(obs_I_CV_ny(fmyear,lmyear));
  for(t=1;t<=tstep;t++)
  {
    I_var_coast(t)=square(obs_I_CV_coast(t)(fmyear,lmyear));
    I_var_bay(t)=square(obs_I_CV_bay(t)(fmyear,lmyear));
  }
  //cout << "end Ivar loop" << endl;

  I_var_age1_bay=square(obs_I_age1_CV_bay(fmyear,lmyear));
  I_var_age1_coast=square(obs_I_age1_CV_coast(fmyear,lmyear));
  
  I_var_yoy_coast=square(obs_yoy_CV_coast(fmyear,lmyear));
  I_var_yoy_bay=square(obs_yoy_CV_bay(fmyear,lmyear));
  

  if(use_age_err==1)
  {
    mod_age_err_a=age_err_a;
      }
  else
  {    
    mod_age_err_a=age_err_a_id;
      }
  //cout << "aging error a" << endl << mod_age_err_a << endl;
  //cout << "aging error b" << endl << mod_age_err_b << endl;
  //cout << "aging error c" << endl << mod_age_err_c << endl;
  //cout << use_age_err << endl ;
  //cout << "mod age err" << endl << mod_age_err_a << endl;
  //exit(1);
  //cout << "finishvar calc " << endl;

  /*
  //test starting parameters
  cout << "start log R" << endl <<  start_log_R << endl; //starting parameter for log recruitment
  cout << "start log F" << endl << start_log_F << endl; //starting parameters for mean log f for each region
  cout << "start log Feq" << endl <<  start_log_Feq << endl; //starting parameters for F in the first year
  cout << "start log sf1 ac" << endl <<  start_log_sf1_ac <<  endl; //starting parameter for slope of selectivity of ac fishery
  cout << "start log sf2 ac" << endl <<  start_log_sf2_ac << endl;  //starting parameter for age at 50% sel of ac fishery
  cout << "start log sf1 cb" << endl << start_log_sf1_cb << endl; //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf2 cb" << endl << start_log_sf2_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log sf3 cb" << endl << start_log_sf3_cb << endl;  //starting parameter for slope of selectivity of cb fishery
  cout << "start log sf4 cb" << endl << start_log_sf4_cb << endl; //starting parameter for age at 50% sel of cb fishery
  cout << "start log ssf1 coast" << endl << start_log_ssf1_coast << endl; //starting parameter for coast survey selectivity slope
  cout << "start log ssf2 coast" << endl << start_log_ssf2_coast << endl; //starting parameter for coast survey selectivity age at 50% sel
  cout << "start log ssf3 coast" << endl << start_log_ssf3_coast << endl; //starting parameter for coast survey sel, slope of descending limb
  cout << "start log ssf4 coast" << endl << start_log_ssf4_coast << endl; //starting parameter for caost survey sel, 50%sel of descending limb
  cout << "start log ssf1 bay" << endl << start_log_ssf1_bay << endl; //starting parameter for bay survey selectivity slope
  cout << "start log ssf2 bay" << endl <<  start_log_ssf2_bay << endl; //starting parameter for bay survey selectivity age at 50% sel
  cout << "start log q coast" << endl <<  start_log_q_coast << endl;  //starting parameter for q for 1+ survey in coast
  cout << "start log q bay" << endl <<start_log_q_bay << endl; //starting parameter for q for 1+ survey in bay
  cout << "start log qage1 coast" << endl << start_log_qage1_coast << endl; //starting parameter for q for age 1 survey in coast
  cout << "start log age1q bay" << endl << start_log_qage1_bay << endl; //starting parameter for q for age 1 survey in bay
  cout << "start log qyoy coast" << endl << start_log_qyoy_coast << endl; //starting parameter for q for yoy survey in coast
  cout << "start log qyoy bay" << endl << start_log_qyoy_bay << endl;  //starting parameter for q for yoy survey in bay
  cout << "start log prop bay" << endl << prop_bay << endl;
  cout << "start log prop coast" << endl << prop_coast << endl;
  cout << "Obs C" << endl << obs_C << endl;            //observed total catch for each year
  cout << "Obs Cp" << endl <<  obs_Cp << endl; //observed proportions at age in the catch
  cout << "Obs CV" << endl << obs_CV << endl;          //coefficient of variation
  cout << "Obs I bay" << endl << obs_I_bay << endl;
  cout << "Obs I coast" << endl << obs_I_coast << endl;
  cout << "Obs Ip bay" << endl << obs_Ip_bay << endl;
  cout << "Obs Ip coast" << endl << obs_Ip_coast << endl;
  cout << "Obs I CV bay" << endl << obs_I_CV_bay << endl;
  cout << "Obs I cv coast" << endl << obs_I_CV_coast << endl;
  cout << "Obs I age1 bay" << endl << obs_I_age1_bay << endl; //observed indices of abudnace for age 1 newyork
  cout << "Obs I age1 coast" << endl << obs_I_age1_coast << endl;
  cout << "Obs I age1 CV" << endl << obs_I_age1_CV_bay << endl; //observed CV for age 1 surveys
  cout << "Obs I age1 CV" << endl << obs_I_age1_CV_coast << endl;
  cout << "Obs 1 yoy bay" << endl << obs_I_yoy_bay << endl; //observed yoy indices of abundance for bay
  cout << "obs 1 yoy coast" << endl << obs_I_yoy_coast << endl; //observed yoy indices of abudance for the coast
  cout << "obs yoy cv bay" << endl << obs_yoy_CV_bay << endl;  //observed yoy indices of abundance for bay
  cout << "obs yoy cv coast" << endl << obs_yoy_CV_coast << endl; //observed yoy indices of abudance for the coast
  //exit(1);
  

  
  //cout << I_var << endl;
  //cout << "data years" << endl << fdyear << endl << ldyear << endl;
  //cout << "model years" << endl << fmyear << endl << lmyear << endl;
  //cout << "ages" << endl << fage << endl << lage << endl;
  //for(r=1;r<=region;r++)
  // {
  //  for(ts=1;ts<=2;ts++)
  //  {
  //    cout << "obs catch" << endl << " r=" << r << " ts=" << ts << endl << obs_C(r,ts) << endl;
  //    cout << "obs prop at age in catch" << endl << " r=" << r << " ts=" << ts << endl << obs_Cp(r,ts) << endl;
  //  }
  //}
  //cout << "obs md index" << endl << obs_I_md << endl;
  //cout << "obs cm  index" << endl << obs_I_cm << endl;
  //cout << "obs md prop at age in survey" << endl << obs_Ip_md << endl;
  //cout << "obs cm prop at age in survey" << endl << obs_Ip_cm << endl;
  //exit(1);

  //cout << "sim num" << endl << sim_num <<endl;
  //cout << "true_Ntot" << endl << true_Ntot <<endl;
  //cout << "true_SSB" << endl << true_SSB <<endl;
  //cout << "true_F" << endl << true_F <<endl;
  //exit(1);
  */

 END_CALCS

PARAMETER_SECTION
  //The parameter section is where we specify the parameters to be estimated (init)
  //and any other values that will depend on the estimated parameters (i.e., variables)
  
  //parameters to determine numbers at age in first year
  //init_number log_N0(-1) //log of mean abundance at age in the first year
  init_bounded_dev_vector log_N0_devs(fage+1,lage,-10,10,-1)//deviations of abudnace at age for the first year

  //*******************
  //fishery selectivity
  //*******************

  init_bounded_matrix log_sf1_ac(1,tblock,1,tstep,-2,2,1) // slope of the logistic function for fsel in the chesapeake bay
  matrix sf1_ac(1,tblock,1,tstep) //arctanget transformation for the slope of logistic function for fishery selectivity
  init_bounded_matrix log_sf2_ac(1,tblock,1,tstep,0,5,1) // 50% selectivity param  of the logistic function for fsel in the chesapeake bay
  matrix sf2_ac(1,tblock,1,tstep) //arctanget transformation for the 50% selectivity param of logistic function for fishery selectivity 

  //double logistic parameters for fishery in the chesapeake bay
  init_bounded_matrix log_sf1_cb(1,tblock,1,tstep,-2,2,1)
  matrix sf1_cb(1,tblock,1,tstep)
  init_bounded_matrix log_sf2_cb(1,tblock,1,tstep,0,5,1)
  matrix sf2_cb(1,tblock,1,tstep)
  init_bounded_matrix log_sf3_cb(1,tblock,1,tstep,-2,2,1)
  matrix sf3_cb(1,tblock,1,tstep)
  init_bounded_matrix log_sf4_cb(1,tblock,1,tstep,0,5,1)
  matrix sf4_cb(1,tblock,1,tstep)

  //*******************
  //survey selectivity
  //*******************
  
  //double logisitc  parameters for coast
  init_bounded_vector log_ssf1_coast(1,tstep,-2,3,1) //log of the slope for coast surveys
  vector ssf1_coast(1,tstep) //slope of the coast survey
  init_bounded_vector log_ssf2_coast(1,tstep,0,5,1) //log of the 50% selected for the coast survey
  vector ssf2_coast(1,tstep) //50% sel for the coast survey
  init_bounded_vector log_ssf3_coast(1,tstep,-2,2,1) //log of the slope for  coast survey
  vector ssf3_coast(1,tstep) //slope of  coast survey
  init_bounded_vector log_ssf4_coast(1,tstep,1,5,1) //log of the 50% sel for  coast survey
  vector ssf4_coast(1,tstep) //50% sel for  coast survey

   // logistic parameters for bay survey
  init_bounded_vector log_ssf1_bay(1,tstep,-2,2,1) //log of the slope for  bay survey
  vector ssf1_bay(1,tstep) //slope of  bay survey
  init_bounded_vector log_ssf2_bay(1,tstep,0,5,1) //log of the 50% sel  bay survey
  vector ssf2_bay(1,tstep) //50% sel for  bay survey
  //init_bounded_vector log_ssf3_bay(1,tstep,-2,2,3) //log of the slope for  bay survey
  //vector ssf3_bay(1,tstep) //slope of  bay survey
  //init_bounded_vector log_ssf4_bay(1,tstep,1,5,3) //log of the 50% sel for  bay survey
  //vector ssf4_bay(1,tstep) //50% sel for  bay survey

  init_bounded_number log_a_sf1(-2,2,1)//log of slope of afsel
  init_bounded_number log_a_sf2(0,5,1) //log of 50% selc for afsel
  number a_sf1 //starting params for slope of afsel
  number a_sf2 //starting param for age at 50% of afsel
  
  init_vector log_q_coast(1,tstep,1) //log of coast catchability
  init_vector log_q_bay(1,tstep,1) //log of bay surv catchability
    
  vector q_coast(1,tstep) //coast catchability
  vector q_bay(1,tstep) //bay catchability
 
  init_number log_q_age1_coast(1)    //log catchability for age1 surveys
  init_number log_q_age1_bay(1)
  init_number log_q_yoy_coast(1)      //log YOY catchability for coast 
  init_number log_q_yoy_bay(1)        //log YOY catchability for bay 
  
  //Recruitment
  init_bounded_vector log_R(1,stock,0,20,1) //mean of log recruitment
  init_bounded_dev_vector log_Rdevs1(fmyear,lmyear,-10,10,1)// rdevs for the bay stock bounded by log(-10) and log(10)  
  init_bounded_dev_vector log_Rdevs2(fmyear,lmyear,-10,10,1)// rdevs for the coast stock bounded by log(-10) and log(10)  

  //Fishing mortality
  init_bounded_vector log_Feq(1,stock,-5,2,1) //logF in the first year
  init_bounded_matrix log_F(1,region,1,tstep,-5,2,1) //mean of log F
  init_bounded_dev_vector log_Fdevs_r1t1(fmyear,lmyear,-15,15,1)//F devs for region 1;time step 1, phase 2
  init_bounded_dev_vector log_Fdevs_r1t2(fmyear,lmyear,-15,15,1)//F devs for region 1;time step 2, phase 2
  init_bounded_dev_vector log_Fdevs_r2t1(fmyear,lmyear,-15,15,1)///F devs for region 2;time step 1, phase 2
  init_bounded_dev_vector log_Fdevs_r2t2(fmyear,lmyear,-15,15,1)//F devs for region 2;time step 2, phase 2
  4darray Freg_4plus(1,region,1,tstep,fmyear,lmyear,fage,lage) //Regional fishing mortality for ages 4 and older
  3darray Fbar(1,stock,fmyear,lmyear,fage,lage) // stock specific fishing mortality for year age
  matrix Fplus(1,stock,fmyear,lmyear) //stock specfic fishing mortality for age 7 and older
  matrix est_F(fmyear,lmyear,fage,lage)//total F for each region/stcok Jan1
  vector est_F_year(fmyear,lmyear)
  
  //set up what we are calculating for population model
  matrix Feq(1,region,fage,lage) //F in equillibrium in the first year
  4darray Nt(1,stock,1,tstep,fmyear,lmyear,fage,lage) // Nt is total abundance at age
  5darray N(1,stock,1,region,1,tstep,fmyear,lmyear,fage,lage) //N is abundance at age in each region
  4darray N_region(1,region,1,tstep,fmyear,lmyear,fage,lage)
  3darray Nbay(1,tstep,fmyear,lmyear,fage,lage)//Nbay is the abundance at age in the bay for each time step
  3darray Ncoast(1,tstep,fmyear,lmyear,fage,lage)//Ncost is the abundance at age in the coast for each time step
  vector est_Ntot(fmyear,lmyear) //total abundance Jan 1
  matrix est_Ntot_stock(1,stock,fmyear,lmyear) //stock specific total jan1 abundance
  4darray Z(1,region,1,tstep,fmyear,lmyear,fage,lage) ///total mortality rate at age
  4darray F(1,region,1,tstep,fmyear,lmyear,fage,lage) //fishing mortality rate at age


  4darray fsel(1,region,1,tstep,fmyear,lmyear,fage,lage) //fishery selectivity at age
  4darray log_fsel(1,region,1,tstep,fmyear,lmyear,fage,lage) //log of fsel
  matrix afsel(1,region,fage,lage) //average fsel across both regions in the first year at equillibrium
  matrix log_afsel(1,region,fage,lage) //log of afsel

  matrix ssel_coast(1,tstep,fage,lage) // surv sel for coast surveys
  matrix ssel_bay(1,tstep,fage,lage) //surv sel for bay surveys
 
  number q_age1_coast //age 1 survey
  number q_age1_bay   //age 1 survey
  //number q_age1  //age 1surveys
  number q_yoy_coast // coast yoy catchability
  number q_yoy_bay    //bay yoy catchability

   //observation model
  4darray est_C(1,stock,1,region,1,tstep,fmyear,lmyear) //estimated total catch by stock for each year, timestep, region,
  3darray est_region_C(1,region,1,tstep,fmyear,lmyear) //estimated total catch for each region, no stock 
  5darray est_C_age(1,stock,1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age
  5darray est_C_age_err(1,stock,1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age with aggin error matrix included
  4darray est_totC_age(1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age for each region
  4darray est_totC_age_err(1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age for each region with age error
  5darray est_Cp(1,stock,1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated proportions at age in the catch by stock
  4darray est_region_Cp(1,region,1,tstep,fmyear,lmyear,fage,lage) //estimated proportions at age by region
  4darray sigma2_Cp(1,region,1,tstep,fmyear,lmyear,fage,lage) //variance of estimate proportions at age in the catch

  //*****************************
  //chesapeake bay
  //*****************************
  
  3darray est_I_age_bay(1,tstep,fmyear,lmyear,fage,lage)//estimated catch at age, ches bay
  3darray est_I_age_bay_err(1,tstep,fmyear,lmyear,fage,lage)//estimated catch at age, ches bay
  3darray est_Ip_bay(1,tstep,fmyear,lmyear,fage,lage)//estimate proportions at age, ches bay
  matrix est_I_bay(1,tstep,fmyear,lmyear)//estimated aggregate index of abundance for ches bay survey
  3darray sigma2_Ip_bay(1,tstep,fmyear,lmyear,fage,lage)//variance of est prop at age 


  //*****************************
  //atlantic coast
  //*****************************

  3darray est_I_age_coast(1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age, atl coast
  3darray est_I_age_coast_err(1,tstep,fmyear,lmyear,fage,lage) //estimated catch at age,with aging error
  3darray est_Ip_coast(1,tstep,fmyear,lmyear,fage,lage)//estimated proportions at age,atl coast
  matrix est_I_coast(1,tstep,fmyear,lmyear)//estimated aggregate index of abundance for atl coast
  3darray sigma2_Ip_coast(1,tstep,fmyear,lmyear,fage,lage)//variance of est prop at age atl coast


  //Age 1 and YOY surveys
  vector est_I_age1_coast(fmyear,lmyear) //  age 1 estimated index of abundace for the atl cosat
  vector est_I_age1_bay(fmyear,lmyear)
  vector est_I_yoy_coast(fmyear,lmyear)   // estimated index of abundance for coast YOY
  vector est_I_yoy_bay(fmyear,lmyear)     //estimated index of abundance for bay YOY

  matrix rw(fmyear,lmyear,fage,lage) //estimate rivard weight
  matrix SSB_w(fmyear,lmyear,fage,lage) //estimated SSBweight at age, adjusted from rivard weight

  3darray B(1,stock,1,region,fmyear,lmyear) //estimated biomass for each year
  vector est_biomass(fmyear,lmyear) //total biomass for each year
  3darray SSB(1,stock,1,region,fmyear,lmyear)  //estimated spawning stock biomass for each year
  vector est_SSB(fmyear,lmyear) //total ssb for each year
  vector J_w(fmyear,lmyear) //estimated january 1 biomass at age

  //vector est_M //estimated migration into bay
  //params for spr model
  number years
  number G_spr
  vector R_eq(1,stock)//equillibrium recruitment
  vector F_spr(1,stock) //f value that gives you 40% spr
  4darray Nf_stock(1,stock,1,tstep,1,100,fage,lage) //will hold ppulation counts
  5darray Nf(1,stock,1,region,1,tstep,1,100,fage,lage)//abundance per recruit at age, for spr model
  matrix SSBf(1,stock,0,150) //holds SSB across f conditions
  matrix SPR(1,stock,0,150) //holds SPR value for different f conditions
  3darray F_eq(1,region,1,tstep,fage,lage)
  vector Fa(fage,lage)
  3darray Fstar(1,region,1,tstep,fage,lage)
  3darray V(1,region,1,tstep,fage,lage)
  vector F_40(1,stock) //F that yields spr40%
  3darray Zf(1,region,1,tstep,fage,lage) //lifetime mortlaity in equillibrium
  vector slope(1,stock)
  vector est_G_40(1,stock) //holds values of F that achieve 40%
  number est_G //holds single G value that is weighted based on recruitmnet
  3darray F_bar(1,region,1,tstep,fage,lage) //calcs F values after you have G
  matrix F_bar_stock(1,stock,fage,lage) // stock specific f values
  vector est_F_spr(1,stock) //F refrence points for each stock
  number est_annual_F //singular refrence pt
  

  init_bounded_matrix log_prop_bay(1,tstep,fage+1,lage,-10,0,1) //log occupancy probabilities at age for the chesapeak bay stock in the bay in each timstep
  init_bounded_matrix log_prop_coast(1,tstep,fage+1,lage,-10,0,1) //log occupancy probabilities at age for the  atlantic coast stock in the chesapeake bay
  //init_3darray log_prop(1,stock,1,tstep,fage,lage) // estimated occupancy probabilities for the chesapeake bay
  4darray prop(1,stock,1,tstep,1,region,fage,lage) //occupancy probabilities to estimate N

  //***********************
  //Likelihood components
  //***********************

  matrix sig2_f(1,region,1,tstep) //variance for each region and timestep
  
  //likelihoods functions
  matrix Lcatch(1,region,1,tstep)
  matrix Lcatchagecomp(1,region,1,tstep)

  vector Lindex_coast(1,tstep)
  vector Lindexagecomp_coast(1,tstep)
  vector Lindex_bay(1,tstep)
  vector Lindexagecomp_bay(1,tstep)
  number Lage1index_coast
  number Lage1index_bay
  number Lyoyindex_coast
  number Lyoyindex_bay

  matrix Lfsel(1,region,1,tstep)
  vector Lssel_bay(1,tstep) //surv selectivity for chesmmap
  vector Lssel_coast(1,tstep) //surv selectivity for ct list
 
  number pen_N0_dev //penalty for deviations of N0
  number pen_f2sel // penalty for fsel in coast of  tblock1 deviating from tblock2
  number pen_F //penalty for F
  number pen_prop // penalty for occupancy probability deviating away from prior
  number pen_prop_bay //penalty for proportion of bay fish in coast
  number pen_cb_sel //cb penalty for fsel
  number pen_rdev //penalty for recruitment deviations
  number pen_fdev //penalty for F deviations

  objective_function_value neg_LL  //value we are going to minimize

 LOCAL_CALCS
  //set starting values for parameters
  //log_N0=log(200000.);//mean abudance
  log_R(1)=start_log_R(1); //mean of log recruitment
  log_R(2)=start_log_R(2); //mean of log recruitment

  //cout << "log r" << endl << log_R << endl;
  //exit(1);

  
  for(r=1;r<=region;r++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      log_F(r,ts)=start_log_F(r,ts); //mean of log F
    }
  }
  log_Feq=start_log_Feq; //F in the first year

  log_a_sf1=start_log_a_sf1; //settinig log of slope of afsel
  log_a_sf2=start_log_a_sf2; // seting log of 50% selc for afsel

  //cout << "log F" << endl << log_F << endl;
  //cout << "log feq" << endl << log_Feq << endl;
  //exit(1);

  //*******************
  //starting parameters for selectivity
  //*******************

  //fishing selectivity

  for(t=1;t<=tblock;t++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      //atlantic coast
       log_sf1_ac(t,ts)=start_log_sf1_ac(ts);
       log_sf2_ac(t,ts)=start_log_sf2_ac(ts);
       //chesapeake bay
       log_sf1_cb(t,ts)=start_log_sf1_cb(ts);
       log_sf2_cb(t,ts)=start_log_sf2_cb(ts);
       log_sf3_cb(t,ts)=start_log_sf3_cb(ts);
       log_sf4_cb(t,ts)=start_log_sf4_cb(ts); 
     }
  }
     
  /*
  cout << "log sf1 ac" << endl << log_sf1_ac << endl;
  cout << "log_sf2_ac" << endl << log_sf2_ac << endl;
  cout << "log sf1 cb" << endl << log_sf1_cb << endl;
  cout << "log_sf2_cb" << endl << log_sf2_cb << endl;
  cout << "log sf3_cb" << endl << log_sf3_cb << endl;
  cout << "log_sf4_cb" << endl << log_sf4_cb << endl;
  exit(1);
  */


  //survey selectivity
  //coast
  log_ssf1_coast=start_log_ssf1_coast;
  log_ssf2_coast=start_log_ssf2_coast;
  log_ssf3_coast=start_log_ssf3_coast;
  log_ssf4_coast=start_log_ssf4_coast;
  //chesbay
  log_ssf1_bay=start_log_ssf1_bay;
  log_ssf2_bay=start_log_ssf2_bay;
  //log_ssf3_bay(1)=0.;
  //log_ssf3_bay(2)=0.;
  //log_ssf4_bay(1)=2.3;
  //log_ssf4_bay(2)=2.3;
  //cout << log_ssf1_bay << endl;
  //exit(1);
  //for(ts=1;ts<=tstep;ts++)
  //{
    log_q_coast=start_log_q_coast; //log of atl coast survey catchability
    log_q_bay=start_log_q_bay; //log of ches bay surv catchability
  //}
  //cout << "log q coast" << endl << log_q_coast << endl;
  //cout << "log q bay" << endl << log_q_coast << endl;
  //exit(1);
  
  log_q_age1_coast=start_log_qage1_coast;       //log age 1 survey catchability
  log_q_age1_bay=start_log_qage1_bay;       //log age 1 survey catchability//log_q_age1m=log(0.000005);       //log survey catchability
  log_q_yoy_coast=start_log_qyoy_coast;   //log YOY catchability for coast 
  log_q_yoy_bay=start_log_qyoy_bay;    //log YOY catchability for bay 

  //defining log_prop_bay and coast so that you are not taking the log of a 0, instead value equal -10
  for(ts=1;ts<=tstep;ts++)
  {
    for(a=fage+1;a<=lage;a++)
    {
      log_prop_bay(ts,a)=prop_bay(ts,a);
    }//close age loop
  }//close timestep loop
  for(ts=1;ts<=tstep;ts++)
  {
    for(a=fage+1;a<=lage;a++)
    {
      log_prop_coast(ts,a)=prop_coast(ts,a);
    }//close age loop
  }//close tstep loop
  //for(ts=1;ts<=tstep;ts++)
  //{
  //  cout << "start prop bay in coast" << "ts=" << ts << endl << mfexp(prop_bay(ts)) << endl;
  //  cout << "start prop coast in coast" << "ts=" << ts << endl << mfexp(prop_coast(ts)) << endl;
  //}
  //cout << age_err_a << endl;
  //exit(1);

  years=100;
  //cout << "finish ss and q calc " << endl;
  
 END_CALCS

PROCEDURE_SECTION
//In the procedure section we specify the model and the likelihood.

  calculate_mortality();

  //cout << "After calculate_mortality" <<endl;

  calculate_N_at_age();

  //cout << "After calculate_N_at_age" <<endl;

  calculate_fishery_catch();

  //cout << "After calculate_fisher_catch" <<endl;

  calculate_F_stock();

  calculate_survey_catch();

  //cout << "After calculate_survey_catch" <<endl;
  
  evaluate_likelihood();  

  //cout << "After calculate_likelihood" <<endl;

  calculate_B_SSB();

  //cout << "After B_SSB" << endl;

  //calculate_SPR();



FUNCTION calculate_mortality

  //*******************************
  // Calculate Fishery Selectivity
  //******************************

  for(t=1;t<=tblock;t++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      sf1_ac(t,ts)=mfexp(log_sf1_ac(t,ts));
      sf2_ac(t,ts)=mfexp(log_sf2_ac(t,ts));
  
      sf1_cb(t,ts)=mfexp(log_sf1_cb(t,ts));
      sf2_cb(t,ts)=mfexp(log_sf2_cb(t,ts));
      sf3_cb(t,ts)=mfexp(log_sf3_cb(t,ts));
      sf4_cb(t,ts)=mfexp(log_sf4_cb(t,ts));
    }
  }
  //cout << sf1_ac << endl;
  //cout << sf1_cb << endl;
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
    

  //cout << "end atan calc" << endl;
  //cout << "fsel" << " " << fsel << endl; 
    r=1;//fill fsel for chesapeake bay
    for(y=ftbyr;y<=ftlyr;y++)
    {
      for(ts=1;ts<=tstep;ts++)
      {
        for(a=fage;a<=lage;a++)
        {
          fsel(r,ts,y,a)=(1./(1.+mfexp(-sf1_cb(t,ts)*(double(a)-sf2_cb(t,ts)))))*
                  (1./(1.+mfexp(-sf3_cb(t,ts)*(sf4_cb(t,ts)- double(a)))));
        }//close age loop
        //fsel(r,ts,y)/=max(fsel(r,ts,y));
      }//close tstep
    } //close year loop for fsel
    r=2;//fill fsel for atlantic coast
    for(y=ftbyr;y<=ftlyr;y++)
    {
      for(ts=1;ts<=tstep;ts++)
      {

        for(a=fage;a<=lage;a++)
        {
          fsel(r,ts,y,a)=1./(1.+mfexp(-sf1_ac(t,ts)*(double(a)-sf2_ac(t,ts))));
        }//close age loop
        //fsel(r,ts,y)/=max(fsel(r,ts,y));
      }//close tstep
   } //close year loop for fsel
  }//close time block
  //cout << "sf1_cb" << endl << sf1_cb << endl;
  //cout << "sf2_cb" << endl << sf2_cb << endl;
  //cout << "sf3_cb" << endl << sf3_cb << endl;
  //cout << "sf4_cb" << endl << sf4_cb << endl;
  //exit(1);
  //cout << "fsel" << endl;
  
  for(y=fmyear;y<=lmyear;y++)
  {
    for(r=1;r<=region;r++)
    {
      for(ts=1;ts<=tstep;ts++)
      {
        log_fsel(r,ts,y)=log(fsel(r,ts,y));
      }//clost ts loop
    }//close region loop
  }//close year loop
  //cout << log_fsel << endl;
  //exit(1);
  /*
  for(ts=1;ts<=tstep;ts++)
  {
    for(r=1;r<=region;r++)
    {
      cout  << "ts=" << ts << ",r=" << r << endl << "fsel" << endl << fsel(r,ts) << endl;
    }
  }
  exit(1);
  //cout << "end fsel" << endl;
  */


  //*******************************
  // Calculate Survey Selectivity
  //******************************

  
  //set starting parameters
  
  ssf1_coast=mfexp(log_ssf1_coast);
  ssf2_coast=mfexp(log_ssf2_coast);
  ssf3_coast=mfexp(log_ssf3_coast);
  ssf4_coast=mfexp(log_ssf4_coast);
  
  ssf1_bay=mfexp(log_ssf1_bay);
  ssf2_bay=mfexp(log_ssf2_bay);
  //ssf3_bay=mfexp(log_ssf3_bay);
  //ssf4_bay=mfexp(log_ssf4_bay);

  //cout << "end starting params" << endl;
 
  //calculating ssel
  
  for(a=fage;a<=lage;a++)
  {
     for(t=1;t<=tstep;t++)
      {
      //coast, double logistic
        ssel_coast(t,a)=(1./(1.+mfexp(-ssf1_coast(t)*(double(a)-ssf2_coast(t)))))*
                          (1./(1.+mfexp(-ssf3_coast(t)*(ssf4_coast(t)-double(a)))));
      //bay, double logistic
        ssel_bay(t,a)=(1./(1.+mfexp(-ssf1_bay(t)*(double(a)-ssf2_bay(t)))));//*
                  //(1./(1.+mfexp(-ssf3_bay(t)*(ssf4_bay(t)-double(a)))));
      //cout << "age" << " " << a << endl << "ts" << " " << ts << endl;
      }//close tstep
  }//close age loop
  //cout << "ssel coast " << endl << ssel_coast << endl;
  //cout << "ssel bay " << endl << ssel_bay << endl;
  //exit(1);
  
  q_coast=mfexp(log_q_coast); //log of atl coast catchability
  q_bay=mfexp(log_q_bay); //log of ches bay catchability
  
  //cout << "q coast" << endl << q_coast << endl;
  //cout << "q bay" << endl << q_bay << endl;
  //exit(1);
 
  q_age1_coast=mfexp(log_q_age1_coast);
  q_age1_bay=mfexp(log_q_age1_bay);
  q_yoy_coast=mfexp(log_q_yoy_coast);
  q_yoy_bay=mfexp(log_q_yoy_bay);  


  //calculate F in the first year nad first time-step
  a_sf1=mfexp(log_a_sf1);
  a_sf2=mfexp(log_a_sf2);
  for(s=1;s<=stock;s++)
  {
    for(a=fage;a<=lage;a++)
    {
      //afsel(1,a)=1./(1.+mfexp(-1.95*(double(a)-3.9)))*(1./(1.+mfexp(-1.8*(7.7-double(a)))));//setting afsel to a logistic curve with slope of 1 and 50% sel at 4
      afsel(s,a)=1./(1.+mfexp(-a_sf1*(double(a)-a_sf2)));//setting afsel to a logistic curve with slope of 1 and 50% sel at 4
    }
    //afsel(s)(fage,lage)/=max(afsel(s));
  }
  //afsel(1)(fage,lage)/=max(afsel(1));
  //cout << afsel << endl;
  //exit(1);

  for(s=1;s<=region;s++)
  {
    for(a=fage;a<=lage;a++)
    {
      Feq(s,a)=afsel(s,a)*mfexp(log_Feq(s));
    }
  }
  //cout << "Feq=" << endl << Feq << endl;
  //exit(1);
  //calculate the fishing mortality rate (F) for each year and age
  
  r=1;
  t=1;
  for(y=fmyear;y<=lmyear;y++)
  {       
    for(a=fage;a<=lage;a++)
    {
        F(r,t,y,a)=fsel(r,t,y,a)*mfexp(log_F(r,t)+log_Fdevs_r1t1(y));
    }//close age loop
  }//close year loop
  //cout << "log_f" << endl << log_F << endl;
 
  //cout << F <<  endl;
  //exit(1);
  //fill out rest of years for  
  t=2;
  for(y=fmyear;y<=lmyear;y++)
  {       
    for(a=fage;a<=lage;a++)
    {
      F(r,t,y,a)=fsel(r,t,y,a)*mfexp(log_F(r,t)+log_Fdevs_r1t2(y));
    }//close age loop
  }//close year loop
  //cout << "Fsel cb, ts=2, fmyear" << endl << fsel(1,2,1) << endl;
  //fill out first year  of F for region 2, timestep 2
  r=2;
  t=1;
  for(y=fmyear;y<=lmyear;y++)
  {       
    for(a=fage;a<=lage;a++)
    {
      F(r,t,y,a)=fsel(r,t,y,a)*mfexp(log_F(r,t)+log_Fdevs_r2t1(y));
    }//close age loop
  }//close year loop
  
  //Fill out F for the rest of years fore region 2, time step 2
  t=2;
  for(y=fmyear;y<=lmyear;y++)
  {       
    for(a=fage;a<=lage;a++)
    {
      F(r,t,y,a)=fsel(r,t,y,a)*mfexp(log_F(r,t)+log_Fdevs_r2t2(y));
    }//close age loop
  }//close year loop
  //cout << F << endl;
  //exit(1);
  /*
  for(t=1;t<=tstep;t++)
    {
      for(r=1;r<=region;r++)
      {
          cout << "F:" << endl << "t=" << t << ",r=" << r << endl << F(r,t,fmyear) << endl;
      }
   }
  */
  //calculate the total mortality rate for each year and age 
  for(y=fmyear;y<=lmyear;y++)
  {
    for(t=1;t<=tstep;t++)
    {
      for(r=1;r<=region;r++)
      {
        Z(r,t,y)=M; //M divided by 2 in dat file to account for time step
        Z(r,t,y)+=F(r,t,y);
      }//close region
    }//close tstep
  }//close year
  /*
   for(t=1;t<=tstep;t++)
    {
      for(r=1;r<=region;r++)
      {
          cout << "Z:" << endl << "t=" << t << ",r=" << r << endl << Z(r,t,fmyear) << endl;
      }
  }
  //cout << "M" << endl << M << endl << endl;
  //cout << "end mort" << endl;
  exit(1);
  */
FUNCTION calculate_N_at_age

  //filling occupancy probabilities for ages 1 and 2 so that fish cannot migrate at these ages
  for(ts=1;ts<=tstep;ts++)
  { 
    prop(1,ts,1,1)=1.;//100% of bay stock at age 1 in bay
    prop(1,ts,2,1)=0.;//0% of bay stock in coast at age 1
    prop(2,ts,1,1)=0.;//0% of coast stock in bay at age 1
    prop(2,ts,2,1)=1.;//100% of coast stock in coast at age 1
  }
  //cout << "prop" << endl << prop << endl;
  //exit(1);
  
  for(s=1;s<=stock;s++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      //for(r=1;r<=region;r++)
      //{
        for(a=fage+1;a<=lage;a++)
        {
          //if(r==1)//if region = chesapeake bay
          //{
            prop(1,ts,1,a)=1.-mfexp(log_prop_bay(ts,a));//chesapeake bay stock in cb
            prop(2,ts,1,a)=1.-mfexp(log_prop_coast(ts,a));//atlantic coast stock in cb
            //prop(s,ts,r,a)=exp(atan(log_prop_bay(ts,a))/(PI/2)*((0-(-10))/2)+(-10/2));// arc tangent transformation of occupancy probabilities to have bounded between log(-10) and log(0)
          //}
          //else
          //{
            prop(1,ts,2,a)=mfexp(log_prop_bay(ts,a));//chesapeake bay stock in ac
            prop(2,ts,2,a)=mfexp(log_prop_coast(ts,a));//atlantic coast stock in ac
          //} //end else statement
        }//close age loop
      //}//close region loop
    }//close ts loop
  }//close stock loop
  /*
  for(ts=1;ts<=tstep;ts++)
    {
      cout << "prop_bay," << "ts=" << ts << endl << mfexp(log_prop_bay(ts))<< endl;
      cout << "prop_coast," << "ts=" << ts << endl << mfexp(log_prop_coast(ts))<< endl;
    }

  for(s=1;s<=stock;s++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(r=1;r<=region;r++)
      {
          cout << "prop" << "s=" << s << "ts="<< ts << "r=" << r <<  endl << prop(s,ts,r)(fage,lage) << endl;
      }
     }
  }
  exit(1);
  */
  
  //fill in abundance at age in the first row of the N-at-age matrix
  //fill in recuitment for the first year
  Nt(1,1,fmyear,fage)=mfexp(log_R(1)+log_Rdevs1(fmyear));//Bay stock (stock, tstep, year,,age); mfexp allows exp of moderate numbers to be differentiable
  Nt(2,1,fmyear,fage)=mfexp(log_R(2)+log_Rdevs2(fmyear));//Atlantic stock(stock, tstep, year,,age) 
  //cout << "recruit" << endl;

  
  //cout << afsel << endl;
  //afsel=(fsel(1,1,fmyear)+fsel(1,2,fmyear)+fsel(2,1,fmyear)+fsel(2,2,fmyear))/4.0;//average selectivity in the first year
  //calculate N at age in the first year and first time step, assuming that population is at equillibrium
  for(s=1;s<=stock;s++)
  {
  //for(r=1;r<=region;r++)
  //{
    for(a=fage+1;a<=lage;a++)
    {
      //cout << "a" << " " << a << endl;
      Nt(s,1,fmyear,a)=Nt(s,1,fmyear,a-1)*mfexp(-(2.*M(a-1)+Feq(s,a-1)));//M*2 because we are getting ages from age 1 to age2, not from one time step to the next +F(2,fmyear,a-1)*fsel(2,fmyear,a-1)));
      //cout << Nt(s,1,fmyear,a) << endl;
    }//close age loop
    //including plus group in first year
    Nt(s,1,fmyear,lage)/=1.-mfexp(-(2.*M(lage)+Feq(s,lage)));//+F(2,fmyear,lage)*fsel(2,fmyear,lage)));
    //}
    //now include N0 deviations in first year of equilibrium
    Nt(s,1,fmyear)(fage+1,lage)=elem_prod(Nt(s,1,fmyear)(fage+1,lage),mfexp(log_N0_devs(fage+1,lage)));

    for(r=1;r<=region;r++)
    {
      //calculate abundance in each region in the first year time step 1
      N(s,r,1,fmyear)=migrate(Nt(s,1,fmyear),prop(s,1,r)); //*MW* 
    }//close region loop
  }//close stock loop
  //cout << afsel << endl;
  //exit(1);

  //for(s=1;s<=stock;s++)
  //{
  //  cout << Nt(s,1,fmyear) << endl;
  //}
  //exit(1);
  //for(s=1;s<=stock;s++)
  //{
  //  cout << "Feq" << endl << Feq(s) << endl;
  //  cout << "Eq_Z" << endl << 2*M+Feq(s) << endl;
  //}
  //exit(1);
  //cout << "Log Feq" << mfexp(log_Feq) <<endl;
  /*
  for(s=1;s<=stock;s++)
  {
    for(r=1;r<=region;r++)
    {
      //for(t=1;t<=tstep;t++)
      //{
      t=1;
      //cout << "s=" << s <<  ", t=" << t << endl;
      //cout << Nt(s,t,fmyear) << endl;
 
        cout << "s=" << s << ", r=" << r << ", t=" << t << endl;
        cout << N(s,r,t,fmyear) << endl;
        //cout << "F=" << endl << mfexp(log_Feq(s))*afsel << endl;
      }
    //}
  }
  exit(1);
  */
  //cout << "end N tstep 1 calc first year" << endl;
  //Fill in abundance in first year, time step 2 
  for(s=1;s<=stock;s++)
  {
    for(a=fage;a<=lage;a++)
    { 
      //Nt(s,2,fmyear,a)=(N(s,1,1,fmyear,a)*mfexp(-(M(a)+F(1,1,fmyear,a)*fsel(1,1,fmyear,a)))+N(s,2,1,fmyear,a)*mfexp(-(M(a)+F(2,1,fmyear,a)*fsel(2,1,fmyear,a))));
      Nt(s,2,fmyear,a)=(N(s,1,1,fmyear,a)*mfexp(-(M(a)+F(1,1,fmyear,a)))+N(s,2,1,fmyear,a)*mfexp(-(M(a)+F(2,1,fmyear,a))));
    }//close age loop
   
    for(r=1;r<=region;r++)
    {
      N(s,r,2,fmyear)=migrate(Nt(s,2,fmyear),prop(s,2,r)); //
    }//close region loop
  }//close stock loop
  
  //for(r=1;r<=region;r++)
  //{
  //  cout << "Z timestep 1 " << "r= " << r << endl << M+(F(r,1,fmyear)) << endl;
  //}
  //cout << "M=" << endl << M << endl;
  //exit(1);

  /*
  //cout << Nt << endl << endl;
  for(s=1;s<=stock;s++)
  {
    for(r=1;r<=region;r++)
    {
      for(t=1;t<=2;t++)
      {
      //t=2;
        cout << "s=" << s << ", r=" << r << ", t=" << t << endl;
        cout << N(s,r,t,fmyear) << endl;
      }
    }
  }
  exit(1);
  */
  //cout << "end N time step 2 fmyear" << endl;
  
  //fill in the rest of the rows of the N-at-age matrix by applying exponential mortality
  
  for(y=fmyear+1;y<=lmyear;y++)
  {
     //calculate recruitment for first time step of each year
     Nt(1,1,y,fage)=mfexp(log_R(1)+log_Rdevs1(y));//Bay stock, (yr,tstep,stock,age)  //*MW*
     Nt(2,1,y,fage)=mfexp(log_R(2)+log_Rdevs2(y));//coast stock, (yr,tstep,stock,age)  //*MW*
     //cout << Nt(1,1,y,fage) << endl;
     //cout << Nt(2,1,y,fage) << endl;
  }
  //for(s=1;s<=stock;s++)
  //{
  //  for(ts=1;ts<=tstep;ts++)
  //    {
  //       cout << Nt(s,ts) << endl;
  //     }
  // }
  //exit(1);
  /*
  for(r=1;r<=region;r++)
  {
    for(t=1;t<=2;t++)
    {
      cout << "r=" << r << ", t=" << t << endl;
      cout << Z(r,t) << endl;
    }
  }
  exit(1);
  */

  for(y=fmyear+1;y<=lmyear;y++)
  {
     for(t=1;t<=tstep;t++)
     {
       if(t==1)//first dothe first tstep for each year; need to refer to the previous year and age
       {
         t2=2; //set the time step for reference on the RHS to 2 (the one before our current time step)
         for(s=1;s<=stock;s++)
         {
           for(a=fage+1;a<=lage;a++)
           {
             Nt(s,t,y,a)=N(s,1,t2,y-1,a-1)*mfexp(-Z(1,t2,y-1,a-1))+N(s,2,t2,y-1,a-1)*mfexp(-Z(2,t2,y-1,a-1));
             //cout << "s=" << s << ", t=" << t << ", y=" << y << ", a=" << a << endl;
             //cout << Nt(s,t,y,a) << " " << N(s,1,t2,y-1,a-1) << " " <<exp(-Z(1,t2,y-1,a-1)) << " " <<N(s,2,t2,y-1,a-1) << " " <<exp(-Z(2,t2,y-1,a-1))<< endl;
           }
           //plus group
           Nt(s,1,y,lage)+=N(s,1,t2,y-1,lage)*mfexp(-Z(1,t2,y-1,lage))+ N(s,2,t2,y-1,lage)*mfexp(-Z(2,t2,y-1,lage));
           for(r=1;r<=region;r++)
           {
             //cout << a << endl;
             //cout << r << endl;
             N(s,r,t,y)=migrate(Nt(s,t,y),prop(s,t,r));
             //cout << endl << "s=" << s <<", r=" << r << ", t=" << t << ", y=" << y << endl;
             //cout << "migrate" << endl << N(s,r,t,y) << endl << Nt(s,t,y) << endl << prop(s,r,t) << endl << endl;
           }//close region loop
         }//close stock loop
       }//close if statement
       else
       {
         //t2=1;//set the time step for reference on the RHS to 1 (the one before our current time step)
         for(s=1;s<=stock;s++)
         {
           Nt(s,t,y)=elem_prod(N(s,1,t-1,y),mfexp(-Z(1,t-1,y)))+ elem_prod(N(s,2,t-1,y),mfexp(-Z(2,t-1,y)));
           //cout << "s=" << s << ", t=" << t << ", y=" << y << endl;
           //cout << Nt(s,t,y) << endl << N(s,1,t-1,y) << endl << exp(-Z(1,t-1,y)) << endl <<  N(s,2,t-1,y) << endl << exp(-Z(2,t-1,y)) << endl;
           //including plus group *MW* no extra plus group calculation for this time step because the animals only increment an age between timestep 2 and 1 of the next year.
           //Nt(s,2,y,lage)+=(N(s,1,1,y,lage)*exp(-M(lage)+F(1,1,y,lage)*fsel(1,1,y,lage))+N(s,2,1,y,lage)*exp(-M(lage)+F(2,1,y,lage)*fsel(2,1,y,lage)));
           for(r=1;r<=region;r++)
           {
             //calculate abundance in each region in the first year
             N(s,r,t,y)=migrate(Nt(s,t,y),prop(s,t,r)); // propar should be read in?
           }//close region loop
         }//close stock loop
       }//close else statement
     }//close t loop
  }//close year loop
  /*
  for(s=1;s<=stock;s++)
  {
   for(r=1;r<=region;r++)
   {
    for(t=1;t<=tstep;t++)
    {
      cout << "s=" << s << "r=," << r << ", t=" << t << endl;
      cout << N(s,r,t) << endl << endl;
     }
    }
  }
  exit(1);
 */
  //for(s=1;s<=stock;s++)
  //{
  /*
  for(r=1;r<=region;r++)
    {
      for(t=1;t<=2;t++)
      {
        //cout << "s=" << s << ", r=" << r << ", t=" << t << endl;
        //cout << N(s,r,t) << endl;
        cout <<  " r=" << r << ", t=" << t << endl;
        cout << Nt(r,t) << endl;
    //  }
    }
  }
  exit(1);
  */
  //cout << "N calcs" << endl;
  //exit(1);

  //calculate the population in each region at each time step first
  for(y=fmyear;y<=lmyear;y++)
  {
    for(t=1;t<=tstep;t++)
    {
      //for(a=fage;a<=lage;a++)
      //{
        N_region(1,t,y)=N(1,1,t,y)+N(2,1,t,y); //N in bay in tstep 1 and 2
        N_region(2,t,y)=N(1,2,t,y)+N(2,2,t,y); //N in coast in tstep 1 and 2
    } //close tstep loop
  }//close year loop
  /*
  if(last_phase())
  {
  for(r=1;r<=region;r++)
  {
    for(t=1;t<=tstep;t++)
    {
       cout << "t=" << t << endl << "r=" << r << endl << N_region(r,t) << endl;
    } //close tstep loop
  }//close region loop
  //exit(1);
  }
  */


  //calc total abundance
  est_Ntot=0.0;//initialize
  for(y=fmyear;y<=lmyear;y++)
  {
    for(s=1;s<=stock;s++)
    {
      //for(r=1;r<=region;r++)
      //{
        est_Ntot(y)+=sum(Nt(s,1,y));
      //}
    }
  }
  est_Ntot_stock=0.0;//initialize
  for(y=fmyear;y<=lmyear;y++)
  {
    for(s=1;s<=stock;s++)
    {
      //for(r=1;r<=region;r++)
      //{
        est_Ntot_stock(s,y)=sum(Nt(s,1,y));
      //}
    }
  }

FUNCTION calculate_B_SSB
  
  for(y=fmyear;y<=lmyear;y++)
  {
    for(s=1;s<=stock;s++)
    {
      for(r=1;r<=region;r++)
      {
        B(s,r,y)=N(s,r,1,y)*rw_age(y)/1000; //January 1 Biomass; using rivard weight
        SSB(s,r,y)=N(s,r,1,y)*elem_prod(sex,elem_prod(ssbw_age(y),m_age))/1000; //Female Spawning Stock biomass
      }//close region loop
    }//close stock loop
  }//close year loop

  //cout << "finish ssb calcs" << endl;
  
  // * is the dot product, which multiplies the values and then sums it

  est_biomass=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    for(s=1;s<=stock;s++)
    {
      for(r=1;r<=region;r++)
      {
        est_biomass(y)+=B(s,r,y);
      }
    }
  }

  est_SSB=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    for(s=1;s<=stock;s++)
    {
      for(r=1;r<=region;r++)
      {
        est_SSB(y)+=SSB(s,r,y);
      }
    }
  }
  //cout << est_biomass <<endl;
  //exit(1);
  est_F=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    for(r=1;r<=region;r++)
    {
    for(t=1;t<=tstep;t++)
    {
      for(a=fage;a<=lage;a++)
      {
        est_F(y,a)+=(F(r,t,y,a)*N_region(r,t,y,a))/(N_region(1,t,y,a)+N_region(2,t,y,a));//denominato is total abundance at that specific age,year, a
       }
      }
    }//close year
  }//clsoe region
  //for(y=fmyear;y<=lmyear;y++)
  //{
  //  est_F_year(y)=sum(est_F(y));
  //}
  //cout << "rw age" << endl << rw_age(30) << endl;
  //cout << "M" << endl << M << endl;
  //cout << "sex prop" << endl << sex << endl;
  //cout << "lmyear" << endl << lmyear << endl;
  //cout << "mat" << endl << m_age << endl;
  //exit(1);

FUNCTION calculate_fishery_catch
  //calculate fishery catch at age using the Baranov catch equation C=(F/Z)*(1-exp(-Z))*N
  for(y=fmyear;y<=lmyear;y++)
  {
    for(t=1;t<=tstep;t++)
    {
      for(r=1;r<=region;r++)
      {
        //cout << "r" << " " << r << endl;
        //cout << "yr" << " " << y << endl;
        est_region_C(r,t,y)=0.0;
        for(s=1;s<=stock;s++)
        {
          //cout << "s" << " " << s << endl;
          est_C_age(s,r,t,y)(fage,lage)=elem_prod(elem_prod(elem_div(F(r,t,y)(fage,lage),Z(r,t,y)(fage,lage)),1.-mfexp(-Z(r,t,y)(fage,lage))),N(s,r,t,y)(fage,lage));
          //cout << est_C_age (s,r,t,y) << endl;
	  est_C(s,r,t,y)=sum(est_C_age(s,r,t,y));  //calculate total catch by stock
          est_C_age_err(s,r,t,y)=mod_age_err_a*est_C_age(s,r,t,y);
          //cout << "r=" << r << endl;
          //cout << "t=" << t << endl;
          //cout << "est_C=" << est_C(s,r,t,y) << endl;
        }//close stock loop
        est_totC_age(r,t,y)(fage,lage)=elem_prod(elem_prod(elem_div(F(r,t,y)(fage,lage),Z(r,t,y)(fage,lage)),1.-mfexp(-Z(r,t,y)(fage,lage))),N_region(r,t,y)(fage,lage));
        est_totC_age_err(r,t,y)=mod_age_err_a*est_totC_age(r,t,y);//calculations to get catch at age for corrected aging error
        est_region_C(r,t,y)+=sum(est_totC_age_err(r,t,y));  //calculate total catch by stock
                 //use est_regon_C  for the likelihood
      } //close region loop
    }//close time step
  }//close year loop for catch at age
  //cout << "est c age" << endl << est_C_age << endl;
  //cout << "est c age err" << endl << est_C_age_err << endl;
  //exit(1);
  /*
  for(t=1;t<=tstep;t++)
  {
    for(r=1;r<=region;r++)
    {
      cout << "t=" << t << ",r=" << r << endl << "est_totC_age" << " " << endl <<  est_totC_age(r,t) <<endl;
      cout << "est_totC_age_err" << " " << endl << est_totC_age_err(r,t) << endl;
    }
  }
  exit(1);
  */
  /*
  for(t=1;t<=tstep;t++)
  {
    for(r=1;r<=region;r++)
    {
        cout << "r=" << r << endl << "t=" << t << endl << "reg_c" << est_region_C(r,t) << endl << "region age" << est_totC_age(r,t) << endl ;
     }
  }
  for(t=1;t<=tstep;t++)
  {
   for(r=1;r<=region;r++)
  {
      for(s=1;s<=stock;s++)
     {
        cout << "r=" << r << endl << "t=" << t << endl << est_totC_age(r,t) << endl;
      }
     }
   }
  exit(1);
  */
  
  //cout << "fish catch " << endl;
  //calculate proportions at age in the catch
  for(y=fmyear;y<=lmyear;y++)
  { 
    for(t=1;t<=tstep;t++)
    { 
      for(r=1;r<=region;r++)
      {
        est_region_Cp(r,t,y)=0.0;
	for(s=1;s<=stock;s++)
        {
          //cout << s << endl;
	  //est_Cp(s,r,t,y)=sum(est_totC_age_err(s,r,t,y)); //sum catch ove age
          //est_Cp(s,r,t,y)(fage,lage)/=est_totC(s,r,t,y); //divide by total catch to get proportions at age
        }
        for(a=fage;a<=lage;a++)
        {
          //cout << "t=" << t << endl << "r=" << r << endl;
          est_region_Cp(r,t,y,a)+=est_totC_age_err(r,t,y,a); //calculating proportions at age for an entire region, regardless of stock
          est_region_Cp(r,t,y,a)/=est_region_C(r,t,y);  //continue calculating prop at age for an entire region, regardless of stock
          //cout << est_region_Cp(r,t,y) << endl;
          }//close age loop
      }//close region loop
    }// close time step
  }//close year loop for estcp
  //cout << "est region cp" << endl;
  //for(t=1;t<=tstep;t++)
  //{ 
  //  for(r=1;r<=region;r++)
  //  {
  //    cout << "est region cp" << endl << "r=," << r << "t=" << t  << endl << est_region_Cp(r,t) << endl;
  //    cout << "obs region cp" << endl << obs_Cp(r,t) << endl;    
  //  }
  // }
  //exit(1);
  
  //calculate variance of expected proportion for catch at age (Fournier - multifanciel)
  for(y=fmyear;y<=lmyear;y++)
  {
    for(r=1;r<=region;r++)
    {
      if(y<ftyear(2) && r==2)
      {
        d=0.05;
      }//close if statement
      else
      {
        d=0.1;
      }//close else statement
       for(t=1;t<=tstep;t++)
       { 
         for(a=fage;a<=lage;a++)
         {
           sigma2_Cp(r,t,y,a)=((1.-est_region_Cp(r,t,y,a))*est_region_Cp(r,t,y,a)+(d/double(lage-fage+1)))/ESS_C(r,t);
         }//close age loop for sigma2cp
       }//close timestep loop
     }//close region
   }//close year
  //cout << "finish C calcs" << endl;
  /*
  cout << "obs CP" << endl;
  cout << obs_Cp << endl;
  cout << "est CP" << endl;  
  cout << est_Cp << endl;
  cout << "sigmaCP" << endl;
  cout << sigma2_Cp << endl;
  exit(1);
  */                     

FUNCTION calculate_F_stock

  //here we are calculating the weighted F
  //Freg_4plus.initialize(); // Initialize the result array
  //for(s=1;s<=stock;s++)
  //{
    for(r=1;r<=region;r++)
    {
      for(ts=1;ts<=tstep;ts++)
      {
        for(y=fmyear;y<=lmyear;y++)
        {
          for(a=4;a<=lage;a++)
          {
            Freg_4plus(r,ts,y,a)=F(r,ts,y,a)*(N_region(r,ts,y,a))/sum(N_region(r,ts,y)(4,lage));//(sum(N(1,r,ts,y)(4,lage))+sum(N(2,r,ts,y))); //N is abundance of each stock in each region, so you want to add abundance for both stocks
          }
        }
      }//close year
    }//clsoe region
  //}
  //cout << "N1" << endl << N << endl;
  //cout << "Freg" << endl << Freg_4plus << endl;
  //exit(1);

  //here we are stimating annual F for each stock
  Fbar.initialize(); // Initialize the result array
  //Fbar=0.0;
  for(s=1;s<=stock;s++)
  {
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=fage;a<=lage;a++)
      {
        for(r=1;r<=region;r++)
        {
          for(ts=1;ts<=tstep;ts++)
          {
            Fbar(s,y,a)+=prop(s,ts,r,a)*F(r,ts,y,a);          
          }
        }
      }
    }
  }
  //cout << "Fbar" << " " << Fbar << endl;

  //calculte F for each stock of fish age 7 and older
  Fplus.initialize(); // Initialize the result array
  //Fplus=0.0;
  for(s=1;s<=stock;s++)
  {
    for(y=fmyear;y<=lmyear;y++)
    {
      for(a=7;a<=lage;a++)
      {
        Fplus(s,y)+=(Fbar(s,y,a)*Nt(s,1,y,a))/sum(Nt(s,1,y)(7,lage)); //Nt(s,1,y)is the stock abundance, in timestep 1, for each year   
      }
    }
  }
  //cout << "Fplus" << endl << Fplus << endl;
  //exit(1);

  //est_F.initialize(); // Initialize the result array
  /*
  est_F=0.0;
  for(y=fmyear;y<=lmyear;y++)
  {
    for(r=1;r<=region;r++)
    {
      for(a=fage;a<=lage;a++)
      {
        //ts=1;
        est_F(y)+=(F(r,1,y,a)*N_region(r,1,y,a))/sum(N_region(r,1,y)(fage,lage));//
      }
    }//close year
  }//clsoe region
  //cout << "Nregion" << endl << N_region << endl;
  //cout << " est f" << endl << est_F << endl;
  //exit(1);
  */        

FUNCTION calculate_survey_catch

  //calculate the population in each region at each time step first
  for(y=fmyear;y<=lmyear;y++)
  {
    for(t=1;t<=tstep;t++)
    {
      //for(a=fage;a<=lage;a++)
      //{
        //Nbay(t,y)=N(1,1,t,y)+N(2,1,t,y); //N in bay in tstep 1 and 2
        //Ncoast(t,y)=N(1,2,t,y)+N(2,2,t,y); //N in coast in tstep 1 and 2
      //} // close age loop
        r=1;
        Nbay(t,y)=N_region(r,t,y); //N in bay in tstep 1 and 2
        r=2;
        Ncoast(t,y)=N_region(r,t,y); //N in coast in tstep 1 and 2
 
      //cout << N(1,1,t,y) << endl << endl << N(2,1,t,y) << endl << endl;
      //cout << N(1,2,t,y) << endl << endl << N(2,2,t,y) << endl << endl;
    } //close tstep loop
  }//close year loop

  //ts=2;
  //y=1;
  //cout << "Ncoast" << endl << Ncoast(ts,y) << endl << endl;
  //cout << "Nregion coast" << endl << N_region(2,ts,y) << endl;
  //cout << "N calcs coast" << endl << N(1,2,ts,y)+N(2,2,ts,y) << endl;
  //cout << Ncoast << endl;
  //exit(1);
  //cout << "nbay ncooast" << endl;
  
  ////////////////////////
  //  Chesapeake bay    //
  ////////////////////////
  
   //calculate estimated survey catch at age for each year
  for(y=fmyear;y<=lmyear;y++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      est_I_age_bay(ts,y)=q_bay(ts)*elem_prod(Nbay(ts,y),ssel_bay(ts));//stock1,region1timstep1,
      est_I_age_bay_err(ts,y)=mod_age_err_a*est_I_age_bay(ts,y);
    }//close ts loop
  }//closing year loop
  //cout << "q_bay" << q_bay << "ssel_bay"<< ssel_bay << endl << Nbay <<  endl;
  //calculate total survey catch at age for each year
  for(ts=1;ts<=tstep;ts++)
  {
    est_I_bay(ts)=rowsum(est_I_age_bay_err(ts));
  }
  //cout <<"est_I_bay" << endl << est_I_age_bay << endl<<  "est i bay  " <<endl << est_I_bay << endl;
  //cout << est_I_md << endl;
  //exit(1);
  for(y=fmyear;y<=lmyear;y++)
  {//calculate proportions at age in the survey catch
    for(ts=1;ts<=tstep;ts++)
    {
      est_Ip_bay(ts,y)=est_I_age_bay_err(ts,y)/est_I_bay(ts,y);
      for(a=fage;a<=lage;a++) //can still use age ranges for each survey
      {
        sigma2_Ip_bay(ts,y,a)=((1.-est_Ip_bay(ts,y,a))*est_Ip_bay(ts,y,a)+0.1/double(lage-fage+1))/ESS_I_bay(ts);
      }//close age loop
    }// close timestep loop
  }//close year loop
  //cout << est_I_age_bay << endl;
  //exit(1);
  
  
  
  ////////////////////////
  //   Atlantic Coast   //
  ////////////////////////

  
  for(y=fmyear;y<=lmyear;y++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      est_I_age_coast(ts,y)=q_coast(ts)*elem_prod(Ncoast(ts,y),ssel_coast(ts));//stock1,region1timstep1,
      est_I_age_coast_err(ts,y)=mod_age_err_a*est_I_age_coast(ts,y);
    }//close ts loop
  }//closing year loop
  //cout << "q coast" << q_bay << "ssel_coast"<< ssel_coast << endl << Ncoast <<  endl;
  //calculate total survey catch at age for each year
  for(ts=1;ts<=tstep;ts++)
  {
    est_I_coast(ts)=rowsum(est_I_age_coast_err(ts));
  }
  //cout <<"est_I_coast" << endl << est_I_age_coast << endl<<  "est i coast  " <<endl << est_I_coast << endl;
  //exit(1);
  for(y=fmyear;y<=lmyear;y++)
  {//calculate proportions at age in the survey catch
    for(ts=1;ts<=tstep;ts++)
    {
      est_Ip_coast(ts,y)=est_I_age_coast_err(ts,y)/est_I_coast(ts,y);
      for(a=fage;a<=lage;a++) //can still use age ranges for each survey
      {
        sigma2_Ip_coast(ts,y,a)=((1.-est_Ip_coast(ts,y,a))*est_Ip_coast(ts,y,a)+0.1/double(lage-fage+1))/ESS_I_coast(ts);
      }//close age loop
    }// close timestep loop
  }//close year loop
  /*
  for(ts=1;ts<=tstep;ts++)
  {
    cout << "est Ip Bay" << endl << "r= 1" << " ts=" << ts << endl << est_Ip_bay(ts) << endl;
    cout << "obs Ip Bay" << endl << "r= 1" << " ts=" << ts << endl << obs_Ip_bay(ts) << endl;
    cout << "est Ip coast" << endl << "r= 2" << " ts=" << ts << endl << est_Ip_coast(ts) << endl;
    cout << "obs Ip coast" << endl <<  "r= 2" << " ts=" << ts << endl << obs_Ip_coast(ts) << endl;
  }
  exit(1);
  */
 
  ///*********************************************
  ///            YOY and age 1 survey
  ///********************************************
  
  //age 1 surveys
  //cout << q_age1n << endl;
  for(y=fmyear;y<=lmyear;y++)
  {
      t=2;
      //cout << "t=" << t << endl;
      est_I_age1_coast(y)=q_age1_coast*Ncoast(t,y,1);
      est_I_age1_bay(y)=q_age1_bay*Nbay(t,y,1);
      //cout << est_I_age1n(o,y) << endl;
   }//close o loop

   //yoy surveys
  for(y=fmyear;y<=lmyear-1;y++)
  {
    t=2;
    //Coast survesy, NJYOY and NY YOY
    est_I_yoy_coast(y)=q_yoy_coast*Ncoast(t,y+1,1);// time step 2?, year, age=1
    est_I_yoy_bay(y)=q_yoy_bay*Nbay(t,y+1,1);//time step 2, year, age=1?
     //cout << y << " " <<  z << " " << q_yoy_coast(z) << " " << Ncoast(2,y+1,1) << " " << est_I_yoy_coast(z,y) << endl;
  }//close yr loop
    
  //exit(1);
  //cout << "finish yoy" << endl;
  //  cout << "catch" << endl;
  //  cout << est_C_age << endl << endl;
  //  cout << "index" << endl;
  //cout << est_I_yoy_bay << endl << endl << endl;
  //exit(1);

FUNCTION evaluate_likelihood

  //lognormal likelihood for total catch
  for(t=1;t<=tstep;t++)
  {
    //cout << "t=" << t << " " << tstep << endl;
    for(r=1;r<=region;r++)
    {
      //cout << "r=" << r << endl;
      Lcatch(r,t)=lognorm_negLL(obs_C(r,t),est_region_C(r,t),C_var(r,t),fmyear,lmyear);
      //cout << est_region_C(r,t) << endl;
    }//close region loop
  }//close time step loop
   //cout << Lcatch << endl;
  //cout << "Lcatch" << endl;
  //exit(1);
  for(ts=1;ts<=tstep;ts++)
  {  
    for(r=1;r<=region;r++)
    {
      Lcatchagecomp(r,ts)=multinom_negLL(obs_Cp(r,ts),est_region_Cp(r,ts),sigma2_Cp(r,ts),fage,lage,fmyear,lmyear);//multinomial for proportions
    }//close region loop
  }//close timestep loop
  
  //cout << "Lcatchagecomp" << endl;
  //lognormal likelkihood for indices of abundance
  
  //multinomial for age composition
  
  for(ts=1;ts<=tstep;ts++)
  {
    //COAST
    Lindex_coast(ts)=lognorm_negLL(obs_I_coast(ts),est_I_coast(ts),I_var_coast(ts),fmyear,lmyear);
    Lindexagecomp_coast(ts)=multinom_negLL(obs_Ip_coast(ts),est_Ip_coast(ts),sigma2_Ip_coast(ts),fage,lage,fmyear,lmyear);//change AGEEE
    //CHES BAY
    Lindex_bay(ts)=lognorm_negLL(obs_I_bay(ts),est_I_bay(ts),I_var_bay(ts),fmyear,lmyear);
    Lindexagecomp_bay(ts)=multinom_negLL(obs_Ip_bay(ts),est_Ip_bay(ts),sigma2_Ip_bay(ts),fage,lage,fmyear,lmyear);
  }
  //cout << "Lindex coast and bay" << endl;

  //**************************
  //age 1 surveys
  //**************************
  
  Lage1index_bay=lognorm_negLL(obs_I_age1_bay,est_I_age1_bay,I_var_age1_bay,fmyear,lmyear);// double check params
  Lage1index_coast=lognorm_negLL(obs_I_age1_coast,est_I_age1_coast,I_var_age1_coast,fmyear,lmyear);

  //cout << "end age 1 lik" << endl;

  //*********************
  //YOY surveys
  //********************
   Lyoyindex_coast=lognormyoy_negLL(obs_I_yoy_coast,est_I_yoy_coast,I_var_yoy_coast,fmyear,lmyear-1);
   Lyoyindex_bay=lognormyoy_negLL(obs_I_yoy_bay,est_I_yoy_bay,I_var_yoy_bay,fmyear,lmyear-1);
   
  //cout << "end yoy like" << endl;
  //cout << Lyoyindex_coast << endl;
  //cout << Lyoyindex_bay << endl;
  //cout << est_I_yoy_coast << endl;
  //cout << est_I_yoy_bay << endl;
  
  //pen_N0_dev=0.5*norm2(log_N0_devs/0.49);
  //pen_f2sel=0.5*norm2((log_fs(2,1)-mean(log_fs(2,1))))/0.25;
  pen_F=0.0;
  if(!last_phase())
   {
    pen_F+=0.5*square(sum(log_F)-log(0.1));//assuming normal penalty for log(F)
  }

  //**SN TURNED OFF rdev and fdev 1/25/24
  pen_rdev=0.0;
  pen_rdev+=norm2(log_Rdevs1)+norm2(log_Rdevs2);//recruitment deviation penalty; assumes logscale variable of mean recruiment has SD 1 and mean of 0

  pen_fdev=0.0;
  for(y=fmyear+1;y<=lmyear;y++)
  {
    pen_fdev+=1./square(0.5)*square(log_Fdevs_r1t1(y)-log_Fdevs_r1t1(y-1)); //penalizing the model for differences in F from one year to the next year
    pen_fdev+=1./square(0.5)*square(log_Fdevs_r1t2(y)-log_Fdevs_r1t2(y-1));
    pen_fdev+=1./square(0.5)*square(log_Fdevs_r2t1(y)-log_Fdevs_r2t1(y-1));
    pen_fdev+=1./square(0.5)*square(log_Fdevs_r2t2(y)-log_Fdevs_r2t2(y-1));
    //square 0.5 suggests that the penalty should have a SD of 0.5 on a normal distribution
   }
  
  //penalty for occupancy probabilities deviating from priors
  pen_prop=0.0;//setting penalty to 0 to start
  
  if(use_pen_prop==1)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage+1;a<=lage;a++)
      {
        pen_prop+=0.5*square((log_prop_bay(ts,a)-prop_bay(ts,a))/log_sd_bay(ts,a));//adding for bay fish in the bay
      //pen_prop+=beta_mig(prop(1,ts,2),alpha_bay(ts),beta_bay(ts)); //feeding in the  occupancy probabilites, alpha, and beta parameters for stock 1 (Bay) in the Coast, for each time step
      }//close age loop
    }//close ts loop
  }//close if statement
  
  //cout << pen_prop << endl;
  //exit(1);
  if(use_pen_prop==1)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage+1;a<=lage;a++)
      {
        pen_prop+=0.5*square((log_prop_coast(ts,a)-prop_coast(ts,a))/log_sd_coast(ts,a));//adding for coast fish in the coast
      }//close age loop
    }//close ts loop
  } //clsoe if loop
  

  //cout << "log prop coast" << endl << log_prop_coast << endl;
  //cout << "prop coast" << endl << prop_coast << endl;
  //cout << "log prop bay" << endl << log_prop_bay << endl;
  //cout << "prop bay" << endl << prop_bay << endl;
  //exit(1);

  //penalty for proportion of CB fish in the coast
  pen_prop_bay=0.0;
  //removed hasegawa 2022 estimates since out of original data framework
  //pen_prop_bay+=-50.*0.61*log(0.01+sum(est_C_age_err(1,2,2,20)(4,lage))/sum(est_C_age_err(1,2,2,20)(4,lage)+est_C_age_err(2,2,2,20)(4,lage)));//binomial log likelihood, taking neg for NLL
  //pen_prop_bay+=-50.*0.39*log(0.01+sum(est_C_age_err(2,2,2,20)(4,lage))/sum(est_C_age_err(1,2,2,20)(4,lage)+est_C_age_err(2,2,2,20)(4,lage)));
  // have a weak penalty here to try to help estimate the occupancies
  //cout << "pen prop" << endl << pen_prop_bay << endl;
  //cout << "est c age err" << endl << est_C_age << endl;
  //exit(1);
  

  //prior for descending limb of chesapeake bay fsel
  //pen_cb_sel=norm2(log_sf3_cb(2)(1,2)-0.)+norm2(log_sf4_cb(2)(1,2)-4.)+norm2(log_sf3_cb(3)(1,2)-0.)+norm2(log_sf4_cb(3)(1,2)-4.); //descending params for fsel in the 2nd timeblock (1990-2015) and 2nd timestep (jul-dec)
  pen_cb_sel=0.0; 
  //add all the components of the negative log likelihood together
  //neg_LL=sum(Lcatch)+sum(Lcatchagecomp)+sum(Lindex_a)+sum(Lindex_b)+sum(Lindex_c)+sum(Lindexagecomp_a)+sum(Lindexagecomp_b)+sum(Lindexagecomp_c)+sum(Lage1index)+sum(Lyoyindex)+Lfsel+Lssel_a+Lssel_b+Lssel_c+pen_N0_dev+pen_f2sel+pen_F; 
  //cout << "Lindex nj" << " " << Lindex_nj << endl;
  neg_LL=sum(Lcatch)+sum(Lcatchagecomp)+sum(Lindex_bay)+sum(Lindex_coast)+sum(Lindexagecomp_bay)+sum(Lindexagecomp_coast)+
          Lage1index_coast+Lage1index_bay+Lyoyindex_bay+Lyoyindex_coast+pen_F+pen_prop+pen_prop_bay+pen_cb_sel+
          pen_rdev+pen_fdev;//sum(Lfsel)+Lssel_md+sum(Lssel_cm)+Lssel_ny+Lssel_nj+sum(Lssel_ct)+Lssel_des+Lssel_de30
  //neg_LL=0.0;
  /*
  cout << "neg_LL " << endl << neg_LL << " = " << sum(Lcatch) << " sum(Lcatch)" << " + " << sum(Lcatchagecomp)
  << " sum (Lcatchagecomp" <<  " + "  << sum(Lindex_coast) << endl << " L index cosat + " << sum(Lindex_bay) <<
  " L Index coast + " << sum(Lindexagecomp_bay) << " L index agecomp bay + " << sum(Lindexagecomp_coast) <<
  "L index agecomp coast + " <<  Lage1index_bay << "Lindex agecomp bay + " << Lage1index_coast << "L age1 index coast + " <<
  Lyoyindex_bay << "Lyoyindex bay + " << endl << Lyoyindex_coast << "Lyoyindex coast + " << sum(Lfsel) <<
  "Lfsel + " << sum(Lssel_coast) << "L ssel coast + " << sum(Lssel_coast) << "Lssel coast + " << pen_F << " pen f + " <<
  pen_prop << " pen prop + " << pen_prop_bay << "pen prop bay + " << pen_cb_sel << " pen cb sel" << pen_rdev << " + pen rdev " 
  << pen_fdev << " + pen f dev " << endl << endl;
  exit(1);
  //cout << "end neg_ll" << endl;
  //exit(1);
  */
  //exit(1);

FUNCTION dvar_vector migrate(dvar_vector N,dvar_vector P)
  return(elem_prod(N,P));

FUNCTION dvariable beta_mig(dvar_vector occ_prob, dvector alpha, dvector beta)
  dvariable betaLL;
  betaLL=0.0;
  for(a=fage+1;a<=lage;a++)
  {
    betaLL+=-(alpha(a)-1)*log(occ_prob(a))-(beta(a)-1)*log(1-occ_prob(a));
  }
  return(betaLL);

  
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
  /*
  report << "observed catch" << endl << obs_C << endl;
  report << "estimated catch" << endl << est_region_C << endl;
  report << "observed index coast" << endl << obs_I_coast << endl;
  report << "observed index bay" << endl << obs_I_bay <<endl;
  report << "estimated index coast" << endl << est_I_coast << endl;
  report << "estimated index bay" << endl << est_I_bay << endl;
  //report << "fishery selectivity" << endl << fsel << endl;
  //report << "survey selectivity" << endl << ssel << endl;
  report << "abundance" << endl << N << endl;
  report << "fishing mortality" << endl << F << endl;
  //report << "biomass" << endl << B << endl;
  //report << "SSB" << endl << SSB << endl;
  
  ofstream catout("catch.txt");
  {
    catout << "year timestep region logobs logpred " << endl;
    for(y=fmyear;y<=lmyear;y++)
    {
      for(t=1;t<=tstep;t++)
      {
       for(r=1;r<=region;r++)
       {
         catout << y << " " << t << " " << r << " "  << log(obs_C(r,t,y)+0.00001) << " " << log(est_region_C(r,t,y)+0.00001) << endl;
        }//close region
      }//close tstep
    }//close yr
   }//close ofstream
   
  
    ofstream ioaout("ioa.txt");
    {
        ioaout << "agegroup survey year timestep obsioa predioa" << endl;
        for(y=fmyear;y<=lmyear;y++)
        {
          for(ts=1;ts<=tstep;ts++)
          {
             ioaout << "1-15" << " " << "bay" << " " << y << " " << ts << " "<< log(obs_I_bay(ts,y)+0.00001) << " " << log(est_I_bay(ts,y)+0.00001) << endl;
            ioaout << "1-15" <<  " " << "coast" << " " << y << " " << ts << " "<< log(obs_I_coast(ts,y)+0.00001) << " " << log(est_I_coast(ts,y)+0.00001) << endl;
          }//close timestep
       }//close year
     }//close ofstream
       
  
   ofstream obscaa("ocaa.txt");//obs fishery catch at age
   {
     obscaa << " region timestep year age obscaa estcaa obsminusest standardresid" << endl;
     for(y=fmyear;y<=lmyear;y++)
     {
       for(t=1;t<=tstep;t++)
       {
           for(a=fage;a<=lage;a++)
           {
             for(r=1;r<=region;r++)
             { 
               obscaa << " " << r << " " << t << " " << y << " " << a << " " <<  log(obs_Cp(r,t,y,a)) << " " << log(est_region_Cp(r,t,y,a)) << " " << obs_Cp(r,t,y,a)-est_region_Cp(r,t,y,a) << " " << (obs_Cp(r,t,y,a)-est_region_Cp(r,t,y,a))/sqrt(C_var(r,t,y)) << endl;
             }//close region
           }//close age      
     }//close tstep
     }//close yr
   }//close ofstream
   
   ofstream obssaa("osaa.txt");
   {
     obssaa << "agegroup survey year timestep age obssaa estsaa obsminusest standardresid" << endl;
     for(y=fmyear;y<=lmyear;y++)
     {
       for(a=fage;a<=lage;a++)
       {
         for(ts=1;ts<=tstep;ts++)
         {
           if(obs_Ip_bay(ts,y,a)!=-99) obssaa << "1-15" << " " << "bay"  << " " << y << " " << ts << " " << a << " " <<  log(obs_Ip_bay(ts,y,a)) << " " <<  log(est_Ip_bay(ts,y,a)) << " " << obs_Ip_bay(ts,y,a)-est_Ip_bay(ts,y,a) << " " << (obs_Ip_bay(ts,y,a)-est_Ip_bay(ts,y,a))/sqrt(I_var_bay(ts,y)) << endl;
           if(obs_Ip_coast(ts,y,a)!=-99) obssaa << "1-15" << " " << "coast"  << " " << y << " " << ts << " " << a << " " <<  log(obs_Ip_coast(ts,y,a)) << " " <<  log(est_Ip_coast(ts,y,a)) << " " << obs_Ip_coast(ts,y,a)-est_Ip_coast(ts,y,a) << " " << (obs_Ip_coast(ts,y,a)-est_Ip_coast(ts,y,a))/sqrt(I_var_coast(ts,y)) << endl;
         }//close tstep
       }//close age
     }//close year
   }//close of stream
   
   ofstream fselout("fsel.txt");//fsel at age
   {
     fselout << "region timestep year age fsel" << endl;
     for(r=1;r<=region;r++)
     {
       for(y=fmyear;y<=lmyear;y++)
       {
         for(t=1;t<=tstep;t++)
         {
           for(a=fage;a<=lage;a++)
           {
             fselout << r  << " " << t << " " << y << " " << a << " " << log(fsel(r,t,y,a)) << endl;
           }//age
         }//close tstep
       }//clse year
     }//closeregion
   }//close ofstream
   
   ofstream sselout("ssel.txt");//survey selectivty
   {
     sselout << "agegroup survey timestep age ssel" << endl;
     for(a=fage;a<=lage;a++)
     {
       for(t=1;t<=tstep;t++)
       {
         sselout << "1-15 " << "bay " << " " << t << " "<< a << " " << log(ssel_bay(t,a)) << endl;
         sselout << "1-15 " << "coast " << " " << t << " "<< a << " " << log(ssel_coast(t,a)) << endl;
         }//close tstep
     }//close a age
   }//close ofstream


   ofstream obsage1("age1.txt");
   {
     obsage1 << "survey year timestep obs est " << endl;
     for(y=fmyear;y<=lmyear;y++)
     {
       if(obs_I_age1_coast(y)!=-99) obsage1 << "coast" << " "  << y << " " << "2" << " "  <<  log(obs_I_age1_coast(y)) << " " <<  log(est_I_age1_coast(y)) << endl;
       if(obs_I_age1_bay(y)!=-99) obsage1 << "bay"  << " " << y << " " << "2" << " " << log(obs_I_age1_bay(y)) << " " <<  log(est_I_age1_bay(y)) << endl;
     }
   }


   ofstream obsyoy("yoy.txt");
   {
     obsyoy << "region year obsyoy estyoy " << endl;
     for(y=fmyear;y<=lmyear;y++)
     {
      if(obs_I_yoy_coast(y)!=-99) obsyoy << "coast" << " "  << y << " " <<  log(obs_I_yoy_coast(y)) << " " <<  log(est_I_yoy_coast(y)) << endl;
      if(obs_I_yoy_bay(y)!=-99) obsyoy  << "bay" << " "  << y << " " <<  log(obs_I_yoy_bay(y)) << " " <<  log(est_I_yoy_bay(y)) << endl;
     }//close year loop
   }//close of stream
   
   
   ofstream pop("pop.txt");// pop across years
   {
      pop << "stock region timstep year age  pop " << endl; 
      for(y=fmyear;y<=lmyear;y++)
      {
        for(t=1;t<=tstep;t++)
        {
          for(a=fage;a<=lage;a++)
          {
            for(r=1;r<=region;r++)
            {
              for(s=1;s<=stock;s++)
              {
               pop << " " << s << " " << r << " " <<  t << " " <<  y << " "  << a << " "  << N(s,r,t,y,a) << endl;
              }//close stock loop
            }//close region
         }//close age
      }//close tstep
     }//close year
   }
  
   ofstream mort("f.txt");// pop across years
   {
     mort << "region timestep year age  f " << endl; 
     for(r=1;r<=region;r++)
      {
           for(y=fmyear;y<=lmyear;y++)
           {
              for(t=1;t<=tstep;t++)
              {
                  for(a=fage;a<=lage;a++)
                  {
                  mort << " " << r << " " << t << " " << y  << " " << a << " " << F(r,t,y,a) << endl;
                  }//close age
              }//close tstep 
            }//close year
        }//close region loop
    }//close ofstream

    ofstream like("lik.txt");
   {
     like << "name likelihood" << endl;
     like << " " << "sumLcatch_bay" << " " << Lcatch(1,1)+Lcatch(1,2) << endl;//adding for both timesteps
     like << " " << "sumLcatch_coast" << " " <<Lcatch(2,1)+Lcatch(2,2) << endl;
     like << " " << "sumLcatchagecomp_bay" << " " << Lcatchagecomp(1,1)+Lcatchagecomp(1,2) << endl;
     like << " " << "sumLcatchagecomp_coast" << " " << Lcatchagecomp(2,1)+Lcatchagecomp(2,2) << endl;   
     like << " " << "sum(Lindex_coast)" << " " << sum(Lindex_coast) << endl;
     like << " " << "sum(Lindex_bay)" << " " << sum(Lindex_bay) << endl;
     like << " " << "Lindexagecomp_bay" << " " << sum(Lindexagecomp_bay) << endl;
     like << " " << "Lindexagecomp_coast" << " " << sum(Lindexagecomp_coast) << endl;
     like << " " << "Lage1index_bay" << " " << Lage1index_bay << endl;
     like << " " << "Lyoyindex_bay" << " " << Lyoyindex_bay << endl;
     like << " " << "Lyoyindex_coast" << " " << Lyoyindex_coast << endl;
     like << " " << "Pen_f" << " " << pen_F << endl;
     like << " " << "pen_prop_bay" << " " << pen_prop_bay << endl;
     like << " " << "pen_prop" << " " << pen_prop << endl;
     like << " " << "pen_cb_sel" << " " << pen_cb_sel << endl;
     like << " " << "pen_rdev" << " " << pen_rdev << endl;
     like << " " << "pen_fdev" << " " << pen_fdev << endl;
     //like << " " << "total_survey_index" << " " << Lindex_md+sum(Lindex_cm)+Lindex_ny+Lindex_nj+sum(Lindex_ct)+Lindex_des+Lindex_de30 << endl;
     //like << " " << "total_survey_agecomp" << " " << Lindexagecomp_md+sum(Lindexagecomp_cm)+Lindexagecomp_ny+Lindexagecomp_nj+sum(Lindexagecomp_ct)+ Lindexagecomp_des+Lindexagecomp_de30 << endl;
     //like << " " << "total_age1_index" << " " << Lage1index_md+sum(Lage1index_ny) << endl;
     //like << " " << "total_yoy_index" << " " << sum(Lyoyindex_bay)+sum(Lyoyindex_coast) << endl;
     //like << " " << "total_neg_ll" << " " << neg_LL << endl;
   }
   
   ofstream occprob("occ.txt");//occupancy probability
   {
     occprob << "param stock timestep age prob logsd" << endl;
       //for(s=1;s<=stock;s++)
       //{
         for(t=1;t<=tstep;t++)
         {
           for(a=fage+1;a<=lage;a++)
           {
              occprob << " " << "est" <<  " " << "1" << " " << t << " " << a  << " " << exp(log_prop_bay(t,a)) << " " << log_sd_bay(t,a) << endl;
              occprob << " " << "obs" << " " << "1" << " " << t << " " << a << " " <<  exp(prop_bay(t,a)) << " " << log_sd_bay(t,a) << endl;
              occprob << " " << "est" << " " << "2" << " " << t << " " << a  << " " << exp(log_prop_coast(t,a)) << " " << log_sd_coast(t,a) << endl;
              occprob << " " << "obs" << " " << "2" << " " << t << " " << a << " " <<  exp(prop_coast(t,a)) << " " << log_sd_coast(t,a) << endl;
           }//close age+1 loop
         }//close tstep 
       //}//close stock loop
    }//close ofstream

   ofstream ssb("ssb.txt");//spawning stock biomass
   {
     ssb << "stock region year ssb" << endl;
       for(s=1;s<=stock;s++)
       {
         for(r=1;r<=region;r++)
         {
           for(y=fmyear;y<=lmyear;y++)
           {
              ssb << " " << s <<  " " << r << " " << y  << " " << SSB(s,r,y) << endl;
           }//close year loop
         }//close region 
       }//close stock loop
    }//close ofstream

   ofstream bio("biomass.txt");//spawning stock biomass
   {
     bio << "stock region year ssb" << endl;
       for(s=1;s<=stock;s++)
       {
         for(r=1;r<=region;r++)
         {
           for(y=fmyear;y<=lmyear;y++)
           {
             //for(ts=1;ts<=tstep;ts++)
             //{
              bio << " " << s <<  " " << r << " " << y  << " " << B(s,r,y) << endl;
             //}
           }//close year loop
         }//close region 
       }//close stock loop
    }//close ofstream
    
   ofstream weightf("weightf.txt");
   {
     weightf << "region timestep year age  f " << endl;
          for(r=1;r<=region;r++)
      {
           for(y=fmyear;y<=lmyear;y++)
           {
              for(ts=1;ts<=tstep;ts++)
              {
                  for(a=4;a<=lage;a++)
                  {
                  weightf << " " << r << " " << ts << " " << y  << " " << a << " " << Freg_4plus(r,ts,y,a) << endl;
                  }//close age
              }//close tstep 
            }//close year
        }//close region loop
   }//close ofstream

   ofstream stockF("stockF.txt");
   {
     stockF << "year stock f" << endl;
     for(y=fmyear;y<=lmyear;y++)
     {
       for(s=1;s<=stock;s++)
       {
         stockF << " " << y << " " << s << " " << Fplus(s,y) <<  endl;
       }//close stock loop
     }//close year loop
   }//close ofstream
  */

FINAL_SECTION
  // CALC SPR MODEL

  //set fishing mortality in equillibrium = to the last year of timeseries
  for(r=1;r<=region;r++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage;a<=lage;a++)
      {
        F_eq(r,ts,a)=F(r,ts,lmyear,a); //year=30, last year in the model
      } //close age loop
    } // close timestep loop
  } // close region loop

  //calculate Fa which is the sum of all F for each age
  Fa=0.;//initializing
  for(r=1;r<=region;r++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage;a<=lage;a++)
      {
        Fa(a)+=F_eq(r,ts,a);
      }//close age loop
    } //close tstep loop
  } //close region loop

  //calculate V, which is regional f/fa
  for(r=1;r<=region;r++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage;a<=lage;a++)
      {
        V(r,ts,a)=F_eq(r,ts,a)/max(Fa);
      }//close age loop
    } //close tstep loop
  } //close region loop

  //calculate R in equillibrium which is equal to the proportion of recruitment in each stock
  for(s=1;s<=stock;s++)
  {
    R_eq(s)=mfexp(log_R(s))/sum(mfexp(log_R));
    //R_eq(s)=Nt(s,1,lmyear,fage)/(Nt(1,1,lmyear,fage)+Nt(2,1,lmyear,fage));
  }//close stock loop
  //cout << R_eq << endl;

   for(s=1;s<=stock;s++)
   {
     SSBf(s)=0.; //initialize
   }
   
  //start loop to find best G value that gives you 40% F
  for(G=0;G<=150;G++)  //looping through f's 
  {
    //G=0.2;
    G_spr=0.01*double(G);  //Fishing mort SPR is 0.01*f
    //Zf=0.0;       //Zf is 0 to start

    //calculate F star, which calls on G values
    for(r=1;r<=region;r++)
    {
      for(ts=1;ts<=tstep;ts++)
      {
        for(a=fage;a<=lage;a++)
        {
          Fstar(r,ts,a)=G_spr*V(r,ts,a); //G is a scalar, g=0 is the unfished condition
        }//close age loop
      } //close tstep loop
    } //close region loop

   //set time-step values for abundance calculations
   //ts1=1;
   //ts2=2;

   //calculation population dynamics in equillibrium for G calculation

   //equillibrium mortlaity
   for(r=1;r<=region;r++)
   {
     for(ts=1;ts<=tstep;ts++)
     {
       for(a=fage;a<=lage;a++)
       {
         Zf(r,ts,a)=Fstar(r,ts,a)+M(a);
       }//close age loop
     } //close tstep loop
   } //close region loop

   //dyanamisc in yr 1
   for(s=1;s<=stock;s++)
   {
     Nf_stock(s,1,fmyear,fage)=R_eq(s);//recruitment in the first yr, first time-step
     //for(a=2;a<=lage;a++)  //looping through the ages
     //{
     //  Nf_stock(s,1,fmyear,a)=Nf_stock(s,1,fmyear,a-1)*mfexp(-Zf(s,1,a-1));   //equillibrium population dynamics, region=stocks, for first timestep
     // }//close age loop
     //  Nf_stock(s,1,fmyear,lage)/=1-mfexp(-(Zf(s,1,lage))); //calculationg the plus group, again region for Zf in first timestep is equal to the stock
      //cout<<Nf<<endl;
    }//close stock loop

   //put fish in their correct regions
   /*
   for(s=1;s<=stock;s++)
   {
     for(r=1;r<=region;r++)
     {
       //for(a=fage;a<=lage;a++)
       //{
         Nf(s,r,1,fmyear)=migrate(Nf_stock(s,1,fmyear),prop(s,1,r));//timestep=1
       //}//close age loop
     }//close region loop
   } //close stock loop

   //calculate dynamics in timestep2, first year
   for(a=fage;a<=lage;a++)
   {
     Nf_stock(1,2,fmyear,a)=Nf(1,1,1,fmyear,a)*mfexp(-Zf(1,1,a))+Nf(1,2,1,fmyear,a)*mfexp(-Zf(2,1,a)); //calculate total stock abundance stock=1
     Nf_stock(2,2,fmyear,a)=Nf(2,1,1,fmyear,a)*mfexp(-Zf(1,1,a))+Nf(2,2,1,fmyear,a)*mfexp(-Zf(2,1,a)); //calc total stock abundance, stock=1
   }//close age loop

   //put fish in their correct regions, yr=1,ts=2
   for(s=1;s<=stock;s++)
   {
     for(r=1;r<=region;r++)
     {
       //for(a=fage;a<=lage;a++)
       //{
         Nf(s,r,2,fmyear)=migrate(Nf_stock(s,2,fmyear),prop(s,2,r));
       //}//close age loop
     }//close region loop
   } //close stock loop
   */
   //now to the reamianing years from year2-100 year
   for(y=2;y<=years;y++)
   {
     for(s=1;s<=stock;s++)
     {
       Nf_stock(s,1,y,fage)=R_eq(s);
       for(a=2;a<=lage;a++)
       {
         Nf_stock(s,1,y,a)=Nf(s,1,2,y-1,a-1)*mfexp(-Zf(1,2,a-1))+Nf(s,2,2,y-1,a-1)*mfexp(-Zf(2,2,a-1)); //time-step=2, previous year and previous age
       }//close age loop
       Nf_stock(s,1,y,lage)+=Nf(s,1,2,y-1,lage)*mfexp(-Zf(1,2,lage))+Nf(s,2,2,y-1,lage)*mfexp(-Zf(2,2,lage));
     //put fish in correct regions
     for(r=1;r<=region;r++)
     {
       Nf(s,r,1,y)=migrate(Nf_stock(s,1,y),prop(s,1,r));
     }//close regionloop

     //now calculate fish abundance in second timestep

     //first age in second time-step
     //Nf_stock(s,2,y,fage)=Nf_stock(s,1,y,fage)*mfexp(-Zf(s,1,fage));
     for(a=fage;a<=lage;a++)
     {
       Nf_stock(s,2,y,a)=Nf(s,1,1,y,a)*mfexp(-Zf(1,1,a))+Nf(s,2,1,y,a)*mfexp(-Zf(2,1,a)); //calculate total stock abundance stock=1
     }//close age loop
     //put fish in correct region
     for(r=1;r<=region;r++)
     {
        Nf(s,r,2,y)=migrate(Nf_stock(s,2,y),prop(s,2,r));
     }//close region loop
    } //clsoe stock loop
   } //close year loop


   for(r=1;r<=region;r++)
   {
     //for(ts=1;ts<=tstep;ts++)
     //{
       SSBf(1,G)+=sum(elem_prod(Nf(1,r,1,100),elem_prod(sex,elem_prod(rw_age(30),m_age)))); //stock=1, timestep=1
       SSBf(2,G)+=sum(elem_prod(Nf(2,r,1,100),elem_prod(sex,elem_prod(rw_age(30),m_age)))); //stock=2, timstep=1
       //cout << SSBf(1,G) << endl;
      //}  //close tstep loop
   } //close region loop
  for(s=1;s<=stock;s++)
  {
    SPR(s,G)=SSBf(s,G)/SSBf(s,0);
  }//close stock loop
  //cout << SPR << endl;
  }//close G loop

  //Find F associated with appropriate reference point (F35%)
  G=0;
  for(s=1;s<=stock;s++)
  {
    while(SPR(s,G)>0.4 && G<150)
    {
      G++;
      //cout<<double(f)*0.01<< " " << SPR(f)<< endl;
    }
    slope(s)=(SPR(s,G)-SPR(s,G-1))/0.01;
    est_G_40(s)=((0.4-SPR(s,G))+slope(s)*double(G)*.01)/slope(s);
  }
  //cout << "SSBf = 1" << endl << SSBf(1) <<  endl << endl;
  //cout << "SSBf =2" << endl << SSBf(2) << endl << endl;
  //cout << est_G_40 << endl;
  //exit(1);

  est_G=est_G_40(1)*(R_eq(1)/sum(R_eq))+est_G_40(2)*(R_eq(2)/sum(R_eq));
  
  F_bar.initialize();
  for(r=1;r<=region;r++)
  {
    for(ts=1;ts<=tstep;ts++)
    {
      for(a=fage;a<=lage;a++)
      {
        F_bar(r,ts,a)+=est_G*V(r,ts,a); //G is a scalar, g=0 is the unfished condition
      }//close age loop
    } //close tstep loop
  } //close region loop

  F_bar_stock.initialize();
  for(s=1;s<=stock;s++)
  {
    for(r=1;r<=region;r++)
    {
      for(ts=1;ts<=tstep;ts++)
      {
        for(a=fage;a<=lage;a++)
        {
          F_bar_stock(s,a)+=F_bar(r,ts,a)*prop(s,ts,r,a); //G is a scalar, g=0 is the unfished condition
        }//close age loop
      } //close tstep loop
    } //close region loop
  }//close stock loop
  
  est_F_spr=0.;
  for(s=1;s<=stock;s++)
  {
    est_F_spr(s)+=F_bar_stock(s,8); //setting stock specific value as age 8 reference
  }
  //est_annual_F=0.;
  est_annual_F=F_bar_stock(1,8)*(R_eq(1)/sum(R_eq))+F_bar_stock(2,8)*(R_eq(2)/sum(R_eq)); //setting annual value as age 8 reference weighted by recruitment
  
  
  //cout << "nf stock cb last year" << endl <<  Nf_stock(1,1,100) << endl;
  //cout << "nf cb  last year" << endl <<  Nf(1,2,1,100) << "  " << endl  << Nf(1,1,1,100) << endl;
  //exit(1);

  // REPORT OUT
  
  //cout << est_N-tN << endl;
  ofstream ofs("sim_results.txt",ios::app);
  {
    for(y=fmyear;y<=lmyear;y++)
    {
     ofs << " " << sim_num << " " << y << " "  << est_F(y,8) << " " << (est_F(y,8)-true_F(y,8))/true_F(y,8) << " " <<
     est_Ntot(y) << " " << (est_Ntot(y)-true_Ntot(y))/true_Ntot(y) << " " <<
     est_Ntot_stock(1,y) << " " << (est_Ntot_stock(1,y)-true_Ntot_stock(1,y))/true_Ntot_stock(1,y) << " " << est_Ntot_stock(2,y) << " " << (est_Ntot_stock(2,y)-true_Ntot_stock(2,y))/true_Ntot_stock(2,y) << " " <<
     est_biomass(y) << " " << (est_biomass(y)-true_biomass(y))/true_biomass(y) << " " <<
     est_SSB(y) << " " << (est_SSB(y)-true_SSB(y))/true_SSB(y) << " " <<
     est_annual_F << " " << (est_annual_F-true_annual_F)/true_annual_F << " " << 
     est_F_spr(1) << " " << (est_F_spr(1)-true_F_spr(1))/true_F_spr(1) << " " << est_F_spr(2) << " " << (est_F_spr(2)-true_F_spr(2))/true_F_spr(2) << " " <<
     log_R(1) << " " << (log_R(1)-start_log_R(1))/start_log_R(1) << " " << log_R(2) << " " << (log_R(2)-start_log_R(2))/start_log_R(2) << " " <<
     objective_function_value::pobjfun->gmax<<endl;
    }
  }
  ofstream ofsf("sim_F_results.txt",ios::app);
  {
    for(y=fmyear;y<=lmyear;y++)
    {
     ofsf << " " << sim_num << " " << y << " " << est_F(y) << " " << elem_div((est_F(y)-true_F(y)),true_F(y)) << " " << objective_function_value::pobjfun->gmax<<endl;
    }
  }
  /*
  ofstream ofs40("sim_F40_results.txt",ios::app);
  {
    for(a=fage;a<=lage;a++)
    {
    //ofs40 << " " << sim_num << " " << est_annual_F << " " << est_G_40(1) << " " << est_G_40(2) << " " <<  est_G << " " << mfexp(log_R(1))/sum(mfexp(log_R)) <<endl;
     ofs40 << " " << sim_num << " " << V(1,1,a) << " " << Fa(a) << " " << F_eq(1,1,a) <<endl;
    }
  }
  */
RUNTIME_SECTION

  maximum_function_evaluations 20000, 20000, 20000 //change the maximum number of iterations for each phase
  
  


  
  


