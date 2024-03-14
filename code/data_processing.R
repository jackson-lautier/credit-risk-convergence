######################################################################################
######################################################################################
######################################################################################
# Data processing scripts to produce the data used in the manuscript:
# 
# "ON THE CONVERGENCE OF CREDIT RISK IN
# CURRENT CONSUMER AUTOMOBILE LOANS"
#
# LAUTIER, POZDNYAKOV, YAN
# 2024
#
#R version 4.3.2 (2023-10-31 ucrt)
#RStudio 2023.03.0+386 "Cherry Blossom" Release
#(3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09) for Windows
######################################################################################
######################################################################################
######################################################################################
######################################################################################
# INSTRUCTIONS
#
# supporting files:
# 'default_time.R' #functions used throughout 
#'mhm_sa19.csv', 'mhm_sa17.csv', #input data, manheim index
# drive194_compiledr.csv', sdart193_compiledr.csv', 'carmx194_compiledr.csv',
# aart193_compiledr.csv', #2019 input data
#'sdart172_compiledr.csv', 'drive171_compiledr.csv',
# carmx172_compiledr.csv', 'aart173_compiledr.csv', #2017 input data
#'csh_lam.csv' #sim study probability inputs
#'refi_svgs2.xlsx' #to calculate the potential loan savings
#
#The code must be run sequentially downwards.
#As the new, cleaned files are prepared, they will be saved in a new
#folder 'processed_data' in the wd.
#For data analysis, proceed directly to 'data_analysis.R'.
#
#The figure and table references correspond to the January 2024 version
#of the working manuscript.  While the order of figures are subject to
#change, the figures themselves are likely to remain stable.
#
######################################################################################
######################################################################################
######################################################################################
######################################################################################
require('lubridate')
#require('ExcelFunctionsR')
#require('ggplot2')
#require('extrafont') #may need to load fonts
#require('cowplot')
#require('purrr')
######################################################################################
######################################################################################
######################################################################################
######################################################################################

#where processed data will be stored
dir.create('./processed_data/')

######################################################################################
######################################################################################
# CREATING THE FILTERED LOAN POPULATION (2017)
######################################################################################
######################################################################################

#default time function
source("./code/default_time.R")

#SDART 2017-2
{
loan_term = 72
len_obs_window = 51 #num. mnths in obs. window

path = "./raw_data/"
sdart <- read.csv(paste(path,'sdart172_compiledr.csv',sep=""))
sdart <- sdart[sdart$coObligorIndicator == "False",]
sdart <- sdart[sdart$vehicleNewUsedCode == 2,]
sdart <- sdart[sdart$subvented == 0,]
sdart <- sdart[sdart$underwritingIndicator == "True",]
sdart <- sdart[sdart$obligorIncomeVerificationLevelCode == 2,]
sdart = sdart[sdart$repossessedIndicator == "False",]
sdart <- sdart[sdart$originalLoanTerm == loan_term,]
#count: 25,877

#calculate remaining payments
sdart_trust_start_date = "05-01-2017"
date <- paste(sdart$originationDate,"-01",sep="")
date <- as.Date(date, "%m/%Y-%d")
age = interval(date,as.Date(sdart_trust_start_date,"%m-%d-%Y")) %/% months(1)
sdart$initialLoanAge = age

sdart = sdart[sdart$initialLoanAge <= 18,]
sdart$remainingTermtoMaturityNumber = sdart$originalLoanTerm - sdart$initialLoanAge
#20,943

#create credit risk categories
sdart$risk_cat_ir <- as.factor(
  ifelse(sdart$originalInterestRatePercentage<0.05,"super_prime",
         ifelse(sdart$originalInterestRatePercentage<0.10,"prime",
                ifelse(sdart$originalInterestRatePercentage<0.15,"near_prime",
                       ifelse(sdart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))

delta = loan_term - max(sdart$remainingTermtoMaturityNumber) 
M = loan_term - min(sdart$remainingTermtoMaturityNumber) - delta
T_start = M + delta + sdart$remainingTermtoMaturityNumber - loan_term 
Y = M + delta - T_start + 1

######################################################################
# algorithm to find loan outcomes (def, repay, cens)
######################################################################
X = vector()
C = vector()
D = vector()
R = vector()

for (j in c(1:nrow(sdart))) {
  c_bond = default_time(sdart[j,])
  X = append(X, c_bond[1])
  C = append(C, c_bond[2])
  R = append(R, c_bond[3])
  D = append(D, c_bond[4])
}

#shift back to the original timeline
Xc = M + delta + X - T_start + 1

######################################################################
######################################################################
sdart = cbind(sdart,Y,X,Xc,C,D,R)

#data integrity check
s_cens = sdart[sdart$C == 1,]
n = nrow(s_cens)
check = c()

for (i in c(1:n)) {
  b_dat = s_cens[i,]
  
  final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
  check = append(check,
                 ifelse(is.na(final_bal),"check",0))
}

bad_data = s_cens$assetNumber[check == "check"]
length(bad_data)
#751 'bad' loans of 20,943

sdart = sdart[!(sdart$assetNumber %in% bad_data),]
write.csv(sdart,"./processed_data/sdart20192_2017.csv")
rm(list=ls())
}

#default time function
source("./code/default_time.R")

#DRIVE 2017-1
{
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "./raw_data/"
  drv <- read.csv(paste(path,'drive171_compiledr.csv',sep=""))
  drv <- drv[drv$coObligorIndicator == "False",]
  drv <- drv[drv$vehicleNewUsedCode == 2,]
  drv <- drv[drv$subvented == 0,]
  drv <- drv[drv$underwritingIndicator == "True",]
  drv <- drv[drv$obligorIncomeVerificationLevelCode == 2,]
  drv = drv[drv$repossessedIndicator == "False",]
  drv <- drv[drv$originalLoanTerm == loan_term,] 
  #count: 30,904
  
  #calculate remaining payments
  drv_trust_start_date = "07-01-2017"
  date <- paste(drv$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(drv_trust_start_date,"%m-%d-%Y")) %/% months(1)
  drv$initialLoanAge = age
  
  drv = drv[drv$initialLoanAge <= 18,]
  drv$remainingTermtoMaturityNumber = drv$originalLoanTerm - drv$initialLoanAge
  #30,095
  
  #create credit risk categories
  drv$risk_cat_ir <- as.factor(
    ifelse(drv$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(drv$originalInterestRatePercentage<0.10,"prime",
                  ifelse(drv$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(drv$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(drv$remainingTermtoMaturityNumber) 
  M = loan_term - min(drv$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + drv$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(drv))) {
    c_bond = default_time(drv[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  drv = cbind(drv,Y,X,Xc,C,D,R)
  
  #data integrity check
  d_cens = drv[drv$C == 1,]
  n = nrow(d_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = d_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = d_cens$assetNumber[check == "check"]
  length(bad_data)
  #1,175 'bad' loans of 30,095
  
  drv = drv[!(drv$assetNumber %in% bad_data),]
  write.csv(drv,"./processed_data/drv28920_2017.csv")
  rm(list=ls())
}

#default time function
source("./code/default_time.R")

#CARMX 2017-2
{
  loan_term_c = 73
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "./raw_data/"
  cmax <- read.csv(paste(path,'carmx172_compiledr.csv',sep="")) #
  cmax <- cmax[cmax$coObligorIndicator == "False",]
  cmax <- cmax[cmax$vehicleNewUsedCode == 2,]
  cmax <- cmax[cmax$subvented == 0,]
  cmax <- cmax[cmax$underwritingIndicator == "True",]
  cmax <- cmax[cmax$obligorIncomeVerificationLevelCode == 2,]
  cmax = cmax[cmax$repossessedIndicator == "False",]
  cmax <- cmax[cmax$originalLoanTerm == loan_term_c,] 
  #count: 7,607
  
  #calculate remaining payments
  cmax_trust_start_date = "05-01-2017"
  date <- paste(cmax$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(cmax_trust_start_date,"%m-%d-%Y")) %/% months(1)
  cmax$initialLoanAge = age
  
  cmax = cmax[cmax$initialLoanAge <= 18,]
  cmax$remainingTermtoMaturityNumber = cmax$originalLoanTerm - cmax$initialLoanAge
  #7,515
  
  #create credit risk categories
  cmax$risk_cat_ir <- as.factor(
    ifelse(cmax$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(cmax$originalInterestRatePercentage<0.10,"prime",
                  ifelse(cmax$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(cmax$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term_c - max(cmax$remainingTermtoMaturityNumber) 
  M = loan_term_c - min(cmax$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + cmax$remainingTermtoMaturityNumber - loan_term_c
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(cmax))) {
    c_bond = default_time(cmax[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  cmax = cbind(cmax,Y,X,Xc,C,D,R)
  
  #data integrity check
  c_cens = cmax[cmax$C == 1,]
  n = nrow(c_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = c_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = c_cens$assetNumber[check == "check"]
  length(bad_data) #680 'bad' loans of 7,515
  
  cmax = cmax[!(cmax$assetNumber %in% bad_data),]
  write.csv(cmax,"./processed_data/cmax6835_2017.csv")
  rm(list=ls())
}

#default time function
source("./code/default_time.R")

#AART 2017-3
{
  loan_term_c = 73
  len_obs_window = 43 #num. mnths in obs. window
  
  path = "./raw_data/"
  aart <- read.csv(paste(path,'aart173_compiledr.csv',sep=""))
  aart <- aart[aart$coObligorIndicator == "False",]
  aart <- aart[aart$vehicleNewUsedCode == 2,]
  aart <- aart[aart$subvented == 0,]
  aart <- aart[aart$underwritingIndicator == "True",]
  aart <- aart[aart$obligorIncomeVerificationLevelCode == 2,]
  aart = aart[aart$repossessedIndicator == "False",]
  aart <- aart[aart$originalLoanTerm == loan_term_c,] 
  #count: 3,638
  
  #calculate remaining payments
  aart_trust_start_date = "06-01-2017"
  date <- paste(aart$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(aart_trust_start_date,"%m-%d-%Y")) %/% months(1)
  aart$initialLoanAge = age
  
  aart = aart[aart$initialLoanAge <= 18,]
  aart$remainingTermtoMaturityNumber = aart$originalLoanTerm - aart$initialLoanAge
  #2195
  
  #create credit risk categories
  aart$risk_cat_ir <- as.factor(
    ifelse(aart$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(aart$originalInterestRatePercentage<0.10,"prime",
                  ifelse(aart$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(aart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term_c - max(aart$remainingTermtoMaturityNumber) 
  M = loan_term_c - min(aart$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + aart$remainingTermtoMaturityNumber - loan_term_c 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(aart))) {
    c_bond = default_time(aart[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  aart = cbind(aart,Y,X,Xc,C,D,R)
  
  a_cens = aart[aart$C == 1,]
  n = nrow(a_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = a_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = a_cens$assetNumber[check == "check"]
  length(bad_data) #24 'bad' loans of 2,195
  
  aart = aart[!(aart$assetNumber %in% bad_data),]
  write.csv(aart,"./processed_data/aart2171_2017.csv")
  rm(list=ls())
}


######################################################################################
######################################################################################
# CREATING THE FILTERED LOAN POPULATION (2019)
######################################################################################
######################################################################################

#step 1: filter 2019 data

#default time function
source("./code/default_time.R")

#SDART 2019-3
{
  loan_term = 72
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "./raw_data/"
  sdart <- read.csv(paste(path,'sdart193_compiledr.csv',sep=""))
  sdart <- sdart[sdart$coObligorIndicator == "False",]
  sdart <- sdart[sdart$vehicleNewUsedCode == 2,]
  sdart <- sdart[sdart$subvented == 0,]
  sdart <- sdart[sdart$underwritingIndicator == "True",]
  sdart <- sdart[sdart$obligorIncomeVerificationLevelCode == 2,]
  sdart = sdart[sdart$repossessedIndicator == "False",]
  sdart <- sdart[sdart$originalLoanTerm == loan_term,]
  #count: 24,799
  
  #calculate remaining payments
  sdart_trust_start_date = "08-01-2019"
  date <- paste(sdart$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(sdart_trust_start_date,"%m-%d-%Y")) %/% months(1)
  sdart$initialLoanAge = age
  
  sdart = sdart[sdart$initialLoanAge <= 18,]
  sdart$remainingTermtoMaturityNumber = sdart$originalLoanTerm - sdart$initialLoanAge
  #20,113
  
  #create credit risk categories
  sdart$risk_cat_ir <- as.factor(
    ifelse(sdart$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(sdart$originalInterestRatePercentage<0.10,"prime",
                  ifelse(sdart$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(sdart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(sdart$remainingTermtoMaturityNumber) 
  M = loan_term - min(sdart$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + sdart$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(sdart))) {
    c_bond = default_time(sdart[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  sdart = cbind(sdart,Y,X,Xc,C,D,R)
  
  #data integrity check
  s_cens = sdart[sdart$C == 1,]
  n = nrow(s_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = s_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = s_cens$assetNumber[check == "check"]
  length(bad_data)
  #151 'bad' loans of 20,113
  
  sdart = sdart[!(sdart$assetNumber %in% bad_data),]
  write.csv(sdart,"./processed_data/sdart19962_2019.csv")
  rm(list=ls())
}

#default time function
source("./code/default_time.R")

#DRIVE 2019-4
{ 
  loan_term = 72
  len_obs_window = 50 #num. mnths in obs. window
  
  path = "./raw_data/"
  drv <- read.csv(paste(path,'drive194_compiledr.csv',sep=""))
  drv <- drv[drv$coObligorIndicator == "False",]
  drv <- drv[drv$vehicleNewUsedCode == 2,]
  drv <- drv[drv$subvented == 0,]
  drv <- drv[drv$underwritingIndicator == "True",]
  drv <- drv[drv$obligorIncomeVerificationLevelCode == 2,]
  drv = drv[drv$repossessedIndicator == "False",]
  drv <- drv[drv$originalLoanTerm == loan_term,] 
  #count: 37,071
  
  #calculate remaining payments
  drv_trust_start_date = "08-01-2019"
  date <- paste(drv$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(drv_trust_start_date,"%m-%d-%Y")) %/% months(1)
  drv$initialLoanAge = age
  
  drv = drv[drv$initialLoanAge <= 18,]
  drv$remainingTermtoMaturityNumber = drv$originalLoanTerm - drv$initialLoanAge
  #31,613
  
  #create credit risk categories
  drv$risk_cat_ir <- as.factor(
    ifelse(drv$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(drv$originalInterestRatePercentage<0.10,"prime",
                  ifelse(drv$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(drv$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(drv$remainingTermtoMaturityNumber) 
  M = loan_term - min(drv$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + drv$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(drv))) {
    c_bond = default_time(drv[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  drv = cbind(drv,Y,X,Xc,C,D,R)
  
  #data integrity check
  d_cens = drv[drv$C == 1,]
  n = nrow(d_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = d_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = d_cens$assetNumber[check == "check"]
  length(bad_data)
  #393 'bad' loans of 31,613
  
  drv = drv[!(drv$assetNumber %in% bad_data),]
  write.csv(drv,"./processed_data/drv31221_2019.csv")
  rm(list=ls())
}

#default time function
source("./code/default_time.R")

#CARMX 2019-4
{
  loan_term_c = 73
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "./raw_data/"
  cmax <- read.csv(paste(path,'carmx194_compiledr.csv',sep="")) #
  cmax <- cmax[cmax$coObligorIndicator == "False",]
  cmax <- cmax[cmax$vehicleNewUsedCode == 2,]
  cmax <- cmax[cmax$subvented == 0,]
  cmax <- cmax[cmax$underwritingIndicator == "True",]
  cmax <- cmax[cmax$obligorIncomeVerificationLevelCode == 2,]
  cmax = cmax[cmax$repossessedIndicator == "False",]
  cmax <- cmax[cmax$originalLoanTerm == loan_term_c,] 
  #count: 14,644
  
  #calculate remaining payments
  cmax_trust_start_date = "09-01-2019"
  date <- paste(cmax$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(cmax_trust_start_date,"%m-%d-%Y")) %/% months(1)
  cmax$initialLoanAge = age
  
  cmax = cmax[cmax$initialLoanAge <= 18,]
  cmax$remainingTermtoMaturityNumber = cmax$originalLoanTerm - cmax$initialLoanAge
  #12,220
  
  #create credit risk categories
  cmax$risk_cat_ir <- as.factor(
    ifelse(cmax$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(cmax$originalInterestRatePercentage<0.10,"prime",
                  ifelse(cmax$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(cmax$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term_c - max(cmax$remainingTermtoMaturityNumber) 
  M = loan_term_c - min(cmax$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + cmax$remainingTermtoMaturityNumber - loan_term_c
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(cmax))) {
    c_bond = default_time(cmax[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  cmax = cbind(cmax,Y,X,Xc,C,D,R)
  
  #data integrity check
  c_cens = cmax[cmax$C == 1,]
  n = nrow(c_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = c_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = c_cens$assetNumber[check == "check"]
  length(bad_data) #496 'bad' loans of 12,220
  
  cmax = cmax[!(cmax$assetNumber %in% bad_data),]
  write.csv(cmax,"./processed_data/cmax11724_2019.csv")
  rm(list=ls())
}

#default time function
source("./code/default_time.R")

#AART 2019-3
{
  loan_term_c = 73
  len_obs_window = 46 #num. mnths in obs. window
  
  path = "./raw_data/"
  aart <- read.csv(paste(path,'aart193_compiledr.csv',sep=""))
  aart <- aart[aart$coObligorIndicator == "False",]
  aart <- aart[aart$vehicleNewUsedCode == 2,]
  aart <- aart[aart$subvented == 0,]
  aart <- aart[aart$underwritingIndicator == "True",]
  aart <- aart[aart$obligorIncomeVerificationLevelCode == 2,]
  aart = aart[aart$repossessedIndicator == "False",]
  aart <- aart[aart$originalLoanTerm == loan_term_c,] 
  #count: 4,135
  
  #calculate remaining payments
  aart_trust_start_date = "08-01-2019"
  date <- paste(aart$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(aart_trust_start_date,"%m-%d-%Y")) %/% months(1)
  aart$initialLoanAge = age
  
  aart = aart[aart$initialLoanAge <= 18,]
  aart$remainingTermtoMaturityNumber = aart$originalLoanTerm - aart$initialLoanAge
  #2,909
  
  #create credit risk categories
  aart$risk_cat_ir <- as.factor(
    ifelse(aart$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(aart$originalInterestRatePercentage<0.10,"prime",
                  ifelse(aart$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(aart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term_c - max(aart$remainingTermtoMaturityNumber) 
  M = loan_term_c - min(aart$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + aart$remainingTermtoMaturityNumber - loan_term_c 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(aart))) {
    c_bond = default_time(aart[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  aart = cbind(aart,Y,X,Xc,C,D,R)
  
  a_cens = aart[aart$C == 1,]
  n = nrow(a_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = a_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = a_cens$assetNumber[check == "check"]
  length(bad_data) #14 'bad' loans of 2,909
  
  aart = aart[!(aart$assetNumber %in% bad_data),]
  write.csv(aart,"./processed_data/aart2895_2019.csv")
  rm(list=ls())
}

######################################################################################
######################################################################################
# CREATING THE NEW CAR DATA (2017)
######################################################################################
######################################################################################

#prepare 2019 new car data
{
  #default time function
  source("./code/default_time.R")
  
  #SDART 2017-2
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "./raw_data/"
  sdart <- read.csv(paste(path,'sdart172_compiledr.csv',sep=""))
  sdart <- sdart[sdart$coObligorIndicator == "False",]
  #new cars
  sdart <- sdart[sdart$vehicleNewUsedCode == 1,]
  sdart <- sdart[sdart$subvented == 0,]
  sdart <- sdart[sdart$underwritingIndicator == "True",]
  sdart <- sdart[sdart$obligorIncomeVerificationLevelCode == 2,]
  sdart = sdart[sdart$repossessedIndicator == "False",]
  sdart <- sdart[sdart$originalLoanTerm >= loan_term,] #72, 74, 75
  #count: 9152
  
  #calculate remaining payments
  sdart_trust_start_date = "05-01-2017"
  date <- paste(sdart$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(sdart_trust_start_date,"%m-%d-%Y")) %/% months(1)
  sdart$initialLoanAge = age
  
  sdart = sdart[sdart$initialLoanAge <= 18,]
  sdart$remainingTermtoMaturityNumber = sdart$originalLoanTerm - sdart$initialLoanAge
  #7778
  
  #create credit risk categories
  sdart$risk_cat_ir <- as.factor(
    ifelse(sdart$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(sdart$originalInterestRatePercentage<0.10,"prime",
                  ifelse(sdart$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(sdart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(sdart$remainingTermtoMaturityNumber) 
  M = loan_term - min(sdart$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + sdart$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(sdart))) {
    c_bond = default_time(sdart[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  sdart = cbind(sdart,Y,X,Xc,C,D,R)
  
  #data integrity check
  s_cens = sdart[sdart$C == 1,]
  n = nrow(s_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = s_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = s_cens$assetNumber[check == "check"]
  length(bad_data)
  #409 'bad' loans of 7778
  
  sdart = sdart[!(sdart$assetNumber %in% bad_data),]
  write.csv(sdart,"./processed_data/sdart7369new_2017.csv")
  rm(list=ls())
  
  #DRIVE 2017-1
  
  #default time function
  source("./code/default_time.R")
  
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "./raw_data/"
  drv <- read.csv(paste(path,'drive171_compiledr.csv',sep=""))
  drv <- drv[drv$coObligorIndicator == "False",]
  #new cars = 1
  drv <- drv[drv$vehicleNewUsedCode == 1,]
  drv <- drv[drv$subvented == 0,]
  drv <- drv[drv$underwritingIndicator == "True",]
  drv <- drv[drv$obligorIncomeVerificationLevelCode == 2,]
  drv = drv[drv$repossessedIndicator == "False",]
  drv <- drv[drv$originalLoanTerm >= loan_term,] 
  #8219
  
  #calculate remaining payments
  drv_trust_start_date = "07-01-2017"
  date <- paste(drv$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(drv_trust_start_date,"%m-%d-%Y")) %/% months(1)
  drv$initialLoanAge = age
  
  drv = drv[drv$initialLoanAge <= 18,]
  drv$remainingTermtoMaturityNumber = drv$originalLoanTerm - drv$initialLoanAge
  #8108
  
  #create credit risk categories
  drv$risk_cat_ir <- as.factor(
    ifelse(drv$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(drv$originalInterestRatePercentage<0.10,"prime",
                  ifelse(drv$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(drv$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(drv$remainingTermtoMaturityNumber) 
  M = loan_term - min(drv$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + drv$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(drv))) {
    c_bond = default_time(drv[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  drv = cbind(drv,Y,X,Xc,C,D,R)
  
  #data integrity check
  d_cens = drv[drv$C == 1,]
  n = nrow(d_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = d_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = d_cens$assetNumber[check == "check"]
  length(bad_data)
  #416 'bad' loans of 8108
  
  drv = drv[!(drv$assetNumber %in% bad_data),]
  write.csv(drv,"./processed_data/drv7629new_2017.csv")
  rm(list=ls())
  
  #default time function
  source("./code/default_time.R")
  
  #CARMX 2017-2
  loan_term = 72
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "./raw_data/"
  cmax <- read.csv(paste(path,'carmx172_compiledr.csv',sep="")) #
  cmax <- cmax[cmax$coObligorIndicator == "False",]
  #new cars = 1
  cmax <- cmax[cmax$vehicleNewUsedCode == 1,]
  cmax <- cmax[cmax$subvented == 0,]
  cmax <- cmax[cmax$underwritingIndicator == "True",]
  cmax <- cmax[cmax$obligorIncomeVerificationLevelCode == 2,]
  cmax = cmax[cmax$repossessedIndicator == "False",]
  cmax <- cmax[cmax$originalLoanTerm >= loan_term,]
  #count: 10
  
  #calculate remaining payments
  cmax_trust_start_date = "05-01-2017"
  date <- paste(cmax$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(cmax_trust_start_date,"%m-%d-%Y")) %/% months(1)
  cmax$initialLoanAge = age
  
  cmax = cmax[cmax$initialLoanAge <= 18,]
  cmax$remainingTermtoMaturityNumber = cmax$originalLoanTerm - cmax$initialLoanAge
  #9
  
  #create credit risk categories
  cmax$risk_cat_ir <- as.factor(
    ifelse(cmax$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(cmax$originalInterestRatePercentage<0.10,"prime",
                  ifelse(cmax$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(cmax$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(cmax$remainingTermtoMaturityNumber) 
  M = loan_term - min(cmax$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + cmax$remainingTermtoMaturityNumber - loan_term
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(cmax))) {
    c_bond = default_time(cmax[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  cmax = cbind(cmax,Y,X,Xc,C,D,R)
  
  #data integrity check
  c_cens = cmax[cmax$C == 1,]
  n = nrow(c_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = c_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = c_cens$assetNumber[check == "check"]
  length(bad_data) #0 'bad' loans of 9
  
  cmax = cmax[!(cmax$assetNumber %in% bad_data),]
  write.csv(cmax,"./processed_data/cmax9new_2017.csv")
  rm(list=ls())
  
  #default time function
  source("./code/default_time.R")
  
  #AART 2017-3
  loan_term = 73
  len_obs_window = 43 #num. mnths in obs. window
  
  path = "./raw_data/"
  aart <- read.csv(paste(path,'aart173_compiledr.csv',sep=""))
  aart <- aart[aart$coObligorIndicator == "False",]
  #new cars
  aart <- aart[aart$vehicleNewUsedCode == 1,]
  aart <- aart[aart$subvented == 0,]
  aart <- aart[aart$underwritingIndicator == "True",]
  aart <- aart[aart$obligorIncomeVerificationLevelCode == 2,]
  aart = aart[aart$repossessedIndicator == "False",]
  aart <- aart[aart$originalLoanTerm >= loan_term,] #73, 74, 75, 76, 77
  #2497
  
  #calculate remaining payments
  aart_trust_start_date = "06-01-2017"
  date <- paste(aart$originationDate,"-01",sep="")
  date <- as.Date(date, "%m/%Y-%d")
  age = interval(date,as.Date(aart_trust_start_date,"%m-%d-%Y")) %/% months(1)
  aart$initialLoanAge = age
  
  aart = aart[aart$initialLoanAge <= 18,]
  aart$remainingTermtoMaturityNumber = aart$originalLoanTerm - aart$initialLoanAge
  #1360
  
  #create credit risk categories
  aart$risk_cat_ir <- as.factor(
    ifelse(aart$originalInterestRatePercentage<0.05,"super_prime",
           ifelse(aart$originalInterestRatePercentage<0.10,"prime",
                  ifelse(aart$originalInterestRatePercentage<0.15,"near_prime",
                         ifelse(aart$originalInterestRatePercentage<0.20,"subprime","deep_subprime")))))
  
  delta = loan_term - max(aart$remainingTermtoMaturityNumber) 
  M = loan_term - min(aart$remainingTermtoMaturityNumber) - delta
  T_start = M + delta + aart$remainingTermtoMaturityNumber - loan_term 
  Y = M + delta - T_start + 1
  
  ######################################################################
  ######################################################################
  ######################################################################
  # algorithm to find loan outcomes (def, repay, cens)
  ######################################################################
  ######################################################################
  ######################################################################
  X = vector()
  C = vector()
  D = vector()
  R = vector()
  
  for (j in c(1:nrow(aart))) {
    c_bond = default_time(aart[j,])
    X = append(X, c_bond[1])
    C = append(C, c_bond[2])
    R = append(R, c_bond[3])
    D = append(D, c_bond[4])
  }
  
  #shift back to the original timeline
  Xc = M + delta + X - T_start + 1
  
  ######################################################################
  ######################################################################
  aart = cbind(aart,Y,X,Xc,C,D,R)
  
  a_cens = aart[aart$C == 1,]
  n = nrow(a_cens)
  check = c()
  
  for (i in c(1:n)) {
    b_dat = a_cens[i,]
    
    final_bal = as.numeric(b_dat[1,paste("BAL",len_obs_window,sep="")])
    check = append(check,
                   ifelse(is.na(final_bal),"check",0))
  }
  
  bad_data = a_cens$assetNumber[check == "check"]
  length(bad_data) #18 'bad' loans of 1360
  
  aart = aart[!(aart$assetNumber %in% bad_data),]
  write.csv(aart,"./processed_data/aart1342new_2017.csv")
  rm(list=ls())  
}
