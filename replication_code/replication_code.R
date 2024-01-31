######################################################################################
######################################################################################
######################################################################################
# REPLICATION CODE FOR
# "ON THE CONVERGENCE OF CREDIT RISK IN
# CURRENT CONSUMER AUTOMOBILE LOANS"
#
# LAUTIER, POZDNYAKOV, YAN
# 2024
#
#R version 4.3.2 (2023-10-31 ucrt)
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
#As the new, cleaned files are prepared, they will be saved in the current
#working directory.
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
require('ExcelFunctionsR')
require('ggplot2')
require('extrafont') #may need to load fonts
require('cowplot')
require('purrr')
######################################################################################
######################################################################################
######################################################################################
######################################################################################


######################################################################################
######################################################################################
# CREATING THE FILTERED LOAN POPULATION (2017)
######################################################################################
######################################################################################

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#SDART 2017-2
{
loan_term = 72
len_obs_window = 51 #num. mnths in obs. window

path = "~/loan_risk_convergence/replication_code/"
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
write.csv(sdart,"~/loan_risk_convergence/replication_code/sdart20192_2017.csv")
rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#DRIVE 2017-1
{
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(drv,"~/loan_risk_convergence/replication_code/drv28920_2017.csv")
  rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#CARMX 2017-2
{
  loan_term_c = 73
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(cmax,"~/loan_risk_convergence/replication_code/cmax6835_2017.csv")
  rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#AART 2017-3
{
  loan_term_c = 73
  len_obs_window = 43 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(aart,"~/loan_risk_convergence/replication_code/aart2171_2017.csv")
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE 1: Classical Consumer Auto Securitization Loss Curves
######################################################################################
######################################################################################

{
path = "~/loan_risk_convergence/replication_code/"
sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))

dfs = data.frame("age" = drv$X, "outcome" = drv$D)
cts = aggregate(outcome ~ age, dfs, sum)
cts$run_tot = cumsum(cts$outcome)
cts$def_perc = cts$run_tot / nrow(drv)

dfp = data.frame("age" = cmax$X, "outcome" = cmax$D)
ctp = aggregate(outcome ~ age, dfp, sum)
ctp$run_tot = cumsum(ctp$outcome)
ctp$def_perc = ctp$run_tot / nrow(cmax)

dfa = data.frame("age" = sdart$X, "outcome" = sdart$D)
cta = aggregate(outcome ~ age, dfa, sum)
cta$run_tot = cumsum(cta$outcome)
cta$def_perc = cta$run_tot / nrow(sdart)

ctp$risk = "cmax"
cts$risk = "drv"
cta$risk = "sdart"

plot_dat = rbind(ctp, cts, cta)
plot_dat$def_perc = plot_dat$def_perc * 100

ggplot(data=plot_dat,
       aes(x=age,y=def_perc,color=risk))+
  geom_point(size=1) +
  geom_line(linewidth=0.5) +
  xlab("Securitization Age (Months)") +
  ylab("Cumulative Defaults (%)") +
  theme_bw() +
  theme(legend.position="none") +
  guides(linetype=guide_legend(""),
         color=guide_legend("")) +
  theme(axis.title.x=element_text(size=9, family="Times New Roman"),
        axis.text.x=element_text(size=9, family="Times New Roman"),
        axis.text.y=element_text(size=9, family="Times New Roman"),
        axis.title.y=element_text(size=9,family="Times New Roman"),
        legend.text=element_text(size=9, family="Times New Roman"),
        strip.text.y = element_text(size = 9, family="Times New Roman"),
        legend.title=element_text(size=10, family="Times New Roman")) +
  geom_vline(xintercept=40, color="grey",linetype=2)
#save plot, if desired
#ggsave("loss_curves.pdf",height=4,width=6,device = cairo_pdf)
rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE 2: Credit Risk Convergence: Subprime and Prime Loans
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = min(delts) #smallest delta from above
  cats = c("subprime","prime")
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  #remove zeros
  sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  #plot subprime vs. prime
  plot_group = c("subprime","prime")
  plot_df = sum_dat_def[sum_dat_def$cat %in% plot_group,]
  plot_df = plot_df[plot_df$Age <= 55,] #limited data beyond age 55
  plot_df$cat = factor(plot_df$cat, 
                       levels=c('subprime','prime'))
  
  ggplot(data=plot_df,
         aes(x=Age,y=lam_hat,linetype=cat)) +
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=cat) +
    geom_ribbon(alpha=0.2) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Defaults)") +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    annotate('segment', x = 33, xend = 38, y = 0.03, yend = 0.03,
             color = "black", linewidth=0.65,
             linetype = 1, arrow = arrow(length = unit(1.75, "mm"))) +
    annotate("label", x = 26, y = 0.03, label = "Approx. Mar 2020 + 3 mo.", family="Times New Roman")
  #save, if desired
  #ggsave("convergence_subprime_prime_demo_COVID.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# TABLE 1: Credit Risk Convergence: Transition Matrix
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = min(delts) #smallest delta from above
  cats = as.character(sort(unique(obs_data$risk_cat)))
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  
  #remove zeros
  #sum_dat_def = sum_dat_def[sum_dat_def$Age >=min_age,]
  sum_dat_def_zero = sum_dat_def #if no convergence via CIs
  sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  #save the full default table
  path = "~/loan_risk_convergence/replication_code"
  write.csv(sum_dat_def,paste(path,"default72_2017.csv",sep="/"))
  
  #remove ages not shared by all categories
  #compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  #sum_dat_def = sum_dat_def[
  #  (sum_dat_def$Age %in% c(as.numeric(names(which(table(sum_dat_def$Age)==length(compare_group)))))),]
  
  #get CI overlap points
  consec_mth = 2
  
  #rule: 3 consecutive overlaps starting after min age
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  risk_conv_mat = matrix(nrow=5,ncol=5)
  rownames(risk_conv_mat) = compare_group
  colnames(risk_conv_mat) = compare_group
  
  
  for (j in c(1:5)){
    low_cat = compare_group[j]
    for (k in c(j:5)) {
      high_cat = compare_group[k]
      
      #get subset of sum_dat_def
      cur_comp = c(low_cat, high_cat)
      cur_def = sum_dat_def[sum_dat_def$cat %in% cur_comp,]
      cat_num = length(unique(cur_comp))
      
      cur_def = cur_def[
        (cur_def$Age %in% c(as.numeric(names(which(table(cur_def$Age)==cat_num))))),]
      
      ages = sort(unique(cur_def$Age))
      
      ci_low = cur_def$CI_lower[(cur_def$cat == low_cat)]
      ci_high = cur_def$CI_upper[(cur_def$cat == high_cat)]
      
      conv_check = ci_high >= ci_low
      index = NA
      if( length( which(conv_check == TRUE) ) == 0){
        index = NA
      }
      
      if( consec_mth == 1){
        index = min(which(conv_check == TRUE))
      }
      if( consec_mth > 1){
        m = 0
        for(r in c(1:length(conv_check))){
          if( conv_check[r] == TRUE){
            m = m + 1
            if(m >= consec_mth){
              index = r - (consec_mth - 1)
              break
            }
          }
          if( conv_check[r] == FALSE){
            m = 0
          }
        }
      }
      
      if( is.na(index) ){
        low = sum_dat_def_zero[sum_dat_def_zero$cat == low_cat,]
        low_zero = low$Age[min(which(low$lam_hat == 0))]
        
        high = sum_dat_def_zero[sum_dat_def_zero$cat == high_cat,]
        high_zero = high$Age[min(which(high$lam_hat == 0))]
        
        if( low_zero >= high_zero){
          risk_conv_mat[low_cat,high_cat] = low_zero
        }
      }
      
      if( is.na(index) == FALSE){
        risk_conv_mat[low_cat,high_cat] = ages[index]
      }
      # overlap = which(ci_high >= ci_low)
      # result = rle(diff(which(ci_high >= ci_low)))
      # grp3 = which((result$lengths>=(consec_mth-1) & result$values==1))
      # index = ifelse(length(grp3)==0,
      #                length(ages),
      #                overlap[[min(grp3)]])
      # 
      # risk_conv_mat[low_cat,high_cat] = ifelse(index == length(ages),
      #                                          NA,
      #                                          ages[index])
    }
  }
  
  risk_conv_mat #deep_subprime, prime = 49 rounded up to 50
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE 4: Estimated Expected Rolling Risk-Adjusted Return by Age, Issuance
######################################################################################
######################################################################################

#recovery estimates (Online supplement: Sec C.4)
{
  #2017 data
  path = "~/loan_risk_convergence/replication_code/"
  
  sdartR = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmaxR = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drvR = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aartR = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #SDART
  recovery_val = c()
  for (j in c(1:nrow(sdartR))){
    
    if(sdartR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:51)){
        #repo_vec <- append(repo_vec,as.numeric(sdartR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(sdartR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(sdartR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_s = data.frame("default" = sdartR$D,
                    "time" = sdartR$Xc,
                    "recovery_amt" = recovery_val / sdartR$originalLoanAmount)
  df_s = subset(df_s,df_s$default == 1)
  
  #CMAX
  recovery_val = c()
  for (j in c(1:nrow(cmaxR))){
    
    if(cmaxR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:49)){
        #repo_vec <- append(repo_vec,as.numeric(cmaxR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(cmaxR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(cmaxR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_c = data.frame("default" = cmaxR$D,
                    "time" = cmaxR$Xc,
                    "recovery_amt" = recovery_val / cmaxR$originalLoanAmount)
  df_c = subset(df_c,df_c$default == 1)
  
  #DRIVE
  recovery_val = c()
  for (j in c(1:nrow(drvR))){
    
    if(drvR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:51)){
        #repo_vec <- append(repo_vec,as.numeric(drvR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(drvR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(drvR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_d = data.frame("default" = drvR$D,
                    "time" = drvR$Xc,
                    "recovery_amt" = recovery_val / drvR$originalLoanAmount)
  df_d = subset(df_d,df_d$default == 1)
  
  #AART
  recovery_val = c()
  for (j in c(1:nrow(aartR))){
    
    if(aartR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:43)){
        #repo_vec <- append(repo_vec,as.numeric(aartR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(aartR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(aartR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_a = data.frame("default" = aartR$D,
                    "time" = aartR$Xc,
                    "recovery_amt" = recovery_val / aartR$originalLoanAmount)
  df_a = subset(df_a,df_a$default == 1)
  
  #COMBINE ALL RECOVERIES INTO ONE DATA FRAME
  df = rbind(df_a,df_s,df_c,df_d)
  
  def_time = sort(unique(df$time))
  
  Z_t = vector()
  for (i in def_time) {
    Z_t = append(Z_t,AVERAGEIF(df$time,i,df$recovery_amt))
  }
  
  #smoothed depreciation curve
  x <- def_time
  y <- Z_t
  lo <- loess(y~x)
  dep <- data.frame("Month" = x, "Obs.Avg" = Z_t, "Smoothed" = predict(lo))
  
  #write the loess estimates
  write.csv(dep,"~/loan_risk_convergence/replication_code/recovery_est2017.csv")
  
  #from Excel Solver, fit a gamma kernel
  a	= 0.0396784842573534
  b	= 2.31865986209146
  c	= 11.9499144731075
  
  fit = (a) * ((dep$Month)^(b-1)) * (exp(-dep$Month / c))
  
  dep$Fitted = fit
  
  plot_df = data.frame("Month" = rep(dep$Month,2),
                       "Obs.Avg" = rep(dep$Obs.Avg,2),
                       "Value" = c(dep$Smoothed,dep$Fitted),
                       "Type" = c(rep("Loess",length(dep$Month)),rep("Gamma-Kern",length(dep$Month))))
  
  write.csv(plot_df, "~/loan_risk_convergence/replication_code/recov_fitted2017.csv")
  plot_df = read.csv("~/loan_risk_convergence/replication_code/recov_fitted2017.csv")[,-1]
  
  ggplot(data=plot_df, aes(x=Month)) +
    geom_point(aes(y=Obs.Avg)) + 
    geom_line(aes(y=Value,linetype=Type)) +
    #geom_line(aes(y=Fitted, color="blue")) +
    xlab("Loan Age (Months)") + 
    ylab("Estimated Recovery: Percentage of Initial Loan Balance") +
    guides(linetype=guide_legend("")) +
    scale_linetype(labels = c("gamma-kernel", "nonparametric loess")) +
    theme_bw() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.position = "bottom")
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank())
  #save, if desired
  #ggsave("recovery_est2017.pdf",height=4,width=6, device = cairo_pdf)
  
  #for rolling exp return calc
  fit = (a) * ((c(1:73))^(b-1)) * (exp(-c(1:73) / c))
  df = data.frame("Month" = c(1:73), "Recovery" = fit)
  write.csv(df,"~/loan_risk_convergence/replication_code/recovery_est2017.csv")
  
  dep[which.max(dep$Obs.Avg),]
  rm(list=ls())
}
#figure plot
{
  #monthly rolling interest rate calculations
  path = "~/loan_risk_convergence/replication_code/"
  lam_def = read.csv(paste(path,"default72_2017.csv",sep="/"))
  recov = read.csv(paste(path,"recovery_est2017.csv",sep="/"))
  
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  col_names = c("assetTypeNumber",
                "risk_cat_ir",
                "originalLoanAmount",
                "originalInterestRatePercentage",
                "originalLoanTerm")
  
  drv_loop = drv[,col_names]
  sdart_loop = sdart[,col_names]
  cmax_loop = cmax[,col_names]
  aart_loop = cmax[,col_names]
  
  full_dat = rbind(drv_loop, sdart_loop, cmax_loop, aart_loop)
  
  aggregate(originalInterestRatePercentage ~ risk_cat_ir, full_dat, mean)
  
  deep_ir = mean( full_dat$originalInterestRatePercentage[full_dat$risk_cat_ir == "deep_subprime"])
  sub_ir = mean( full_dat$originalInterestRatePercentage[full_dat$risk_cat_ir == "subprime"])
  near_ir = mean( full_dat$originalInterestRatePercentage[full_dat$risk_cat_ir == "near_prime"])
  prime_ir = mean( full_dat$originalInterestRatePercentage[full_dat$risk_cat_ir == "prime"])
  super_ir = mean( full_dat$originalInterestRatePercentage[full_dat$risk_cat_ir == "super_prime"])
  
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  ir_mat = matrix(c(deep_ir,sub_ir,near_ir,prime_ir,super_ir),nrow=5,ncol=1)
  rownames(ir_mat) = compare_group
  
  #amortization schedule
  amort_bal <- function(time,orig_bal,int_rate,payment){
    calc1 = orig_bal * ((1 + int_rate)^time)
    calc2 = payment * ( ((1+int_rate)^time - 1) / int_rate )
    return(max(0,calc1 - calc2))
  }
  
  #recoveries
  rec_amt = function(time,loan_value){
    return(loan_value * recov$Recovery[time])
  }
  
  #remove ages not shared by all categories
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  lam_def_ages = lam_def[
    (lam_def$Age %in% c(as.numeric(names(which(table(lam_def$Age)==length(compare_group)))))),]
  
  loan_amt = 100
  loan_term = 72
  calc_months = sort(unique(lam_def_ages$Age))
  m_return = matrix(NA,nrow=5,ncol=(length(calc_months)))
  colnames(m_return) = calc_months
  rownames(m_return) = compare_group
  
  for (cat in compare_group) {
    #overall loan information for risk category
    loan_ir = as.numeric((1 + ir_mat[cat,]))^(1/12) - 1
    annuity_factor = (1 - (1 + loan_ir)^(-loan_term)) / loan_ir
    loan_pmt = round(loan_amt / annuity_factor,2)
    loan_amort = 
      sapply(c(1:loan_term),amort_bal,orig_bal=loan_amt,int_rate=loan_ir,payment=loan_pmt)
    recov_amt = 
      sapply(c(1:loan_term),rec_amt,loan_value = loan_amt)
    #probabilities for risk category
    lam_probs = lam_def$lam_hat[lam_def$cat == cat]
    
    for (t in calc_months){
      p = lam_def[(lam_def$cat == cat) & (lam_def$Age == t),]$lam_hat
      E_pmt = (1 - p) * (loan_pmt + loan_amort[t+1]) +
        p * recov_amt[t+1]
      solve_r = E_pmt / loan_amort[t] - 1
      m_return[cat,as.character(t)] = (1+solve_r)^12 - 1
    }
  }
  
  df17 = data.frame("time" = rep(calc_months,5),
                    "value" = c(m_return[1,],m_return[2,],m_return[3,],m_return[4,],m_return[5,]),
                    "risk_band" = factor(c(rep("deep_subprime",length(calc_months)),
                                           rep("subprime",length(calc_months)),
                                           rep("near_prime",length(calc_months)),
                                           rep("prime",length(calc_months)),
                                           rep("super_prime",length(calc_months))), 
                                         levels=c('deep_subprime','subprime','near_prime','prime','super_prime')))
  df17$year = "2017"
  df = df17
  
  ggplot(data=df,
         aes(x=time,y=value,linetype=risk_band,color=risk_band))+
    geom_point(size=1) +
    geom_line(linewidth=0.5) +
    xlab("Loan Age (Months)") + ylab("One-Month Risk-Adjusted Return (Annual % Yield)") +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           color=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          strip.text.y = element_text(size = 9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    geom_hline(yintercept=0, color="grey") #+
  #facet_grid(rows=vars(year))
  #save, if desired
  #ggsave("rolling_exp_ret2.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE 5: Outstanding Loan-to-value by Loan Age, Risk Band
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #estimate original payments
  pmt_calc = function(org_loan_amt, org_ir, org_term){
    
    m_rate = (1 + org_ir)^(1/12) - 1
    ann_fact = (1 - (1 + m_rate)^(-org_term))/m_rate
    return( org_loan_amt / ann_fact )
    
  }
  
  sdart$pmt = pmt_calc(sdart$originalLoanAmount, sdart$originalInterestRatePercentage, sdart$originalLoanTerm)
  cmax$pmt = pmt_calc(cmax$originalLoanAmount, cmax$originalInterestRatePercentage, cmax$originalLoanTerm)
  drv$pmt = pmt_calc(drv$originalLoanAmount, drv$originalInterestRatePercentage, drv$originalLoanTerm)
  aart$pmt = pmt_calc(aart$originalLoanAmount, aart$originalInterestRatePercentage, aart$originalLoanTerm)
  
  bond_list = list(sdart, cmax, drv, aart)
  
  #function to populate balance by age & risk category
  get_loan_info = function(month, risk_category){
    
    t_id = c()
    t_ir = c()
    t_ola = c()
    t_bal = c()
    t_opmt = c()
    t_vv = c()
    
    for(j in c(1:4)){
      
      bond = subset(bond_list[[j]], 
                    (risk_cat_ir == risk_category) & (Y < month) & (Xc >= month))
      if( nrow(bond) == 0) {next}
      
      m = month - bond$Y
      bal_num = ifelse(m == 1,
                       "reportingPeriodBeginningLoanBalanceAmount",
                       paste("BAL",(m-1),sep=""))
      cur_bal=c()
      for(k in c(1:nrow(bond))){
        cur_bal=append(cur_bal,bond[k,bal_num[k]])
      }
      
      t_id = append(t_id, bond$assetNumber)
      t_ir = append(t_ir, bond$originalInterestRatePercentage)
      t_opmt = append(t_opmt,bond$pmt)
      t_ola = append(t_ola, bond$originalLoanAmount)
      t_vv = append(t_vv, bond$vehicleValueAmount)
      t_bal = append(t_bal,cur_bal)
    }
    
    return(data.frame("month"=rep(month,length(t_id)),
                      "cat"=rep(risk_category,length(t_id)),
                      "id" = t_id,
                      "ir" = t_ir,
                      "orig_pmt"=t_opmt,
                      "orig_la"=t_ola,
                      "bal" = t_bal,
                      "veh_val" = t_vv))
    
  }
  
  categories = c("deep_subprime", "subprime", "near_prime", "prime", "super_prime")
  months = c(12, 15, 18, 24, 30, 36, 42, 48, 50, 54, 60)
  
  
  columns= c("month","cat","id","ir","orig_pmt","orig_la","bal", "veh_val")
  master = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(master) = columns
  
  for( c in categories ){
    
    for (m in months){
      master = rbind(master,
                     get_loan_info(m,c))
    }
    
  }
  
  names(master)[8] = "orig_veh_val"
  
  dep_rate = (31/12) / 100 #storchman (2004)
  master$veh_val = master$orig_veh_val * exp(- dep_rate * master$month)
  master$ltv = master$bal / master$veh_val
  
  write.csv(master, "~/loan_risk_convergence/replication_code/bal_by_age2.csv")
  
  #create the side by side box plots
  master$month = as.factor(master$month)
  master$cat = factor(master$cat, levels=c("deep_subprime", "subprime", "near_prime","prime","super_prime"))
  master$ltv = master$ltv * 100
  
  ggplot(data = master, aes(x=month, y=ltv)) +
    geom_boxplot(aes(fill=cat)) +
    xlab("Loan Age (Months)") + ylab("Loan-to-Value (%)") +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           color=guide_legend(""),
           fill=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          strip.text.y = element_text(size = 9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman"))# +
  #save, if desired
  #ggsave("ltv_by_age.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Table 2: Estimated Svaings by Risk Band, Loan Age
######################################################################################
######################################################################################

#balances for table 3; see `refi_svgs2.xlsx` for table calcs
{
  
  master = read.csv("~/loan_risk_convergence/replication_code/bal_by_age2.csv")
  
  #counts by month
  table(subset(master,cat=="deep_subprime")$month)
  table(subset(master,cat=="subprime")$month)
  table(subset(master,cat=="near_prime")$month)
  table(subset(master,cat=="prime")$month)
  table(subset(master,cat=="super_prime")$month)
  
  #averages
  bal = aggregate(bal ~ month + cat, master, mean)$bal
  opmt = aggregate(orig_pmt ~ month + cat, master, mean)$orig_pmt
  ola = aggregate(orig_la ~ month + cat, master, mean)$orig_la
  oir = aggregate(ir ~ month + cat, master, mean)$ir
  cts = aggregate(ir ~ month + cat, master, length)
  names(cts)[3] = "cts"
  
  df_add = data.frame("bal" = bal, "orig_pmt" = opmt,
                      "orig_la" = ola, "orig_ir" = oir)
  
  df = cbind(cts, df_add)
  
  aggregate(ir ~ month + cat, master, mean)
  
  aggregate((bal) ~ month + cat, master, mean)
  #aggregate(bal ~ month + cat, master, median)
  
  aggregate(orig_pmt ~ month + cat, master, mean)
  #aggregate(orig_pmt ~ month + cat, master, median)
  
  aggregate(orig_la ~ month + cat, master, mean)
  #aggregate(orig_la ~ month + cat, master, median)
  
  aggregate(ir ~ month + cat, master, mean)
  #aggregate(ir ~ month + cat, master, median)
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE 6: Consumer Prepayment Behavior, used Autos, Economic Stimulus
######################################################################################
######################################################################################

#get prepay csh's
{
  #2017
  
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #functions used repeatedly for each risk category
  f_star_rep <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$R == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_rep <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$R == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_rep <- function(x){
    num = f_star_rep(x) * (est_C(x) - f_star_rep(x))
    den = (est_C(x))^3
    return(num/den)
  }
  
  delta = 1 #smallest delta for SDART
  
  #calculate the hazard rate and confidence intervals for each risk category
  cats = as.character(sort(unique(obs_data$risk_cat)))
  
  sum_dat_rep = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_rep = sapply(c((delta+1):(max(z))),est_haz_rep)
    var_hat_rep = sapply(c((delta+1):(max(z))),Var_est_rep)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_rep,
      "Est_Var" = var_hat_rep
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_rep = rbind(sum_dat_rep,est_dist)
    
  }
  
  
  #remove zeros
  #sum_dat_def = sum_dat_def[sum_dat_def$Age >=min_age,]
  #sum_dat_rep_zero = sum_dat_rep #if no convergence via CIs
  sum_dat_rep = sum_dat_rep[sum_dat_rep$lam_hat > 0,]
  
  path = "~/loan_risk_convergence/replication_code"
  write.csv(sum_dat_rep,paste(path,"repayment72_2017.csv",sep="/"))
  rm(list=ls())
  
}
#figure plot
{
  path = "~/loan_risk_convergence/replication_code"
  sum_dat_rep = read.csv(paste(path,"repayment72_2017.csv",sep="/"))
  
  #plot results
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  sum_dat_rep = sum_dat_rep[sum_dat_rep$cat %in% compare_group,]
  
  #remove ages not shared by all categories
  sum_dat_rep = sum_dat_rep[
    (sum_dat_rep$Age %in% c(as.numeric(names(which(table(sum_dat_rep$Age)==length(compare_group)))))),]
  sum_dat_rep$cat = factor(sum_dat_rep$cat, 
                           levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  
  sort(unique(sum_dat_rep$Age)) #import ages 10-60
  
  mhm_dat = read.csv(paste(path,"mhm_sa17.csv",sep="/"))
  
  mhm_2017 = data.frame("age" = sort(unique(sum_dat_rep$Age)),
                        "mhm" = as.numeric(mhm_dat$mhm_2017)/1000)
  
  m17 <-
    ggplot(data=mhm_2017,
           aes(x=age,y=mhm)) +
    geom_line(linewidth=1.05) +
    xlab("Loan Age (Months)") + ylab("Manheim Used Car Index ($000)") +
    scale_x_continuous(breaks = 10 * c(1:6), labels = c("10", "20", "30", "40", "50", "60"),
                       lim=c(10,55)) + #xlim(0,60) +
    ylim(10,25) +
    theme_bw() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=7,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman"))
  
  r17 <-
    ggplot(data=sum_dat_rep,
           aes(x=Age,y=lam_hat,linetype=cat)) +
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=cat) +
    ylim(0,0.08) +
    scale_x_continuous(breaks = 10 * c(1:6), labels = c("10", "20", "30", "40", "50", "60"),
                       lim=c(10,55)) + #xlim(0,60) +
    geom_ribbon(alpha=0.2) +
    xlab("Loan Age (Months)") + ylab("Estimated CSH Rate (Repayment)") +
    theme_bw() +
    theme(legend.position="none") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    #scale_fill_grey() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=7,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    annotate('segment', x = 37, xend = 37, y = 0, yend = 0.065,
             size = 1,
             alpha = 0.4,
             color = "black",
             linetype = 1) +
    annotate("text", x = 37, y = 0.07, label = "E1", family="Times New Roman", size=3) +
    annotate('segment', x = 44, xend = 44, y = 0, yend = 0.065,
             size = 1,
             alpha = 0.4,
             color = "black",
             linetype = 1) +
    annotate("text", x = 44, y = 0.07, label = "E2", family="Times New Roman", size=3) +
    annotate('segment', x = 48, xend = 48, y = 0, yend = 0.065,
             size = 1,
             alpha = 0.4,
             color = "black",
             linetype = 1) +
    annotate("text", x = 48, y = 0.07, label = "E3", family="Times New Roman", size=3) +
    annotate('segment', x = 52, xend = 52, y = 0, yend = 0.065,
             size = 1,
             alpha = 0.4,
             color = "black",
             linetype = 1) +
    annotate("text", x = 52, y = 0.07, label = "C", family="Times New Roman", size=3)
  #xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Repayment)") +
  #theme(legend.position="bottom")
  
  set_null_device(cairo_pdf)
  plot_grid(m17, r17, nrow=2)
  #ggsave("repay_2017.pdf",height=4,width=6,device=cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE B1: Credit Risk Convergence: All Risk Bands (2017)
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  lam_def = read.csv(paste(path,"default72_2017.csv",sep="/"))
  
  #plot results
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  lam_def = lam_def[lam_def$cat %in% compare_group,]
  
  #remove ages not shared by all categories
  lam_def = lam_def[
    (lam_def$Age %in% c(as.numeric(names(which(table(lam_def$Age)==length(compare_group)))))),]
  lam_def$cat = factor(lam_def$cat, 
                       levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  
  comparisons = list(c("deep_subprime","deep_subprime"),
                     c("deep_subprime","subprime"),
                     c("deep_subprime","near_prime"),
                     c("deep_subprime","prime"),
                     c("deep_subprime","super_prime"),
                     c("subprime","deep_subprime"),
                     c("subprime","subprime"),
                     c("subprime","near_prime"),
                     c("subprime","prime"),
                     c("subprime","super_prime"),
                     c("near_prime","deep_subprime"),
                     c("near_prime","subprime"),
                     c("near_prime","near_prime"),
                     c("near_prime","prime"),
                     c("near_prime","super_prime"),
                     c("prime","deep_subprime"),
                     c("prime","subprime"),
                     c("prime","near_prime"),
                     c("prime","prime"),
                     c("prime","super_prime"),
                     c("super_prime","deep_subprime"),
                     c("super_prime","subprime"),
                     c("super_prime","near_prime"),
                     c("super_prime","prime"),
                     c("super_prime","super_prime"))
  
  calc_months = sort(unique(lam_def$Age))
  
  for(z in c(1:25)){
    
    ###################################################################
    low_cat = comparisons[[z]][1]
    high_cat = comparisons[[z]][2]
    ###################################################################
    
    full_dat = subset(lam_def,(cat == low_cat)|(cat == high_cat))
    
    #risk_cat_key = full_dat$risk_cat
    
    low_cat_mean = full_dat$lam_hat[full_dat$cat == low_cat]
    low_cat_95CI_upper = full_dat$CI_upper[full_dat$cat == low_cat]
    low_cat_95CI_lower = full_dat$CI_lower[full_dat$cat == low_cat]
    
    high_cat_mean = full_dat$lam_hat[full_dat$cat == high_cat]
    high_cat_95CI_upper = full_dat$CI_upper[full_dat$cat == high_cat]
    high_cat_95CI_lower = full_dat$CI_lower[full_dat$cat == high_cat]
    
    df_low = data.frame("month" = calc_months, 
                        "lambda" = low_cat_mean,
                        "CI_upper" = low_cat_95CI_upper,
                        "CI_lower" = low_cat_95CI_lower,
                        "cat" = rep(low_cat,length(calc_months)))
    
    df_high = data.frame("month" = calc_months, 
                         "lambda" = high_cat_mean,
                         "CI_upper" = high_cat_95CI_upper,
                         "CI_lower" = high_cat_95CI_lower,
                         "cat" = rep(high_cat,length(calc_months)))
    
    df = rbind(df_low, df_high)
    df$low_cat = low_cat
    df$high_cat = high_cat
    
    nam <- paste("dat",z,sep="")
    assign(nam,df)
  }
  
  test_dat = dat1
  for (i in c(2:25)){
    test_dat = rbind(test_dat,get(paste("dat",i,sep="")))
  }
  
  test_dat$high_low = ifelse(test_dat$cat == test_dat$low_cat, "row", "column")
  test_dat$low_cat_f = factor(test_dat$low_cat, levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  test_dat$high_cat_f = factor(test_dat$high_cat, levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  
  ggplot(data=test_dat,
         aes(x=month,y=lambda,linetype=high_low))+
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=high_low)+
    geom_ribbon(alpha=0.5) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Default)") +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    #scale_fill_grey() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    facet_grid(rows = vars(low_cat_f), cols = vars(high_cat_f))
  #save, if desired
  #ggsave("haz_grid_default2017.pdf",height=5,width=5,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE C1: Borrower Credit Profile and APR by Bond, Risk Band
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  df_ir = data.frame(value = c(sdart$originalInterestRatePercentage,
                               cmax$originalInterestRatePercentage,
                               drv$originalInterestRatePercentage,
                               aart$originalInterestRatePercentage),
                     bond = c(rep("SDART",nrow(sdart)),
                              rep("CARMX",nrow(cmax)),
                              rep("DRIVE",nrow(drv)),
                              rep("AART",nrow(aart))),
                     risk_band = c(sdart$risk_cat_ir, cmax$risk_cat_ir, drv$risk_cat_ir, aart$risk_cat_ir))
  
  aggregate(value ~ risk_band, df_ir, mean)
  
  df_ir$bond = factor(df_ir$bond, levels = c("DRIVE", "SDART", "CARMX", "AART"))
  
  df_cs = data.frame(value = c(sdart$obligorCreditScore,cmax$obligorCreditScore,
                               drv$obligorCreditScore, aart$obligorCreditScore),
                     bond = c(rep("SDART",nrow(sdart)),
                              rep("CARMX",nrow(cmax)),
                              rep("DRIVE",nrow(drv)),
                              rep("AART",nrow(aart))),
                     risk_band = c(sdart$risk_cat_ir, cmax$risk_cat_ir, drv$risk_cat_ir, aart$risk_cat_ir))
  
  df_cs$bond = factor(df_ir$bond, levels = c("DRIVE", "SDART", "CARMX", "AART"))
  
  df_cs = df_cs[!is.na(df_cs$value),]
  df_cs = df_cs[-which(df_cs$value == "NONE"),]
  df_cs$value = as.numeric(df_cs$value)
  
  g_cs <- ggplot(data=df_cs, aes(x=value, fill=type)) +
    geom_density(alpha=0.5, fill="lightblue") +
    theme_bw() +
    theme(legend.position="bottom") +
    xlab("Obligor Credit Score") + 
    ylab("Density") +
    #scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8, family="Times New Roman"),
          strip.text.x = element_text(size = 8, family="Times New Roman", margin = margin(b=0,t=0)),
          legend.text=element_text(size=8, family="Times New Roman"),
          axis.text.x = element_text(size=7, family="Times New Roman"),
          #axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    facet_grid(cols = vars(bond))
  
  g_ir <- ggplot(data=df_ir, aes(x=value, fill=type)) +
    geom_density(alpha=0.5, fill="lightblue") +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Obligor Interest Rate Percentage", labels = scales::percent_format(accuracy = 1)) +
    ylab("Density") +
    #scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8, family="Times New Roman"),
          strip.text.x = element_text(size = 8, family="Times New Roman", margin = margin(b=0,t=0)),
          legend.text=element_text(size=8, family="Times New Roman"),
          axis.text.x = element_text(size=7, family="Times New Roman"),
          #axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    facet_grid(cols = vars(bond))
  
  df_cs$risk_band = factor(df_cs$risk_band, levels = c("deep_subprime", "subprime", "near_prime", "prime","super_prime"))
  
  g_cs2 <- ggplot(data=df_cs, aes(x=value, fill=type)) +
    geom_density(alpha=0.5, fill="lightblue") +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Obligor Credit Score")+#, labels = scales::percent_format(accuracy = 1)) +
    ylab("Density") +
    #scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8, family="Times New Roman"),
          strip.text.x = element_text(size = 8, family="Times New Roman", margin = margin(b=0,t=0)),
          legend.text=element_text(size=8, family="Times New Roman"),
          axis.text.x = element_text(size=7, family="Times New Roman"),
          #axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    facet_grid(cols = vars(risk_band))
  
  df_ir$risk_band = factor(df_ir$risk_band, levels = c("deep_subprime", "subprime", "near_prime", "prime","super_prime"))
  
  g_ir2 <- ggplot(data=df_ir, aes(x=value, fill=type)) +
    geom_density(alpha=0.5, fill="lightblue") +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Obligor Interest Rate Percentage", labels = scales::percent_format(accuracy = 1)) +
    ylab("Density") +
    #scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8, family="Times New Roman"),
          strip.text.x = element_text(size = 8, family="Times New Roman", margin = margin(b=0,t=0)),
          legend.text=element_text(size=6, family="Times New Roman"),
          axis.text.x = element_text(size=7, family="Times New Roman"),
          #axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    facet_grid(cols = vars(risk_band))
  
  #theme(strip.text.x = element_text( margin = margin( b = 0, t = 0) ) )
  
  plot_grid(g_cs, g_ir, g_cs2, g_ir2, nrow=4)
  #save plot, if desired
  #ggsave("2017_summary2.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Table C1: Borrower Counts by Risk Band, Bond, and Loan Outcome
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  sdart = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  state = c(sdart$obligorGeographicLocation,
            drv$obligorGeographicLocation,
            cmax$obligorGeographicLocation,
            aart$obligorGeographicLocation)
  
  round(sort(table(state)) / sum(nrow(sdart),nrow(drv),nrow(cmax),nrow(aart)),2)
  tail(round(sort(table(state)) / sum(nrow(sdart),nrow(drv),nrow(cmax),nrow(aart)),2))
  
  vehicle = c(sdart$vehicleManufacturerName,
              drv$vehicleManufacturerName,
              cmax$vehicleManufacturerName,
              aart$vehicleManufacturerName)
  
  round(sort(table(tolower(vehicle))) / sum(nrow(sdart),nrow(drv),nrow(cmax),nrow(aart)),2)
  tail(round(sort(table(tolower(vehicle))) / sum(nrow(sdart),nrow(drv),nrow(cmax),nrow(aart)),2))
  
  df = data.frame("default" = c(drv$D, sdart$D, cmax$D, aart$D),
                  "repaid" = c(drv$R, sdart$R, cmax$R, aart$R),
                  "censored" = c(drv$C, sdart$C, cmax$C, aart$C),
                  "risk_cat_ir" = c(drv$risk_cat_ir, sdart$risk_cat_ir, cmax$risk_cat_ir, aart$risk_cat_ir))
  
  table(df$risk_cat_ir); table(df$risk_cat_ir) / nrow(df)
  
  table(drv$risk_cat_ir); nrow(drv)
  table(drv$risk_cat_ir)/table(df$risk_cat_ir); nrow(drv) / nrow(df)
  table(sdart$risk_cat_ir); nrow(sdart)
  table(sdart$risk_cat_ir)/table(df$risk_cat_ir); nrow(sdart) / nrow(df)
  table(cmax$risk_cat_ir); nrow(cmax)
  table(cmax$risk_cat_ir)/table(df$risk_cat_ir[!(df$risk_cat_ir=="deep_subprime")]); nrow(cmax) / nrow(df)
  table(aart$risk_cat_ir); nrow(aart)
  table(aart$risk_cat_ir)/table(df$risk_cat_ir[!(df$risk_cat_ir=="deep_subprime")]); nrow(aart) / nrow(df)
  
  r_band = c(drv$risk_cat_ir, sdart$risk_cat_ir, cmax$risk_cat_ir, aart$risk_cat_ir)
  table(r_band); length(r_band)
  table(r_band)/ length(r_band)
  
  df = data.frame("default" = c(drv$D, sdart$D, cmax$D, aart$D),
                  "repaid" = c(drv$R, sdart$R, cmax$R, aart$R),
                  "censored" = c(drv$C, sdart$C, cmax$C, aart$C),
                  "risk_cat_ir" = c(drv$risk_cat_ir, sdart$risk_cat_ir, cmax$risk_cat_ir, aart$risk_cat_ir))
  
  table(df$risk_cat_ir[df$default == 1]); sum(df$default)
  table(df$risk_cat_ir[df$default == 1])/table(df$risk_cat_ir)
  table(df$risk_cat_ir[df$censored == 1]); sum(df$censored)
  table(df$risk_cat_ir[df$censored == 1])/table(df$risk_cat_ir)
  table(df$risk_cat_ir[df$repaid == 1]); sum(df$repaid)
  table(df$risk_cat_ir[df$repaid == 1])/table(df$risk_cat_ir)
  
  c(sum(df$default), sum(df$censored), sum(df$repaid)) / nrow(df)
  
  table( drv$risk_cat_ir[ drv$D == 1] ); sum(drv$D)
  table( drv$risk_cat_ir[ drv$D == 1] )/table(drv$risk_cat_ir); sum(drv$D)/nrow(drv)
  table( drv$risk_cat_ir[ drv$C == 1] ); sum(drv$C)
  table( drv$risk_cat_ir[ drv$C == 1] )/table(drv$risk_cat_ir); sum(drv$C)/nrow(drv)
  table( drv$risk_cat_ir[ drv$R == 1] ); sum(drv$R)
  table( drv$risk_cat_ir[ drv$R == 1] )/table(drv$risk_cat_ir); sum(drv$R)/nrow(drv)
  table( drv$risk_cat_ir )
  
  table( sdart$risk_cat_ir[ sdart$D == 1] ); sum(sdart$D)
  table( sdart$risk_cat_ir[ sdart$D == 1] )/table(sdart$risk_cat_ir); sum(sdart$D)/nrow(sdart)
  table( sdart$risk_cat_ir[ sdart$C == 1] ); sum(sdart$C)
  table( sdart$risk_cat_ir[ sdart$C == 1] )/table(sdart$risk_cat_ir); sum(sdart$C)/nrow(sdart)
  table( sdart$risk_cat_ir[ sdart$R == 1] ); sum(sdart$R)
  table( sdart$risk_cat_ir[ sdart$R == 1] )/table(sdart$risk_cat_ir); sum(sdart$R)/nrow(sdart)
  table( sdart$risk_cat_ir )
  
  table( cmax$risk_cat_ir[ cmax$D == 1] ); sum(cmax$D)
  table( cmax$risk_cat_ir[ cmax$D == 1] )/table(cmax$risk_cat_ir); sum(cmax$D)/nrow(cmax)
  table( cmax$risk_cat_ir[ cmax$C == 1] ); sum(cmax$C)
  table( cmax$risk_cat_ir[ cmax$C == 1] )/table(cmax$risk_cat_ir); sum(cmax$C)/nrow(cmax)
  table( cmax$risk_cat_ir[ cmax$R == 1] ); sum(cmax$R)
  table( cmax$risk_cat_ir[ cmax$R == 1] )/table(cmax$risk_cat_ir); sum(cmax$R)/nrow(cmax)
  table( cmax$risk_cat_ir )
  
  table( aart$risk_cat_ir[ aart$D == 1] ); sum(aart$D)
  table( aart$risk_cat_ir[ aart$D == 1] )/table(aart$risk_cat_ir[!(aart$risk_cat_ir=="subprime")]); sum(aart$D)/nrow(aart)
  table( aart$risk_cat_ir[ aart$C == 1] ); sum(aart$C)
  table( aart$risk_cat_ir[ aart$C == 1] )/table(aart$risk_cat_ir[!(aart$risk_cat_ir=="subprime")]); sum(aart$C)/nrow(aart)
  table( aart$risk_cat_ir[ aart$R == 1] ); sum(aart$R)
  table( aart$risk_cat_ir[ aart$R == 1] )/table(aart$risk_cat_ir); sum(aart$R)/nrow(aart)
  table( aart$risk_cat_ir)
  
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE C3: Estimation of the Recovery Upon Default Assumption
######################################################################################
######################################################################################

#recovery estimates
{
  #2017 data
  path = "~/loan_risk_convergence/replication_code/"
  
  sdartR = read.csv(paste(path,"sdart20192_2017.csv",sep=""))
  cmaxR = read.csv(paste(path,"cmax6835_2017.csv",sep=""))
  drvR = read.csv(paste(path,"drv28920_2017.csv",sep=""))
  aartR = read.csv(paste(path,"aart2171_2017.csv",sep=""))
  
  #SDART
  recovery_val = c()
  for (j in c(1:nrow(sdartR))){
    
    if(sdartR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:51)){
        #repo_vec <- append(repo_vec,as.numeric(sdartR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(sdartR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(sdartR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_s = data.frame("default" = sdartR$D,
                    "time" = sdartR$Xc,
                    "recovery_amt" = recovery_val / sdartR$originalLoanAmount)
  df_s = subset(df_s,df_s$default == 1)
  
  #CMAX
  recovery_val = c()
  for (j in c(1:nrow(cmaxR))){
    
    if(cmaxR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:49)){
        #repo_vec <- append(repo_vec,as.numeric(cmaxR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(cmaxR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(cmaxR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_c = data.frame("default" = cmaxR$D,
                    "time" = cmaxR$Xc,
                    "recovery_amt" = recovery_val / cmaxR$originalLoanAmount)
  df_c = subset(df_c,df_c$default == 1)
  
  #DRIVE
  recovery_val = c()
  for (j in c(1:nrow(drvR))){
    
    if(drvR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:51)){
        #repo_vec <- append(repo_vec,as.numeric(drvR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(drvR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(drvR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_d = data.frame("default" = drvR$D,
                    "time" = drvR$Xc,
                    "recovery_amt" = recovery_val / drvR$originalLoanAmount)
  df_d = subset(df_d,df_d$default == 1)
  
  #AART
  recovery_val = c()
  for (j in c(1:nrow(aartR))){
    
    if(aartR$D[j] == 1){
      #repo_vec = c()
      rec_vec = c()
      for (i in c(1:43)){
        #repo_vec <- append(repo_vec,as.numeric(aartR[j,paste("REP",i,sep="")]))
        rec_vec <- append(rec_vec,as.numeric(aartR[j,paste("REC",i,sep="")]))
      }
      #repo_vec = as.vector(na.omit(repo_vec))
      rec_vec = as.vector(na.omit(rec_vec))
      recovery_val = append(recovery_val,sum(rec_vec))
      #recovery_val = append(recovery_val,max(sum(repo_vec),sum(rec_vec)))
    }
    if(aartR$D[j] == 0){
      recovery_val = append(recovery_val,0)
    }
  }
  df_a = data.frame("default" = aartR$D,
                    "time" = aartR$Xc,
                    "recovery_amt" = recovery_val / aartR$originalLoanAmount)
  df_a = subset(df_a,df_a$default == 1)
  
  #COMBINE ALL RECOVERIES INTO ONE DATA FRAME
  df = rbind(df_a,df_s,df_c,df_d)
  
  def_time = sort(unique(df$time))
  
  Z_t = vector()
  for (i in def_time) {
    Z_t = append(Z_t,AVERAGEIF(df$time,i,df$recovery_amt))
  }
  
  #smoothed depreciation curve
  x <- def_time
  y <- Z_t
  lo <- loess(y~x)
  dep <- data.frame("Month" = x, "Obs.Avg" = Z_t, "Smoothed" = predict(lo))
  
  #write the loess estimates
  #write.csv(dep,"~/loan_risk_convergence/replication_code/recovery_est2017.csv")
  
  #from Excel Solver, fit a gamma kernel
  a	= 0.0396784842573534
  b	= 2.31865986209146
  c	= 11.9499144731075
  
  fit = (a) * ((dep$Month)^(b-1)) * (exp(-dep$Month / c))
  
  dep$Fitted = fit
  
  plot_df = data.frame("Month" = rep(dep$Month,2),
                       "Obs.Avg" = rep(dep$Obs.Avg,2),
                       "Value" = c(dep$Smoothed,dep$Fitted),
                       "Type" = c(rep("Loess",length(dep$Month)),rep("Gamma-Kern",length(dep$Month))))
  
  #write.csv(plot_df, "~/loan_risk_convergence/replication_code/recov_fitted2017.csv")
  #plot_df = read.csv("~/loan_risk_convergence/replication_code/recov_fitted2017.csv")[,-1]
  
  ggplot(data=plot_df, aes(x=Month)) +
    geom_point(aes(y=Obs.Avg)) + 
    geom_line(aes(y=Value,linetype=Type)) +
    #geom_line(aes(y=Fitted, color="blue")) +
    xlab("Loan Age (Months)") + 
    ylab("Estimated Recovery: Percentage of Initial Loan Balance") +
    guides(linetype=guide_legend("")) +
    scale_linetype(labels = c("gamma-kernel", "nonparametric loess")) +
    theme_bw() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.position = "bottom")
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank())
  #ggsave("recovery_est2017.pdf",height=4,width=6, device = cairo_pdf)
  
  #for rolling exp return calc
  #fit = (a) * ((c(1:73))^(b-1)) * (exp(-c(1:73) / c))
  #df = data.frame("Month" = c(1:73), "Recovery" = fit)
  #write.csv(df,"~/loan_risk_convergence/replication_code/recovery_est2017.csv")
  
  #dep[which.max(dep$Obs.Avg),]
  rm(list=ls())
}

######################################################################################
######################################################################################
# FIGURE D1: Credit Risk Convergence: COVID Sensitivity
######################################################################################
######################################################################################

#step 1: filter 2019 data

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#SDART 2019-3
{
  loan_term = 72
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(sdart,"~/loan_risk_convergence/replication_code/sdart19962_2019.csv")
  rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#DRIVE 2019-4
{ 
  loan_term = 72
  len_obs_window = 50 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  #392 'bad' loans of 31,613
  
  drv = drv[!(drv$assetNumber %in% bad_data),]
  write.csv(drv,"~/loan_risk_convergence/replication_code/drv31221_2019.csv")
  rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#CARMX 2019-4
{
  loan_term_c = 73
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(cmax,"~/loan_risk_convergence/replication_code/cmax11724_2019.csv")
  rm(list=ls())
}

#default time function
source("~/loan_risk_convergence/replication_code/default_time.R")

#AART 2019-3
{
  loan_term_c = 73
  len_obs_window = 46 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(aart,"~/loan_risk_convergence/replication_code/aart2895_2019.csv")
  rm(list=ls())
}

#2019 counts
{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart19962_2019.csv",sep=""))
  cmax = read.csv(paste(path,"cmax11724_2019.csv",sep=""))
  drv = read.csv(paste(path,"drv31221_2019.csv",sep=""))
  aart = read.csv(paste(path,"aart2895_2019.csv",sep=""))
  
  nrow(sdart); nrow(cmax); nrow(drv); nrow(aart)
  sum(nrow(sdart), nrow(cmax), nrow(drv), nrow(aart))
  
  risk_band = data.frame("risk_band" = 
                           c(sdart$risk_cat_ir, cmax$risk_cat_ir,
                             drv$risk_cat_ir, aart$risk_cat_ir))
  
  table(risk_band$risk_band)
  table(risk_band$risk_band)/nrow(risk_band)
  rm(list=ls())
}

#figure plot
{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart19962_2019.csv",sep=""))
  cmax = read.csv(paste(path,"cmax11724_2019.csv",sep=""))
  drv = read.csv(paste(path,"drv31221_2019.csv",sep=""))
  aart = read.csv(paste(path,"aart2895_2019.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = min(delts) #smallest delta from above
  cats = c("subprime","prime")
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  #remove zeros
  sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  #plot subprime vs. prime
  plot_group = c("subprime","prime")
  plot_df = sum_dat_def[sum_dat_def$cat %in% plot_group,]
  plot_df = plot_df[plot_df$Age <= 53,] #limited data beyond age 45
  plot_df$cat = factor(plot_df$cat, 
                       levels=c('subprime','prime'))
  
  ggplot(data=plot_df,
         aes(x=Age,y=lam_hat,linetype=cat)) +
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=cat) +
    geom_ribbon(alpha=0.2) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Defaults)") +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    annotate('segment', x = 20, xend = 12, y = 0.0275, yend = 0.0275,
             color = "black", linewidth=0.65,
             linetype = 1, arrow = arrow(length = unit(1.75, "mm"))) +
    annotate("label", x = 23, y = 0.0275, label = "Approx. Mar 2020 + 3 mo.", family="Times New Roman")
  #save plot, if desired
  #ggsave("convergence_subprime_prime_demo_COVID2019.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Table D1: Credit Risk Convergence: 2019 Transition Matrix
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart19962_2019.csv",sep=""))
  cmax = read.csv(paste(path,"cmax11724_2019.csv",sep=""))
  drv = read.csv(paste(path,"drv31221_2019.csv",sep=""))
  aart = read.csv(paste(path,"aart2895_2019.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = min(delts) #smallest delta from above
  cats = as.character(sort(unique(obs_data$risk_cat)))
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  
  #remove zeros
  #sum_dat_def = sum_dat_def[sum_dat_def$Age >=min_age,]
  sum_dat_def_zero = sum_dat_def #if no convergence via CIs
  sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  #save the full default table
  path = "~/loan_risk_convergence/replication_code"
  write.csv(sum_dat_def,paste(path,"default72_2019.csv",sep="/"))
  
  #remove ages not shared by all categories
  #compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  #sum_dat_def = sum_dat_def[
  #  (sum_dat_def$Age %in% c(as.numeric(names(which(table(sum_dat_def$Age)==length(compare_group)))))),]
  
  #get CI overlap points
  consec_mth = 2
  
  #rule: 3 consecutive overlaps starting after min age
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  risk_conv_mat = matrix(nrow=5,ncol=5)
  rownames(risk_conv_mat) = compare_group
  colnames(risk_conv_mat) = compare_group
  
  
  for (j in c(1:5)){
    low_cat = compare_group[j]
    for (k in c(j:5)) {
      high_cat = compare_group[k]
      
      #get subset of sum_dat_def
      cur_comp = c(low_cat, high_cat)
      cur_def = sum_dat_def[sum_dat_def$cat %in% cur_comp,]
      cat_num = length(unique(cur_comp))
      
      cur_def = cur_def[
        (cur_def$Age %in% c(as.numeric(names(which(table(cur_def$Age)==cat_num))))),]
      
      ages = sort(unique(cur_def$Age))
      
      ci_low = cur_def$CI_lower[(cur_def$cat == low_cat)]
      ci_high = cur_def$CI_upper[(cur_def$cat == high_cat)]
      
      conv_check = ci_high >= ci_low
      index = NA
      if( length( which(conv_check == TRUE) ) == 0){
        index = NA
      }
      
      if( consec_mth == 1){
        index = min(which(conv_check == TRUE))
      }
      if( consec_mth > 1){
        m = 0
        for(r in c(1:length(conv_check))){
          if( conv_check[r] == TRUE){
            m = m + 1
            if(m >= consec_mth){
              index = r - (consec_mth - 1)
              break
            }
          }
          if( conv_check[r] == FALSE){
            m = 0
          }
        }
      }
      
      if( is.na(index) ){
        low = sum_dat_def_zero[sum_dat_def_zero$cat == low_cat,]
        low_zero = low$Age[min(which(low$lam_hat == 0))]
        
        high = sum_dat_def_zero[sum_dat_def_zero$cat == high_cat,]
        high_zero = high$Age[min(which(high$lam_hat == 0))]
        
        if( low_zero >= high_zero){
          risk_conv_mat[low_cat,high_cat] = low_zero
        }
      }
      
      if( is.na(index) == FALSE){
        risk_conv_mat[low_cat,high_cat] = ages[index]
      }
      # overlap = which(ci_high >= ci_low)
      # result = rle(diff(which(ci_high >= ci_low)))
      # grp3 = which((result$lengths>=(consec_mth-1) & result$values==1))
      # index = ifelse(length(grp3)==0,
      #                length(ages),
      #                overlap[[min(grp3)]])
      # 
      # risk_conv_mat[low_cat,high_cat] = ifelse(index == length(ages),
      #                                          NA,
      #                                          ages[index])
    }
  }
  
  risk_conv_mat
  #ages
  rm(list=ls())
}

######################################################################################
######################################################################################
# Figure D2: Credit Risk Convergence: All Risk Bands (2019)
######################################################################################
######################################################################################

{
  path = "~/loan_risk_convergence/replication_code/"
  
  lam_def = read.csv(paste(path,"default72_2019.csv",sep="/"))
  
  #plot results
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  lam_def = lam_def[lam_def$cat %in% compare_group,]
  
  #remove ages not shared by all categories
  lam_def = lam_def[
    (lam_def$Age %in% c(as.numeric(names(which(table(lam_def$Age)==length(compare_group)))))),]
  lam_def$cat = factor(lam_def$cat, 
                       levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  
  comparisons = list(c("deep_subprime","deep_subprime"),
                     c("deep_subprime","subprime"),
                     c("deep_subprime","near_prime"),
                     c("deep_subprime","prime"),
                     c("deep_subprime","super_prime"),
                     c("subprime","deep_subprime"),
                     c("subprime","subprime"),
                     c("subprime","near_prime"),
                     c("subprime","prime"),
                     c("subprime","super_prime"),
                     c("near_prime","deep_subprime"),
                     c("near_prime","subprime"),
                     c("near_prime","near_prime"),
                     c("near_prime","prime"),
                     c("near_prime","super_prime"),
                     c("prime","deep_subprime"),
                     c("prime","subprime"),
                     c("prime","near_prime"),
                     c("prime","prime"),
                     c("prime","super_prime"),
                     c("super_prime","deep_subprime"),
                     c("super_prime","subprime"),
                     c("super_prime","near_prime"),
                     c("super_prime","prime"),
                     c("super_prime","super_prime"))
  
  calc_months = sort(unique(lam_def$Age))
  
  for(z in c(1:25)){
    
    ###################################################################
    low_cat = comparisons[[z]][1]
    high_cat = comparisons[[z]][2]
    ###################################################################
    
    full_dat = subset(lam_def,(cat == low_cat)|(cat == high_cat))
    
    #risk_cat_key = full_dat$risk_cat
    
    low_cat_mean = full_dat$lam_hat[full_dat$cat == low_cat]
    low_cat_95CI_upper = full_dat$CI_upper[full_dat$cat == low_cat]
    low_cat_95CI_lower = full_dat$CI_lower[full_dat$cat == low_cat]
    
    high_cat_mean = full_dat$lam_hat[full_dat$cat == high_cat]
    high_cat_95CI_upper = full_dat$CI_upper[full_dat$cat == high_cat]
    high_cat_95CI_lower = full_dat$CI_lower[full_dat$cat == high_cat]
    
    df_low = data.frame("month" = calc_months, 
                        "lambda" = low_cat_mean,
                        "CI_upper" = low_cat_95CI_upper,
                        "CI_lower" = low_cat_95CI_lower,
                        "cat" = rep(low_cat,length(calc_months)))
    
    df_high = data.frame("month" = calc_months, 
                         "lambda" = high_cat_mean,
                         "CI_upper" = high_cat_95CI_upper,
                         "CI_lower" = high_cat_95CI_lower,
                         "cat" = rep(high_cat,length(calc_months)))
    
    df = rbind(df_low, df_high)
    df$low_cat = low_cat
    df$high_cat = high_cat
    
    nam <- paste("dat",z,sep="")
    assign(nam,df)
  }
  
  test_dat = dat1
  for (i in c(2:25)){
    test_dat = rbind(test_dat,get(paste("dat",i,sep="")))
  }
  
  test_dat$high_low = ifelse(test_dat$cat == test_dat$low_cat, "row", "column")
  test_dat$low_cat_f = factor(test_dat$low_cat, levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  test_dat$high_cat_f = factor(test_dat$high_cat, levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  
  ggplot(data=test_dat,
         aes(x=month,y=lambda,linetype=high_low))+
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=high_low)+
    geom_ribbon(alpha=0.5) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Default)") +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    #scale_fill_grey() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) +
    facet_grid(rows = vars(low_cat_f), cols = vars(high_cat_f))
  #save, if desired
  #ggsave("haz_grid_default2019.pdf",height=5,width=5,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Figure D3: Credit Risk Convergence: Collateral Sensitivity
######################################################################################
######################################################################################

#prepare 2019 new car data
{
  #default time function
  source("~/loan_risk_convergence/replication_code/default_time.R")
  
  #SDART 2017-2
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(sdart,"~/loan_risk_convergence/replication_code/sdart7369new_2017.csv")
  rm(list=ls())
  
  #DRIVE 2017-1
  
  #default time function
  source("~/loan_risk_convergence/replication_code/default_time.R")
  
  loan_term = 72
  len_obs_window = 51 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(drv,"~/loan_risk_convergence/replication_code/drv7629new_2017.csv")
  rm(list=ls())
  
  #default time function
  source("~/loan_risk_convergence/replication_code/default_time.R")
  
  #CARMX 2017-2
  loan_term = 72
  len_obs_window = 49 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(cmax,"~/loan_risk_convergence/replication_code/cmax9new_2017.csv")
  rm(list=ls())
  
  #default time function
  source("~/loan_risk_convergence/replication_code/default_time.R")
  
  #AART 2017-3
  loan_term = 73
  len_obs_window = 43 #num. mnths in obs. window
  
  path = "~/loan_risk_convergence/replication_code/"
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
  write.csv(aart,"~/loan_risk_convergence/replication_code/aart1342new_2017.csv")
  rm(list=ls())  
}

#summary counts
{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart7369new_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax9new_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv7629new_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart1342new_2017.csv",sep=""))
  
  nrow(sdart); nrow(cmax); nrow(drv); nrow(aart)
  sum(nrow(sdart), nrow(cmax), nrow(drv), nrow(aart))
  
  risk_band = data.frame("risk_band" = 
                           c(sdart$risk_cat_ir, cmax$risk_cat_ir,
                             drv$risk_cat_ir, aart$risk_cat_ir))
  
  table(risk_band$risk_band)
  #table(risk_band$risk_band)/nrow(risk_band)
  rm(list=ls())
}

#figure plot
{
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart7369new_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax9new_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv7629new_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart1342new_2017.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = min(delts) #smallest delta from above
  cats = c("subprime","prime")
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    int_lam = est_dist$lam_hat
    int_var = est_dist$Est_Var
    #m = 0
    for (m in c(1:length(int_lam))){
      #m = m + 1
      #condition to see if remaining lam_hat all zeros
      if( sum(int_lam[m:length(int_lam)]) == 0 ){
        int_lam[m:length(int_lam)] = 0
        int_var[m:length(int_lam)] = 0
        break
      }
      
      #if (m < min_age){
      #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
      #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
      #  next
      #}
      
      #condition to see if lam_hat is zero
      if( int_lam[m] == 0){
        #get number of consecutive zeros
        con0 = int_lam[m:length(int_lam)]
        zero_count = 0
        for( k in c(1:length(con0))){
          if( con0[k] == 0 ){
            zero_count = zero_count + 1
          }
          if( con0[k] > 0 ){
            break
          }
        }
        n1 = int_lam[m-1]
        #n2 = int_lam[m + zero_count]
        #lam_val = (n1 + n2) / (zero_count + 2)
        #int_lam[(m-1):(m + zero_count)] = lam_val
        int_lam[m:(m + zero_count - 1)] = n1
        
        v1 = int_var[m-1]
        #v2 = int_var[m + zero_count]
        #var_val = (v1 + v2) / (zero_count + 2)^2
        #int_var[(m-1):(m + zero_count)] = var_val
        int_var[m:(m + zero_count - 1)] = v1
        
        m = m + zero_count
      }
      
      if( int_lam[m] > 0 ){
        #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
        #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
        next
      }
      
      
    }
    
    est_dist$lam_hat = int_lam
    est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  #remove zeros
  sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  #plot subprime vs. prime
  plot_group = c("subprime","prime")
  plot_df = sum_dat_def[sum_dat_def$cat %in% plot_group,]
  plot_df = plot_df[plot_df$Age <= 50,] #limited data beyond age 45
  plot_df$cat = factor(plot_df$cat, 
                       levels=c('subprime','prime'))
  
  ggplot(data=plot_df,
         aes(x=Age,y=lam_hat,linetype=cat)) +
    geom_line() +
    aes(ymin=CI_lower,ymax=CI_upper,fill=cat) +
    geom_ribbon(alpha=0.2) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Defaults)") +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman")) #+
  #annotate('segment', x = 20, xend = 12, y = 0.0275, yend = 0.0275,
  #         color = "black", linewidth=0.65,
  #         linetype = 1, arrow = arrow(length = unit(1.75, "mm"))) +
  #annotate("label", x = 23, y = 0.0275, label = "Approx. Mar 2020 + 3 mo.", family="Times New Roman")
  #save, if desired
  #ggsave("convergence_subprime_prime_demo_new2017.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Figure D4: Credit Risk Convergence: All Risk Bands, Point Estimates
######################################################################################
######################################################################################

{
  #new empirical results
  path = "~/loan_risk_convergence/replication_code/"
  sdart = read.csv(paste(path,"sdart7369new_2017.csv",sep=""))
  cmax = read.csv(paste(path,"cmax9new_2017.csv",sep=""))
  drv = read.csv(paste(path,"drv7629new_2017.csv",sep=""))
  aart = read.csv(paste(path,"aart1342new_2017.csv",sep=""))
  
  #Z: Time-of-event
  #Y: Truncation Time
  #C: Censor Indicator (1=censored)
  #D: Default Indicator (1=default)
  #R: Repayment Indicator (1=repayment)
  
  #build obs data set ~ sdart
  obs_data <- data.frame(sdart$risk_cat_ir,sdart$Xc,sdart$Y,sdart$C,sdart$D,sdart$R)
  names(obs_data)[names(obs_data) == 'sdart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'sdart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'sdart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'sdart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'sdart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'sdart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_sdart = obs_data
  obs_dat_sdart$bond = "sdart"
  
  #build obs data set ~ cmax
  obs_data <- data.frame(cmax$risk_cat_ir,cmax$Xc,cmax$Y,cmax$C,cmax$D,cmax$R)
  names(obs_data)[names(obs_data) == 'cmax.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'cmax.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'cmax.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'cmax.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'cmax.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'cmax.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_cmax = obs_data
  obs_dat_cmax$bond = "carmax"
  
  #build obs data set ~ drive
  obs_data <- data.frame(drv$risk_cat_ir,drv$Xc,drv$Y,drv$C,drv$D,drv$R)
  names(obs_data)[names(obs_data) == 'drv.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'drv.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'drv.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'drv.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'drv.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'drv.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_drv = obs_data
  obs_dat_drv$bond = "drive"
  
  #build obs data set ~ aart
  obs_data <- data.frame(aart$risk_cat_ir,aart$Xc,aart$Y,aart$C,aart$D,aart$R)
  names(obs_data)[names(obs_data) == 'aart.Xc'] <- 'Z'
  names(obs_data)[names(obs_data) == 'aart.Y'] <- 'Y'
  names(obs_data)[names(obs_data) == 'aart.C'] <- 'C'
  names(obs_data)[names(obs_data) == 'aart.D'] <- 'D'
  names(obs_data)[names(obs_data) == 'aart.R'] <- 'R'
  names(obs_data)[names(obs_data) == 'aart.risk_cat_ir'] <- 'risk_cat'
  
  obs_dat_aart = obs_data
  obs_dat_aart$bond = "ally"
  
  #############################################################################
  #############################################################################
  #combine information
  obs_data = rbind(obs_dat_sdart,obs_dat_cmax,obs_dat_drv,obs_dat_aart)
  n = nrow(obs_data)
  
  #############################################################################
  #############################################################################
  #calculate default cause-specific hazard
  #############################################################################
  #############################################################################
  #functions used repeatedly for each risk category
  f_star_def <- function(x) {
    res = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    return(res/n)
  }
  
  est_C <- function(x) {
    ans = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(ans/n)
  }
  
  est_haz_def <- function(x) {
    num = sum( (cur_dat$C == 0) & (cur_dat$D == 1) & (cur_dat$Z == x))
    den = sum( (x >= cur_dat$Y) & (x <= cur_dat$Z))
    return(num/den)
  }
  
  Var_est_def <- function(x){
    num = f_star_def(x) * (est_C(x) - f_star_def(x))
    den = (est_C(x))^3
    return(num/den)
  }
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  
  delts = c(72 - max(drv$remainingTermtoMaturityNumber),
            73 - max(aart$remainingTermtoMaturityNumber),
            72 - max(sdart$remainingTermtoMaturityNumber),
            73 - max(cmax$remainingTermtoMaturityNumber))
  
  #calculate the hazard rate and confidence intervals for each risk category
  delta = max(min(delts),0) #smallest delta from above; flr at zero bc LT >= 72
  cats = as.character(sort(unique(obs_data$risk_cat)))
  
  sum_dat_def = data.frame()
  min_age = 10
  
  for (c in cats) {
    cur_dat = obs_data[obs_data$risk_cat == c,]
    
    z = sort(unique(cur_dat$Z))
    lam_hat_def = sapply(c((delta+1):(max(z))),est_haz_def)
    var_hat_def = sapply(c((delta+1):(max(z))),Var_est_def)
    
    est_dist = data.frame(
      "Age" = c((delta+1):(max(z))),
      "lam_hat" = lam_hat_def,
      "Est_Var" = var_hat_def
    )
    
    est_dist = est_dist[est_dist$Age >= min_age,]
    
    # int_lam = est_dist$lam_hat
    # int_var = est_dist$Est_Var
    # #m = 0
    # for (m in c(1:length(int_lam))){
    #   #m = m + 1
    #   #condition to see if remaining lam_hat all zeros
    #   if( sum(int_lam[m:length(int_lam)]) == 0 ){
    #     int_lam[m:length(int_lam)] = 0
    #     int_var[m:length(int_lam)] = 0
    #     break
    #   }
    #   
    #   #if (m < min_age){
    #   #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
    #   #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
    #   #  next
    #   #}
    #   
    #   #condition to see if lam_hat is zero
    #   if( int_lam[m] == 0){
    #     #get number of consecutive zeros
    #     con0 = int_lam[m:length(int_lam)]
    #     zero_count = 0
    #     for( k in c(1:length(con0))){
    #       if( con0[k] == 0 ){
    #         zero_count = zero_count + 1
    #       }
    #       if( con0[k] > 0 ){
    #         break
    #       }
    #     }
    #     n1 = int_lam[m-1]
    #     #n2 = int_lam[m + zero_count]
    #     #lam_val = (n1 + n2) / (zero_count + 2)
    #     #int_lam[(m-1):(m + zero_count)] = lam_val
    #     int_lam[m:(m + zero_count - 1)] = n1
    #     
    #     v1 = int_var[m-1]
    #     #v2 = int_var[m + zero_count]
    #     #var_val = (v1 + v2) / (zero_count + 2)^2
    #     #int_var[(m-1):(m + zero_count)] = var_val
    #     int_var[m:(m + zero_count - 1)] = v1
    #     
    #     m = m + zero_count
    #   }
    #   
    #   if( int_lam[m] > 0 ){
    #     #int_lam[m] = est_dist$lam_hat[est_dist$Age == a]
    #     #int_var[m] = est_dist$Est_Var[est_dist$Age == a]
    #     next
    #   }
    #   
    #   
    # }
    # 
    # est_dist$lam_hat = int_lam
    # est_dist$Est_Var = int_var
    
    CI_lower = log(est_dist$lam_hat) -
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    CI_upper = log(est_dist$lam_hat) + 
      qnorm(0.975) * sqrt( (est_dist$Est_Var/ (est_dist$lam_hat)^2) /n )
    
    est_dist$CI_lower = exp(CI_lower)
    est_dist$CI_upper = exp(CI_upper)
    
    est_dist$cat = rep(c,nrow(est_dist))
    
    sum_dat_def = rbind(sum_dat_def,est_dist)
    
  }
  
  #remove zeros
  #sum_dat_def = sum_dat_def[sum_dat_def$lam_hat > 0,]
  
  path = "~/loan_risk_convergence/replication_code"
  write.csv(sum_dat_def,paste(path,"default72new_2017.csv",sep="/"))
  
  #plot point estimates
  compare_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  plot_group = c("deep_subprime","subprime","near_prime","prime","super_prime")
  plot_df = sum_dat_def[sum_dat_def$cat %in% plot_group,]
  plot_df$cat = factor(plot_df$cat,
                       levels=c('deep_subprime','subprime','near_prime','prime','super_prime'))
  ggplot(data=plot_df,
         aes(x=Age,y=lam_hat,linetype=cat)) +
    geom_line() +
    #aes(ymin=CI_lower,ymax=CI_upper,fill=cat) +
    ylim(0,0.06) +
    #geom_ribbon(alpha=0.2) +
    xlab("Loan Age (Months)") + ylab("Estimated Cause-Specific Hazard Rate (Defaults)") +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman"))
  #save, if desired
  #ggsave("haz_def_new_all.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}

######################################################################################
######################################################################################
# Figure E1: Simulation Study Results
######################################################################################
######################################################################################

#precise simulation results will differ for each run;
#overall result very stable
{
  dat <- read.csv('~/loan_risk_convergence/replication_code/csh_lam.csv')
  
  #input the specified cause-specific hazards
  lam01 <- dat$lam_01x
  lam02 <- dat$lam_02x
  f01 <- dat$f_01_star
  f02 <- dat$f_02_star
  Cx <- dat$Cx
  
  #create the distribution for X from lam01 + lam02
  surv_X <- function(x){
    C = vector()
    for(i in c(1:x)){
      C = append(C,1-(lam01[i-1]+lam02[i-1]))
    }
    return(prod(C))
  }
  
  #create the simulation lookup distribution
  max = c()
  cum = c()
  for (j in c(1:10)){
    cum = append(cum,surv_X(j) - surv_X(j+1))
    max = append(max,sum(cum))
  }
  
  X = c(1:10)
  min = c(0,max[1:9])
  lookup_d <- data.frame(X,min,max)
  
  X_inv <- function(q) {
    row_ind = (lookup_d$min <= q) * (lookup_d$max > q)
    return( lookup_d$X[which(row_ind == 1)] )
  }
  
  #bernoulli comp. risk function
  risk_cat = function(x){
    q = runif(1,0,1)
    p = lam01[x] / (lam01[x] + lam02[x])
    return(ifelse(q <= p, 1, 2))
  }
  
  #estimate functions
  
  f_star_est = function(i,x) {
    ind1 = (sim_dat$Xj <= sim_dat$Cj)
    ind2 = (sim_dat$Zj == i)
    ind3 = (pmin(sim_dat$Xj, sim_dat$Cj) == x)
    num = (1/nrow(sim_dat)) * sum( ind1 * ind2 * ind3)
    return(num)
  }
  
  c_est = function(x) {
    ind4 = (sim_dat$Yj <= x)*(x <= pmin(sim_dat$Xj, sim_dat$Cj))
    den = (1/nrow(sim_dat)) * sum( ind4 )
    return(den)
  }
  
  lam_star_est = function(i,x){
    #numerator
    ind1 = (sim_dat$Xj <= sim_dat$Cj)
    ind2 = (sim_dat$Zj == i)
    ind3 = (pmin(sim_dat$Xj, sim_dat$Cj) == x)
    num = (1/nrow(sim_dat)) * sum( ind1 * ind2 * ind3)
    
    #denominator
    ind4 = (sim_dat$Yj <= x)*(x <= pmin(sim_dat$Xj, sim_dat$Cj))
    den = (1/nrow(sim_dat)) * sum( ind4 )
    
    return(num/den)
  }
  
  
  #performing the simulation
  n = 10000
  r = 1000
  tau = 5
  
  fstar_est_d = matrix(NA,r,10)
  fstar_est_r = matrix(NA,r,10)
  
  C_est = matrix(NA,r,10)
  
  lam_d = matrix(NA,r,10)
  lam_r = matrix(NA,r,10)
  
  lam_var_d = matrix(NA,r,10)
  lam_var_r = matrix(NA,r,10)
  
  for (k in (c(1:r))) {
    
    #simulated data
    sim_Y = rdunif(n, 1, 5)
    sim_X = sapply(runif(n,0,1),X_inv)
    sim_C = sim_Y + tau
    sim_Z = sapply(sim_X,risk_cat)
    
    sim_dat = data.frame(Xj = sim_X,
                         Yj = sim_Y,
                         Cj = sim_C,
                         Zj = sim_Z)
    
    #remove truncated data
    sim_dat = sim_dat[(sim_dat$Yj <= sim_dat$Xj),]
    
    #calculate the estimates
    for (j in c(1:10)){
      fstar_est_d[k,j] = f_star_est(1,j)
      fstar_est_r[k,j] = f_star_est(2,j)
    }
    for (j in c(1:10)){
      C_est[k,j] = c_est(j)
    }
    for (j in c(1:10)){
      lam_d[k,j] = lam_star_est(1,j)
      lam_r[k,j] = lam_star_est(2,j)
    }
    for (j in c(1:10)){
      lam_var_d[k,j] = (f_star_est(1,j) * (c_est(j) - f_star_est(1,j))) / (c_est(j))^3
    }
    for (j in c(1:10)){
      lam_var_r[k,j] = (f_star_est(2,j) * (c_est(j) - f_star_est(2,j))) / (c_est(j))^3
    }
    
    if ((k %% 100) == 0) {print(k)}
  }
  
  #prepare CI plots
  #defaults
  var_diag <- vector()
  for (i in c(1:10)) {
    var_diag = append(var_diag,f01[i]*Cx[i]*(Cx[i]-f01[i]))
  }
  
  D = diag(var_diag,10,10)
  
  B_diag = vector()
  for (i in c(1:10)) {
    B_diag = append(B_diag,(Cx[i])^(-2))
  }
  
  B = diag(B_diag,10,10)
  
  COV = B %*% D %*% t(B)
  
  ######################
  #organize results into graph
  sim_out <- lam_d
  sim_var <- lam_var_d
  LAM = lam01
  VAR = diag(COV)
  
  #log scale
  CI_lower_log = log(LAM) - qnorm(0.975) * sqrt( (VAR/(LAM)^2) / n) 
  CI_upper_log = log(LAM) + qnorm(0.975) * sqrt( (VAR/(LAM)^2) / n)
  
  q25 = vector()
  for (i in c(1:10)) {
    q25 = append(q25,quantile(log(sim_out[,i]),0.025,na.rm=TRUE)[[1]])
  }
  
  q975 = vector()
  for (i in c(1:10)) {
    q975 = append(q975,quantile(log(sim_out[,i]),0.975,na.rm=TRUE)[[1]])
  }
  
  #removes zeros / non-estiamtes (NAs)
  lam_est_log = vector()
  for (i in c(1:10)) {
    lam_est_log = append(lam_est_log,mean(log(sim_out[,i])))
    #subset(sim_out[,i],sim_out[,i]>0)),na.rm=TRUE))
  }
  
  lam_est = vector()
  for (i in c(1:10)) {
    lam_est = append(lam_est,mean(sim_out[,i]))
  }
  
  var_emp = vector()
  for (i in c(1:10)) {
    var_emp = append(var_emp,mean(sim_var[,i],na.rm=TRUE))
  }
  
  #CI from simulated data
  CI_lower_data_log = vector()
  for (i in c(1:10)) {
    CI_lower_data_log = append(CI_lower_data_log, lam_est_log[i] - qnorm(0.975) * sqrt( (var_emp[i]/(lam_est[i])^2) / n))
  }
  CI_upper_data_log = vector()
  for (i in c(1:10)) {
    CI_upper_data_log = append(CI_upper_data_log, lam_est_log[i] + qnorm(0.975) * sqrt( (var_emp[i]/(lam_est[i])^2) / n))
  }
  
  
  xval = c(1:10)
  df_01 = data.frame(value=c(exp(CI_lower_log), exp(CI_upper_log),exp(CI_lower_data_log),
                             exp(CI_upper_data_log),exp(q25), exp(q975), LAM, exp(lam_est_log)),
                     type=c(rep("true",length(xval)),
                            rep("true",length(xval)),
                            rep("estimate",length(xval)),
                            rep("estimate",length(xval)),
                            rep("empirical",length(xval)),
                            rep("empirical",length(xval)),
                            rep("lam_true",length(xval)),
                            rep("lam_est",length(xval))),
                     high_low=c(rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("lam_true",length(xval)),
                                rep("lam_est",length(xval))),
                     month=rep(c(1:10),8))
  
  ci_high01 = subset(df_01,high_low == "high")
  names(ci_high01)[1] = "ci_high"
  ci_low01 = subset(df_01,high_low == "low")
  names(ci_low01)[1] = "ci_low"
  
  ribbon_df1 = data.frame("ci_high" = ci_high01$ci_high,
                          "ci_low" = ci_low01$ci_low,
                          "type" = ci_high01$type,
                          "month" = ci_low01$month,
                          "lambda" = c(LAM, exp(lam_est_log), exp(lam_est_log)))
  ribbon_df1$cause = "Cause_01"
  
  line_df1 = subset(df_01,(type=="lam_true")|(type=="lam_est"))
  line_df1$cause = "Cause_01"
  
  #repayments
  var_diag <- vector()
  for (i in c(1:10)) {
    var_diag = append(var_diag,f02[i]*Cx[i]*(Cx[i]-f02[i]))
  }
  
  D = diag(var_diag,10,10)
  
  B_diag = vector()
  for (i in c(1:10)) {
    B_diag = append(B_diag,(Cx[i])^(-2))
  }
  
  B = diag(B_diag,10,10)
  
  COV = B %*% D %*% t(B)
  
  ######################
  #organize results into graph
  sim_out <- lam_r
  sim_var <- lam_var_r
  LAM = lam02
  VAR = diag(COV)
  
  #log scale
  CI_lower_log = log(LAM) - qnorm(0.975) * sqrt( (VAR/(LAM)^2) / n) 
  CI_upper_log = log(LAM) + qnorm(0.975) * sqrt( (VAR/(LAM)^2) / n)
  
  q25 = vector()
  for (i in c(1:10)) {
    q25 = append(q25,quantile(log(sim_out[,i]),0.025,na.rm=TRUE)[[1]])
  }
  
  q975 = vector()
  for (i in c(1:10)) {
    q975 = append(q975,quantile(log(sim_out[,i]),0.975,na.rm=TRUE)[[1]])
  }
  
  #removes zeros / non-estiamtes (NAs)
  lam_est_log = vector()
  for (i in c(1:10)) {
    lam_est_log = append(lam_est_log,mean(log(sim_out[,i])))
    #subset(sim_out[,i],sim_out[,i]>0)),na.rm=TRUE))
  }
  
  lam_est = vector()
  for (i in c(1:10)) {
    lam_est = append(lam_est,mean(sim_out[,i]))
  }
  
  var_emp = vector()
  for (i in c(1:10)) {
    var_emp = append(var_emp,mean(sim_var[,i],na.rm=TRUE))
  }
  
  #CI from simulated data
  CI_lower_data_log = vector()
  for (i in c(1:10)) {
    CI_lower_data_log = append(CI_lower_data_log, lam_est_log[i] - qnorm(0.975) * sqrt( (var_emp[i]/(lam_est[i])^2) / n))
  }
  CI_upper_data_log = vector()
  for (i in c(1:10)) {
    CI_upper_data_log = append(CI_upper_data_log, lam_est_log[i] + qnorm(0.975) * sqrt( (var_emp[i]/(lam_est[i])^2) / n))
  }
  
  
  xval = c(1:10)
  df_02 = data.frame(value=c(exp(CI_lower_log), exp(CI_upper_log),exp(CI_lower_data_log),
                             exp(CI_upper_data_log),exp(q25), exp(q975), LAM, exp(lam_est_log)),
                     type=c(rep("true",length(xval)),
                            rep("true",length(xval)),
                            rep("estimate",length(xval)),
                            rep("estimate",length(xval)),
                            rep("empirical",length(xval)),
                            rep("empirical",length(xval)),
                            rep("lam_true",length(xval)),
                            rep("lam_est",length(xval))),
                     high_low=c(rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("low",length(xval)),
                                rep("high",length(xval)),
                                rep("lam_true",length(xval)),
                                rep("lam_est",length(xval))),
                     month=rep(c(1:10),8))
  
  ci_high02 = subset(df_02,high_low == "high")
  names(ci_high02)[1] = "ci_high"
  ci_low02 = subset(df_02,high_low == "low")
  names(ci_low02)[1] = "ci_low"
  
  ribbon_df2 = data.frame("ci_high" = ci_high02$ci_high,
                          "ci_low" = ci_low02$ci_low,
                          "type" = ci_high02$type,
                          "month" = ci_low02$month,
                          "lambda" = c(LAM, exp(lam_est_log), exp(lam_est_log)))
  ribbon_df2$cause = "Cause_02"
  
  line_df2 = subset(df_02,(type=="lam_true")|(type=="lam_est"))
  line_df2$cause = "Cause_02"
  
  ribbon_df = rbind(ribbon_df1, ribbon_df2)
  line_df = rbind(line_df1, line_df2)
  
  ggplot()+
    geom_ribbon(data=ribbon_df,aes(x=month,ymin=ci_low,
                                   ymax=ci_high,fill=type),alpha=0.5)+
    geom_line(data=line_df, aes(x=month, y=value, linetype=type)) +
    xlab("Lifetime Variable (X)") +
    ylab("Estimated Cause-Specific Hazard + 95% Confidence Intervals") +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(linetype=guide_legend(""),
           fill=guide_legend("")) +
    #scale_fill_grey() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          strip.text.x = element_text(size = 7.25, family="Times New Roman"),
          strip.text.y = element_text(size = 7.25, family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman"))+
    facet_grid(cols = vars(cause))
  #save, if desired
  #ggsave("sim_study.pdf",height=4,width=6,device = cairo_pdf)
  rm(list=ls())
}