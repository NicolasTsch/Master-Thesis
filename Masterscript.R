#Packages
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyquant)){install.packages("tidyquant")}
if(!require(tibble)){install.packages("tibble")}
if(!require(dygraphs)){install.packages("dygraphs")}
if(!require(tseries)){install.packages("tseries")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(vars)){install.packages("vars")}
if(!require(MTS)){install.packages("MTS")}
if(!require(BVAR)){install.packages("BVAR")}
if(!require(readxl)){install.packages("readxl")}
if(!require(zoo)){install.packages("zoo")}
if(!require(mice)){install.packages("mice")}
if(!require(Kendall)){install.packages("Kendall")}
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
if(!require(janitor)){install.packages("janitor")}
if(!require(writexl)){install.packages("writexl")}
if(!require(BVARverse)){install.packages("BVARverse")}
if(!require(extrafont)){install.packages("extrafont")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(plm)){install.packages("plm")}
library(tidyverse)
library(dplyr)
library(tidyquant)
library(tibble)
library(dygraphs)
library(tseries)
library(corrplot)
library(vars)
library(MTS)
library(BVAR)
library(readxl)
library(zoo)
library(mice)
library(Kendall)
library(PerformanceAnalytics)
library(janitor)
library(writexl)
library(BVARverse)
library(extrafont)
library(stargazer)
library(gridExtra)
library(plm)

# -----------------------------------------------------------------------------
############################################################# 
#### TO LOAD THE DATA VIA GITHUB DIRECTLY GO TO LINE 266 ####
#############################################################
# -----------------------------------------------------------------------------

# GitHub Link:
# https://github.com/NicolasTsch/Master-Thesis

# working directory (set individually)
setwd("G:/SFS/01 Makroprudenzielle Aufsicht/Daten/Daten/Bloomberg Daten/Nicolas Thesis")

#------------------------------------------------------------------------------------------------------------------

###############################################
#----------------- Data -----------------------
###############################################

##################
# Data from SNB (monthly)
##################
import1 <- read_xlsx("Daten_Final_v2.xlsx", sheet="SNB_Monthly")                                      # in Mio. CHF
SNB_data <- import1 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(as.yearmon(Date)),                 # date vector
                                                      SNBTOTM_abs = SNBTOTM-dplyr::lag(SNBTOTM),      # balance sheet total, absolute changes
                                                      SNBSDM_abs = SNBSDM-dplyr::lag(SNBSDM),         # domestic sight deposits, absolute changes
                                                      SNBTOTM_ret = (SNBTOTM/dplyr::lag(SNBTOTM))-1,  # balance sheet total, relative changes
                                                      SNBSDM_ret = (SNBSDM/dplyr::lag(SNBSDM))-1,     # domestic sight deposits, relative changes
                                                      CHFREALMINV = 1/CHFREALM)
                                                      
  
View(SNB_data)


##################
# Monthly Bond Yields
##################
import2 <- read_xlsx("Daten_Final_v2.xlsx", sheet="INT_Monthly")
import2.1 <- import2 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>%
  dplyr::mutate(across(!Date, ~./100)) %>%                   # make decimals
  pivot_longer(!Date, names_to="Country", values_to="Yield") # bring into long format
View(import2.1)

# account for EA membership
import3 <- read_xlsx("Daten_Final_v2.xlsx", sheet="EA_member")
EA_monthly <- import3 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>% 
  pivot_longer(!Date, names_to="Country", values_to="Member") # bring into long format

EA_members <- EA_monthly %>% dplyr::filter(Member==0) %>% pivot_wider(names_from = "Country", values_from="Member") %>% 
  dplyr::select(-Date) %>% colnames()  # save EA member countries names, member == 0 denotes the start point of membership

# bring together
INT_monthly <- import2.1 %>% left_join(EA_monthly, by=c("Date", "Country")) %>% dplyr::mutate(Member=as.numeric(Member))
View(INT_monthly)

# code for transforming into wide-format and accounting for EA membership over all countries
INT_monthly %>% dplyr::filter(Member==0) %>% dplyr::select(-Member) %>% pivot_wider(names_from = "Country", values_from = "Yield")
# code for transforming into wide-format and accounting for EA membership over all countries; based on a flexible time-frame for before EUR adaption
# => Member <= X: X states the number of months to go back before the membership starts  
INT_monthly %>% group_by(Country) %>% dplyr::filter(Member<=60) %>% dplyr::select(-Member) %>% 
  pivot_wider(names_from = "Country", values_from = "Yield") %>% ungroup()

#save(INT_monthly, file="INT_monthly.RData")

##################
# Monthly SNB, CPI, and FX Data from Bloomberg
##################
import4 <- read_xlsx("Daten_Final_v2.xlsx", sheet="Main_Monthly")
MAIN_monthly <- import4 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>%
  dplyr::select(-c(SNBSDM_BB,EURCHFM_BB)) %>%
  left_join(SNB_data%>%dplyr::select(Date, CH2M, CH10M), by="Date") %>%               # join CH bond yields
  left_join(INT_monthly %>% dplyr::select(-Member) %>% 
              pivot_wider(names_from = "Country", values_from = "Yield")%>%           # join EA bond yields (harmonised)
              dplyr::select(Date, EA2M, EA10M), by="Date")%>%
  dplyr::mutate(across(SNBPRM:CH10M, ~./100)) %>%                                     # make decimals
  dplyr::mutate(EACHCPIM=EACPIM-CHCPIM,                                               # inflation differential                 
                EACHCPIM_abs=EACHCPIM - dplyr::lag(EACHCPIM),                         # inflation differential absolute changes
                EACHCPIM_ret= (EACHCPIM - dplyr::lag(EACHCPIM))/dplyr::lag(EACHCPIM), # inflation differential relative changes
                EACH10M=EA10M-CH10M,                                                  # 10-year interest rate differential 
                EACH10M_abs=EACH10M-dplyr::lag(EACH10M),                              # 10-year interest rate differential abs. changes
                EACH10M_ret=(EACH10M - dplyr::lag(EACH10M))/dplyr::lag(EACH10M),      # 10-year interest rate differential rel. changes
                EACH2M=EA2M-CH2M,                                                     # 2-year interest rate differential 
                EACH2M_abs=EACH2M-dplyr::lag(EACH2M),                                 # 2-year interest rate differential abs. changes
                EACH2M_ret=(EACH2M - dplyr::lag(EACH2M))/dplyr::lag(EACH2M))          # 2-year interest rate differential rel. changes
  
  
View(MAIN_monthly)


##################
# Monthly Gold, EPU, CISS, etc. 
##################
import5 <- read_xlsx("Daten_Final_v2.xlsx", sheet="Others_Monthly")
OTHERS_monthly <- import5 %>% as_tibble() %>%
  dplyr::mutate(Date=as.Date(as.yearmon(Date)),
                GOLDCHFM = GOLDUSDM/CHFUSD) %>%       #convert gold price to CHF
  tidyr::fill(-Date,.direction = "up") %>%            #for SNB variables (not needed below)
  dplyr::mutate(M2toGDP = CHM2/CHBIP)
View(OTHERS_monthly)

CISS <- read_delim("CISS.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)

SovCISS <- CISS %>% arrange(Date) %>% dplyr::filter(Date<="2022-01-01") %>% rename(SovCISS = Value)
plot(SovCISS, type="l")                               #attention: only available from September 2000!



##################
# Daily Bond Yields
##################
# import6 <- read_xlsx("Daten_Final_v2.xlsx", sheet="INT_Daily")
# import6.1 <- import6 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(Date)) %>%
#   dplyr::mutate(across(!Date, ~./100)) %>% 
#   pivot_longer(!Date, names_to="Country", values_to="Yield") #bring into long format
# 
# # account for EA membership
# import7 <- read_xlsx("Daten_Final_v2.xlsx", sheet="EA_member_daily")
# EA_daily <- import7 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(Date)) %>% 
#   pivot_longer(!Date, names_to="Country", values_to="Member")
# 
# # bring together
# INT_daily <- import6.1 %>% left_join(EA_daily, by=c("Date", "Country")) %>% dplyr::mutate(Member=as.numeric(Member))
# View(INT_monthly)
# 
# # code for transforming into wide-format and accounting for EA membership over all countries
# INT_daily %>% filter(Member==0) %>% dplyr::select(-Member) %>% pivot_wider(names_from = "Country", values_from = "Yield")
# 
# # code for transforming into wide-format and accounting for EA membership over all countries; based on a flexible time-frame for before EURO adaption
# INT_daily %>% filter(Date=="1996-01-01",Country=="DE10D") #to check for the correct member no.
# INT_daily %>% group_by(Country) %>% filter(Member<=784) %>% dplyr::select(-Member) %>% pivot_wider(names_from = "Country", values_from = "Yield")
# 
# 
# ##################
# # Daily SNB, CPI, and FX Data from Bloomberg
# ##################
# import8 <- read_xlsx("Daten_Final_v2.xlsx", sheet="Main_Daily") %>%
#   dplyr::mutate(across(EUCPID:CHFAROND, ~./100))
# MAIN_daily <- import8 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(as.yearmon(Date)))


##################
# Daily Gold, EPU, etc.
##################
# to be imported

##################
# Shadow exchange rate from Hanke et al (2019)
##################
import10 <- load("ShadowCHFEUR.RData") %>% as_tibble ()                  # data was provided

# Transform the date vector from Hanke et al (2019) to a date format
length(D$Latent$Date)                                                    # check series length
x<-seq(as.Date("2011-09-06"),as.Date("2015-01-15"),by = 1)               # create date vector from start to end of minimum exchange rate
length(x)                                                                # check series length
x<-x[!weekdays(x) %in% c("Samstag","Sonntag")]                           # exclude weekends (as also stated in Hanke et al (2019, pp. 8-9), exchange rates are quoted on weekdays. They take the interval Fri. - Mon. as one day)
length(x)                                                                # check length again

FXlatent_daily <- D$Latent$FXLatent %>% as_tibble() %>% add_column(x) %>% # date column to raw data
  rename (Date=x, FXLATENTD=value) %>% dplyr::select(Date, FXLATENTD)

help1 <- FXlatent_daily 
help1$month <- floor_date(help1$Date, "month")                            # get month
FXlatent_monthly <- help1 %>% group_by(month) %>% summarize(FXLATENTM=mean(FXLATENTD)) %>%  # calculate monthly average
  mutate(Date=as.Date(as.yearmon(month))) %>% 
  dplyr::select(Date, FXLATENTM)

View(FXlatent_monthly)

# Add to FX data
SNB_data <- SNB_data %>% left_join(FXlatent_monthly, by="Date") %>%   # add shadow exchange rate to SNB data set
  dplyr::mutate(EURCHFM=ifelse(!is.na(FXLATENTM),FXLATENTM,EURCHF),   # replace min exchange rate period with shadow exchange rate
                EURCHFM_ret = log(EURCHFM)-dplyr::lag(log(EURCHFM)))

# check
plot(SNB_data$EURCHFM, type="l", col="red")                           # check
lines(SNB_data$EURCHF, col="black")                                   # add plot with bloomberg EUR/CHF data to compare the series'


##################
# Weights (both not particularly needed for results)
##################
#GDP data
import11 <- read_xlsx("Daten_Final_v2.xlsx", sheet="GDP_Monthly") %>% as_tibble() %>% # empty table with all dates and countries
  dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>%
  pivot_longer(-Date,names_to="Country", values_to="GDP")
import12 <- read_xlsx("OECD/OECD_GDPann.xlsx") %>% as_tibble() %>%                    # GDP data from the IMF
  dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>%
  pivot_longer(-Date,names_to="Country", values_to="GDP")

GDP_monthly <- import11 %>% left_join (import12, by=c("Date","Country")) %>%          # join GDP data
  dplyr::select(-GDP.x) %>% rename(GDP=GDP.y) %>%
  group_by(Country) %>% 
  fill(-Date,.direction="down") %>%                                                   # fill down because its quarterly data
  dplyr::mutate(across(!Date,~./100000))                                              # make millions

GDP_monthly %>%
  pivot_wider(names_from = "Country", values_from = "GDP") %>% dplyr::select(-Date) %>% colSums()



# Weights: 0 = membership start, 1 = one month before membership and so on, NA = no membership
import13 <- read_xlsx("Daten_Final_v2.xlsx", sheet="Weights") %>% as_tibble() %>%     # get weight matrix from excel
  dplyr::mutate(Date=as.Date(as.yearmon(Date)))
Weights <- import13 %>% dplyr::mutate(across(!Date,~ifelse(is.na(.) | .>0 ,0,1))) %>% # make 0/1 weights
  pivot_longer(!Date, names_to="Country", values_to="Weight")


# save data
# save(Weights, file="Weights.RData")
# save(GDP_monthly, file="GDP_monthly.RData")
# save(SNB_data, file="SNB_data.RData")
# save(SovCISS, file="SovCISS.RData")
# save(MAIN_monthly, file="MAIN_monthly.RData")
# save(INT_monthly, file="INT_monthly.RData")

##################
# clean environment
##################
remove(import1, import2, import2.1, import3, import4, import5, import6, import6.1, import7, import8, import10, import11, 
       import12, import13, help1,D)


##################
# Load data quickly
##################
load("INT_monthly.RData")
load("GDP_monthly.RData")
load("Weights.RData")

# combine data of all EA members
data <- INT_monthly %>%
  left_join(GDP_monthly, by=c("Date","Country")) %>%
  left_join(Weights, by=c("Date","Country")) %>%
  dplyr::mutate(Weight = ifelse(is.na(Member) | Member>120 | is.na(Yield),0,1),    # 0 = not include, else weight = 1
                GDP    = ifelse(is.na(Member) | Member>120 | is.na(Yield),0,GDP))  # 0 = not include, else GDP weight
# -----------------------------------------------------------------------------------------------------------------
# the weights matrix with 1 or 0 is depending on the membership start, i.e., start until present = 1, otherwise 0.
# Because of the member vector in INT_monthly this can be adjusted by setting the number of months to look back in
# the ifelse function above => Member>x, where x is the required number of months to look back. Thus, countries are included
# based on their membership start and optional an equal number of months before the start date (is needed for oos to have
# an oos turbulence already in 1999). Default as it is now is 1 == start membership.
# The same is done for GDP data to avoid weighting for countries where NAs are in the yield vectors
# -----------------------------------------------------------------------------------------------------------------


# final input for turb measure
Dates <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>% #dplyr::filter(Date<="2004-12-01") %>%   # filter needed for IS estimation
  dplyr::select(-c(Member, GDP, Weight)) %>%
  pivot_wider(names_from = "Country", values_from = "Yield") %>%
  dplyr::mutate(Date=as.yearmon(Date)) %>% .$Date

# input for EA 12 without LUX
input <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>% #dplyr::filter(Date<="2004-12-01") %>%   # filter needed for IS estimation     
  dplyr::select(-c(Member,GDP, Weight)) %>%
  pivot_wider(names_from = "Country", values_from = "Yield") %>%
  dplyr::select(BEL10M,	BEL2M,	DE10M,	DE2M,	FR10M,FR2M,
                IT10M,	IT2M,	NL10M,	NL2M,	IRL10M,	IRL2M,
                GR10M,	GR2M,	POR10M,	POR2M, ESP10M,	ESP2M,	FIN10M,
                FIN2M,	AT10M,	AT2M) %>% 
  xts(., order.by=Dates) 

# weights 1
weights <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>% #dplyr::filter(Date<="2004-12-01") %>% # filter needed for IS estimation   
  dplyr::select(-c(Member,Yield, GDP)) %>%                          # exclude GDP or Weight      
  pivot_wider(names_from = "Country", values_from = "Weight")%>%    # include GDP or Weight
  dplyr::mutate(across(!Date, ~replace_na(., 0))) %>%
  dplyr::select(BEL10M,	BEL2M,	DE10M,	DE2M,	FR10M,FR2M,
                IT10M,	IT2M,	NL10M,	NL2M,	IRL10M,	IRL2M,
                GR10M,	GR2M,	POR10M,	POR2M, ESP10M,	ESP2M,	FIN10M,
                FIN2M,	AT10M,	AT2M)


# weights 2: 60 obs ahead with weight 0
# weights <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>%
#   dplyr::select(-c(Member, GDP, Weight)) %>%
#   pivot_wider(names_from = "Country", values_from = "Yield") %>%
#   dplyr::mutate(Date=as.yearmon(Date))
# weights[1:2,-1] <- as.numeric(0)
# weights <- weights %>% as.matrix() %>% replace_na(.,0) %>% as_tibble() %>%
#   dplyr::select(-c(Date, LUX10M, EST10M)) %>%
#   dplyr::mutate_all(.,~as.numeric(.)) %>%
#   dplyr::mutate_all(., ~replace(., rep(which(. == 0), each = 60) + c(1:60), 0)) %>%
#   dplyr::mutate_all(.,~as.numeric(.)) %>%
#   dplyr::mutate_all(., ~ifelse(.!=0, 1, 0)) %>%
#   dplyr::select(BEL10M,	BEL2M,	DE10M,	DE2M,	FR10M,FR2M,
#                 IT10M,	IT2M,	NL10M,	NL2M,	IRL10M,	IRL2M,
#                 GR10M,	GR2M,	POR10M,	POR2M, ESP10M,	ESP2M,	FIN10M,
#                 FIN2M,	AT10M,	AT2M)


#check
dim(input)
dim(weights)


# input for EA all without LUX, MLT, CYP, LTV, LTU, EST
# input <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>%       
#   dplyr::select(-c(Member,GDP, Weight)) %>%
#   pivot_wider(names_from = "Country", values_from = "Yield") %>%
#   dplyr::select(-c(Date,LUX10M, MLT10M, CYP10M,CYP1M, LV7M, LV2M, LT10M, LT2M,EST10M)) %>% 
#   xts(., order.by=Dates) 
# 
# weights <- data %>% dplyr::filter(Weight==1, !is.na(Member)) %>%    
#   dplyr::select(-c(Member,Yield, GDP)) %>%                          # exclude GDP or Weight      
#   pivot_wider(names_from = "Country", values_from = "Weight")%>%    # include GDP or Weight
#   dplyr::mutate(across(!Date, ~replace_na(., 0))) %>%
#   dplyr::select(-c(Date,LUX10M, MLT10M, CYP10M,CYP1M, LV7M, LV2M, LT10M, LT2M,EST10M))

#------------------------------------------------------------------------------------------------------------------

###############################################
#---------- Euro area sovereign stress --------
###############################################

#----- FUNCTIONS ARE BUILT IN THE ISTurbulence AND OSTurbulence SCRIPTS! -----

# NOTE: I changed the position of the code snippets where the cturb is calculated, because cturb = turb/mturb
# and if the option squared == FALSE, turb and mturb change after the calculation of cturb by ^0.5.


# define some critical events for plotting later
events_area <- data.frame(Start = as.Date(c("2008-01-01", "2011-09-07")),
                          End   = as.Date(c("2009-10-31", "2015-01-11")),
                          Event = c("Global Financial Crisis", "EUR/CHF Cap"))

events_line <- data.frame(Date  = as.Date(c("2010-05-02", 
                                    "2010-11-28",
                                    "2011-05-17",
                                    "2012-02-12",
                                    "2012-09-06",
                                    "2015-08-01")),
                          Event = c("1st GR bail-out", 
                                    "IRL bail-out",
                                    "POR bail-out",
                                    "2nd GR bail-out",
                                    "'Whatever it takes'",
                                    "3rd GR bail-out"))
##################
# Calculation of EASS
##################

# MFU1: In-sample, 1999-01 until 2021-12, equal weights
turb1 <- ISturbulence(X=input, norm=TRUE, GW=TRUE,weights=weights)
turb2 <- turb1$turb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::select(Date, turb)
turb.grad <- turb1$turb.grad %>% as_tibble() %>% add_column(Date=as.Date(Dates))
turb.GW <- turb1$turb.GW %>% as_tibble() %>% add_column(Date=as.Date(Dates))

# MFU2: Out-of-sample, 5-year rolling window, 1990-01 until 2021-12, equal weights (SET INPUT DATA ABOVE CORRECTLY!)
turb1 <- OSturbulence(X=input, GW=TRUE, norm=TRUE, weights=weights, rolling = TRUE,roll.obs=60, use="pairwise.complete.obs")
turb2 <- turb1$turb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::select(Date, turb)
turb.grad <- turb1$turb.grad %>% as_tibble() %>% add_column(Date=as.Date(Dates))
turb.GW <- turb1$turb.GW %>% as_tibble() %>% add_column(Date=as.Date(Dates))
turb.m <- turb1$mturb %>% as_tibble() %>% add_column(Date=as.Date(Dates))
turb.c <- turb1$cturb %>% as_tibble() %>% add_column(Date=as.Date(Dates))

# MFU3: Out-of-sample, 10-year rolling window, 1990-01 until 2021-12, equal weights (SET INPUT DATA ABOVE CORRECTLY!)
turb1 <- OSturbulence(X=input, GW=TRUE, norm=TRUE, weights=weights, rolling = TRUE, roll.obs=120, use="pairwise.complete.obs")
turb2 <- turb1$turb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::select(Date, turb)
turb.grad <- turb1$turb.grad %>% as_tibble() %>% add_column(Date=as.Date(Dates))
turb.GW <- turb1$turb.GW %>% as_tibble() %>% add_column(Date=as.Date(Dates))

### MFU4: IS 1999 bis 2004, rest OOS 5Y (adjust input above)
#IS
turbIS <- ISturbulence(X=input, norm=TRUE, GW=TRUE,weights=weights)
turbIS.turb <- turbIS$turb %>% 
  as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::select(Date, turb) %>% dplyr::filter(Date<="2004-12-31")
turbIS.GW <- turbIS$turb.GW %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date<="2004-12-31")
turbIS.m <- turbIS$mturb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date<="2004-12-31")
turbIS.c <- turbIS$cturb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date<="2004-12-31")

#OOS (adjust input above)
turbOS <- OSturbulence(X=input, GW=TRUE, norm=TRUE, weights=weights, rolling = TRUE,roll.obs=60, use="pairwise.complete.obs")
turbOS.turb <-  turbOS$turb %>% 
  as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::select(Date, turb) %>% dplyr::filter(Date>"2004-12-31")
turbOS.GW <- turbOS$turb.GW %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date>"2004-12-31")
turbOS.m <- turbOS$mturb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date>"2004-12-31")
turbOS.c <- turbOS$cturb %>% as_tibble() %>% add_column(Date=as.Date(Dates)) %>% dplyr::filter(Date>"2004-12-31")

#turb
turb <- rbind(turbIS.turb, turbOS.turb)
turb.GW <- rbind(turbIS.GW, turbOS.GW)
turb.c <- rbind(turbIS.c, turbOS.c)
turb.m <- rbind(turbIS.m, turbOS.m)




# deal with breaks in MFU: set 0
turb <- turb2 %>% dplyr::filter(Date>="1999-01-01") %>% dplyr::mutate(turb=replace_na(turb, 0)) # set negative stress periods that produce NaN to 0
turb.dates <- turb$Date
plot(turb2, type="l")

# deal with breaks in MFU: interpolate linearly
turb <- turb2 %>% dplyr::filter(Date>="2000-01-01") %>% dplyr::mutate(turb=na.approx(turb)) # interpolate na values
turb.dates <- turb$Date
plot(turb, type="l")


# plot IS
IS.plot <-
turb %>%
  ggplot()+
  geom_line(aes(x=Date,y=turb), lwd=1)+
  scale_y_continuous(limits = c(0.4,2.65), breaks = seq(0.5,2.5,by=0.5)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(title="In-sample calculation",
       y = "Stress level")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13, family="serif"),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=-19), hjust=0.05)
  )

# plot OOS
#OOS10.plot <-
#OOS5.plot <-
turb %>%
  ggplot()+
  geom_rect(data = events_area,
             aes(xmin=Start,
             xmax=End,
             ymin=-0.2, ymax=Inf), color="black", alpha=0.2)+
   geom_text(data=events_area,
             aes(x=Start, y=0, label=Event, fontface=2), size=3.5, vjust=2, hjust=c(0.2, -0.35))+
   geom_linerange(data = events_line,
              aes(x=Date, ymin=0, ymax=16.25), 
              color="brown", alpha=0.7, lwd=1)+
   geom_text(data=events_line,
             aes(x=Date, y=16.25, label=Event, angle=90, fontface=2), size=3.5, vjust=-0.5, hjust=1)+
  geom_area(aes(x=Date,y=turb), lwd=1, color="black", alpha=1)+
  scale_y_continuous(limits = c(-0.35,16.25), 
                     breaks = seq(0,16,by=2)) +
  scale_x_date(date_breaks = "2 year", 
               date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(#title="Out-of-sample calculation, 10-year rolling window",
       y = "Stress level", x= "Date")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=-20), hjust=0.05)
  )

#arrange in one plot
grid.arrange(IS.plot, OOS5.plot, OOS10.plot, ncol=1)
#ggsave()

##################
# Tests
##################

# MannKendall test (trend analysis, H0: no trend)
MannKendall(turb %>% dplyr::select(turb) %>% ts(., frequency = 12))

# stationarity (H0: not stationary)
adf.test(turb$turb, alternative="stationary", k=8)

##################
# Plot contributions
##################

########
# Countries
#######
# plot contribution of countries (from Garthwaite & Koch contribution)
turb.contribution <- turb.GW %>% dplyr::filter(Date>="1999-01-01") %>% # filter data after 1999
  mutate(across(-Date,~as.numeric(.))) %>%                             # make the columns numeric
  mutate(across(-Date,~replace_na(.,0))) %>%                           # replace NA values with 0 (i.e., zero contribution or zero overall index)
  dplyr::select(-Date)
  
turb.contribution2 <- sapply(unique(substr(names(turb.contribution), 1, 2)),  # unique store country name patterns in a vector
                            function(xx) rowSums(turb.contribution[,grep(xx, names(turb.contribution)), drop=FALSE])) %>%
  as_tibble() %>%
  add_column(Date=turb.dates,.before = 1) %>%
  add_column(EASS=turb$turb,.before = 2) %>% 
  pivot_longer(-Date, names_to="Variable", values_to="Value") # for ggplot


# use ggplot  ¢export with 15 x 9.29)   
turb.contribution2  %>%
  ggplot() +
  geom_area(data=subset(turb.contribution2, Variable!="EASS"),aes(x=Date,y=Value,fill=Variable), stat='identity',
            color=1, lwd=0.2) +
  # geom_line(data=subset(turb.contribution2, Variable="EASS"),aes(x=Date,y=Value), 
  #           color="black", size=0.7)+
  geom_hline(yintercept=0, color="grey")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0, 0))+
  ylab("Stress level")+
  guides(fill=guide_legend("Country"))+
  theme_light()+
  theme(
    legend.text = element_text(size = 11),
    legend.position = c(0.95, 0.7),
    axis.text = element_text(size = 13, colour = "black"),
    text = element_text(size = 13),
    axis.title.x = element_text(face="bold", margin=margin(t=10)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black")
  ) #+
  labs(title="Individual Country Contribution to the EASS Index", subtitle = "1999-01-01 to 2021-12-31")


# contribution summarised over years
turb.contribution2 %>% pivot_wider(names_from="Variable",values_from="Value") %>% 
  dplyr::filter(Date>="1999-01-01", Date<="2021-12-31") %>%
  group_by(year = lubridate::floor_date(Date, "year")) %>% 
  summarise(across(-Date,~sum(.))) %>% dplyr::mutate(year = lubridate::year(year)) %>%
  adorn_totals("row") %>% as_tibble() %>%
  dplyr::mutate(across(-year, ~round(.,1))) %>%
  write_xlsx(.,"country_contrib.xlsx") 

#formula for excel/latex transform: =A1&"&"&B1&"&"&C1&"&"&D1&"&"&E1&"&"&F1&"&"&G1&"&"&H1&"&"&I1&"&"&J1&"&"&K1&"&"&L1&"&"&M1&"\\"


########
# Variables
#######
# plot contribution of countries (from Garthwaite & Koch contribution)
turb.contribution <- turb.GW %>% dplyr::filter(Date>="1999-01-01") %>% # filter data after 1999
  mutate(across(-Date,~as.numeric(.))) %>%                             # make the columns numeric
  mutate(across(-Date,~replace_na(.,0))) %>%
  rowwise() %>%
  dplyr::mutate(Y2 = sum(c_across(contains('2M'))),
                Y10= sum(c_across(contains("10M")))) %>%
  ungroup() %>%
  dplyr::select(Date, Y2, Y10) %>%
  pivot_longer(-Date, names_to="Variable", values_to="Value")


# use ggplot     
turb.contribution  %>%
  ggplot()+
  geom_area(aes(x=Date,y=Value, fill=Variable), stat='identity', color=1, lwd=0.2)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0, 0))+
  ylab("Stress level")+
  guides(fill=guide_legend("Variable"))+
  theme_light()+
  theme(
    legend.text = element_text(size = 11),
    legend.position = c(0.95, 0.75),
    axis.text = element_text(size = 13, colour = "black"),
    text = element_text(size = 13),
    axis.title.x = element_text(face="bold", margin=margin(t=10)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black")
  )#+
  #labs(title="Contribution of 2-year and 10-year government bonds to the EASS", subtitle = "1999-01-01 to 2021-12-31")


# contribution summarised over years
turb.contribution2 %>% pivot_wider(names_from="Variable",values_from="Value") %>% 
  dplyr::filter(Date>="1999-01-01", Date<="2021-12-31") %>%
  group_by(year = lubridate::floor_date(Date, "year")) %>% 
  summarise(across(-Date,~sum(.))) %>% adorn_totals("row") %>% as_tibble() %>% write_xlsx(.,"var_contrib.xlsx")



########
# Mean and Corr.
#######
# plot contribution of mean (single country) or correlation (between countries)
turb.m %>% left_join(turb.c, by="Date") %>%
  dplyr::filter(Date>="1999-01-01") %>%
  mutate(across(-Date,~as.numeric(.))) %>%                             
  mutate(across(-Date,~replace_na(.,0))) %>%
  pivot_longer(-Date, names_to="Variable", values_to="Value") %>%
  ggplot()+
  geom_area(aes(x=Date,y=Value, fill=Variable), stat='identity', color=1, lwd=0.2)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0, 0))+
  ylab("Stress level")+
  #guides(fill=guide_legend("Dimension"))+
  scale_fill_discrete(name = "Dimension", labels = c("Correlation", "Mean"))+
  theme_light()+
  theme(
    legend.text = element_text(size = 11),
    legend.position = c(0.925, 0.8),
    axis.text = element_text(size = 13, colour = "black"),
    text = element_text(size = 13),
    axis.title.x = element_text(face="bold", margin=margin(t=10)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black")
  )#+
  labs(title="Mean and correlation turbulence", subtitle = "1999-01-01 to 2021-12-31")




#------------------------------------------------------------------------------------------------------------------
###############################################
#--------------- Combine Data -----------------
###############################################
testdata <- turb %>%
  mutate(across(-Date,~as.numeric(.))) %>%
  left_join(SNB_data, by="Date")%>%
  left_join(MAIN_monthly, by="Date")%>%
  left_join(OTHERS_monthly, by="Date")%>%
  dplyr::mutate(EASS_ret= log(turb)-dplyr::lag(log(turb)),
                EASS_sqr = turb^2,                         # squared MFU
                EASS_above = as.integer(turb>mean(turb)),  # Dummy: MFU above average
                EASS_below = as.integer(turb<mean(turb)),  # Dummy: MFU below average
                EASS_above_roll = as.integer(turb > apply.fromstart(.$turb, FUN = "mean" , gap = 1)), # above rolling mean
                EASS_below_roll = as.integer(turb < apply.fromstart(.$turb, FUN = "mean" , gap = 1)), # below rolling mean
                EURCHFM_lag = dplyr::lag(EURCHFM),
                CHFREALM_ret = log(CHFREALM)-dplyr::lag(log(CHFREALM)),
                GOLDUSDM_abs = GOLDUSDM - dplyr::lag(GOLDUSDM),
                GOLDUSDM_ret = log(GOLDUSDM) - dplyr::lag(log(GOLDUSDM)),
                GOLDCHFM_abs = GOLDCHFM - dplyr::lag(GOLDCHFM),
                GOLDCHFM_ret = log(GOLDCHFM) - dplyr::lag(log(GOLDCHFM))
                )%>% 
  dplyr::select(Date, EASS=turb, EASS_ret, EASS_sqr, EASS_above, EASS_below, EASS_above_roll, EASS_below_roll, EUEPUM, EURCHFspot=EURCHF, EURCHFM, 
                EURCHFM_ret, EURCHFM_lag, CHFREALM, CHFREALM_ret, CHFREALMINV, SNBTOTM, SNBSDM, SNBTOTM_abs, SNBSDM_abs, SNBTOTM_ret, SNBSDM_ret, 
                SNBPRM, CHFARONM, CHCPIM, EACHCPIM, EACHCPIM_abs, EACHCPIM_ret, EACH10M, EACH10M_abs, EACH10M_ret, EACH2M, 
                EACH2M_abs, EACH2M_ret, GOLDUSDM, GOLDUSDM_abs, GOLDUSDM_ret, GOLDCHFM, GOLDCHFM_abs, GOLDCHFM_ret
                ) %>%
  mutate(EASS_ret=ifelse(!is.finite(EASS_ret),0,EASS_ret)) %>%      # replace inf and NA values in MFU return with 0
  na.omit() #%>%                                                  # NA's only at beginning and end
  mutate(across(-c(Date,EASS_above, EASS_below, EASS_above_roll, EASS_below_roll),~as.numeric(scale(.))))  # option: scale data (exclude date and binary variables)

  
# Data for robustness (CISS)
robustness.data <- turb %>%
  mutate(across(-Date,~as.numeric(.))) %>%
  left_join(SNB_data, by="Date")%>%
  left_join(MAIN_monthly, by="Date")%>%
  left_join(OTHERS_monthly, by="Date")%>%
  left_join(CISS, by="Date") %>%
  dplyr::mutate(CISS_ret = log(Value)-dplyr::lag(log(Value))) %>%
  dplyr::select(Date, EASS=turb, CISS=Value,CISS_ret,EURCHFspot=EURCHF, EURCHFM, EURCHFM_ret, CHFREALMINV,SNBSDM, SNBSDM_abs, SNBSDM_ret, 
                EACHCPIM, EACHCPIM_abs, EACHCPIM_ret, EACH10M, EACH10M_abs, EACH10M_ret, EACH2M, EACH2M_abs, EACH2M_ret) %>%
  na.omit() %>%                                                  # NA's only at beginning and end
  mutate(across(-Date,~as.numeric(scale(.))))
  
  
###############################################
#--------------- Stationarity ---------------
###############################################
adf.test(testdata$EURCHFM, alternative="stationary", k=5)

# CISS
adf.test(robustness.data$CISS, alternative="stationary", k=5)

###############################################
#--------------- Visualisations ---------------
###############################################

# EUR/CHF, MFU, REER (inverted!)
testdata %>% dplyr::select(Date, EASS, `EUR/CHF`=EURCHFM, `REER (inverted)` = CHFREALMINV) %>% pivot_longer(-Date, names_to="Variable", values_to="Value") %>%
  ggplot(aes(x=Date,y=Value,colour=Variable,group=Variable))+
  geom_hline(yintercept=0, color="black", size=0.5)+
  geom_line(size=0.75)+
  ylab("Value (standardised)")+
  theme_light()+
  theme(
    legend.text = element_text(size = 11),
    legend.position = c(0.9, 0.75),
    axis.text = element_text(size = 13, colour = "black"),
    text = element_text(size = 13),
    axis.title.x = element_text(face="bold", margin=margin(t=10)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black")
  )#+
  labs(title="EASS and (EUR/)CHF development", subtitle = "1999-01-01 to 2021-12-31")

# EUR/CHF YoY
turb %>%
  dplyr::mutate(across(-Date,~as.numeric(.))) %>%
  left_join(SNB_data, by="Date") %>%
  dplyr::select(Date, EASS=turb, EURCHFM, EURCHFM_ret) %>%
  dplyr::mutate(EURCHFM_yoy = log(EURCHFM)-dplyr::lag(log(EURCHFM),12)) %>%
  na.omit() %>%
  dplyr::select(Date, EASS, EURCHFM_yoy) %>%
  mutate(across(-Date,~as.numeric(scale(.)))) %>%
  #pivot_longer(-Date, names_to="Variable", values_to="Value") %>%
  #ggplot(aes(x=Date,y=Value,colour=Variable,group=Variable))+
  ggplot()+
  geom_hline(yintercept=0, color="black", size=0.5)+
  geom_point(x=EURCHFM_yoy, color="red")+
  geom_point(y=EASS, color="blue")+
  theme_light() +
  labs(title="EASS and EUR/CHF YoY changes", subtitle = "2000-01-01 to 2021-12-31")

# EUR/CHF with and without shadow exchange rate
testdata %>% dplyr::select(Date, EURCHFspot, EURCHFM) %>% pivot_longer(-Date, names_to="Variable", values_to="Value") %>%
  ggplot(aes(x=Date,y=Value,colour=Variable,group=Variable))+
  geom_hline(yintercept=1, color="black", size=0.5)+
  geom_line(size=0.75)+
  theme_light() +
  labs(title="EUR/CHF spot rate with and without the shadow exchange rate", subtitle = "1999-01-01 to 2021-12-31")



# Controlling variables
  testdata %>% dplyr::select(Date, SNBSDM,CHCPIM) %>% pivot_longer(-Date, names_to="Variable", values_to="Value") %>%
    ggplot(aes(x=Date,y=Value,colour=Variable,group=Variable))+
    geom_line()+
    geom_hline(yintercept=0, color="black")#+
  labs(title="Controlling variables", subtitle = "1999-01-01 to 2021-12-31")


# Check all LT and ST yields
yield.plots <- INT_monthly %>% dplyr::select(-Member) %>% dplyr::filter(Date>="1999-01-01")%>% 
  pivot_wider(names_from="Country", values_from = "Yield")
# EA 12
par(mfrow=c(4,3)) #export inches: 10.72, 7.61
yield.plots %>% dplyr::select(AT10M,AT2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Austria",col=c("red","black"))
yield.plots %>% dplyr::select(BEL10M,BEL2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Belgium",col=c("red","black"))
yield.plots %>% dplyr::select(FIN10M,FIN2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Finland",col=c("red","black"))
yield.plots %>% dplyr::select(FR10M,FR2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="France",col=c("red","black"))
yield.plots %>% dplyr::select(DE10M,DE2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Germany",col=c("red","black"))
yield.plots %>% dplyr::select(GR10M,GR2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Greece",col=c("red","black"))
yield.plots %>% dplyr::select(IRL10M,IRL2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Ireland",col=c("red","black"))
yield.plots %>% dplyr::select(IT10M,IT2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Italy",col=c("red","black"))
yield.plots %>% dplyr::select(LUX10M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Luxembourg",col=c("red","black"))
yield.plots %>% dplyr::select(NL10M,NL2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Netherlands",col=c("red","black"))
yield.plots %>% dplyr::select(POR10M,POR2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Portugal",col=c("red","black"))
yield.plots %>% dplyr::select(ESP10M,ESP2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Spain",col=c("red","black"))


# Rest
par(mfrow=c(3,3))
yield.plots %>% dplyr::select(SLO10M,SLO3M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Slovenia",col=c("red","black"))
yield.plots %>% dplyr::select(MLT10M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Malta",col=c("red","black"))
yield.plots %>% dplyr::select(CYP10M,CYP1M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Cyprus",col=c("red","black"))
yield.plots %>% dplyr::select(SLK10M,SLK3M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Slovakia",col=c("red","black"))
yield.plots %>% dplyr::select(LV7M,LV2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Latvia",col=c("red","black"))
yield.plots %>% dplyr::select(LT10M,LT2M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Lithuania",col=c("red","black"))
yield.plots %>% dplyr::select(EST10M) %>%  ts(.,start=c(1999,1),frequency = 12) %>% ts.plot(.,xlab="Estonia",col=c("red","black"))


###############################################
#------------- Descriptive Stat. -------------
###############################################
testdata %>% dplyr::select(EASS, EURCHFM, EURCHFM_ret, CHFREALMINV, SNBSDM, SNBSDM_ret, SNBSDM_abs,
                           EACH2M, EACH2M_abs, EACHCPIM, EACHCPIM_abs) %>%
  as_data_frame() %>%
  stargazer(., summary=TRUE)


###############################################
#------------- Linear Regressions -------------
###############################################


# exchange rate explained by MFU, SNB sight deposits, inflation differential, 10y interest rate differential (all in levels)
summary(lm(EURCHFM~EASS,data=testdata))     # EUR/CHF in levels
summary(lm(EURCHFM_ret~EASS,data=testdata)) # EUR/CHF returns
summary(lm(EURCHFM_ret~EASS+EACHCPIM_abs+EACH2M_abs,data=testdata)) # other explanatory variables added

## SNB sight deposits: problem of endogeneity => two stage least squares
summary(lm(EURCHFM_ret~EASS+SNBSDM_abs,data=testdata))
endo1 <- lm(EURCHFM_ret~EASS+SNBSDM,data=testdata)
cor(endo1$residuals, testdata$SNBSDM)

EURCHFM_fit <- fitted.values(lm(EURCHFM_ret~EASS+SNBSDM_abs,data=testdata))
SNBSDM_fit <- fitted.values(lm(SNBSDM_abs~EASS+EURCHFM_ret,data=testdata))
summary(lm(testdata$EURCHFM_ret~testdata$EASS+testdata$SNBSDM_abs+SNBSDM_fit))
summary(lm(testdata$SNBSDM_abs~testdata$EASS+testdata$EURCHFM_ret+EURCHFM_fit))


summary(lm(SNBSDM_abs~CHCPIM_abs, data=testdata))        #instrumental variable: CH inflation
summary(lm(SNBSDM_abs~EASS+GOLDUSDM_abs, data=testdata))  #instrumental variable: Gold in USD (SNBSDM_abs on GOLDUSDM_abs, SNBSDM_ret on GOLDUSDM_abs)
summary(lm(SNBSDM_abs~EASS+GOLDCHFM_abs, data=testdata))  #instrumental variable: Gold in USD
summary(lm(SNBSDM_abs~SMIM_abs, data=testdata))      #instrumental variable: SMI Index
summary(lm(SNBSDM_abs~SNBAKTM_abs, data=testdata))   #instrumental variable: SNB share price
summary(lm(SNBSDM_abs~CHBIP_abs, data=testdata))     #instrumental variable: CH BIP
summary(lm(SNBSDM_abs~CHREIND_ret, data=testdata))   #instrumental variable: CH housing price index
summary(lm(SNBSDM_abs~SNBGOLDCHF_abs, data=testdata)) #instrumental variable: SNB Gold reserves
summary(lm(SNBSDM_abs~CHUNEMPLY_abs, data=testdata)) #instrumental variable: CH unemployment
summary(lm(SNBSDM_abs~M2toGDP_abs, data=testdata))   #instrumental variable: M2toGDP (siehe Adler et al (2019))
# try: SARON/LIBOR Futures


instrumental.var <- fitted.values(lm(SNBSDM_abs~EASS+GOLDUSDM_abs, data=testdata)) 
summary(lm(testdata$EURCHFM_ret~testdata$EASS+instrumental.var))

summary(lm(EURCHFM_ret~EASS+EUCHCPIM+EUCH2M_abs+instrumental.var,data=testdata))


# MFU squared and above/below average dummy
summary(lm(EURCHFM_ret~EASS_sqr,data=testdata))


# REER
summary(lm(CHFREALM~EASS,data=testdata))
summary(lm(CHFREALM_ret~EASS,data=testdata))

# SovCISS
summary(lm(EURCHFM_ret~CISS,data=robustness.data))
summary(lm(EURCHFM_ret~CISS_ret,data=robustness.data))

#for LATEX
OLS1 <- lm(EURCHFM_ret~EASS+SNBSDM_abs,data=testdata)
OLS2 <- lm(SNBSDM_abs~EASS+GOLDUSDM_abs, data=testdata)
OLS3 <- lm(testdata$EURCHFM_ret~testdata$EASS+instrumental.var)
stargazer(OLS1, OLS2, OLS3)

stargazer(lm(EURCHFM_ret~CISS,data=robustness.data), lm(EURCHFM_ret~CISS+EACHCPIM_abs+EACH2M_abs,data=robustness.data),
          align=TRUE)
###############################################
#------------------ VAR -----------------------
###############################################

#granger causality
grangertest(EASS~EUCHCPIM_abs, order = 5, data=testdata) # MFU causes EURCHF
grangertest(EASS~EURCHFM_ret, order = 5, data=testdata) # test reverse causation

#optimal lag length
VARselect(testdata %>% dplyr::select(EASS,EURCHFM,SNBSDM_ret,EACH2M_abs, EACHCPIM_abs), lag.max=6, type="const")
VARselect(robustness.data %>% dplyr::select(CISS,CHFREALMINV,SNBSDM_ret,EACH2M_abs, EACHCPIM_abs), lag.max=6, type="const")


#Model 1
var1 <- vars::VAR(testdata %>% dplyr::select(EASS,EURCHFM,SNBSDM_ret,EACH2M_abs, EACHCPIM_abs), 
            p = 5, type = "const")
irf1 <- vars::irf(var1, n.ahead = 12)
plot(irf1)


#Model 2 (MFU_ret)
var1 <- vars::VAR(testdata %>% dplyr::select(EASS,SNBSDM_abs,EURCHFspot), 
                  p = 1, type = "const")
irf1 <- vars::irf(var1, n.ahead = 12)
plot(irf1)

#Model 3 (robustness)
var1 <- vars::VAR(robustness.data %>% dplyr::select(CISS,EURCHFM,SNBSDM_ret,EACH2M_abs, EACHCPIM_abs), 
                  p = 2, type = "const")
irf1 <- vars::irf(var1)
plot(irf1)


#nice plots (LOAD SEPARATE FUNCTION FROM SCRIPT: "VAR-tools")
all_varirf <- extract_varirf(irf1)
head(all_varirf)

VAR1 <- all_varirf %>% dplyr::filter(period<12) %>% dplyr::mutate(period = seq(1,12,1)) %>%
  ggplot(aes(x=period, y=irf_eass_eachcpim_abs, ymin=lower_eass_eachcpim_abs, ymax=upper_eass_eachcpim_abs)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  ggtitle("VAR: EASS on inflation rate differential")+
  ylab("")+
  xlab("Months") +
  theme(plot.title = element_text(size = 11, hjust=0.5,face="bold"),
        axis.text = element_text(size = 10, colour = "black"),
        text = element_text(size = 11),
        axis.title.x = element_text(face="bold", margin=margin(t=5)),
        axis.title.y = element_text(face="bold", margin=margin(r=10)),
        legend.title = element_text(face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(colour = 'grey', linetype='solid'),
        panel.border = element_rect(fill=NA, colour="black"),
        axis.ticks = element_line(color="black"))+
  scale_x_continuous(breaks = seq(1, 12, 1), expand = c(0, 0))


grid.arrange(VAR1, VAR2, BVAR1, BVAR2, ncol=2)
#ggsave()


###############################################
#------------------ BVAR ----------------------
###############################################


#original
bvar_1 <- BVAR::bvar(testdata %>% dplyr::select(EASS, EURCHFM, SNBSDM_ret, EACHCPIM_abs, EACH2M_abs),
                     lags=5, n_draw = 10000L, n_burn = 2000L)        # lags => how much information is used for today's value?

irf_bvar1 <- BVAR::irf(bvar_1, horizon = 12, identification=TRUE)   # horizon => periods to look into future after shock
plot(irf_bvar1)


#robustness
bvar_1 <- BVAR::bvar(robustness.data %>% dplyr::select(CISS,CHFREALMINV, SNBSDM_ret,EACH2M_abs, EACHCPIM_abs),
                     lags=2, n_draw = 10000L, n_burn = 2000L)        # lags => how much information is used for today's value?

irf_bvar1 <- BVAR::irf(bvar_1, horizon = 12, identification=TRUE)   # horizon => periods to look into future after shock
plot(irf_bvar1)

#sub-sample
bvar_1 <- BVAR::bvar(testdata %>%dplyr::filter(Date>="2009-01-01", Date<"2016-01-01")%>%
                       dplyr::select(EASS, EURCHFM, SNBSDM_ret, EACHCPIM_abs, EACH2M_abs),
                     lags=1, n_draw = 10000L, n_burn = 2000L)        # lags => how much information is used for today's value?

irf_bvar1 <- BVAR::irf(bvar_1, horizon = 12, identification=TRUE)


#bv_ggplot(irf_bvar1)

# plot uniformly with VAR plots
bvar.plot <- irf_bvar1 %>% BVARverse::tidy() %>%                     # tidy function from BVARverse package
  dplyr::mutate(value = as.numeric(value),
                quantile = as.numeric(quantile),
                quantile = case_when(quantile == 1 ~ "lower",        
                                     quantile == 2 ~ "irf",     
                                     quantile == 3 ~ "upper")) %>%
  pivot_wider(names_from = quantile, values_from = value)


BVAR2 <- bvar.plot %>%
  dplyr::filter(impulse=="CISS", response=="CHFREALMINV") %>%
  ggplot(aes(x=time)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line(aes(y=irf)) +
  ggtitle("(ii) REER") +
  ylab("")+
  xlab("Months") +
  theme_light()+
  theme(plot.title = element_text(size = 11, hjust=0.5, face="bold"),
        axis.title.x = element_text(face="bold", margin=margin(t=10)),
        axis.title.y = element_text(face="bold", margin=margin(r=10), size=11),
        legend.title = element_text(face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(colour = 'grey', linetype='solid'),
        panel.border = element_rect(fill=NA, colour="black"),
        axis.ticks = element_line(color="black"))+
  scale_x_continuous(breaks = seq(0, 12, 1), expand = c(0, 0))
  


grid.arrange(BVAR1, BVAR2, BVAR3, BVAR4, ncol=2)



##################################
# CISS graphic and correlations
##################################
CISS %>%
  ggplot()+
  geom_line(aes(x=Date,y=Value), lwd=1)+
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7,by=0.1)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(y = "SovCISS stress level")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13, family="serif"),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=-19), hjust=0.05)
  )



##################################
# other uncertainty measures
##################################
import1 <- read_xlsx("Uncertainty measures/Global_Policy_Uncertainty_Data.xlsx") 

uncertainty_measures <- import1 %>% as_tibble() %>% mutate(Date=as.Date(Date)) %>%
  dplyr::filter(Date>="2000-01-01", Date<="2021-01-01")

uncertainty_measures %>%
  ggplot()+
  geom_line(aes(x=Date,y=EPU_EUR), lwd=1, col="blue")+
  geom_line(aes(x=Date,y=EPU_EURoUK), lwd=1, col="brown")+
  scale_y_continuous(limits = c(40,450), breaks = seq(100,400,by=100)) +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(title="European EPU with and without UK",
       y = "Stress level")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13, family="serif"),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=5), hjust=0.01)
  )


grid.arrange(GEPU, Macro, Financial, ncol=1)



##################################
# other plots
##################################
import1 <- read_xlsx("SNB/REER.xlsx") 
REER <- import1 %>% as_tibble() %>% mutate(Date=as.Date(Date))

REER %>%
  ggplot()+
  geom_line(aes(x=Date,y=REER), lwd=1, col="dark red")+
  geom_line(aes(x=Date,y=REER_EA), lwd=1, col="dark green")+
  geom_hline(yintercept=100, color="black", linetype="dashed")+
  scale_y_continuous(limits = c(60,130), breaks = seq(60,130,by=20)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(y = "Index")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13, family="serif"),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=5), hjust=0.01)
  )



SNB_data %>%
  ggplot()+
  geom_rect(aes(xmin=as.Date("2011-09-01"),
                xmax=as.Date("2015-01-01"),
                ymin=-Inf, ymax=Inf),fill="light grey", alpha = 0.05)+
  geom_vline(xintercept=as.numeric(as.Date("2011-09-01")), color="black", linetype="dashed")+
  geom_vline(xintercept=as.numeric(as.Date("2015-01-01")), color="black", linetype="dashed")+
  geom_line(aes(x=Date,y=EURCHFM), lwd=1, col="red")+
  geom_line(aes(x=Date,y=EURCHF), lwd=1, col="black")+
  scale_y_continuous(limits = c(0.9,1.7), breaks = seq(1,1.7,by=0.2)) +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y",expand = c(0.02, 0.02))+
  labs(y = "Index")+
  theme_light()+
  theme(
    axis.text = element_text(size = 11, colour = "black"),
    text = element_text(size = 13, family="serif"),
    axis.title.x = element_text(face="bold", margin=margin(t=5)),
    axis.title.y = element_text(face="bold", margin=margin(r=10)),
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(colour = 'grey', linetype='solid'),
    panel.border = element_rect(fill=NA, colour="black"),
    axis.ticks = element_line(color="black"),
    plot.title = element_text(size=11, margin=margin(t=10, b=5), hjust=0.01)
  )



