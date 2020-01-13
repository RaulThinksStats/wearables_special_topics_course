rm(list=ls())
## The code below is divided into the following "chunks"
##  - Section 0: Install and load all necessary packages including the rnhanesdata package
##               which contains the accelerometry data used in our analysis. 
##               Load relevant pre-processed data in the rnhanesdata package.
##               Also includes some set-up (defining directories to download data/save figures, etc.).
##  - Section 1: Data processing & EDA
##              1a: Prep accelerometry data for analysis and merge all data
##              1b. Process new variables of interest which were downloaded in 1a
##                  * Self reported overall health status
##                  * number of bad mental days in the last 30 days
##                  * poverty-to-income ratio
##                  * employment status
##  - Section 2: Data applicationds/model fitting
##              2a: Exploratory and model fitting for FoFR modelling activity as a function of age, gender and weekend vs weekday
##              2b. Exploratory analysis of PA vs race, gender, and employment status


########################################
##                                    ##
##  Section 0: load required packages ##
##                                    ##
########################################

## Check for packages needed to run analyses/install the rnhanesdata package.
## Note: all these packages are available on CRAN and can therefore be downloaded using the install.packages() function,
##       the rnhanesdata package is not on CRAN due to package size
pckgs <- c("fields",                          ## package for heatmap colors
           "gridExtra",                       ## package for plotting >1 ggplot objects in a single figure
           "devtools",                        ## package used to download R packages stored on GitHub
           "data.table",                      ## package for data manipulation (better for "wide" data than tidyverse)
           "tidyverse",                       ## package(s) for data manipulation/plotting
           "mgcv","refund","qgam",            ## packages used for smoothing/functional regression
           "survey"                           ## package used for analyzing (complex) survey data
)

sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

## Install the rnhanesdata package and dependencies.
## This may take a few minutes because of the size of the data package.
if(!require("rnhanesdata")){
    devtools::install_github("andrew-leroux/rnhanesdata", build = TRUE, 
                             build_opts = c("--no-resave-data", "--no-manual"))
    require("rnhanesdata")
}

## set up directory paths
code_path    <- file.path(".","code")    ## path to supplemental (figure) code
figure_path  <- file.path(".","figures") ## path to save figures
data_path    <- file.path(".","data")    ## path to save data
make_plots   <- TRUE                     ## change to FALSE if you don't want to create figures

## Load data pre-processed in the rnhanesdata package
## Note the naming convention _* denotes NHANES wave. 
##      _C = 2003-2004 
##      _D = 2005-2006
data("PAXINTEN_C");data("PAXINTEN_D")    ## activity count data matrices
data("Flags_C");data("Flags_D")          ## wear/non-wear flag data matrices
data("Covariate_C");data("Covariate_D")  ## demographic/comorbidity data matrices


## set up some theme options for plotting done later
textsize <- 24
theme_set(theme_bw(base_size=textsize) + 
              theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                    plot.title = element_text(hjust = 0.5))
)



###########################################################################
##                                                                       ##
##  Section 1b: Prep accelerometry data for analysis and merge all data  ##
##                                                                       ##
###########################################################################

## Re-code activity counts which are considered "non-wear" to be 0.
## This doesn't impact much data, most estimated non-wear times correspond to 0 counts anyway
PAXINTEN_C[,paste0("MIN",1:1440)] <- PAXINTEN_C[,paste0("MIN",1:1440)]*Flags_C[,paste0("MIN",1:1440)]
PAXINTEN_D[,paste0("MIN",1:1440)] <- PAXINTEN_D[,paste0("MIN",1:1440)]*Flags_D[,paste0("MIN",1:1440)]


## Merge accelerometry (activity counts + wear/non-wear flags) and covariate data.
## We will drop the flag information shortly, but we first use it to identify "good" days of data based on
## estimated wear time
data_C <- 
    PAXINTEN_C %>% 
    ## note that both PAXINTEN_* and Covariate_* have a column
    ## called "SDDSRVYR" indicating which NHANES wave the data is associated with.
    left_join(Covariate_C, by=c("SEQN","SDDSRVYR")) %>% 
    ## Similarly, the activity count (PAXINTEN_*) and wear/non-wear flag matrices (Flags_*) share 
    ## SEQN, PAXCAL, PAXSTAT, WEEKDAY, SDDSRVR variables.
    ## In addition, when we join activity and flag data we have duplicated column names.
    ## Supply meaningful suffixes so we can differentiate them
    left_join(Flags_C, by=c("SEQN","PAXCAL","PAXSTAT","WEEKDAY","SDDSRVYR"), suffix=c(".AC",".Flag"))
data_D <- 
    PAXINTEN_D %>% 
    left_join(Covariate_D, by=c("SEQN","SDDSRVYR")) %>% 
    left_join(Flags_D, by=c("SEQN","PAXCAL","PAXSTAT","WEEKDAY","SDDSRVYR"), suffix=c(".AC",".Flag"))

## Combine 2003-2004 and 2005-2006 data into a single data frame
data <- bind_rows(data_C, data_D)

## Estimate total daily wear time and determine whether a day is "good" based on
## >= 10 hours (600 minutes) of wear time + device calibration/quality flags.
## Calculate number of good days per participant (this will be used as an exclusion criteria later -- the standard is >= 3 days),
## Then remove wear/non-wear flags from the data since we no longer need them for this analysis.
data <- 
    data %>% 
    mutate("wear_time" = rowSums(select(., one_of(paste0("MIN",1:1440,".Flag"))), na.rm=TRUE),
           "good_day"  = as.numeric(wear_time >= 600),
           "good_day"  = good_day * (PAXCAL %in% 1) * (PAXSTAT %in% 1)
    ) %>% 
    group_by(SEQN) %>% 
    mutate("n_good_days" = sum(good_day)) %>% 
    ungroup() %>% 
    select(-one_of(paste0("MIN",1:1440,".Flag"))) %>% 
    rename_at(vars(paste0("MIN",1:1440,".AC")), ~paste0("MIN",1:1440))

## clean up the workspace (free up RAM)
rm(list=c("data_C","data_D",
          "PAXINTEN_C","PAXINTEN_D",
          "Flags_C","Flags_D",
          "Covariate_C","Covariate_D"))





##############################################################################
##                                                                          ##
##  Section 1c: create new variables/relevel factor variables for analyses  ##
##                                                                          ##
##############################################################################

data <- 
    data %>% 
    mutate(
        ## re-code age in months at examination (accelerometer wear) to years
        ## bin age into certain groups (will use for plotting later)
        Age = RIDAGEEX/12,
        Age_cat = cut(Age, c(0, 1, 3, 6, 12, 16, 20, 30, 40, 50, 60, 70, 80, 85,Inf), right=FALSE),
        
        ## re-code day of the week to be a meaningful factor variable
        DoW = factor(WEEKDAY, levels=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
        
    ) %>% 
    ## re-order the data such that the activity columns are last
    ## absolutely not necessary, just a personal preference 
    select(-one_of(paste0("MIN",1:1440)), one_of(paste0("MIN",1:1440))) 



###############################
##                           ##
##  Section 2: Data Analysis ##
##                           ##
###############################



## fast code combining tidyverse and data.table functionality
data_fit_all_days <- 
    data %>%
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>%
    select(SEQN, Age, DoW, Gender, one_of(paste0("MIN",1:1440))) %>% 
    gather(key="time", value="AC", -SEQN,-Age,-Gender,-DoW) %>% 
    mutate(time = as.numeric(str_extract(time, "[0-9]+")),
           time = time/max(time))

start_time <- Sys.time()
# fit_z <- gam(list(AC ~ s(time, bs="cc",fx=TRUE,k=10) + ti(time, Age, bs=c("cc","cr"), mc=c(FALSE, TRUE),fx=TRUE,k=10), 
#                      ~ s(time, bs="cc",fx=TRUE,k=10) + ti(time, Age, bs=c("cc","cr"), mc=c(FALSE, TRUE),fx=TRUE,k=10)), 
#              family=ziplss(), data=data_fit_all_days)
fit_z <- bam(AC ~ s(time, bs="cc",k=10) + ti(time, Age, bs=c("cc","cr"), mc=c(FALSE, TRUE),k=10),
             family=quasipoisson(), data=data_fit_all_days,discrete=TRUE,method="fREML",chunk.size=500000, nthreads=5)
# fit_z_q50 <- qgam(AC ~ s(time, bs="cc",k=10) + ti(time, Age, bs=c("cc","cr"), mc=c(FALSE, TRUE),k=10),
#              data=data_fit_all_days,qu=0.5)
end_time <- Sys.time()
end_time- start_time 


start_time_q <- Sys.time()
fit_z_q50 <- qgam(list(AC ~ s(time, bs="cc",k=10) + ti(time, Age, bs=c("cc","cr"), mc=c(FALSE, TRUE),k=10)
                          ~ s(time, bs="cc",k=10)),
             data=data_fit_all_days,qu=0.5)
end_time_q <- Sys.time()
end_time- start_time 




## obtain predicted values at each age/time/day of the week combination
tind <- seq(0,1,len=100)
aind <- seq(6,85,len=100)
df_pred <- data.frame(time=rep(tind, length(aind)),
                      Age=rep(aind, each=length(tind)))
df_pred <- data.frame(df_pred, predict(fit_z, newdata=df_pred, type='terms'), "yhat"=predict(fit_z, newdata=df_pred, type='response'))

beta1 <- df_pred$`s.time.`[1:length(tind)] + coef(fit_z)["(Intercept)"]
beta2 <- matrix(df_pred$`ti.time.Age`, ncol=length(tind), nrow=length(aind), byrow=FALSE)
yhat_mat <- matrix(df_pred$yhat, ncol=length(tind), nrow=length(aind), byrow=FALSE)
par(mfrow=c(1,3))
plot(tind,beta1, type='l')
image.plot(tind, aind, beta2)
image.plot(tind, aind, yhat_mat)

gamma1 <- df_pred$`s.1.time.`[1:length(tind)]





