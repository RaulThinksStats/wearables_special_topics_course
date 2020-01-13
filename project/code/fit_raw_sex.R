rm(list=ls())
pckgs <- c("tidyverse",                       ## package(s) for data manipulation/plotting
           "mgcv"
)
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

data_path <- "./data"
code_path <- "./code"

# data_path <- "./project/data"
# code_path <- "./project/code"

if(!dir.exists(data_path)){}

if(!file.exists(file.path(data_path, "raw_accel_data_deocs.rds"))){
    source(file.path(code_path, "raw_data_process.R"))
} else {
    df_fit <- read_rds(file.path(data_path, "raw_accel_data_deocs.rds"))
}


df_fit$male <- as.numeric(df_fit$gender == "male")
train_j <- 1:200
test_j  <- 200:380
df_train <- subset(df_fit, J %in% train_j)
df_test <- subset(df_fit,  J %in% test_j)



fit_sex_m1   <- gam(male ~ s(smat_sub, by=lmat_sub), method="REML", data=df_train,family=quasibinomial())
fit_sex      <- gam(male ~ te(umat, smat, dmat, by=lmat), method="REML", data=df_train,family=quasibinomial())

write_rds(fit_sex, file.path(data_path,"sex_fit_model2.rds"))





yhat_sex_m1 <- predict(fit_sex_m1, newdata=df_test, type='response')

df_test$sp_inx <- rep(1:500, each=ceiling(nrow(df_fit)/500))[1:nrow(df_test)]
yhat_sex       <- sapply(split(df_test, df_test$sp_inx), function(x) predict.gam(fit_sex, newdata=x, type='response'))
yhat_sex       <- as.vector(yhat_sex)

df_test <- 
    df_test %>% 
    dplyr::mutate(yhat_sex_m1 = yhat_sex_m1,
                  yhat_sex = yhat_sex) %>% 
    group_by(id) %>%
    dplyr::mutate(yhat_sex_m1_ind = mean(yhat_sex_m1),
                  yhat_sex_ind = mean(yhat_sex)) %>% 
    ungroup()
df_test_ind <- 
    df_test %>%
    select(id, age, yhat_sex_m1_ind, yhat_sex_ind, male)%>%
    group_by(id) %>% 
    slice(1) %>% 
    ungroup()


library("pROC")
roc(df_test_ind$male, df_test_ind$yhat_sex_m1_ind)
roc(df_test_ind$male, df_test_ind$yhat_sex_ind)

# 
# ## out of sample R^2
# R2_m1 <- 1-sum((df_test_ind$yhat_sex_m1_ind-df_test_ind$age)^2)/sum((df_test_ind$age-mean(df_test_ind$age) )^2)
# R2_m1 <- sprintf("%5.2f", 100*round(R2_m1,4))
# R2_m2 <- 1-sum((df_test_ind$yhat_sex_ind-df_test_ind$age)^2)/sum((df_test_ind$age-mean(df_test_ind$age) )^2)
# R2_m2 <- sprintf("%5.2f", 100*round(R2_m2,4))
# 
# par(mfrow=c(1,2))
# plot(df_test_ind$age, df_test_ind$yhat_sex_m1_ind,pch=16,xlim=c(20,55),ylim=c(20,55),xlab="Age", 
#      ylab="Estimated Age",main=bquote("Model 1:" ~ hat(R)^2 == .(R2_m1)))
# abline(a=0,b=1,col='red',lty=2,lwd=2)
# plot(df_test_ind$age, df_test_ind$yhat_sex_ind,pch=16,xlim=c(20,55),ylim=c(20,55),xlab="Age", 
#      ylab="Estimated Age",main=bquote("Model 2:" ~ hat(R)^2 == .(R2_m2)))
# abline(a=0,b=1,col='red',lty=2,lwd=2)


