rm(list=ls())
pckgs <- c("tidyverse",                       ## package(s) for data manipulation/plotting
           "mgcv"
)
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

# data_path <- "./data"
# code_path <- "./code"

data_path <- "./project/data"
code_path <- "./project/code"

if(!dir.exists(data_path)){}

if(!file.exists(file.path(data_path, "raw_accel_data_deocs.rds"))){
    source(file.path(code_path, "raw_data_process.R"))
} else {
    df_fit <- read_rds(file.path(data_path, "raw_accel_data_deocs.rds"))
}

train_j <- 1:200
test_j  <- 200:380
df_train <- subset(df_fit, J %in% train_j)
df_test  <- subset(df_fit,  J %in% test_j)

fit_age_m1   <- gam(age ~ s(smat_sub, by=lmat_sub), method="REML", data=df_train)
fit_age      <- gam(age ~ te(umat, smat, dmat, by=lmat), method="REML", data=df_train)

write_rds(fit_age, file.path(data_path,"age_fit_model2.rds"))




yhat_age_m1 <- predict(fit_age_m1, newdata=df_test, type='response')

df_test$sp_inx <- rep(1:500, each=ceiling(nrow(df_fit)/500))[1:nrow(df_test)]
yhat_age       <- sapply(split(df_test, df_test$sp_inx), function(x) predict.gam(fit_age, newdata=x, type='response'))
yhat_age       <- as.vector(yhat_age)

df_test <- 
    df_test %>% 
    dplyr::mutate(yhat_age_m1 = yhat_age_m1,
                  yhat_age = yhat_age) %>% 
    group_by(id) %>%
    dplyr::mutate(yhat_age_m1_ind = mean(yhat_age_m1),
                  yhat_age_ind = mean(yhat_age)) %>% 
    ungroup()
df_test_ind <- 
    df_test %>%
    select(id, age, yhat_age_m1_ind, yhat_age_ind)%>%
    group_by(id) %>% 
    slice(1) %>% 
    ungroup()
    

## out of sample R^2
R2_m1 <- 1-sum((df_test_ind$yhat_age_m1_ind-df_test_ind$age)^2)/sum((df_test_ind$age-mean(df_test_ind$age) )^2)
R2_m1 <- sprintf("%5.2f", 100*round(R2_m1,4))
R2_m2 <- 1-sum((df_test_ind$yhat_age_ind-df_test_ind$age)^2)/sum((df_test_ind$age-mean(df_test_ind$age) )^2)
R2_m2 <- sprintf("%5.2f", 100*round(R2_m2,4))

par(mfrow=c(1,2))
plot(df_test_ind$age, df_test_ind$yhat_age_m1_ind,pch=16,xlim=c(20,55),ylim=c(20,55),xlab="Age", 
     ylab="Estimated Age",main=bquote("Model 1:" ~ hat(R)^2 == .(R2_m1)))
abline(a=0,b=1,col='red',lty=2,lwd=2)
plot(df_test_ind$age, df_test_ind$yhat_age_ind,pch=16,xlim=c(20,55),ylim=c(20,55),xlab="Age", 
     ylab="Estimated Age",main=bquote("Model 2:" ~ hat(R)^2 == .(R2_m2)))
abline(a=0,b=1,col='red',lty=2,lwd=2)








