rm(list=ls())
pckgs <- c("tidyverse",                       ## packheight_in(s) for data manipulation/plotting
           "mgcv"
)
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packheight_ins(x)
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

train_j <- 1:200
test_j  <- 200:380
df_train <- subset(df_fit, J %in% train_j)
df_test  <- subset(df_fit,  J %in% test_j)

fit_height_in_m1   <- gam(height_in ~ s(smat_sub, by=lmat_sub), method="REML", data=df_train)
fit_height_in      <- gam(height_in ~ te(umat, smat, dmat, by=lmat), method="REML", data=df_train)

write_rds(fit_height_in, file.path(data_path,"height_in_fit_model2.rds"))




yhat_height_in_m1 <- predict(fit_height_in_m1, newdata=df_test, type='response')

df_test$sp_inx <- rep(1:500, each=ceiling(nrow(df_fit)/500))[1:nrow(df_test)]
yhat_height_in       <- sapply(split(df_test, df_test$sp_inx), function(x) predict.gam(fit_height_in, newdata=x, type='response'))
yhat_height_in       <- as.vector(yhat_height_in)

df_test <- 
    df_test %>% 
    dplyr::mutate(yhat_height_in_m1 = yhat_height_in_m1,
                  yhat_height_in = yhat_height_in) %>% 
    group_by(id) %>%
    dplyr::mutate(yhat_height_in_m1_ind = mean(yhat_height_in_m1),
                  yhat_height_in_ind = mean(yhat_height_in)) %>% 
    ungroup()
df_test_ind <- 
    df_test %>%
    select(id, height_in, yhat_height_in_m1_ind, yhat_height_in_ind)%>%
    group_by(id) %>% 
    slice(1) %>% 
    ungroup()


## out of sample R^2
R2_m1 <- 1-sum((df_test_ind$yhat_height_in_m1_ind-df_test_ind$height_in)^2)/sum((df_test_ind$height_in-mean(df_test_ind$height_in) )^2)
R2_m1 <- sprintf("%5.2f", 100*round(R2_m1,4))
R2_m2 <- 1-sum((df_test_ind$yhat_height_in_ind-df_test_ind$height_in)^2)/sum((df_test_ind$height_in-mean(df_test_ind$height_in) )^2)
R2_m2 <- sprintf("%5.2f", 100*round(R2_m2,4))

par(mfrow=c(1,2))
plot(df_test_ind$height_in, df_test_ind$yhat_height_in_m1_ind,pch=16,xlim=c(58,76),ylim=c(58,76),xlab="height (in)", 
     ylab="Estimated height (in)",main=bquote("Model 1:" ~ hat(R)^2 == .(R2_m1)))
abline(a=0,b=1,col='red',lty=2,lwd=2)
plot(df_test_ind$height_in, df_test_ind$yhat_height_in_ind,pch=16,xlim=c(58,76),ylim=c(58,76),xlab="height_in", 
     ylab="Estimated height (in)",main=bquote("Model 2:" ~ hat(R)^2 == .(R2_m2)))
abline(a=0,b=1,col='red',lty=2,lwd=2)








