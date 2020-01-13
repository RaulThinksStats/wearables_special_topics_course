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
df_train   <- subset(df_fit, J %in% train_j)
df_test    <- subset(df_fit,  J %in% test_j)
df_test$sp_inx <- rep(1:500, each=ceiling(nrow(df_fit)/500))[1:nrow(df_test)]
uid        <- unique(df_fit$id)
nid        <- length(uid)

lpmat <- lpmat_m1 <- matrix(NA, ncol=nid, nrow=nrow(df_test))
expit <- function(x) 1/(1+exp(-x))
for(i in 1:nid){
    df_train$Y <- as.numeric(df_train$id == uid[i])
    fit_i_m1 <- gam(Y ~ s(smat_sub, by=lmat_sub), method="REML", data=df_train,family=quasibinomial())
    fit_i <- gam(Y ~ te(umat, smat, dmat, by=lmat), method="REML", data=df_train,family=quasibinomial())
    
    lp_i_m1 <- predict(fit_i_m1, newdata=df_test, type='link')
    lp_i       <- sapply(split(df_test, df_test$sp_inx), function(x) predict.gam(fit_i, newdata=x, type='link'))
    
    lpmat[,i] <- as.vector(lp_i)
    lpmat_m1[,i] <- lp_i_m1
    
    rm(list=c("fit_i_m1","fit_i","lp_i_m1","lp_i"))
    df_train$Y <- NULL
    gc()
    
    print(round(apply(lpmat[,1:i,drop=FALSE],2, function(x)tapply(expit(x), df_test$id, mean)),2))
    print(i)
}

# save(lpmat, lpmat_m1, file=file.path(data_path, "multinom_probs.Rdata"))

sum(expit(lpmat[2,]))

yhat_mat <- t(apply(lpmat, 1, function(x) expit(x)/sum(expit(x))))
yhat_mat_ind <- apply(yhat_mat, 2, function(x) tapply(x, df_test$id, mean))
# yhat_mat_ind <- apply(lpmat, 2, function(x) tapply(expit(x), df_test$id, mean))
Ytilde <- apply(yhat_mat_ind, 1, function(x) which(x == max(x)))

yhat_mat_m1 <- t(apply(lpmat_m1, 1, function(x) expit(x)/sum(expit(x))))
yhat_mat_m1_ind <- apply(yhat_mat_m1, 2, function(x) tapply(x, df_test$id, mean))
Ytilde_m1 <- apply(yhat_mat_m1_ind, 1, function(x) which(x == max(x)))




round(yhat_mat_ind,2)
round(yhat_mat_m1_ind,2)


textsize <- 2
jpeg("~/Desktop/pred_subjs.jpeg",height=500,width=1000,quality=100)
par(mar=c(5,7,1,1))
plot(-1000,-1000, xlim=c(0,32),ylim=c(0,1), xlab="Subject",ylab=expression(hat(Pr)(Y[i]==k)), main="Model 2",
     cex.axis=textsize,cex.lab=textsize,cex.main=textsize,las=1)
for(i in 1:32){
    # inx_max_i <- which(yhat_mat_ind[i,] == max(yhat_mat_ind[i,]))
    col_i <- rep(1,32)
    col_i[i] <- 2
    points(jitter(rep(i, 32), 0.1), yhat_mat_ind[i,], col=col_i,pch=16,cex=textsize/2)
}
dev.off()



textsize <- 2
jpeg("~/Desktop/pred_subjs_m1.jpeg",height=500,width=1000,quality=100)
par(mar=c(5,7,1,1))
plot(-1000,-1000, xlim=c(0,32),ylim=c(0,1), xlab="Subject",ylab=expression(hat(Pr)(Y[i]==k)), main="Model 1",
     cex.axis=textsize,cex.lab=textsize,cex.main=textsize,las=1)
for(i in 1:32){
    # inx_max_i <- which(yhat_mat_ind[i,] == max(yhat_mat_ind[i,]))
    col_i <- rep(1,32)
    col_i[i] <- 2
    points(jitter(rep(i, 32), 0.1), yhat_mat_m1_ind[i,], col=col_i,pch=16,cex=textsize/2)
}
dev.off()


boxplot(t(yhat_mat_ind))

print(round(apply(lpmat,2, function(x)tapply(expit(x), df_test$id, mean)),2))
print(round(apply(lpmat_m1,2, function(x)tapply(expit(x), df_test$id, mean)),2))



print(diag(round(apply(lpmat,2, function(x)tapply(expit(x), df_test$id, mean)),2)))
print(diag(round(apply(lpmat_m1,2, function(x)tapply(expit(x), df_test$id, mean)),2)))
