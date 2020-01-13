rm(list=ls())
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


data_path <- "./data"
if(!dir.exists(data_path)) dir.create(data_path)
acc_dat.dir <- file.path(data_path, "IU_walking_driving_climbing")
acc_dat.zip <- file.path(data_path, "IU_walking_driving_climbing.zip")
acc_dat.zip.url <- "https://www.dropbox.com/s/pf5l2ki9t2ae2df/IU_walking_driving_climbing.zip?dl=1"


## If files not downloaded yet, download (182.6 MB)
if (!dir.exists(acc_dat.dir) & !file.exists(acc_dat.zip)){
    ## Download .zip file
    download.file(acc_dat.zip.url, acc_dat.zip)
    ## Unzip into data/ directory
    unzip(acc_dat.zip, exdir = data_path)
    ## Remove .zip file
    file.remove(acc_dat.zip)
}



fpath.demog <- file.path(acc_dat.dir, "participant_demog.csv")
df.demog    <-  as.data.frame(fread(fpath.demog))

accel_files <- list.files(file.path(acc_dat.dir, "raw_accelerometry_data"))

df_ls <- lapply(accel_files, function(x) data.frame(fread(file.path(acc_dat.dir, "raw_accelerometry_data",x))))
xdf   <- bind_rows(df_ls, .id="id")  
rm(list=c("df_ls","accel_files","fpath.demog","acc_dat.zip","acc_dat.zip.url"))


map_vec <- rep(NA, 99)
map_vec[c(1,2,3,4,77,99)] <- c("walking", "descending_stairs",  "ascending_stairs",
                               "driving", "clapping", "non_study_activity")
xdf$activity_lab <- map_vec[xdf$activity]
table(xdf$activity_lab)

xdf <- 
    xdf %>% 
    mutate("VM_lw" = sqrt(lw_x^2 + lw_y^2 + lw_z^2),
           "id" = as.numeric(id))


tlen <- 1

xdf_w <- 
    xdf %>%
    filter(activity_lab=="walking") %>% 
    dplyr::select(id, time_s,VM_lw) %>% 
    dplyr::mutate(time_g = time_s - min(time_s), 
                  time_g = floor(time_g/tlen)) %>% 
    group_by(id) %>% 
    mutate(time_g = time_g-min(time_g),
           j = 1:n()) %>% 
    # ungroup() %>% 
    group_by(id, time_g) %>% 
    dplyr::mutate(n_g = n(),
                  time_s_g=1:n()) %>% 
    ungroup() %>% 
    filter(n_g == tlen*100)




uid <- unique(xdf_w$id)
nid <- length(uid)


nk <- (tlen*100)*(tlen*100-1)/2
ji <- vapply(uid, function(x) length(unique(xdf_w$time_g[xdf_w$id==x])), numeric(1))
N <- sum(ji)
smat_w <- umat_w <- dmat <- matrix(NA, ncol=nk, nrow=N)
id_vec <- time_g_vec <- rep(NA, N)

inx_samp <- lapply(1:(100*tlen-1), function(x){
    ret <- data.frame("u"=x,"s"=(x+1):(100*tlen))
    ret
})
inx_samp <- bind_rows(inx_samp)
inx <- 1
for(i in seq_along(uid)){
    df_i <- filter(xdf_w, id == i)
    j_i    <- unique(df_i$time_g)
    for(j in seq_along(j_i)){
        Y_ij <- df_i$VM_lw[df_i$time_g == j_i[j]]
        umat_w[inx,] <- Y_ij[inx_samp$u]
        smat_w[inx,] <- Y_ij[inx_samp$s]
        dmat[inx,] <- inx_samp$s-inx_samp$u
        time_g_vec[inx] <- ji[j]
        id_vec[inx] <- df_i$id[1]
        inx <- inx + 1
    }
}


set.seed(122)

df_fit <- data.frame("id" = id_vec, 
                     "umat" = I(umat_w),
                     "smat" = I(smat_w),
                     "dmat" = I(dmat),
                     "lmat" = I(matrix(1/nk, ncol=nk, nrow=N)))
rm(list=c("smat_w","umat_w","dmat","xdf_w","xdf"))
gc()

df.demog$id <- 1:nrow(df.demog)
df_fit <- left_join(df_fit, df.demog, by="id")
df_fit$age_random <- sample(df_fit$age)
df_fit <- df_fit %>% 
    group_by(id) %>% 
    dplyr::mutate(J = 1:n()) %>% 
    ungroup() %>% 
    dplyr::mutate(male = as.numeric(gender=="male"),
                  male_random=sample(male, replace=FALSE))

df_fit$smat_sub <- I(cbind(df_fit$umat[,1], df_fit$smat[,1:99]))
df_fit$lmat_sub <- I(matrix(1/100,ncol=100,nrow=nrow(df_fit)))

write_rds(df_fit, file.path(data_path,"raw_accel_data_deocs.rds"))

rm(list=setdiff(ls(), "df_fit"))
