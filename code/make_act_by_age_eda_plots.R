### This tidyverse code is extremely slow! 
### I've commented it out, but it's here for your reference
# data_plt_wknd_age <-
#     data %>%
#     filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>%
#     mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend")),
#            "Age" = round(Age)) %>%
#     mutate_at(vars(MIN1:MIN1440),.funs=function(x) log(1+x))  %>%
#     group_by(SEQN, Weekend) %>%
#     summarize_at(vars(MIN1:MIN1440),.funs=mean,na.rm=TRUE) %>%
#     group_by(Age_yrs = round(Age), Weekend) %>%
#     summarize_at(vars(MIN1:MIN1440),mean,na.rm=TRUE)  %>%
#     gather(key="time", value="AC", MIN1:MIN1440) %>%
#     mutate("time" = as.numeric(str_extract(time, "[0-9]+")))


## much faster code combining tidyverse and data.table functionality
data_plt_wknd_age <- 
    data %>%
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>%
    mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend"))) %>% 
    select(SEQN, Age, Weekend, one_of(paste0("MIN",1:1440))) %>% 
    mutate_at(vars(one_of(paste0("MIN",1:1440))), .funs=function(x) log(1+x)) %>% 
    data.table()
data_plt_wknd_age <- 
    data_plt_wknd_age[, lapply(.SD, mean, na.rm=TRUE), by=.(SEQN, Age, Weekend), .SDcols=paste0("MIN",1:1440)]
    
## transform to long format so we can use ggplot.
## If you aren't determined to use ggplot, you can use the function fields::image.plot()
## on the matrix format data directly (suggestion: set the option useRaster=TRUE if you use image.plot())
data_plt_wknd_age <- 
    data_plt_wknd_age %>% 
    gather(key="time", value="AC", -SEQN,-Age,-Weekend) %>% 
    mutate(time = as.numeric(str_extract(time, "[0-9]+")))
## this is a bit of a lazy formulation of the model 
fit_wknd_age <- bam(AC ~ te(time, Age, by=Weekend, bs=c("cc","cr"), k=12), data=data_plt_wknd_age, method="fREML",discrete=TRUE)
## obtain estimated response values over the range of observed ages/time of day
tind <- seq(1,1440,len=100)
aind <- seq(6,85,len=100)
df_pred <- data.frame(time=rep(rep(tind, each=length(aind)),2),
                      Age=rep(rep(aind, length(tind)),2), 
                      Weekend=rep(c("Weekend","Weekday"), each=length(aind)*length(tind)))
df_pred <- data.frame(df_pred, AC=predict(fit_wknd_age, newdata=df_pred, type='response'))


jpeg(file.path(figure_path, paste0("PA_profiles_by_age_by_wknd.jpeg")), height=800, width=650, quality=100)
df_pred %>% 
    ggplot(aes(x=time, y=Age)) + geom_raster(aes(fill=AC)) + scale_y_continuous(breaks=seq(10,80,by=10),limits=c(6,85)) + 
    facet_wrap(~Weekend, ncol=1, strip.position = "right") + xlab("Time of Day") + ylab("Age (years)") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"),limits=c(0,1440)) + 
    scale_fill_gradientn(colours=tim.colors(100))+ theme(legend.position="none") 
dev.off()



jpeg(file.path(figure_path, paste0("PA_profiles_by_age_by_wknd_with_lines.jpeg")), height=800, width=650, quality=100)
df_pred %>% 
    ggplot(aes(x=time, y=Age)) + geom_raster(aes(fill=AC)) + scale_y_continuous(breaks=seq(10,80,by=10),limits=c(6,85)) + 
    facet_wrap(~Weekend, ncol=1, strip.position = "right") + xlab("Time of Day") + ylab("Age (years)") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"),limits=c(0,1440)) + 
    scale_fill_gradientn(colours=tim.colors(100))+ theme(legend.position="none") + 
    geom_abline(intercept=c(15,25,65), slope=0,col='grey',lty=2)
dev.off()


rm(list=c("data_plt_wknd_age","df_pred","tind","aind","fit_wknd_age"))


