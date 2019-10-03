set.seed(16)
data_profile_plt <- 
    data %>% 
    filter(n_good_days %in% c(4,7)) %>%
    distinct(SEQN, n_good_days) %>% 
    group_by(n_good_days) %>%
    sample_n(1, replace=FALSE) %>% 
    inner_join(data, ., by="SEQN") %>% 
    group_by(SEQN) %>% 
    mutate("day_obs" = 1:n()) %>% 
    ungroup()
id1 <- 
    data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[1]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    facet_wrap(~ DoW,ncol=1,strip.position = "right",scales="free") + xlab("Time of Day") + ylab("Activity Count") + 
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"))+ 
    scale_y_continuous(limits=c(0,15500))
id2 <- data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[2]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    facet_wrap(~ DoW,ncol=1,strip.position = "right",scales="free") + xlab("Time of Day") + ylab("Activity Count") + 
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00")) + 
    scale_y_continuous(limits=c(0,15500))

id1_l <- 
    data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[1]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+")),
           "AC"=log(1+AC)) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    geom_smooth(aes(x=time, y=AC,color="firebrick2"), method="gam",formula=y~s(x,bs="tp",k=50),se=FALSE,color="firebrick2")+
    facet_wrap(~ DoW,ncol=1,strip.position = "right",scales="free") + xlab("Time of Day") + ylab("Activity Count") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"))+ 
    scale_y_continuous(limits=c(-1,10),
                       breaks=log(1+c(0,10, 100, 1000, 10000)), 
                       labels=c(0,10,100,1000, 10000))

id2_l <- data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[2]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+")),
           "AC"=log(1+AC)) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    facet_wrap(~ DoW,ncol=1,strip.position = "right",scales="free") + xlab("Time of Day") + ylab("Activity Count") +
    geom_smooth(aes(x=time, y=AC,color="firebrick2"), method="gam",formula=y~s(x,bs="tp",k=50),se=FALSE,color="firebrick2")+
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00")) + 
    scale_y_continuous(limits=c(-1,10),
                       breaks=log(1+c(0,10, 100, 1000, 10000)), 
                       labels=c(0,10,100,1000, 10000))



jpeg(file.path(figure_path, paste0("profile_id1.jpeg")), height=1000, width=650, quality=100)
id1
dev.off()   


jpeg(file.path(figure_path, paste0("profile_id1_id2.jpeg")), height=1000, width=1300, quality=100)
grid.arrange(id1,id2,ncol=2)
dev.off()   


jpeg(file.path(figure_path, paste0("profile_id1_id2_log.jpeg")), height=1000, width=1300, quality=100)
grid.arrange(id1_l,id2_l,ncol=2)
dev.off()   

rm(list=c("data_profile_plt","id1","id2","id1_l","id2_l"))
