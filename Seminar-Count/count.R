
library(pipeR)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)

setwd("E:/Project/WISE R Club/seminars")

dat<-read.csv('AGL_001.csv',na.strings = "", header = TRUE, stringsAsFactors = FALSE)

dat1 <- dat %>>%
    mutate(Date_Time=parse_date_time(DateTime,"%Y%m%d %H:%M:%S", truncated = 2L, quiet = TRUE))%>>%
    # truncated : integer, number of formats that can be missing.
    filter(Date_Time > as.POSIXct("2014-09-15 08:00:00"))%>>%
    mutate(Date=as.POSIXct(matrix(unlist(strsplit(DateTime, " ")),ncol=2, byrow=TRUE)[,1]))%>>%
    select(No, EnNo, Date, Date_Time)

	
# modified method
dat4 <- ddply(dat1, .(EnNo, Date), function(df){
    if (nrow(df)==1){
        tms <- 0
    }else{
        df <- arrange(df, No)
        Date_Time <- df$Date_Time
        timeInterval <- difftime(Date_Time[2:length(Date_Time)],Date_Time[1:(length(Date_Time)-1)],units ="mins")
        logi <- timeInterval >= as.difftime(30,  units="mins")
        a <- which(logi==TRUE)
        if (nrow(df)>6){
            tms <- ceiling(sum(logi,na.rm = TRUE)/2)
        }else{
            tms <- ifelse(all(diff(a)>1)|length(a)==1,length(a), length(a)-ceiling(length(which(diff(a)==1))/2))
        }
    }
    return(tms)
})


dat_mod <- summarise(group_by(dat4, EnNo), sum(V1))%>>%
    arrange(EnNo)
names(dat_mod)[2] <- "Times"

# output
write.csv(dat_mod, "SeminarTimes_20141220.csv")
