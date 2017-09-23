library(dplyr)

dat<-read.csv("activity.csv",colClasses=c("numeric","Date","numeric"), na.strings="NA")

daily <- dat[!is.na(dat$steps),] %>%  group_by(date) %>% summarize(total=sum(steps), mean=mean(steps), median=median(steps))
daily<-daily %>% mutate(date=as.character(date))
hist((daily$total), breaks=50, main="histogram of daily total",xlab="steps")


by_interval <- dat[!is.na(dat$steps) ,] %>% group_by(interval) %>% summarize(mean=mean(steps))
plot(by_interval$interval, by_interval$mean, type="l", main="activity in 24 hours", ylab="steps", xlab="time")
five_minutes_max<-max(by_interval$mean)

## solution 1
missing<-sum(is.na(dat$steps))
nonzero_interval <- dat[!is.na(dat$steps) & dat$steps>0,] %>% 
  group_by(interval) %>% 
  summarize( count=n(), total=sum(steps), mean=mean(steps), median=median(steps))

q<-quantile(nonzero_interval$count)
plot(nonzero_interval$interval, nonzero_interval$count, 
     type="s", 
     xlab="time in 24 hour", ylab="frequency", 
     main ="active interval")
abline(h=q[[3]],col="red")
abline(h=q[[4]],col="blue")

## solution 2
activity_pct<-dat %>% group_by(interval) %>% summarise(
      cnt=n(), 
      missing_cnt=sum(is.na(steps)), 
      active_cnt=sum(steps[!is.na(steps)] >0),
      inactive_cnt=sum(steps[!is.na(steps)]==0),
      active_pct=active_cnt/(cnt-missing_cnt),
      nonzero_median=median(steps[steps>0 & !is.na(steps)])
      )

# copy
dat1 <- dat
# number of days
n_days <- length(unique(dat1$date))
# init empty data frame to store simulated date
interval_sims<-data.frame()

# loop every interval
for (i in seq(1,dim(activity_pct)[1]) ) {
  # percentage of active(non-zero steps)
  pct<-as.numeric(activity_pct[i, "active_pct"])
  # interval value, later used to match rows in dat1
  int_val<-as.numeric(activity_pct[i, "interval"])
  # no active for all sampled days for this particular interval
  if ( pct == 0 ) {
    # all zero steps
    interval_sims <- rbind(interval_sims,rep(0, n_days))
  # there's active at least once during sampled days
  }else{
    # median value of non-zero days for this interval
    v<-as.numeric(activity_pct[i, "nonzero_median"])
    # sample for n_days time, with given chance(active percentage), either 0 or non-zero median value
    interval_sims <- rbind( interval_sims, as.integer(sample(x=c(v, 0), size= n_days, replace = T, prob=c(pct, 1-pct) )))
  }
}

dat1[is.na(dat1$steps) & dat1$interval == int_val, "steps"] <- fillin



# solution 3, pick any value at same interval
dat1 <- dat
dat1 <- dat1 %>% 
  group_by(interval) %>% 
  mutate( sim_steps =
            ifelse( 
              #copy if is not na,
              ! is.na(steps), steps,  
              # pick one value from dat at same interval
              sample( dat[dat$interval==interval & !is.na(dat$steps), "steps" ], 1)
                  )
        )
# verify 
dat_diff<-dat1[is.na(dat1$steps),]
plot(group_by(dat_diff, interval) %>% summarise(sum(sim_steps)))