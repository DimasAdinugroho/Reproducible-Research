
library (plyr)
library (ggplot2)
files = 'activity.csv'
activity <- read.csv(files,header=TRUE,sep=",", colClasses = c('integer', 'Date', 'integer'))
totalstepperday <- ddply(activity, .(date), summarize, step = sum(steps), mean = mean(steps))
act_miss_rep  <- transform(act, steps = ifelse(is.na(steps), round(mean(steps, na.rm=TRUE)), steps))
.f = function() {
  
  
  totalstepperday [is.na(totalstepperday )] <- 0
  plot1 <- ggplot(totalstepperday, aes(x = date, y = step, colour="black", fill="red")) + geom_bar(stat = "identity")
  
  #activity$interval_fact <- as.factor(activity$interval)
  activity[is.na(activity)] <- 0
  interv <- ddply(activity, .(interval), summarize, step = mean(steps))
  plot2 <- ggplot(data = interv, aes(interval, step)) + geom_line(colour="red")
  plot2
}
