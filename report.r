library(reshape2)
library(ggplot2)
library(data.table)


w241 <- read.csv(file='./results.csv',sep=',',header=T)
#w241.models <- read.csv(file='./Models.csv',sep=',',header=T)
str(w241)
w241 <- melt(w241,id.vars="Models")
names(w241)=c("Models","Treatment", "beta" )

w241 <- as.data.table(w241)
w241 <- w241[, std.error:= c(0.157, 0.157, 0.088,0.082, 0.155, 0.155, 0.088, 0.082)]
#w241 <- w241[, Model.Description:= c( "Model 1 without covariates" ,"Model 2 with covariates but no funding", "Model 3 with covariates and funding", "Model 4 with covariates and fixed effect for participants", "Model 1 - Full Model without covariates" ,"Model 2 - Full Model with covariates but no funding", "Model 3 - Full Model with covariates and funding", "Model 4 - Full Model with covariates and fixed effect for participants" )]



avg.ratings.plot <- ggplot(w241, aes(x=Models,y=beta,fill=Treatment))+ geom_bar(stat="identity",position="dodge")   + labs(x="Regression Models", y="Beta parameter", title="")
avg.ratings.plot <-avg.ratings.plot+theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
avg.ratings.plot <-avg.ratings.plot+ geom_errorbar(aes(ymin=beta-1.96*std.error, ymax=beta+1.96*std.error),
                                                   width=.2,                    # Width of the error bars
                                                   position=position_dodge(.9))
#avg.ratings.plot <- avg.ratings.plot + ggtitle("Treatment Effects") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29))
avg.ratings.plot <- avg.ratings.plot + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
avg.ratings.plot <- avg.ratings.plot + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
avg.ratings.plot <- avg.ratings.plot+ theme(legend.text=element_text(size=12)) + theme(legend.title=element_text(size=15))
avg.ratings.plot


avg.ratings.plot = avg.ratings.plot +scale_fill_manual(
  "CI horizontal line", values=rep(1,4),
  guide=guide_legend(override.aes = list(colour=c("orange", "darkred")))
)

avg.ratings.plot 

avg.ratings.plot = avg.ratings.plot + scale_fill_manual(
  name   = 'Margin',
  breaks = c('upper', 'lower'), # <<< corresponds to fill aesthetic labels
  values = c('#b0dd8d', '#fdba9a'),
  labels = c('Over', 'Under'))

avg.ratings.plot 

avg.ratings.plot2 <- avg.ratings.plot + geom_point(data = w241.models$Description, aes(size="0", shape = NA), colour = "grey50")
avg.ratings.plot2


ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

tg <- ToothGrowth
head(tg)
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
tgc <- summarySE(tg, measurevar="len", groupvars=c("supp","dose"))
tgc




# Standard error of the mean
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point()


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)