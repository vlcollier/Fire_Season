library(reshape2)
library(ggplot2)
library(plyr)
library(reshape2)
library(MASS)
library(scales)
library(reshape2)
library(dplyr)
library(FSA)
library(tidyr)

data <- read.table("H:/Shared drives/Vanessa_Jen/Fire_Season/akff_download_MOA.csv", header=TRUE, sep=",", na.strings="NA", stringsAsFactors=FALSE)

# simplify df
data2<-data[ which(data$Type  =='BUI'), ]
data3<- subset(data2, select = -c (Month,Type, Fire.Season.Phases))
str(data3)

## convert to long
library(reshape2)
data3_long<-melt(data3, id.vars=c("Julian.Day"))
str(data3_long)
na.omit(data3_long)
str(data3_long)
data3_long$Year<-9999
data3_long$Year<-as.numeric(substr(data3_long$variable,2,6)) 
str(data3_long)
table(data3_long$Year)

## calculate average
names(data3_long)[names(data3_long) == "value"] <- "BUI"
# simplify df
## calculate average
includenulls2 <- ddply(data3_long, c("Julian.Day"), summarise,
                       n    = sum(!is.na(BUI)),   # sample count 
                       min = min(BUI, na.rm=TRUE), # minimum
                       max = max(BUI, na.rm=TRUE), # minimum
                       Mean = mean(BUI, na.rm=TRUE), # sample mean
                       IQRt = IQR(BUI, na.rm=TRUE), # sample mean 
                       sd   = sd(BUI, na.rm=TRUE), # sample standard deviation 
                       se   = sd / sqrt(n))  # standard error estimate 
print(includenulls2, row.names = FALSE)
includenulls2
includenulls2$LIQR<-includenulls2$Mean -includenulls2$IQRt
includenulls2$UIQR<-includenulls2$Mean +includenulls2$IQRt
head(includenulls2)

### join together data
# list the columns in the database
nam<-names(data2)
namelist <- data.frame(matrix(t(nam)));namelist
colnames(data2)[29] = "Yr2019"
colnames(data2)[30] = "Yr2020"
colnames(data2)[31] = "Yr2021"
colnames(data2)[32] = "Yr2022"
data4 <- data2[c(3,29:32)]; data4 

## bind them together
total <- merge(includenulls2, data4,by="Julian.Day");total



# figure
plot3 <- ggplot(total, aes(x=Julian.Day)) +
  geom_line(aes(y = Yr2019), color="darkred", size=1.5, linetype='solid') +
  geom_line(aes(y = Yr2020), color="blue3", size=1.5, linetype='dotted') +
  geom_line(aes(y = Yr2021), color="magenta", size=1.5, linetype='twodash') +
  geom_line(aes(y = Yr2022), color="cyan", size=1.5, linetype='dashed') +
  geom_ribbon(aes(ymin = min, ymax = max, color = NULL,alpha=0.05,), fill="wheat");plot3

plot3 + scale_x_continuous(breaks = seq(120, 250, by = 10)) + 
  ggtitle("Build-up Index (BUI): Historical values shaded (1994-2022)") +
  xlab("Julian Day") + ylab("BUI") +
  scale_y_continuous("Incidents", sec.axis = sec_axis(~ .* 0.05, name = derive())) +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust=0.5),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(size=14, angle=0, vjust=0.5), 
    axis.text.y = element_text(size=14, angle=0)
  ) 
