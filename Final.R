##in session we build the path
##
##reading the csv file and save it in data variable
##


data<-read.csv("cities_r2.csv", sep = ',', h = TRUE)
library(ggplot2)
library(plotrix)

##
##Pie Plot for total male,female,children population in india
##we took means and plotted the chart
##


slices <- c(mean(data$population_female), mean(data$population_male), mean(data$population_total))
lbls <- c("Male", "Female", "Child")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Indian Population", explode = 0.1, radius = 2.0, cex= 0.6)

##
##
##Male literates Vs state_names
##
##MaxAndMaleMaleLiteratesPresentInINDIA(maxstate,minstate)
##

data1<-data
x<-data.frame(data$state_name,data$literates_male)
lit1<-aggregate(x$data.literates_male,by = list(liter = x$data.state_name),FUN = sum)
lit1
ggplot(data1, aes(x=state_name, y=literates_male,fill=data1$state_name), alpha=0.5) +
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle=50, vjust = 1, hjust=1))+
  theme(axis.title.y=element_text(angle=90))+ggtitle("MaleLiteratesAccoringToStates")+
  labs(x = "StateNames", y = "MaleLiterates")
  

##
##
##Female Literates Vs States(ploting female literates to find highest and lowest)
##
##

y<-data.frame(data$state_name,data$literates_female)
lit2<-aggregate(y$data.literates_female,by = list(liter = y$data.state_name),FUN = sum)
lit2
ggplot(data1, aes(x=state_name, y=literates_female,fill=data1$state_name), alpha=0.5) +
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle=50, vjust = 1, hjust=1))+
  theme(axis.title.y=element_text(angle=90))+ggtitle("FemaleLiteratesAccoringToStates")+
  labs(x = "StateNames", y = "FemaleLiterates")


##
##
##plotted a graph for avg male and female literates, graduates in INDIA
##
##



d <- data.frame(row.names=c("1-2","2-3"), literates = c(mean(data$literates_male), mean(data$literates_female)), 
                graduates = c(mean(data$male_graduates), mean(data$female_graduates)))
d <- do.call(rbind, d)
barplot(d, beside = TRUE, ylim=c(0, mean(data$literates_total)), legend.text = rownames(d), args.legend = list(x = "topleft", bty="n"), main = "Avg Male and Female Literates,Graduates in India", xlab = "Male                                                                   Female

                    ", ylab = "mean of total literates", col = c("violetred", "deepskyblue"))


##
##
##TotalPopulationToFindMaxAndMinPopulatedStates
##
##



library(ggplot2)
x_d<-data.frame(state_name = factor(data$state_name), literates_total = data$literates_total, population_total = data$population_total)
y_d = aggregate(x_d$population_total, by=list(Category=x_d$state_name), FUN=sum)
y_d
c = max(y_d$x)
d = min(y_d$x)

col1 = c(ifelse(y_d$x == c, "max",
                ifelse(y_d$x == d, "min", "median"
                )))
col1

qplot(y_d$x, y_d$Category, col = col1, main = "TotalPopulation", xlab = "Population", ylab = "States")




##
##
##Using Polar Coordinates Displayed the total Literacies
##
##



data1<-data
library(ggplot2)
states<-data1$state_name
states
Total_Literates<-data1$literates_total
p<-ggplot(data1, aes(x=states, y=Total_Literates,fill = data1$state_name)) +
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle=50, vjust = 1, hjust=1))+
  theme(axis.title.y=element_text(angle=90)) + ggtitle("Total Literates according to the states")
p+coord_polar()



##
##
##Grouping The Data
##
##

pop_male <-data.frame(data$state_name,data$population_male)
pop_male
pop_tot <-data.frame(data$state_name,data$population_total)
pm_agg <- aggregate(pop_male$data.population_male,by=list(states=data$state_name),FUN=sum)
pt_agg <- aggregate(pop_tot$data.population_total, by=list(states=pop_tot$data.state_name), FUN=sum)
pt_agg
pt_agg$population_male1 <- pm_agg$x
pt_agg


pop_female <-data.frame(data$state_name,data$population_female)
pop_female

pf_agg <- aggregate(pop_female$data.population_female,by=list(states=data$state_name),FUN=sum)
pf_agg
#pt_agg1 <- aggregate(pop_tot$data.population_total, by=list(states=pop_tot$data.state_name), FUN=sum)
#pt_agg1
pt_agg$population_female1 <- pf_agg$x
pt_agg
dm <- data.frame(pt_agg)
dm
df <- data.frame(pt_agg)
df
library(ggplot2)
df$id <- 1:29 
df

##
##Male Population According To States
##

qplot(id, population_male1, data = df, color = states, horiz = TRUE, 
      size = population_male1, las = 2,vjust = 0.5,main = "MalePopulationInEachState", xlab = "StateId", ylab = "MalePopulation")
##
##FemalePopulationaccordingToStates
##
qplot(id, population_female1, data = df, color = states, horiz = TRUE, 
      size = population_female1, las = 2,vjust = 0.5, main = "FemalePopulationInEachState", xlab = "StateId", ylab = "FemalePopulation")

##
##
##Regression using plot
##
##



x<-data$population_total
y<-data$total_graduates

r <- lm(y~x)
plot(x, y, abline(lm(y~x)), ylim = rev(range(y)), main = "Regression B/W graduates and Population", xlab = "Graduates", ylab = "Population", col = data$state_name, lwd = 5, cex = 0.5)
?plot
lines(x, y, pch=18, col="blue", type="b", lty=2)

legend(1, 1, legend=data$state_name,
       col=data$state_name, cex=0.8)

##ggplot(data, aes(x,y,fill=state_name))+stat_summary(fun.data=mean_cl_normal) + 
##geom_smooth(method='lm',formula=y~x)
