library(ggplot2)

#Read and explore data #

data2 <-read.csv("terms.csv")
listfreqs<-(data2$absolute_frequency)  # Select frequencies values
listfreqs<-sort(listfreqs, decreasing=TRUE)  # Arrange values from higher to lower
nterms<-length(listfreqs) # How many terms?
x<-c(1:nterms)
highest<-listfreqs[1]  # Highest frequency value (number of occurrences of most frequent token)



# Absolute frequencies and Zipf's first law

plot(listfreqs, ylim = c(0,highest), xlim=c(0,nterms), xlab="Termos", ylab="Frequência", type="n")
text(listfreqs, as.character(data2$term), cex = 0.8)

totaloccur <-sum(listfreqs)

cat("total number of occurrences is ", totaloccur, ".\n")

absplot<-qplot(x, listfreqs, xlab = "Tipos", ylab= "Frequência absoluta")
absplot

zipfs1<- lm(log(x)~log(listfreqs))   # fit a model, also done by geom_smooth in plot

zipf1<-qplot(log(x), log(listfreqs), xlab = "log(ranking tipos)", ylab= "log(frequência absoluta)")
zipf1+ geom_point()+ geom_smooth(method = "lm")


#Other tests#


# Relative frequency #
total_corpus<-sum(data2$absolute_frequency) # how many tokens?
relfreqs<-listfreqs/total_corpus
qplot(x, relfreqs, xlab = "Ranking Tipos", ylab= "Frequência Relativa")

logrelfreqs<-log(relfreqs)  # logarithm of relative frequency
qplot(log(x), logrelfreqs, xlab = "log(ranking)", ylab= "Log(frequência relativa)")

zipfs2<- lm(log(x)~log(relfreqs))   # fit a model, also done by geom_smooth in plot
zipf2<-qplot(log(x), log(relfreqs), xlab = "log(ranking tipos)", ylab= "log(frequência relativa)")
zipf2+ geom_point()+ geom_smooth(method = "lm")

#Kornai. x axis relative too #

rel_rank<-x/nterms
all_rell<-qplot(log(rel_rank), logrelfreqs, xlab = "log(ranking)", ylab= "Log(frequência relativa)")
all_rell + geom_point()+ geom_smooth(method = "lm")

all_rell<-qplot(rel_rank, logrelfreqs, xlab = "log(ranking)", ylab= "Log(frequência relativa)")
all_rell + geom_point()+ geom_smooth(method = "lm")

  
# Second Law

rankfreqs <-table(data2$absolute_frequency, dnn = c("frequência"))

rankfreqs <-as.data.frame(rankfreqs)
plot(rankfreqs,   xlab = " Valor da frequência", ylab= "Número de tipos")

# x= frequency of value from lowest to highest and y= frequency of frequency
valfreq<-as.vector(rankfreqs$frequência)
valfreq<-as.numeric(valfreq)
numtypes<-as.numeric(rankfreqs$Freq)
qplot(valfreq, numtypes, data=rankfreqs,  xlab = " Valor da frequência", ylab= "Número de tipos com esta frequência")


log_valfreq<-log(valfreq)
log_numtypes<-log(numtypes)
qplot(log_valfreq, log_numtypes, xlab = "log(valor da frequência)", ylab= "log(número de tipos com esta frequência)")

# Models for log transform
x1<-log(x)
y1<-log(listfreqs)
#Print two graphs with linear model
par(mfrow=c(1,2))
qplot(x, relfreqs, xlab = "Ranking Tipos", ylab= "Frequência Relativa")
zipf1<-qplot(log(x), log(listfreqs), xlab = "log(ranking tipos)", ylab= "log(frequência absoluta)")
zipf1+ geom_point()+ geom_smooth(method = "lm")


par(mfrow=c(1,1))

#Linear model in detail

modline1<- lm(y1~x1)

plot(x1, y1, xlim=c(min(x1), max(x1)), ylim=c(min(y1), max(y1)+3))
abline(modline1, lwd=2)

# calculate residuals and predicted values
# http://www.r-bloggers.com/how-to-plot-points-regression-line-and-residuals/
# To explore: http://www.r-bloggers.com/visualising-theoretical-distributions-of-glms/

res <- signif(residuals(modline1), 5)
pre <- predict(modline1) # plot distances between points and the regression line
segments(x1, y1, x1, pre, col="red")

# add labels (res values) to points
library(calibrate)
textxy(x1, y1, res, cx=0.7)

# model 2
# http://www.magesblog.com/2015/08/generalised-linear-models-in-r.html
lin.mod <- glm(y1 ~ x1, 
               family=gaussian(link="identity"))
library(arm) # for 'display' function only
display(lin.mod)

