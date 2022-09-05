setwd("/Users/milanlabus/Documents")

data <- read.csv("exam_2022.csv", header=TRUE, ",")
attach(data)
data

str(data)
library(psych)
Descriptive <- describeBy(age,smoke,ptl,bwt,mat=TRUE)
Descriptive

#for the entire sample
n<-length(bwt)
n
m<-mean(bwt)
m
std_error <- 500/sqrt(n)
std_error


#95% confidence CI of the sameple (Q2)
margin_error <-qnorm(0.975)*500/sqrt(n)
left <- m-margin_error
right <- m+margin_error
left 
right

#group of non smokers we will call A Q3
A <- subset(data, smoke == 0 )
Adata<-A$bwt
meanA<-mean(Adata)
sdA<-sd(Adata)
sdA
meanA

#group of smokers we will call B Q3
B <- subset(data, smoke == 1 )
Bdata<-B$bwt
meanB<-mean(Bdata)
sdB<-sd(Bdata)
sdB
meanB

#plotting the histogram
hist(bwt, density = 50, breaks = 40,prob=TRUE,main="Histogram of body weight",xlab="Body Weight (g)")
curve(dnorm(x,mean=m,sd=500), col="darkblue",lwd=2,add=TRUE, yaxt="n")

#histogram (check one for normality) Q3
par(mfrow=c(1,2))
hist(Adata, density = 50, breaks = 20,prob=TRUE,main="Histogram of body weight for non smoking",xlab="Body Weight (g)")
curve(dnorm(x,mean=meanA,sd=sdA), col="darkblue",lwd=2,add=TRUE, yaxt="n")

hist(Bdata, density = 50, breaks = 20,prob=TRUE,main="Histogram of body weight for smoking",xlab="Body Weight (g)")
curve(dnorm(x,mean=meanB,sd=sdB), col="darkblue",lwd=2,add=TRUE, yaxt="n")

#qqplot(check for normality 2)
par(mfrow=x(1,2))
qqnorm(Adata)
qqline(Adata)
qqnorm(Bdata)
qqline(Bdata)

#hypothesis test (third check for normality)
with(data,shapiro.test(bwt[smoke==0]))
with(data,shapiro.test(bwt[smoke==1]))


#Assesing Variability Q3
par(mfrow=c(1,1))
boxplot(bwt~smoke,data=data,main="Box plot of body weights")
equalvariance <- var.test(bwt~smoke,data=data)
equalvariance

#Q3 performing two sample ttest 
t.test(bwt~smoke,data=data,var.equal=TRUE)

#Question 4
#group of non hypertenion we will call C
C <- subset(data, ht == 0 )
Cdata<-A$bwt
meanC<-mean(Adata)
sdC<-sd(Cdata)
sdC
meanC

#group of hypertension we will call D
D <- subset(data, ht == 1 )
Ddata<-B$bwt
meanD<-mean(Ddata)
sdD<-sd(Ddata)
sdD
meanD


#first test for normality histogram
par(mfrow=c(1,2))
hist(Cdata, density = 50, breaks = 20,prob=TRUE,main="Histogram of body weight for non hypertension",xlab="Body Weight (g)")
curve(dnorm(x,mean=meanC,sd=sdC), col="darkblue",lwd=2,add=TRUE, yaxt="n")

hist(Ddata, density = 50, breaks = 20,prob=TRUE,main="Histogram of body weight for hypertension",xlab="Body Weight (g)")
curve(dnorm(x,mean=meanD,sd=sdD), col="darkblue",lwd=2,add=TRUE, yaxt="n")

#Second check for normality qqplot
par(mfrow=x(1,2))
qqnorm(Cdata)
qqline(Cdata)
qqnorm(Ddata)
qqline(Ddata)

#test 3 for normality 
with(data,shapiro.test(bwt[ht==0]))
with(data,shapiro.test(bwt[ht==1]))


#check for equality of variance
par(mfrow=c(1,1))
boxplot(bwt~ht,data=data,main="Box plot of body weights")
equalvariance <- var.test(bwt~ht,data=data)
equalvariance

#Q4 performing two sample ttest 
t.test(bwt~ht,data=data,var.equal=TRUE)






#find z
mu<-3000
z=(m-mu)/std_error
z



