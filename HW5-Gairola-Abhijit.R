setwd("C:/Users/abhijit331/Dropbox/math185-Spring16")
x = runif(10000,min = -1, max = 1)
y = x*x
B = 999
p.val.pearson = c()
p.val.kendall = c()
p.val.spearman = c()
r.data.pearson = cor(x,y)
r.data.kendall = cor(x,y,method = "kendall")
r.data.spearman = cor(x,y,method = "spearman")
for(i in 1:100)
{
  r.perm.pearson = c()
  r.perm.kendall = c()
  r.perm.spearman = c()
  for(j in 1:B)
  {
    y = sample(y)
    r.perm.pearson[j] = cor(x,y)
    r.perm.kendall[j] = cor(x,y,method = "kendall")
    r.perm.spearman[j] = cor(x,y,method= "spearman")
  }
  p.val.pearson[i] = (length(which(r.perm.pearson >= r.data.pearson)) +1)/(B+1)
  p.val.kendall[i] = (length(which(r.perm.kendall >= r.data.kendall)) +1)/(B+1)
  p.val.spearman[i] = (length(which(r.perm.spearman >= r.data.spearman)) +1)/(B+1)
  }
boxplot(p.val.pearson,p.val.kendall,p.val.spearman,names = c("Pearson","Kendall","Spearman"))

####################################################################################################
# Problem 2
library(Hmisc)
hoeff.test = function(z,B = 999)
{
  p.val.hoeff = c()
  x = z[,1]
  y = z[,2]
  hoeff.original = hoeffd(x,y)$D[1,2]
  for(j in 1:100)
  {
  
  hoeff.perm = c()
  for(i in 1:B)
  {
    y = sample(y)
    hoeff.perm[i] = hoeffd(x,y)$D[1,2]
  }
  p.val.hoeff[j] = (length(which(hoeff.perm >= hoeff.original))+ 1)/(B+1)
  }
  print(p.val.hoeff)
  return(length(which(p.val.hoeff <= 0.05))/length(p.val.hoeff))
}
hoeff.test(data.frame(x=x,y=y))

# To calculate the power of the test, we assume that the null hypothesis is to be rejected everytime, 
# find the fraction of time the test rejects the null, given that the hypothesis is false. It is the probability 
# of committing a type II error . 
####################################################################################################
# Problem 3 

sea.ice = read.table("http://www.amstat.org/publications/jse/v21n1/witt/sea_ice_data.txt",header = T,fill = T)
sea.ice =sea.ice[,1:2]

#part a

affine.fit = lm(September ~ Year, data = sea.ice)
plot(sea.ice,pch = 16)
abline(affine.fit,col = "blue",lwd = 2)

#part b
r.sq = c()
for( i in 1:10)
{
  fit = lm(September ~ poly(Year,degree = i,raw = T),data = sea.ice)
  r.sq[i] = summary(fit)$r.squared
}
plot(1:10,r.sq,xlab = "Degree",ylab = "R-Squared values")
lines(1:10,r.sq,type = 'l',lwd = 4,col = "red")

# From the graph, we can see that for degree > 4, there is no appreciable improvement in the r-squared values of the fit

fit.04 = lm(September ~ poly(Year,degree = 4,raw = T),data =sea.ice)
pts = seq(1980,2010,len = 30)
val = predict(fit.04,data.frame(Year = pts))
plot(sea.ice,pch = 16)
lines(pts,val,col = "blue",lwd = 2)
abline(affine.fit,col = "red",lwd = 2)

#part c
iso.reg = isoreg(sea.ice)
plot(iso.reg)
# From the plot, we see that there is only one knot, right at the end.

#part d
plot(sea.ice,pch = 16)
plot(iso.reg,pch =16,xlab = "",ylab = "",main = "") # monotone regression
abline(affine.fit,col = "green",lwd = 2) # straight line
lines(pts,predict(lm(September ~ poly(Year,2,raw = T),data = sea.ice),data.frame(Year = pts)),col = "cyan")
lines(pts,predict(lm(September ~ poly(Year,3,raw = T),data = sea.ice),data.frame(Year = pts)),col = "brown")
lines(pts,val,col = "blue",lwd = 2) # polynomial of degree 4
title(xlab = "year",ylab = "Arctic Sea ice extent(1,000,000 sq km)",main = "Average September ice extent 1979-2012 ")
legend("topright",c("Linear fit","Iso Fit","Poynomial Fit(degree 2)","Polynomial Fit(degree 3)","Polynomial Fit(degree 4)"),fill = c("green","red","cyan","brown","blue"))

# From the plot, we see that the polynomial of degree 4 fits the model most accurately, with models of degree 2 and 3 almost overlapping.
# The monotone regression line has only one knot, and appears to be horizontal.

###################################################################################################
# Q4

boot.regression = function(x,y,conf = 0.95,residual = FALSE, B =999)
{
  fit = lm(y ~ x)
  residuals  = fit$residuals
  y.hat = fit$fitted.values
  beta0.hat = fit$coefficients[1]
  beta1.hat = fit$coefficients[2]
  beta0.boot = c()
  beta1.boot  =c()
  if(residual)
  {
  for(i in 1:B)
  {
    y.boot =  y.hat + sample(residuals,replace= T)
    fit = lm(y.boot ~ x)
    beta0.boot[i] = fit$coefficients[1]
    beta1.boot[i] = fit$coefficients[2]
    }
  }
  else
  {
    for(i in 1:B)
    {
      index = sample(1:length(x),replace = T)
      fit = lm(y[index] ~ x[index])
      beta0.boot[i] = fit$coefficients[1]
      beta1.boot[i] = fit$coefficients[2]
    }
  }
    lim = (1 - conf)/2
    beta0.ci = quantile(beta0.boot,probs = c(lim,1-lim))
    beta1.ci = quantile(beta1.boot,probs= c(lim,1-lim))
    CI = data.frame(beta0 = 2*beta0.hat-beta0.ci,beta1 = 2*beta1.hat - beta1.ci)
    rownames(CI) = c(100*lim,100*(1-lim))
    return(CI)
}

boot.regression(sea.ice[,1],sea.ice[,2],conf = 0.95,residual = T,B = 999)

