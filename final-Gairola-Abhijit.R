# Problem 1 
#a.)
n  = c(20,50,100)
power.wilcox =matrix(0,nrow = 3,ncol = 5)
power.ks = matrix(0,nrow = 3,ncol = 5)
for(i in 1:3)
{
  mu = seq(0,1,length.out = 5)
 
  for(j in 1:length(mu))
  {
  p.value.wilcox = c()
  p.value.ks = c()
  
  for(k in 1:200)
  {
    X = rnorm(n[i],mean = 0,sd = 1)
    Y = rnorm(n[i],mean = mu[j],sd = 1)
    p.value.wilcox[k] = wilcox.test(X,Y)$p.value
    p.value.ks[k] = ks.test(X,Y)$p.value
  }
  power.wilcox[i,j] = (length(which(p.value.wilcox <=0.05)))/length(p.value.wilcox)
  power.ks[i,j] = (length(which(p.value.ks <=0.05)))/length(p.value.ks)
 
  }
}


power.wilcox = data.frame(power.wilcox)
power.ks = data.frame(power.ks)
colnames(power.wilcox) = mu
colnames(power.ks) = mu
rownames(power.wilcox) = n
rownames(power.ks) = n
print(power.wilcox)
print(power.ks)


plot(x = mu,y = power.wilcox[3,],col = "red",type = "ol",main = "Comparision of Wilcox Rank sum and KS test for different (n,mean) pairs",xlab = "mean",ylab = "Statistical Power")
lines(x = mu, y = power.ks[3,],col = "red",type = "o",lty = 2)
lines(x = mu, y = power.wilcox[2,],col = "blue",type = "ol")
lines(x = mu, y = power.ks[2,],col = "blue",type = "o",lty = 2)
lines(x = mu, y = power.wilcox[1,],col = "black",type = "ol")
lines(x = mu, y = power.ks[1,],col = "black",type = "o",lty = 2)
legend('topleft',col = c("red","blue","black"),lty = c(rep(1,3),rep(2,3)),legend = c("Wilcox, n = 100","Wilcox,n = 50","Wilcox,n = 20","KS,n = 100","KS,n = 50","KS,n =20"))


# From the wilcox and KS tables, we can see that as mean increases, we can see the power of the test shift towards 1.
# From the plot, we see that the statistical power, as calculated by the Wilcox test, is higher than the power calculated
# from the KS test , for the same value of mean, for all sample size (20,50,100). Thus , the performance of the Wilcoxon
# test is better for the location model as compared to the KS model.






#b.) 
n  = c(20,50,100)
power.wilcox.scale=matrix(0,nrow = 3,ncol = 5)
power.ks.scale = matrix(0,nrow = 3,ncol = 5)
for(i in 1:3)
{
  sigma = seq(1,5,length.out = 5)
  
  for(j in 1:length(sigma))
  {
    p.value.wilcox = c()
    p.value.ks = c()
    
    for(k in 1:200)
    {
      X = rnorm(n[i],mean = 0,sd = 1)
      Y = rnorm(n[i],mean = 0,sd = sigma[j])
      p.value.wilcox[k] = wilcox.test(X,Y)$p.value
      p.value.ks[k] = ks.test(X,Y)$p.value
    }
    power.wilcox.scale[i,j] = (length(which(p.value.wilcox <=0.05)))/length(p.value.wilcox)
    power.ks.scale[i,j] = (length(which(p.value.ks <=0.05)))/length(p.value.ks)
  }
}


power.wilcox.scale = data.frame(power.wilcox.scale)
power.ks.scale = data.frame(power.ks.scale)
colnames(power.wilcoxscale) = sigma
colnames(power.ks.scale) = sigma
rownames(power.wilcox.scale) = n
rownames(power.ks.scale) = n
print(power.wilcox.scale)
print(power.ks.scale)



plot(x = sigma,y = power.ks.scale[3,],col = "red",type = "o",main = "Comparision of Wilcox Rank sum and KS test for different (n,mean) pairs",xlab = "sigma",ylab = "Statistical Power",lty = 2)
lines(x = sigma, y = power.wilcox.scale[3,],col = "red",type = "ol")
lines(x = sigma, y = power.ks.scale[2,],col = "blue",type = "o",lty = 2)
lines(x = sigma, y = power.wilcox.scale[2,],col = "blue",type = "ol")
lines(x = sigma, y = power.ks.scale[1,],col = "black",type = "o",lty = 2)
lines(x = sigma, y = power.wilcox.scale[1,],col = "black",type = "ol")
legend('topleft',col = c("red","blue","black"),lty = c(rep(1,3),rep(2,3)),legend = c("Wilcox, n = 100","Wilcox,n = 50","Wilcox,n = 20","KS,n = 100","KS,n = 50","KS,n =20"))

# In the scale model, we see that the statistical power calculated from the KS method increases with increasing sigma. 
# The sample size also has an effect on statistical power, as the power for KS tests on larger samples are higher than 
# smaller samples for a given sigma. We also see that the performance of the Wilcoxon tests are not as good as the Ks
# tests. Thus , for scale model , the KS method performs better as compared to the  Wilcoxon method.

##############################################################################################
## Problem 2 
load("smokers.rda")
boot.combined.test = function(dat,B = 999)
{
  dat.vector = c()
  for(i in 1:dim(dat)[2])
  {
    dat.vector = c(dat.vector,dat[,i])
  }
  dat.vector.mean = mean(dat.vector)
  SST.orig = 0
  SST.boot = rep(0,B)
  for(i in 1:dim(dat)[2])
  {
    SST.orig = SST.orig + dim(dat)[1]*dim(dat)[2]*(mean(dat[,i])-dat.vector.mean)^2
  }

  for(i in 1:B)
  {
    
    dat.vector.boot = sample(dat.vector,replace = T)
    dat.boot = as.data.frame(matrix(dat.vector.boot,nrow = dim(dat)[1],ncol = dim(dat)[2]))
    for(j in 1:dim(dat)[2])
    {
      SST.boot[i] = SST.boot[i] + dim(dat)[1]*dim(dat)[2]*(mean(dat.boot[,j]) - dat.vector.mean)^2
    }
    
  }
  p.value = (length(which(SST.boot >= SST.orig))+1)/(B+1)
  return(p.value)
}
boot.combined.test(smokers,999)

# From the p-valules, we see that the null is rejected at alpha = 95%.The null distribution for the test is that all the 
# groups are from the same distribution.


####################################################################
# Problem 3

# The function multiple.categorical.test takes in x,y and B(the number of permutation runs). The function calculates
# the frequency of each integer in x and y, and calculates the expected number of frequencies under the null that both
# x and y come from the same distribution. It calculates the D statistic for each integer in the frequency vector of x
# and y, and stores it in the vector. Then, the function permutes x and y and recalculates the D statistic vector for 
# each permutation and compares it with the D statistic vector for the input vectors x and y, and calculates the p-value 
# by checking how many times the D statistic of a given integer is greater than the D statistic for the same integer
# in  the original D vector. 

multiple.categorical.test = function(x,y,B = 999)
{
  freq.x = table(x)
  freq.y = table(y)
  expected.x = ((freq.x+ freq.y)/(length(x) + length(y)) ) * length(x)
  expected.y = ((freq.x+ freq.y)/(length(x) + length(y)) ) * length(y)
  D.original.data = (freq.x - expected.x)^2/(expected.x) + (freq.y - expected.y)^2/expected.y
  D.boot = matrix(0,nrow = B,ncol = length(D.original.data))
  p.value = rep(0,length(D.original.data))
  for(i in 1:B)
  {
    
    Z = c(x,y)
    Z = sample(Z)
    x.boot = Z[1:length(x)]
    y.boot = Z[(length(x)+1):length(Z)]
    freq.x.boot = table(x.boot)
    freq.y.boot = table(y.boot)
    expected.x.boot = ((freq.x.boot+ freq.y.boot)/(length(x.boot) + length(y.boot)) ) * length(x.boot)
    expected.y.boot = ((freq.x.boot+ freq.y.boot)/(length(x.boot) + length(y.boot)) ) * length(y.boot)
    D.boot[i,] = (freq.x.boot - expected.x.boot)^2/(expected.x.boot) + (freq.y.boot - expected.y.boot)^2/expected.y.boot
    p.value = p.value + as.numeric(D.boot[i,] >= D.original.data)
  }
  p.value = (p.value + 1)/(B+1)
  reject.index = which(p.value <= 0.05)
  if(length(reject.index) == 0)
  {
    return("No p-value less than 0.05")
  }
  else
  {
  return(reject.index)
  }
}

range = seq(1,20)
X  = sample(range,1000,replace = T)
Y  = sample(range,1000,replace = T)
multiple.categorical.test(X,Y)
# rejected indices are uniformly spread. p-value is 1 for nearly equal frequency for a particular j in both X and Y.


X1 = sample(range,1000,replace = T)
probs = c(rep(1/12,12),rep(1/8,8))
Y1 = sample(range,1000,replace = T,prob = probs)
multiple.categorical.test(X1,Y1)

# rejected indices mostly closer to 20 than 1, since values from 12-20 have a higher probability of getting sampled in Y.

#############################################################3
# Problem 4
#install.packages("splines")
library(splines)

f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)

smooth.spline.CV = function(x,y,k = 10)
{
  data = data.frame(x,y)
  data = data[sample(1:length(x)),]
  folds = cut(seq(1,length(x)),breaks=k,labels=FALSE)
  pred.error = c()
  for(j in 2:20)
  {
    temp = 0
  for(i in 1:k)
  {
    index  = which(folds == i)
    train = data[-index,]
    validation =  data[index,]
    fit = smooth.spline(train$x,train$y,df = j)
    pred.y = predict(fit,validation$x)$y
    temp = temp + 1/100*sum((pred.y - validation$y)^2)
  }
    pred.error = c(pred.error,temp)
  }
  df.opt = which(pred.error == min(pred.error)) + 1
  fit.opt = smooth.spline(x,y,df = df.opt)
  fit.opt$df = df.opt
  plot(x,y,main = "smooth.spline.CV fit")
  # fitting the best-fit line.
  lines(fit.opt,col = "red",lwd = 2)
  return(fit.opt) 
  # since the first entry in pred.error is the prediction sum of squared error for DF = 2, and not DF = 1
}
n = 1e3
x = runif(n)
x = sort(x)
y = f(x) + rnorm(n)
smooth.spline.CVfit = smooth.spline.CV(x,y,k = 10)
print(smooth.spline.CVfit$df)
