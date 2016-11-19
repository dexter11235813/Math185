
# Problem 1 

#A
# For the vector Epsilon of length n which obtains it's value from the set c(-1,1), each on the n positions can be filled in two ways(+1 or -1).
# Thus, the vector Epsilon can be constructed in 2^n unique ways. Therefore, the P value is equal to the number of constructions of Epsilon vector
#  such that Y_epsilon is greater than Y_star divided by the total number of ways the Epsilon vector can be constructed.

#B
flipSignTest1 = function(x,B = 999)
{
  boot.mean = c()
  x.mean = mean(x)                                # Y_star vector
  for(i in 1:B)
  {
  epsilon = sample(c(-1,1),length(x),replace = T) # creating epsilon vector using bootstrap
  boot.sample.epsilon = epsilon*x                 
  boot.mean[i] = mean(boot.sample.epsilon)        # creating Y_epsilon vector
  
  }
  boot.greater =length(which(boot.mean >= x.mean)) # number of values in Y_epsilon vector greater than Y_star 
  p.val = (boot.greater+1)/(B+1)                   # calculating P-value
  return(p.val)
}


#C
flipSignTest2 = function(x,B = 999)
{
  boot.mean = c()
  x.mean = mean(x)
  for(i in 1:B)
  {
    epsilon = sample(c(-1,1),length(x),replace = T)
    boot.sample.epsilon = epsilon*x
    boot.mean[i] = mean(boot.sample.epsilon)
    
  }
  boot.greater =length(which(boot.mean >= abs(x.mean))) # count only the values of the mean of bootstrap values that are  greater than the absolute value of mean of x
  p.val = (boot.greater+1)/(B+1)
  return(p.val)
}

########################################

# Problem 2 
#A
library(UsingR)
colnames(father.son) = c("FatherHeight","SonHeight")
boxplot(father.son)

# From the boxplot, we can see that the median of SonHeight variable is higher than the median of the FatherHeight variable.
#B
z = father.son$SonHeight - father.son$FatherHeight 
flipSignTest1(z)
flipSignTest2(z)

# The null hypothesis for the test is that the mean of difference between the heights of son and father are zero,i.e,
# the values are symmetric around zero. The alternative hypothesis is that the mean of difference between son and father is non zero,i.e,
# the values are not symmetric around zero. Since the result of the FlipSignTest1 is lower than 95% CI(0.001), we reject the null hypothesis.
# Thus, the mean of the difference in heights of sons and fathers is not symmetric around zero.


# Ths is a permutation test, as permutation tests involve comparing the means of two distributions , and the difference in means between the two 
# samples gives us the test static. In this exercise , we are comparing the mean of the difference in the heights of fathers and sons in the dataset with the
# null hypothesis that the mean is identically distributed. Thus, this exercise is a permutation test.

########################################
# Problem 3


data = father.son$SonHeight - father.son$FatherHeight
theta_hat = mean(data)

B =999
samp.mean = numeric(B)
for (b in 1:B)
{
  samp = sample(data,length(data),replace = T)
  samp.mean[b] = mean(samp)
}
# the one sided 1-alpha=90% bootstrap confidence interval is [theta_90%,inf)
L = 2*theta_hat - quantile(samp.mean,0.9)
print(L)

# From the one sided bootstrap pivot confidence interval, we can see that the 
# CI for mean of the difference in the heights between son and father,with 1-alpha = 0.9, is between 
# 0.88 and  inf. Thus , the mean is not centered around zero. This supports the solution of the second question,
# where the null hypothesis was rejected.

#  The population we are drawing inference from is the difference in height between sons and fathers from the father.son
#  dataset in the UsingR package. 







