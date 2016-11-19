#Q1
head(ToothGrowth)
SST  = function(dat,Y...)
{
  temp1 = c()
  for(i in 1:2)
  {
    temp1[i] = mean(dat[,i,])
  }
  return(30*sum((temp1 - Y...)^2))
}
# for B = 1:999, we permute data within each block and calculate SST of that table, and compare it with the SST of the original data.
twowayPermTest = function(ToothGrowth, B = 999)
{
  a11 = subset(ToothGrowth,(supp == "VC") & (dose ==0.5))$len
  a12 = subset(ToothGrowth,(supp == "VC") & (dose ==1))$len
  a13 = subset(ToothGrowth,(supp == "VC") & (dose ==2))$len
  a21 = subset(ToothGrowth,(supp == "OJ") & (dose ==0.5))$len
  a22 = subset(ToothGrowth,(supp == "OJ") & (dose ==1))$len
  a23 = subset(ToothGrowth,(supp == "OJ") & (dose ==2))$len
  two.way.table = data.frame(a11,a12,a13,a21,a22,a23)
  v = c()

  for(i in 1:10)
  {
    for(j in 1:6)
    {
      v = c(v,two.way.table[i,j])
    }
  }
  # store the data from ToothGrowth in a 3x2x10  array
  
  two.way.table.3D = array(v,c(3,2,10))
  colnames(two.way.table.3D) = c("VC","OJ")
  rownames(two.way.table.3D) = c(0.5,1,2)
 
  Y... = mean(two.way.table.3D)
  
  # calculating SST on the original given data 
  d = SST(two.way.table.3D, Y...)
  #
  
  
  # Permuting across all treatements within each block
  d.perm = c()
  for(j in 1:B)
  {
  
  for(i in 1:3)
  {
      temp = c(two.way.table.3D[i,1,],two.way.table.3D[i,2,])
      temp = sample(temp)
      two.way.table.3D[i,1,] = temp[1:10]
      two.way.table.3D[i,2,] = temp[11:20]
  }
    d.perm[j] = SST(two.way.table.3D,Y...) 
  }
  p.val = (length(which(d.perm >= d))+1)/(B+1)
  return(p.val)
}

print(twowayPermTest(ToothGrowth,999))
# pvalues range from 0.01 - 0.03


#Q2
load("alon.RData")
alon = as.data.frame(alon)
colnames(alon) = c(seq(1:2000),"y")
p.val.vec = c()
for(i in 1:2000)
{
  p.val.vec[i] = as.numeric(t.test(alon[,i] ~ y,data = alon)$p.value)
}
p.val.vec = as.vector(p.val.vec)

#sorting the vector containing p values
temporary = p.val.vec[order(p.val.vec)]
# number of Hypothesis rejected by using the Bonferroni correction
length(which(p.adjust(temporary,"bon") <= 0.05))
# number of Hypothesis rejected by using the Holm correction
length(which(p.adjust(temporary,"holm") <= 0.05))
# number of Hypothesis rejected by using the Hochberg correction 
length(which(p.adjust(temporary,"hoch")<=0.05))
# number of Hypothesis rejected by using the FDR correction 
length(which(p.adjust(temporary,"BH")<=0.05))

# From the above runs,we see that FDR is the most conservative of all corrections, with 190 rejected hypothesis,whereas
# Bonferroni,Holm and Hochberg all reject 11 out of 2000 hypothesis.


#Q3
B = 50 # low B value used to reduce runtime
t.test.stat = c()
t.test.stat.perm = c()

#vector that holds the p-value for each of the 2000 genes. 
p.value = 0
for(i in 1:2000)
{
  t.test.stat[i] = t.test(alon[,i] ~ y , data = alon)$statistic
}
for(i in 1:B)
{
  # sampling the subjects 'y'
  alon$y = sample(alon$y)
  for(j in 1:2000)
  {
    t.test.stat.perm[j] = t.test(alon[,j] ~ y,data = alon)$statistic
  }
  p.value = p.value + ifelse((abs(t.test.stat.perm) >= abs(t.test.stat)),1,0)/(B+1)
}
print(p.value)