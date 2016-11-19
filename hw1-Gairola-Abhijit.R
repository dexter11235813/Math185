
load("earthquakes-2014.rda")

# Problem 1

head(dat)
sub = subset(dat,magnitude >= 2) # subset with magnitude >=2
sub_tab = table(sub$month)  #tabulating the subsets by month
print(sub_tab)
barplot(sub_tab) #frequency barplot
chisq.test(sub_tab)

# we can perform a chi-square test for independence by testing the sub_tab 
# dataset against uniform distribution . The null hypothesis for the chi-sq
# test is that the frequency of earthquake is uniformly distributed,i.e, X = np

# The p value from the chisq.test is lower than the significance level(0.05). Therefore,
# we reject the null hypothesis. 


#We can calibrate the function by assigning prior probabilities  in the chisq.test() function. 
# We can also choose to apply continuity correction and generate thep values using the Monte Carlo simulation 
#using by specifying parameters as function arguments to the  chisq.test() function.

#################################

# Problem 2

#a.)
###
# We can test if gender played a role in admissions at UC Berkley by comparing the actual admission numbers against
# uniform distribution and performing a chi squared test. The null hypothesis is that the acutal admission numbers are 
# uniformly distributed, against the alternative hypothesis that the admission numbers are not uniformly distributed.

#b.)
###
Male = c(3738,8442-3738)
Female = c(1494,4321 - 1494)
table.Berk = data.frame(Male,Female)
rownames(table.Berk) = c("Admitted","Rejected")
print(table.Berk)
prop.table.Berk = prop.table(as.matrix(table.Berk),margin = 2)
print(prop.table.Berk)
barplot(prop.table.Berk,col = heat.colors(length(rownames(table.Berk))))
p = chisq.test(table.Berk)
print(p)

# The low p-value indicates that we can reject the null hypothesis and conclude that the admission numbers are not uniformly
# distributed 
####################################

#Problem 3 

library(MASS)
data("UCBAdmissions")
p.val = c()

#a.)
#####
# printing the 2x2 table for each department
UCBAdmissions[,,1:6]

# grouping all the departments together 
combined.depts =UCBAdmissions[,,1]
for(i in 2:6)
{
  combined.depts = combined.depts + UCBAdmissions[,,i]
}
# the combined department matrix 
print(combined.depts)
# calculating p value for the combined departmental datasets  
print(chisq.test(combined.depts))

# since the p-value is low, we reject the null hypothesis that the admission numbers are uniformly distributed

#b.)
###
# plotting data for all the 6 departments groupped together
prop.combined.depts = as.matrix(prop.table(combined.depts,margin = 2))
barplot(prop.combined.depts,beside = TRUE,col = c("red","yellow","blue","green"),xlab = "Department",ylab = "proportion")
legend("topright",inset = c(0.50,-0.2),legend=c("Male Admitted","Male Rejected","Female Admitted","Female Rejected"),fill = c("red","yellow","Blue","Green"))
# we can see that the graph for combined admission data is not uniformly distributed, which is the same result we get form
# the p-value for chisq.test on the grouped data in part 1.

# plotting the data for all 6 dept using barplot
Berk.barplot = data.frame()
Berk.table = data.frame()
for (i in 1:6)
{
# converting the 2x2 matrix for each dept into a 1x4 vector, and storing it in a matrix
table.B = as.matrix(UCBAdmissions[,,i])
Berk.table = rbind(Berk.table,as.vector(table.B))
#converting the matrix into a proportion table along the columns
prop.table.B = prop.table(as.matrix(UCBAdmissions[,,i]),margin = 2)
Berk.barplot = rbind(Berk.barplot,as.vector(prop.table.B))
}
#displaying the Berk.table dataset
colnames(Berk.table) =c("Male Admitted","Male Rejected","Female Admitted","Female Rejected")
rownames(Berk.table) = c("A","B","C","D","E","F")
print(Berk.table)

# displaying the proportion matrix
colnames(Berk.barplot) =c("Male Admitted","Male Rejected","Female Admitted","Female Rejected")
rownames(Berk.barplot) = c("A","B","C","D","E","F")
print(Berk.barplot)
par(mar = c(5.1,4.1,4.1,2.1),xpd = TRUE)
#renaming the row and column names of the Berk.barplot dataframe

Berk.barplot = t(as.matrix(Berk.barplot))
barplot(Berk.barplot,width = 2,beside = TRUE,col = c("red","yellow","Blue","Green"),xlab = "Department",ylab = "proportion")
legend("topright",inset = c(0.50,-0.2),legend=c("Male Admitted","Male Rejected","Female Admitted","Female Rejected"),fill = c("red","yellow","Blue","Green"))


#c.)
# instead of pooling all the data together, we can run the chisq.test function on each matrix(department), and
# check for the p-values for each matrix to see if the admission numbers are uniform distributed.
vec = apply(UCBAdmissions,3,FUN = chisq.test)
pval = do.call("c",lapply(vec,FUN = function(i) i$p.value))
print(pval)
# from the output of pval, we see the p value for the chisq.test() for the admission matrix of each dept. We see that
# while we reject the null hypothesis for department A, we cannot reject the null hypothesis for the other departments.
# This is congruent with the result we get from the plot for separate departments in part B . 