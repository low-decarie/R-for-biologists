#This script demonstrates issues with testing normality on a whole dataset
#Saddly, such testing on whole datasets prior to modeling is often advocated.

#Create normally distributed data for two levels of a factor
example <- data.frame(factor=c(rep("A",200),rep("B",200)),
                      response=c(rnorm(n = 200,mean = 1,sd=1),
                                 rnorm(n = 200,mean = 5,sd=1)))

#Test for normality of the distribution of the response across the dataset
qqnorm(example$response)
qqline(example$response)
shapiro.test(example$response)
ks.test(example$response,pnorm)

#Repeat the test for a subset including only responses for a single level
qqnorm(example$response[example$factor=="A"])
qqline(example$response[example$factor=="A"])
shapiro.test(example$response[example$factor=="A"])
ks.test(example$response[example$factor=="A"],pnorm)
