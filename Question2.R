#Question2

mydata <- read.table("C:\\assignment\\B2.txt",header=TRUE)
show(mydata)

#Data Extraction for HS1 and HS2
x = mydata$HS1[1:15]
y = mydata$HS2[1:15]
meanD = x-y
Tdata =c(x,y)
Tmean = mean(Tdata)
Tmean

#for HS1
meanx = mean(x)
meanx
sdx = sd(x)
sdx

#for HS2
meany = mean(x)
meany
sdy = sd(y)
sdy

#Boostrapping the means
set.seed(1000)
BootStrapSamples = replicate(5000,sample(Tdata, 1, replace = TRUE))
bs_means1 = BootStrapSamples[1:15]
bs_means2 = BootStrapSamples[16:30]

bs_means11 = apply(bs_means1,1,mean)
bs_means22 = apply(bs_means2,1,mean)

sortmean0 = sort(BootStrapSamples)
sortmean0

Tdata = c(bs_means1,bs_means2)
sortmean = sort(Tdata)
sortmean

bs_diffs = bs_means1 - bs_means2
count=length(sortmean[sortmean>63.89])
hist(bs_diffs, col = "blue")
p_value = count/5000
p_value
meanbsrt = mean(BootStrapSamples)

#Lower and Upper mean
lowermean = sortmean0[0.025*5000]
uppermean = sortmean0[0.975*5000]

#Lower and Upper Error
lowerError = meanbsrt-lowermean
upperError = meanbsrt-uppermean

#Lower and Upper Bound
lowerB = upperError-Tmean
upperB = lowerError-Tmean

#95% confidence interval
cat("(",lowerB,",",upperB,")")

#Variance Equality
set.seed(1000)
BootStrapSamplesv= replicate(5000,var(Tdata, 1, replace = TRUE))

sortvariance = sort(BootStrapSamplesv)
sortvariance

Tdatav = var(Tdata)
count1 = length(sortvariance[sortvariance>Tdatav])
p_value = count1/5000 
p_value

#Standard Approach
F_statistic = var(x)/var(y)
F_statistic
pf(F_statistic,14,14,lower.tail = FALSE)*2

