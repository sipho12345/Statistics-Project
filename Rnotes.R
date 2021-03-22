### ##################################### CHAPTER 1.s1 ###########################################
##################################################################################################

pdelays = c(37,42,38,44,39,35,43,41,42,38,36,34,37,42,36,38,41,39,39,37,34,34,41,41,40,38,38,46,38,42)
mean(pdelays)
sd(pdelays)
min(pdelays)
max(pdelays)
median(pdelays)

boxplot(pdelays,main="Box-and-Whisker")
hist(pdelays,col="maroon",breaks = 8)

## Bootstrapping

bstr <- matrix(0,ncol=30,nrow=5000)
for( i in 1:5000)
{samp=sample(pdelays,size=30,replace=TRUE)
bstr[i,]=samp}
bstrm=apply(bstr,1,mean)
sortmean=sort(bstrm)
# 95 % CI 
cat("(",sortmean[0.025*5000],",",sortmean[0.975*5000],")")
####### How many bootstrap means exceed 40? ##########################

count=length(sortmean[sortmean>40])

bst_pval=count/nrow(bstr)
bst_pval
NormalTh_pval=1-pt((abs(mean(pdelays)-38)*sqrt(30)/sd(pdelays)),29)
NormalTh_pval
#############################       CHAPTER 1 S2  ###########################################
#############################################################################################

#BERNOULLI 
xx=vector(length=200000)
set.seed(13)
for(j in 1:200000)
{xx[j]=as.numeric(runif(1)<0.25)}
gpe1=matrix(xx,ncol=10)
gpe1mean=apply(gpe1,1,mean)
gpe2=matrix(xx,ncol=100)
gpe2mean=apply(gpe2,1,mean)
par(mfrow=c(1,2))
hist(gpe1mean,col="blue",main="Distr of Means (n=10, N = 20000)")
hist(gpe2mean,col="blue",main="Distr of Means (n=100, N=2000)")

#########################################################################
#########################################################################

rnd=rnorm(45000,mean=0,sd=3)
n=length(rnd)
gpe9=matrix(rnd,ncol=9,nrow=5000,byrow=T)
gpe9mean=apply(gpe9,1,mean)
hist(gpe9mean,col="blue",main="Distribution of sample mean (n=9, N=5000)")
#gpe9var=apply(gpe9,1,var)
#hist(gpe9var,col="red",main="Distribution of the sample variance")
sdev=apply(gpe9,1,sd)
T=gpe9mean/(sdev/3)
hist(T,col="blue",main="Distribution of t-statistic with 8 degrees of freedom")
hist(sdev,col="blue",main="Distribution of S")
length(T[T>2.5])/length(T)
#######################################################################
##################### Chapter 1 #######################################
#######################################################################
setA=c(11,0,2,0,3,4,1,3,10,2,8,6,4,5,0,0,6,1)
n=length(setA)
bootA=matrix(0,nrow=1000,ncol=18)
N=nrow(bootA)
for(j in 1:1000)
{bsamp=sample(setA,size=18,replace=TRUE)
bootA[j,]=bsamp}
bootAmean=apply(bootA,1,mean)
s_bootAmean=sort(bootAmean)
cat("(",round(s_bootAmean[0.025*N],2),",",round(s_bootAmean[0.975*N],2),")")
cat("Error limit 1 =",round(s_bootAmean[0.025*N],2)-mean(setA)," and Error limit 2 = ",round(s_bootAmean[0.975*N],2)-mean(setA))
cat("(",mean(setA)-(round(s_bootAmean[0.975*N],2)-mean(setA)),",",mean(setA)-(round(s_bootAmean[0.025*N],2)-mean(setA)),")")
cat("(",round(mean(setA)-pt(0.025,n-1)*sd(setA)/sqrt(n),2),",",round(mean(setA)+pt(0.025,n-1)*sd(setA)/sqrt(n),2),")")
#######################################################################
################### Two SAMPLES #######################################
pdelays_c=c(39,35,44,33,19,6,27,24,40,13,35,34,34,33,61,56,43,19,34,40,34,28,29,45,46,28,41,46,25,17)
pdelays_z=c(39,32,22,32,55,34,46,37,31,45,41,40,46,66,36,42,37,43,35,62,48,47,34,42,43,33,47,41,34,41)
summary(pdelays_c)
summary(pdelays_z)
all_c_z=c(pdelays_c,pdelays_z)
bstr1=bstr2=matrix(0,ncol=30,nrow=5000)
for(j in 1:5000)
{samp=sample(all_c_z,size=60,replace=TRUE)
 bstr1[j,] =samp[1:30]
 bstr2[j,]=samp[31:60]
}
bstr1mean=apply(bstr1,1,mean)
bstr2mean=apply(bstr2,1,mean)
meandiff=bstr1mean-bstr2mean
hist(meandiff,col="blue")
sortmeandiff=sort(meandiff)
sortmeandiff[125]
sortmeandiff[4875]
#########################################################################################################
####################################TRAFFIC EXAMPLE######################################################

LOC=list("numeric",length=5)
LOC[[1]]=c(344,382,353,395,207,312,407,421,366,222)
LOC[[2]]=c(365,391,538,471,431,450,299,371,442,343)
LOC[[3]]=c(261,429,402,391,239,295,129,301,317,386)
LOC[[4]]=c(422,408,470,523,398,387,433,440)
LOC[[5]]=c(367,445,480,323,366,325,316,381,407,339)
n=c(length(LOC[[1]]),length(LOC[[2]]),length(LOC[[3]]),length(LOC[[4]]),length(LOC[[5]]))
N=sum(n)
k=length(LOC)
B=5000
RATIOS=Yi.=vector("numeric")
Y..=mean(unlist(LOC))
SSE=SST=0
for(j in 1:length(LOC))
{Yi.[j]=mean(LOC[[j]])
SSE=SSE+sum((LOC[[j]]-Yi.[j])^2)
SST=SST+n[j]*(Yi.[j]-Y..)^2
}
F_or=(SST/(k-1))/(SSE/(N-k))

# Bootstraps #
ALL=unlist(LOC)
for(h in 1:B)
{SSE=SST=0 
 Yi.=vector("numeric")
 LOC=list("numeric",length=5)
 DRAW=sample(ALL,size=48,replace=TRUE)
 LOC[[1]]=DRAW[1:10]
 LOC[[2]]=DRAW[11:20]
 LOC[[3]]=DRAW[21:30]
 LOC[[4]]=DRAW[31:38]
 LOC[[5]]=DRAW[39:48]
 for(j in 1:k)
 {Yi.[j]=mean(LOC[[j]])
 SSE=SSE+sum((LOC[[j]]-Yi.[j])^2)
 SST=SST+n[j]*(Yi.[j]-Y..)^2
 }
 RATIOS[h]=(SST/(k-1))/(SSE/(N-k))
}
pvalue=length(RATIOS[RATIOS>F_or])/B
theory_pvalue=pf(F_or,k-1,N-k,lower.tail = FALSE)
pvalue
theory_pvalue
#########################################################################################################
##################################ANOVA##################################################################
## Exam question 4 solutions#######
st1=c(236,250,252,233,239)
st2=c(238,239,262,247)
st3=c(241,233,212,231,213)
stations=c(st1,st2,st3)
groups=c(rep("a",5),rep("b",4),rep("c",5))
oaov=lm(formula=stations~groups)
o_ratio=as.numeric(summary(oaov)$fstatistic[1])
ratios=numeric()
for(j in 1:1000)
{
 bsamp=sample(stations,size=14,replace=TRUE) 
 fitb=lm(formula=bsamp~groups)
 ratios[j]=as.numeric(summary(fitb)$fstatistic[1])
}
ratios=round(sort(ratios),3)
ratios
p_value=length(ratios[ratios>o_ratio])/length(ratios)
p_value

## Supp question 2 solutions#######
sy1=c(10.2,23.5,54,5.7,10.6)
sy2=c(6.4,33.5,25.7,54.1,10.7)
sy3=c(51,5.6,10.3,14.3,27.5)
systems=c(sy1,sy2,sy3)
groups=c(rep("a",5),rep("b",5),rep("c",5))
oaov=lm(formula=systems~groups)
o_ratio=as.numeric(summary(oaov)$fstatistic[1])
ratios=numeric()
for(j in 1:1000)
{
  bsamp2=sample(systems,size=15,replace=TRUE) 
  fitb2=lm(formula=bsamp2~groups)
  ratios[j]=as.numeric(summary(fitb2)$fstatistic[1])
}
ratios=round(sort(ratios),3)
ratios
p_value=length(ratios[ratios>o_ratio])/length(ratios)
p_value
################################################################################
###############################Question 3 exam sol##############################

pp1=function(mm,nn)
{pp=diag(nrow(mm))
for(i in 1:nn)
{pp=pp%*%mm}
return(pp)}
pp1(MM,4)
#############################################################################
#######################################GLM###################################

dat=read.csv("I:\\STA3030F\\LectureSlides\\lawn.bunch.grass.csv",skip = 6,header=TRUE)
head(dat)
names(dat)
table(dat$Community)
prop.table(table(dat$Community))
dat$LG <- ifelse(dat$Community_ == "LG", 1, 0)
t1 <- with(dat, table(LG, Slope))
t2 <- prop.table(t1, 2)
par(mar = c(5.5, 5.5, 1, 1), mgp = c(4, 1, 0))
plot(0:19, t2[2,], las =1, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "proportion lawn grass",pch = 19,col = "firebrick3",
     xlab = "slope")
m1 <- glm(LG ~ Slope, family = binomial, data = dat)
summary(m1)
plot(jitter(LG) ~ jitter(Slope), data = dat, las = 1, 
     xlab = "Slope", pch = 20, col = "firebrick3",  
     ylab = "lawn grass", cex.lab = 1.5, cex.axis = 1.5, yaxt = "n")
axis(2, at = seq(0, 1, by = 0.1), cex.axis = 1.5, las = 1)

x <- seq(0, 20, length = 500)
y <- predict(m1, newdata = data.frame(Slope = x), type = "response")
lines(x, y, col = "blue", lwd = 4)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
#######################################################################################
#############################Tire example##############################################
#######################################################################################
Nesign=c(70,83,78,46,74,56,74,52,99,57,77,84,72,98,81,63,88,69,54,97)
Cdesign=c(47,65,59,61,75,65,73,85,97,84,72,39,72,91,64,63,79,74,76,43)
Alldesigns=c(70,83,78,46,74,56,74,52,99,57,77,84,72,98,81,63,88,69,54,97,
             47,65,59,61,75,65,73,85,97,84,72,39,72,91,64,63,79,74,76,43)
