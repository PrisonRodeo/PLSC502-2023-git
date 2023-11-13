#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro bits...                                        ####
#
# PLSC 502 -- Fall 2023
#
# Day Twelve materials: Linear Association / Regression
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:

P<-c("readr","htmltab","gmodels","DescTools","psych",
     "epitools","epiDisplay","polycor","mvtnorm","car",
     "plyr","dplyr","L1pack")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set significant digits, etc.:

options(digits=3)
options(scipen=99)

# Also -setwd()- in here if you'd like...
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interval/Ratio Measures of Association               ####
#
# Linearity!
#
# Linear, Logarithmic and Exponential plots:

set.seed(7222009)
N <- 100
X <- runif(N,0,5)
Ylin <- X + runif(N)
Ylog <- log(X)+runif(N)
Yexp <- exp(X)+runif(N,0,20)

pdf("XYLinLogExp.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ylin,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = X + u")
plot(X,Ylog,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = ln(X) + u")
plot(X,Yexp,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Y = exp(X) + u")
dev.off()

# Other relationships:

Ystep <- ifelse(X>2.5,4+runif(N),2+runif(N))
Ypoly <- 5 + 5*X - X^2 + 2*runif(N)
Ythresh <- ifelse(X>2.5,mean(X)-2.5+(X)+runif(N),mean(X)+runif(N))

pdf("XYOthers.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ystep,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Step Function")
plot(X,Ypoly,pch=19,xlim=c(0,5),ylab="Y")
legend("topright",bty="n",legend="Polynomial")
plot(X,Ythresh,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Threshold /\nChange Point")
dev.off()

# Pearson's r plot:

set.seed(7222009)
N <- 100
X <- runif(N,0,5)
Yr <- X + 2*rnorm(N)

pdf("PearsonsRPlotR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Yr,pch=19,ylab="Y")
abline(h=mean(Yr),lty=2,lwd=2,col="red")
abline(v=mean(X),lty=2,lwd=2,col="red")
text(4,7,"I",col="red")
text(4,-1,"II",col="red")
text(1,-1,"III",col="red")
text(1,7,"IV",col="red")
text(mean(X)-0.08,-2.9,"mean of X",col="red",pos=4)
text(0.55,mean(Yr)-0.05,"mean of Y",col="red",pos=3)
dev.off()

# Four linear relationships:

set.seed(7222009)
XY9 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.9,0.9,1),ncol=2))
XY5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1),ncol=2))
XY0 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0,0,1),ncol=2))
XYN5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,-0.5,-0.5,1),ncol=2))

pdf("FourLinearsR.pdf",7,7)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
plot(XY9,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.9")
plot(XY5,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.5")
plot(XY0,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = 0")
plot(XYN5,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = -0.5")
dev.off()

# Perfect Linearities:

YP1 <- X
YP2 <- 0.2*X - 3
YP3 <- 3*X - 5

pdf("PerfectRs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,YP3,pch=19,ylab="Y")
points(X,YP1,pch=4,col="darkgreen")
points(X,YP2,pch=17,col="red")
legend("topleft",bty="n",legend="All have r = 1.0")
dev.off()

# Quadratic & other bad non-linearities

set.seed(7222009)

# Quadratic
YQuad <- 5 + 5*X - X^2 + 2*runif(N)
YQr <- cor(X,YQuad)
# "Steps"
Ysteps <- ifelse(X>1.5,runif(N),3+runif(N))
Ysteps <- ifelse(X>3.5,3+runif(N),Ysteps)
YSr <- cor(X,Ysteps)
# Outlier
Yout <- runif(N)
X <- X[order(X)]
Yout <- Yout[order(X)]
Yout[N-1] <- 10
YOr <- cor(X,Yout)

pdf("BadPearsonsR.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(X,YQuad,pch=19,xlab="X",ylab="Y",main="Quadratic")
legend("topleft",bty="n",legend=paste0("r = ",round(YQr,2)))
plot(X,Ysteps,pch=19,xlab="X",ylab="Y",main="Step Function")
legend("bottomright",bty="n",legend=paste0("r = ",round(YSr,2)))
plot(X,Yout,pch=19,xlab="X",ylab="Y",main="Outlier")
legend("topleft",bty="n",legend=paste0("r = ",round(YOr,2)))
dev.off()

# Fisher's transformation plot:

r <- seq(-0.99,0.99,by=0.01)
z <- 0.5 * log((1+r)/(1-r))

pdf("RZPlotR.pdf",6,6)
par(mar=c(4,4,2,2))
plot(r,z,t="l",lwd=3)
dev.off()

# A little Pearson-Spearman comparison; first w/N=10:

Nreps <- 1000
N <- 10
Actual <- numeric(Nreps)
Rs <- numeric(Nreps)
Rhos <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  Actual[i] <- foo
  Xs<-rmvnorm(N,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  Rs[i]<-cor(Xs[,1],Xs[,2])
  Rhos[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Then with N=1000:

Nreps <- 1000
NK <- 1000
ActualK <- numeric(Nreps)
RsK <- numeric(Nreps)
RhosK <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  ActualK[i] <- foo
  Xs<-rmvnorm(NK,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  RsK[i]<-cor(Xs[,1],Xs[,2])
  RhosK[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Plots:

pdf("RvsRhoR.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Rs,Rhos,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 10")
plot(RsK,RhosK,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 1000")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Example: ANES 2016 feeling thermometers              ####
#
# Get the ANES data and extract the feeling 
# thermometers...

ANES <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2023-git/master/Data/ANES2016.csv")

Tvars <- c("V162310","V162311","V162312","V162313",
           "V162314","V162078","V162079","V162080",
           "V162081","V162091","V162092","V162093",
           "V162094","V162095","V162096","V162097",
           "V162098","V162099","V162100","V162101",
           "V162102","V162103","V162104","V162105",
           "V162106","V162107","V162108","V162109",
           "V162110","V162111","V162112","V162113")

Therms <- ANES[Tvars]
Therms[Therms==-5] <- NA
Therms[Therms==-6] <- NA
Therms[Therms==-7] <- NA
Therms[Therms==-9] <- NA
Therms[Therms==998] <- NA
Therms[Therms==999] <- NA
Therms <- na.omit(Therms)
colnames(Therms) <- c("Asian-Americans","Hispanics","Blacks",
                      "Illegal Immigrants","Whites","Dem. Pres. Candidate",
                      "GOP Pres. Candidate","Libertarian Pres. Candidate",
                      "Green Pres. Candidate","Dem. VP", "GOP VP",
                      "John Roberts", "Pope Francis",
                      "Christian Fundamentalists","Feminists","Liberals",
                      "Labor Unions","Poor People","Big Business",
                      "Conservatives","SCOTUS","Gays & Lesbians",
                      "Congress","Rich People","Muslims","Christians",
                      "Jews","Tea Party","Police","Transgender People",
                      "Scientists","BLM")

describe(Therms,range=FALSE)

# Clinton and Trump:

pdf("ClinTrumpScatter.pdf",5,5)
par(mar=c(4,4,2,2))
with(Therms, plot(`Dem. Pres. Candidate`,`GOP Pres. Candidate`,
                  pch=20,main="",xlab="Clinton",ylab="Trump"))
abline(with(Therms, lm(`GOP Pres. Candidate`~`Dem. Pres. Candidate`)),
       lwd=2,col="darkorange")
dev.off()

rCT<-with(Therms, cor(`Dem. Pres. Candidate`,`GOP Pres. Candidate`))
rCT

rCT2<-with(Therms, cor.test(`Dem. Pres. Candidate`,`GOP Pres. Candidate`))
rCT2

# Identical:

(rCT*sqrt(nrow(Therms)-2)) / sqrt(1-(rCT^2))

# Liberals and conservatives:

# Clinton and Trump:

pdf("LibConScatter.pdf",5,5)
par(mar=c(4,4,2,2))
with(Therms, plot(Liberals,Conservatives,
                  pch=20,main=""))
abline(with(Therms, lm(Liberals~Conservatives)),
       lwd=2,col="darkorange")
dev.off()

rLC<-with(Therms, cor.test(Liberals,Conservatives))
rLC

rhoLC<-with(Therms, SpearmanRho(Liberals,Conservatives))
rhoLC


# Now do all (32*31)/2 = 496 of them, comparing r and rho:

labs<-colnames(Therms)
nvar<-length(labs)
pairs<-nvar^2 # no. of pairs of vars
Rs<-data.frame(matrix(nrow=pairs,ncol=3)) # a place to keep Rs
Therms<-as.data.frame(Therms) # get rid of the "tibble" nonsense

z <- 1 # a counter...
for(i in 1:nvar) {
  for(j in 1:(nvar)) {
    Rs[z,1]<-paste0(labs[j]," - ",labs[i])
    Rs[z,2]<-cor(Therms[,j],Therms[,i])
    Rs[z,3]<-SpearmanRho(Therms[,j],Therms[,i])
    z <- z+1
  }
}

Rs$Diff <- Rs$X2 - Rs$X3 # diff. b/w r and rho

# Plot difference vs. r:

pdf("R-Rho-Diffs.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Rs$X2,Rs$Diff,xlab="Pearson's r",ylab="r minus rho",
     pch=20)
abline(h=0,lwd=1,lty=2,col="red")
abline(v=0,lwd=1,lty=2)
dev.off()

# Pairs with the biggest differences:

pdf("WhiteHispScatter.pdf",5,5)
par(mar=c(4,4,4,2))
with(Therms, scatterplot(Hispanics,Whites,
                         pch=20,main="Difference = 0.06"))
dev.off()

pdf("GreenLibScatter.pdf",5,5)
par(mar=c(4,4,4,2))
with(Therms, scatterplot(`Green Pres. Candidate`,
                         `Libertarian Pres. Candidate`,
                         pch=20,main="Difference = -0.061"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Linear Regression!                                   ####
#
# First hypothetical scatterplot with regression lines:

set.seed(7222009)
X<-runif(100,-5,5)
Y<-6+X+rnorm(100) # B0=6, B1=1

pdf("VariousRegressionLines.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=20)
abline(v=0,lty=3,col="grey")
abline(lm(Y~X),lwd=3)
abline(a=8,b=1,lwd=3,lty=2,col="navy")
abline(a=6,b=2,lwd=3,lty=4,col="orange")
legend("topleft",bty="n",lty=c(2,4,1),lwd=3,
       col=c("navy","orange","black"),
       legend=c("No, that's too high","No, that's too steep",
                "Yes, that's just right"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "World's simplest regression"                        ####

x <- c(1,2)
y <- c(3,5)
d <- data.frame(x=x,y=y)
d

pdf("WorldsSimplestRegression.pdf",5,5)
par(mar=c(4,4,2,2))
plot(d,pch=19,xlab="X",ylab="Y",xlim=c(0,3),ylim=c(2,6))
abline(lm(y~x),lwd=1,lty=2,col="grey")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS data example...                              ####
#
# This is code to aggregate SCOTUS votes from the raw 
# vote data at http://scdb.wustl.edu/data.php:

temp <- tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2022_01/SCDB_2022_01_justiceCentered_Docket.csv.zip",temp)
SCDB <- read.csv(unz(temp, "SCDB_2022_01_justiceCentered_Docket.csv"))
unlink(temp)
rm(temp)

CivLib<-SCDB[SCDB$issueArea==2,]
CivLib$LibVote<-CivLib$direction-1
Justices <- ddply(CivLib,.(justiceName),summarize,
                  justice = mean(justice),
                  CivLibs = mean(LibVote,na.rm=TRUE)*100)

# Now get and merge the "Segal-Cover" scores:

url<-"https://en.wikipedia.org/wiki/Segal%E2%80%93Cover_score"
SC<-htmltab(doc = url,rm_nodata_cols=FALSE)
rm(url)
SC<-SC[SC$Nominee!="Harlan F. Stone",]
SC<-SC[SC$Nominee!="James F. Byrnes",]
SC<-SC[SC$Nominee!="Clement Haynsworth, Jr.",]
SC<-SC[SC$Nominee!="G. Harrold Carswell",]
SC<-SC[SC$Nominee!="Robert H. Bork",]
SC<-SC[SC$Nominee!="Douglas Ginsburg",]
SC<-SC[SC$Nominee!="Harriet E. Miers",]
SC<-SC[SC$Nominee!="Merrick Garland",]
SC<-SC[SC$SenateVote!="45 – 43 *",] # Fortas CJ
SC<-SC[SC$SenateVote!="65 – 33",]   # Rehnquist CJ
SC$justice<-as.numeric(SC$Nom.Order)+77
SC$justice<-ifelse(SC$justice>82,SC$justice-1,SC$justice)
SC$justice<-car::recode(SC$justice,"100=99;103=100;104=101;105=102;
                   106=103;107=104;109=105;112=106;113=107;
                   114=108;115=109;116=110;117=111;119=112;
                   120=113;121=114;123=115;124=116;125=117;
                   126=118")

# Merge (messily):

SCOTUS<-merge(Justices,SC,by=c("justice"))
SCOTUS$IdeologyScore<-as.numeric(SCOTUS$IdeologyScore)
SCOTUS$Year<-as.numeric(SCOTUS$Year)

describe(SCOTUS,skew=FALSE,trim=0)

pdf("SCOTUSScatterR1.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=20,col="firebrick2",
                  xlim=c(-0.1,1.1),ylim=c(15,100),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
abline(v=mean(SCOTUS$IdeologyScore),lty=3)
abline(h=mean(SCOTUS$CivLibs),lty=3)
abline(lm(CivLibs~IdeologyScore, SCOTUS),lwd=2)
dev.off()

pdf("SCOTUSScatterR2.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=20,cex=0.8,
                  col="firebrick2",xlim=c(-0.2,1.2),ylim=c(15,100),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
with(SCOTUS, text(IdeologyScore,CivLibs,labels=Nominee,
                  cex=0.5,pos=1,offset=0.3))
abline(v=mean(SCOTUS$IdeologyScore),lty=3)
abline(h=mean(SCOTUS$CivLibs),lty=3)
abline(lm(CivLibs~IdeologyScore, SCOTUS),lwd=2)
dev.off()

# Betas:

Beta1 <- with(SCOTUS, (sum((IdeologyScore - mean(IdeologyScore)) * 
                           (CivLibs - mean(CivLibs))) / 
                        sum((IdeologyScore - mean(IdeologyScore))^2)))
Beta1

Beta0 <- with(SCOTUS, mean(CivLibs) - (Beta1 * mean(IdeologyScore)))
Beta0

# Residuals, etc.

SCOTUS$Yhats <- with(SCOTUS, Beta0 + Beta1*IdeologyScore)
SCOTUS$Uhats <- with(SCOTUS, CivLibs - Yhats)

# Y itself:
describe(SCOTUS$CivLibs)

# Predicted Ys:
describe(SCOTUS$Yhats)

# Residuals:
describe(SCOTUS$Uhats)

pdf("SCOTUSYhats.pdf",5,7)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Yhats,col="grey",main="",
                  breaks=8,xlab="Predicted Y Values"))
dev.off()

pdf("SCOTUSresids.pdf",5,7)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Uhats,col="grey",main="",
     breaks=8,xlab="Residual Values"))
dev.off()

# A picture of the residuals:

pdf("SCOTUSScatterWithResids.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=19,col="firebrick2",
                  xlim=c(-0.1,1.1),ylim=c(15,100),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
abline(lm(CivLibs~IdeologyScore, SCOTUS),lwd=2)
with(SCOTUS,segments(IdeologyScore,CivLibs,IdeologyScore,Yhats,
                     lty=2,lwd=1,col="firebrick2"))
with(SCOTUS,points(IdeologyScore,Yhats,pch=20,cex=0.4,
                   col="firebrick2"))
dev.off()


# Sums of squares:

TotalYVar <- with(SCOTUS, sum((CivLibs - mean(CivLibs))^2))
TotalYVar

TotalUVar <- with(SCOTUS, sum((Uhats)^2))
TotalUVar

TotalModelVar <- with(SCOTUS, sum((Yhats - mean(CivLibs))^2))
TotalModelVar

RSE <- with(SCOTUS, sqrt(TotalUVar / (nrow(SCOTUS)-2)))
RSE
  
# Using lm:

fit<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(fit)

anova(fit)

# /fin
