#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro...                                             ####
#
# PLSC 502 -- Fall 2023
#
# Day Thirteen materials: Linear Regression, II
# (including things like basic inference and model
# fit...)
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:

P<-c("readr","htmltab","gmodels","DescTools","psych",
     "epitools","epiDisplay","mvtnorm","car",
     "plyr","dplyr","L1pack","stargazer","coefplot")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run ^that code^ a few times until you get all smileys :)
#
# Set significant digits, etc.:

options(digits=3)
options(scipen=9)

# Also -setwd()- in here if you'd like...
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Supreme Court Judicial Database data...               ####
#
# Once again: code to aggregate SCOTUS votes from the raw 
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

# Clean up:

rm(SCDB,CivLib,Justices,SC)

describe(SCOTUS,skew=FALSE,trim=0)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# The inference bit...                           ####

with(SCOTUS, describe(CivLibs))
with(SCOTUS, describe(IdeologyScore))

# Scatterplot:

pdf("SCOTUSPlot23.pdf",7,6) # <- create PDF
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=19,
                  xlab="Segal-Cover Liberalism Score",
                  ylab="Voting Liberalism"))
with(SCOTUS, abline(v=mean(IdeologyScore,na.rm=TRUE),lty=2))
with(SCOTUS, abline(h=mean(CivLibs,na.rm=TRUE),lty=2))
dev.off()

# Regression:

SCLib<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(SCLib)   # regression
anova(SCLib)     # ANOVA

# Other things:

vcov(SCLib)
sqrt(vcov(SCLib))
confint(SCLib)
confint(SCLib,level=0.99)
SEs<-predict(SCLib,interval="confidence")
SEs

# Plot of predictions + CIs:

Sort<-order(SCOTUS$IdeologyScore)

pdf("SCLib-CI23.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeologyScore,SCOTUS$CivLibs,pch=20,
     xlab="Segal-Cover Ideology Score",
     ylab="Voting Liberalism")
abline(SCLib,lwd=3)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,3],col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(1,2),lwd=2,col=c("black","red"),
       legend=c("Fitted Line","95 percent C.I.s"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Model Fit...                                 ####
#
# Simulations:

seed <- 7222009
set.seed(seed)

X<-rnorm(250)
Y1<-5+2*X+rnorm(250,mean=0,sd=sqrt(0.2))
Y2<-5+2*X+rnorm(250,mean=0,sd=sqrt(20))
fit<-lm(Y1~X)
summary(fit)

pdf("TightLine-R.pdf",5,5)
plot(X,Y1,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,18))
abline(fit,lwd=3)
text(-1.5,15,labels="R-squared = 0.95")
dev.off()

fit2<-lm(Y2~X)
summary(fit2)

pdf("ScatteredLine-R.pdf",5,5)
plot(X,Y2,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,18))
abline(fit2,lwd=3)
text(1.5,-8,labels="R-squared = 0.20")
dev.off()

# R^2 = 0 plots:

seed<-7222009
set.seed(seed)

pdf("RSqZero.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
X<-(runif(100))*10
Yzero<-(runif(100))*10
Yquad<-30-((X-5)^2)+(2*runif(100))
Ystep<-ifelse(abs(X-5)>2.5,5+runif(100),1+runif(100))
Ytype<-rep(0:1,50)
Yvar<-ifelse(Ytype==1,X+(2*runif(50)),10-X+2*runif(50))
plot(Yzero~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yzero~X),lwd=3)
plot(Yquad~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yquad~X),lwd=3)
plot(Ystep~X,xlab="X", ylab="Y",pch=20)
abline(lm(Ystep~X),lwd=3)
plot(Yvar[Ytype==0]~X[Ytype==0],xlab="X", ylab="Y",pch=20)
points(Yvar[Ytype==1]~X[Ytype==1],pch="o")
abline(lm(Yvar~X),lwd=3)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS data, again...                            ####
#
# The linear regression:

fit<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(fit)

anova(fit)

# R-squared:

anova(fit)$`Sum Sq`[1] / (anova(fit)$`Sum Sq`[1] + anova(fit)$`Sum Sq`[2])

# F-statistic:

anova(fit)$`Mean Sq`[1] / anova(fit)$`Mean Sq`[2]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Stupid Regression Tricks                     ####
#
# Back to our regression:

fit<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(fit)

SEs<-predict(fit,interval="confidence")
Sort<-order(SCOTUS$IdeologyScore)

pdf("SRTF123.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeologyScore, SCOTUS$CivLibs, 
     xlab="Editorial-Based Liberalism",
     ylab="Pro-Civil Rights Voting Percentage",pch=16) 
abline(fit,lwd=3)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 3 to IdeologyScore:

SCOTUS$IdeoPlus3 <- SCOTUS$IdeologyScore + 3

fit2<-lm(CivLibs~IdeoPlus3,data=SCOTUS)
summary(fit2)

SEs2<-predict(fit2,interval="confidence")
Sort2<-order(SCOTUS$IdeoPlus3)

pdf("SRTF223.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeoPlus3, SCOTUS$CivLibs, 
     xlab="Editorial-Based Liberalism + 3",
     ylab="Pro-Civil Rights Voting Percentage",pch=16) 
abline(fit2,lwd=3)
lines(sort(SCOTUS$IdeoPlus3),SEs2[Sort2,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeoPlus3),SEs2[Sort2,3],col="red",lwd=2,lty=2)
dev.off()

# Multiply Y times -10:

SCOTUS$CivLibNeg10 <- -10 * SCOTUS$CivLibs

fit3<-lm(CivLibNeg10~IdeologyScore,data=SCOTUS)
summary(fit3)

SEs3<-predict(fit3,interval="confidence")
Sort3<-order(SCOTUS$IdeologyScore)

pdf("SRTF323.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeologyScore, SCOTUS$CivLibNeg10, 
     xlab="Editorial-Based Liberalism",
     ylab="Pro-Civil Rights Voting Percentage x -10",pch=16) 
abline(fit3,lwd=3)
lines(sort(SCOTUS$IdeologyScore),SEs3[Sort3,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeologyScore),SEs3[Sort3,3],col="red",lwd=2,lty=2)
dev.off()

# Reversing the X and Y Scales:

SCOTUS$CivLibCons <- 100 - SCOTUS$CivLibs
SCOTUS$IdeolCons <- 1 - SCOTUS$IdeologyScore

fit4<-lm(CivLibCons~IdeolCons,data=SCOTUS)
summary(fit4)

SEs4<-predict(fit4,interval="confidence")
Sort4<-order(SCOTUS$IdeolCons)

pdf("SRTF423.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeolCons, SCOTUS$CivLibCons, 
     xlab="Editorial-Based Conservatism",
     ylab="Anti-Civil Rights Voting Percentage",pch=16) 
abline(fit4,lwd=3)
lines(sort(SCOTUS$IdeolCons),SEs4[Sort4,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeolCons),SEs4[Sort4,3],col="red",lwd=2,lty=2)
dev.off()

# Centering variables:

SCOTUS$CivLibCentered <- SCOTUS$CivLibs - mean(SCOTUS$CivLibs)
SCOTUS$IdeolCentered <- SCOTUS$IdeologyScore - mean(SCOTUS$IdeologyScore)

fit5<-lm(CivLibCentered~IdeolCentered,data=SCOTUS)
summary(fit5)

SEs5<-predict(fit5,interval="confidence")
Sort5<-order(SCOTUS$IdeolCentered)

pdf("SRTF523.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeolCentered, SCOTUS$CivLibCentered, 
     xlab="Centered Liberalism",
     ylab="Centered Civil Rights Voting Percentage",pch=16) 
abline(v=0,lty=2,col="grey")
abline(h=0,lty=2,col="grey")
abline(fit5,lwd=3)
lines(sort(SCOTUS$IdeolCentered),SEs5[Sort5,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeolCentered),SEs5[Sort5,3],col="red",lwd=2,lty=2)
dev.off()

# Standardizing a variable:

SCOTUS$IdeolStd <- scale(SCOTUS$IdeologyScore)

fit6<-lm(CivLibs~IdeolStd,data=SCOTUS)
summary(fit6)

SEs6<-predict(fit6,interval="confidence")
Sort6<-order(SCOTUS$IdeolStd)

pdf("SRTF623.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeolStd, SCOTUS$CivLibs, 
     xlab="Standardized Liberalism",
     ylab="Civil Rights Voting Percentage",pch=16) 
abline(v=0,lty=2,col="grey")
abline(fit6,lwd=3)
lines(sort(SCOTUS$IdeolStd),SEs6[Sort6,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeolStd),SEs6[Sort6,3],col="red",lwd=2,lty=2)
dev.off()

# Rescaling for interpretability:

fit7<-lm(CivLibs~Year,data=SCOTUS)
summary(fit7)

SCOTUS$Year1900<-SCOTUS$Year-1900
fit8<-lm(CivLibs~Year1900,data=SCOTUS)
summary(fit8)

# Binary Predictors and T-tests:

SCOTUS$Chief<-ifelse(is.na(SCOTUS$ChiefJustice),0,1)
fit9<-lm(CivLibs~Chief,data=SCOTUS)
summary(fit9)

t.test(CivLibs~Chief,data=SCOTUS,var.equal=TRUE)

# Presentation of regression results...
#
# Original regression:

summary(fit)

# Easy LaTeX table using *stargazer*:

stargazer(fit,
          type="latex",
          title="OLS Regression Model of SCOTUS Voting",
          dep.var.caption="",
          dep.var.labels="Model I",
          digits=2,
          no.space=TRUE,
          intercept.top=TRUE,intercept.bottom=FALSE,
          covariate.labels=c("(Constant)","
                             Ideological Liberalism"))

# Default-y -fitplot-:

fitplot<-coefplot(fit,shorten=TRUE,color="navy",
                  title=" ",zeroType=1,zeroColor="black",
                  zeroLWD=0.6)

pdf("BivariateLadderPlot23.pdf",6,5)
fitplot+theme_bw()
dev.off()

# /fin