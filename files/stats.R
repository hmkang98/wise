# 9 
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/onesample.sav")
t.test(dsn$diff)

# 14
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/twosamples.sav")
var.test(dsn$after ~ dsn$antibiotics)

# 15
t.test(dsn$after ~ dsn$antibiotics, var.equal=T)

# 18
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/pairedsample.sav")
t.test(dsn$before, dsn$after, paired=T)

# 32
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/oneway.sav")
install.packages("Rcmdr") # 패키지 설치
library(Rcmdr) # 패키지 불러오기
leveneTest(dsn$after ~ dsn$antibiotics,center=mean)

# 34
oneway.test(dsn$after ~ dsn$antibiotics, var.equal=T)

# 36
aov(dsn$after ~ dsn$antibiotics)

# 38
anova(aov(dsn$after ~ dsn$antibiotics))

# 43
install.packages("agricolae")
library(agricolae)
model <- aov(dsn$after ~ dsn$antibiotics)
out <- duncan.test(model, "dsn$antibiotics")
out

# 51
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/onesample.sav")
wilcox.test(dsn$diff)

# 55
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/twosamples.sav")
groupA <- subset(dsn$after,dsn$antibiotics=='A')
groupF <- subset(dsn$after,dsn$antibiotics=='F')
wilcox.test(groupA,groupF)

# 58
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/pairedsample.sav")
wilcox.test(dsn$before, dsn$after, paired=T)

# 64
library(foreign)
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/oneway.sav")
kruskal.test(dsn$after~dsn$antibiotics)
install.packages("PMCMRplus")
library(PMCMRplus)
kwAllPairsNemenyiTest(x=dsn$after,g=dsn$antibiotics,method="Tukey")

# 71
blood <- c("O","A","B","AB")
freq <- c(38,43,10,5)
cat <- xtabs(freq~blood)
chisq.test(cat)

# 76
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/category1.sav")
cat <- xtabs(amount ~ treatment+bone, data=dsn)
chisq.test(cat)

# 82
dsn <- read.spss(file="http://pluto.hallym.ac.kr/data/category2.sav")
chisq.test(dsn$asthma, dsn$poison)

# 84
install.packages("Deducer")
library(Deducer)
likelihood.test(dsn$asthma, dsn$poison)

# 89
power.t.test(sd=5.1,delta=3.3,type="one.sample", alternative="two.side",power=.8,sig.level=0.05)

# 92
power.t.test(sd=10,delta=3,type="two.sample",  alternative="two.side",power=.8,sig.level=0.05)

# 98
install.packages("pwr")
library(pwr)
pwr.p.test(h=ES.h(0.4,0.65),sig.level=0.05,power=0.8,alternative="two.sided")

# 102
power.prop.test(p1=0.56, p2=0.30, sig.level=0.05, power=0.8, alternative="two.sided")

# 104
(1.96*sqrt((1+3)*0.43*(1-0.43))+0.84*sqrt(0.56*(1-0.56)+3*0.3*(1-0.3)))^2 / (3*(0.56-0.3)^2)
(qnorm(0.975)*sqrt((1+3)*0.43*(1-0.43))+qnorm(0.8)*sqrt(0.56*(1-0.56)+3*0.3*(1-0.3)))^2 / (3*(0.56-0.3)^2)

