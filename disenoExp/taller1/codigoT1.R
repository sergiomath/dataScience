library(xtable)

vBeta=data.frame(VBeta=c(100,102,130,140,150,160,90,103,95))
vDelta=data.frame(VDelta=c(128,156,100,98,120,160,150,120,129,137,140))

xtable(t(vBeta),digits = 0)
xtable(t(vDelta),digits=0)
########Descriptivo###########
datos=data.frame(vBeta,vDelta)
xtable(summary(vBeta))
summary(vDelta)
qnorm(0.975)
##########################################################
#prueba de normalidad#

##Se usa shapiro pues la muestra es menor a 50 datos

shapiro.test(vBeta)
shapiro.test(vDelta)
boxplot(vBeta)
boxplot(vDelta)




