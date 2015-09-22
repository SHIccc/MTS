# ------------------------------------------------------------------------------
# Project:     MTS - Modeling of Term Structure for Inflation Estimation
# ------------------------------------------------------------------------------
# Quantlet:    MTS_comexpinf_cds
# ------------------------------------------------------------------------------
# Description: Produce the estimation results derived from the joint modeling 
#              of IE dynamics with macroeconomic factor. Graphic showing the 
#              common inflation factor and the model residuals.
# ------------------------------------------------------------------------------
# Keywords:    bond, plot, estimation, time-series, curve, interest-rate, 
#              similarity, term structure
# ------------------------------------------------------------------------------
# See also:    
# ------------------------------------------------------------------------------
# Author:      Shi Chen
# ------------------------------------------------------------------------------

## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("zoo", "FKF", "expm", "Matrix", "formatR")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## read CDS data: eucds1 contains CDS across five countries: 06.2009-12.2014
## eucds2 contains CDS only for Italy
setwd("C:/Users/chenshic.hub/Dropbox/Panel_NS_AF/Code/MTS_Qcodes/")
##############delect this line setwd######################

eucds = read.csv("eurocds_33343.csv", header = T, sep = ";")
eucds_date = eucds[, 1]
st1 = which(eucds_date == "01.11.2009")
et1 = which(eucds_date == "01.12.2014")
eucds1 = eucds[(st1:et1), 2:6]
mcds1 = as.matrix(t(eucds1))
mcds2 = matrix(rep(mcds1[4, ], 5), nr = 5)

## load dataset
load("expinf.RData")
uk.epi = expinf[,1]
fr.epi = expinf[,2]
sw.epi = expinf[,3]
it.epi = expinf[,4]
de.epi = expinf[,5]
ukts2 = ts(uk.epi, frequency = 12, start = c(2006, 6))
dets2 = ts(de.epi, frequency = 12, start = c(2009, 6))
frts2 = ts(fr.epi, frequency = 12, start = c(2006, 6))
itts2 = ts(it.epi, frequency = 12, start = c(2007, 6))
swts2 = ts(sw.epi, frequency = 12, start = c(2007, 4))

## select the time period contains all five countries
ukepi = uk.epi[39:100]
frepi = fr.epi[39:100]
swepi = sw.epi[25:86]
itepi = it.epi[27:88]
deepi = de.epi[1:62]
y51 = matrix(c(ukepi, frepi, itepi, swepi, deepi), nr = 5)

## the model with macroeconomic factor - default proxy
jointmodel = function(q1, b1, b2, b3, b4, b5, a1, a2, a3, a4, a5, p1) {
  Tt = matrix(c(q1), 1, 1)
  Zt = rbind(b1, b2, b3, b4, b5)
  ct = matrix(diag(c(a1, a2, a3, a4, a5)), 5, 5) %*% mcds2
  dt = matrix(c(p1), 1, 1)
  GGt = matrix(0.1 * diag(5), 5, 5)
  HHt = matrix(0.1 * diag(1), 1, 1)
  a0 = mean(y51[, 1])
  P0 = HHt * 10
  return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, 
              HHt = HHt))
}

objective = function(theta, yt) {
  sp = jointmodel(theta["q1"], theta["b1"], theta["b2"], theta["b3"], theta["b4"], 
              theta["b5"], theta["a1"], theta["a2"], theta["a3"], theta["a4"], 
              theta["a5"], theta["p1"])
  ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, 
            Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
  return(-ans$logLik)
}

theta = c(q1 = c(0.2), b = c(0.5, 0.5, 0.5, 0.5, 0.5), 
          a = c(0.3, 0.3, 0.3, 0.3, 0.3), p1 = c(0.8))

fit = optim(theta, objective, yt = y51, hessian = TRUE)
sp = jointmodel(fit$par["q1"], fit$par["b1"], fit$par["b2"], fit$par["b3"], 
            fit$par["b4"], fit$par["b5"], fit$par["a1"], fit$par["a2"], fit$par["a3"], 
            fit$par["a4"], fit$par["a5"], fit$par["p1"])
ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt, 
          HHt = sp$HHt, GGt = sp$GGt, yt = y51)

plot(ans$vt[1, ], ylim = c(-2, 2), type = "l")
lines(ans$vt[2, ])
lines(ans$vt[3, ])
lines(ans$vt[4, ])
lines(ans$vt[5, ])
abline(h = 0, col = "grey", lwd = 2)

plot(ukepi, type = "l", col = "deeppink2", ylim = c(0, 3), lwd = 2)
lines(frepi, col = "dodgerblue1", lwd = 2)
lines(deepi, col = "darkolivegreen4", lwd = 2)
lines(itepi, col = "orange1", lwd = 2)
lines(swepi, col = "gray", lwd = 2)
comexp0914mac = c(ans$at[-1])
comexp0914filtermac = c(ans$att[-1])
lines(comexp0914mac, col = "black", lwd = 2)

## with proxy
commonfit0915mac = fit
commonans0915mac = ans
save(commonfit0915mac, file = "commonfit0915mac.RData")
save(commonans0915mac, file = "commonans0915mac.RData")
#load("commonfit0915mac.RData")
#load("commonans0915mac.RData")


## select the time period contains four countries
#eucds = read.csv("eurocds_33343.csv", header = T, sep = ";")
#eucds_date = eucds[, 1]
#st1 = which(eucds_date == "01.01.2008")
st1 = which(eucds_date == "01.01.2008")
et1 = which(eucds_date == "01.10.2009")
eucds1 = eucds[(st1:et1), 2:6]
mcds1 = as.matrix(t(eucds1))
mcds3 = matrix(rep(mcds1[4, ], 4), nr = 4)
#ukepi = uk.epi[15:39]
#frepi = fr.epi[15:39]
#swepi = sw.epi[5:29]
#itepi = it.epi[3:27]
## From 01.01.2008
ukepi = uk.epi[18:39]
frepi = fr.epi[18:39]
swepi = sw.epi[8:29]
itepi = it.epi[6:27]

y51 = matrix(c(ukepi, frepi, itepi, swepi),nr=4)
mcds3 = matrix(rep(mcds1[4, ], 4), nr = 4)
## select the joint model for estimation
## the joint model incorporates the default proxy
jointmodel = function(q1,b1,b2,b3,b4,a1,a2,a3,a4,p1){
  Tt = matrix(c(q1),1,1)
  Zt = rbind(b1,b2,b3,b4)
  ct = matrix(diag(c(a1, a2, a3, a4)), 4, 4) %*% mcds3
  dt = matrix(c(p1),1,1)
  GGt = matrix(0.1*diag(4),4,4)
  HHt = matrix(0.1*diag(1),1,1)
  a0 = mean(y51[,1])
  P0 = HHt * 10
  return(list(a0=a0,P0=P0,ct=ct,dt=dt,Zt=Zt,Tt=Tt,GGt=GGt,HHt=HHt))
}

objective = function(theta, yt) {
  sp = jointmodel(theta["q1"], theta["b1"], theta["b2"], theta["b3"], theta["b4"], 
               theta["a1"], theta["a2"], theta["a3"], theta["a4"], theta["p1"])
  ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, 
             Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
  return(-ans$logLik)
}

theta = c(q1 = c(0.3), b = c(0.3, 0.3, 0.3, 0.3), a = c(0.2, 0.2, 0.2, 0.2), p1 = c(0.7))

theta = c(q1 = c(0.2), b = c(0.2, 0.2, 0.2, 0.2), 
          a = c(0.3, 0.3, 0.3, 0.3), p1 = c(0.8))

theta = c(q1=c(0.5), b=c(0.6, 0.6, 0.6, 0.6), a=c(0.2,0.2,0.2,0.2), p1=c(0.8))

theta = c(q1=c(0.8), b=c(0.6, 0.6, 0.6, 0.6), a=c(0.2,0.2,0.2,0.2), p1=c(0.8))

fit = optim(theta, objective, yt = y51, hessian = TRUE)
sp = jointmodel(fit$par["q1"], fit$par["b1"], fit$par["b2"], fit$par["b3"], fit$par["b4"], 
             fit$par["a1"], fit$par["a2"], fit$par["a3"], fit$par["a4"], fit$par["p1"])
ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, 
           Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = y51)

plot(ans$vt[1, ], ylim = c(-2, 2), type = "l")
lines(ans$vt[2, ])
lines(ans$vt[3, ])
lines(ans$vt[4, ])
abline(h = 0, col = "grey", lwd = 2)

plot(ukepi, type="l",col="deeppink2",ylim=c(-1,5), lwd=2)
lines(frepi,col="dodgerblue1", lwd=2)
lines(itepi, col="orange1", lwd=2)
lines(swepi, col="gray", lwd=2)

comexp0709mac = c(ans$at[-1])
comexp0709filtermac = c(ans$att[-1])

lines(comexp0709mac, col="black", lwd=2)

## with proxy
commonfit0709mac = fit
commonans0709mac = ans

save(commonfit0709mac, file="commonfit0709mac.RData")
save(commonans0709mac, file="commonans0709mac.RData")
#load("commonfit0709mac.RData")
#load("commonans0709mac.RData")
comexp0714mac = c(comexp0709mac, comexp0914mac)
comexpts1 = ts(comexp0714mac, frequency=12, start=c(2007,1))

comexp0714filtermac = c(comexp0709filtermac, comexp0914filtermac)
comexpts2 = ts(comexp0714filtermac, frequency=12, start=c(2007,1))

## Plot 1: common inflation factor
plot(ukts2, col = "grey", lwd = 2, ylim = c(-1, 4))
lines(dets2, col = "grey", lwd = 2)
lines(frts2, col = "grey", lwd = 2)
lines(itts2, col = "grey", lwd = 2)
lines(swts2, col = "grey", lwd = 2)
lines(comexpts1, col = "red", lwd = 2.5)
lines(comexpts2, col = "blue", lty = 2, lwd = 2.5)

## Plot 2: model residuals
res.uk = c(commonans0709mac$vt[1, -1], commonans0915mac$vt[1, -1])
res.fr = c(commonans0709mac$vt[2, -1], commonans0915mac$vt[2, -1])
res.sw = c(commonans0709mac$vt[3, -1], commonans0915mac$vt[3, -1])
res.it = c(commonans0709mac$vt[4, -1], commonans0915mac$vt[4, -1])
res.de = c(commonans0915mac$vt[5, -1])
res1 = ts(res.uk, frequency = 12, start = c(2007, 9))
res2 = ts(res.fr, frequency = 12, start = c(2007, 9))
res3 = ts(res.sw, frequency = 12, start = c(2007, 9))
res4 = ts(res.it, frequency = 12, start = c(2007, 9))
res5 = ts(res.de, frequency = 12, start = c(2009, 9))

plot(res1, lwd = 2, ylim = c(-3, 3), col = "red", type = "l")
lines(res5, lwd = 2, col = "gray")
lines(res2, lwd = 2, col = "blue", lty = 2)
lines(res3, lwd = 3, col = "green3", lty = 4)
lines(res4, lwd = 2, col = "black", lty = 3)

