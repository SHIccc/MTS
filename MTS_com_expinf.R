# ------------------------------------------------------------------------------
# Project:     MTS - Modeling of Term Structure for Inflation Estimation
# ------------------------------------------------------------------------------
# Quantlet:    MTS_com_expinf
# ------------------------------------------------------------------------------
# Description: The estimation results derived from the joint modeling across 
#              countries. Graphic showing the model-implied inflation 
#              expectation across different countries.
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
eucds = read.csv("eurocds_33343.csv", header = T, sep = ";")
eucds_date = eucds[, 1]
st1 = which(eucds_date == "01.11.2009")
et1 = which(eucds_date == "01.12.2014")
eucds1 = eucds[(st1:et1), 2:6]
mcds1 = as.matrix(t(eucds1))
mcds2 = matrix(rep(mcds1[4, ], 5), nr = 5)

plot(mcds1[1, ], type = "l", ylim = c(-10, 500))
lines(mcds1[2, ], col = "red")
lines(mcds1[3, ], col = "blue")
lines(mcds1[4, ], col = "green")
lines(mcds1[5, ], col = "purple")

## the plot of model-implied inflation expectation for the five countries
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
plot(ukts2, lty = 3, lwd = 3, col = "red", ylim = c(-1, 4), ylab = "Inflation expectation")
lines(dets2, lty = 2, col = "blue", lwd = 3)
lines(frts2, lty = 1, col = "black", lwd = 3)
lines(itts2, lty = 4, col = "green3", lwd = 3)
lines(swts2, lty = 1, col = "grey", lwd = 3)
## select the time period contains all five countries
ukepi = uk.epi[39:100]
frepi = fr.epi[39:100]
swepi = sw.epi[25:86]
itepi = it.epi[27:88]
deepi = de.epi[1:62]
y51 = matrix(c(ukepi, frepi, itepi, swepi, deepi), nr = 5)

## select the joint model for estimation
## the joint model incorporates the default proxy
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

## the joint model without the default proxy
jointmodel = function(q1, b1, b2, b3, b4, b5, a1, a2, a3, a4, a5, p1) {
  Tt = matrix(c(q1), 1, 1)
  Zt = rbind(b1, b2, b3, b4, b5)
  ct = rbind(a1, a2, a3, a4, a5)
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

#theta = c(q1 = c(0.3), b = c(0.3, 0.3, 0.3, 0.3, 0.3), 
#          a = c(0.2, 0.2, 0.2, 0.2, 0.2), p1 = c(0.7))

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
comexp0914 = c(ans$at[-1])
lines(comexp0914, col = "black", lwd = 2)

commonfit0915 = fit
commonans0915 = ans
save(commonfit0915, file = "commonfit0915.RData")
save(commonans0915, file = "commonans0915.RData")
load("commonfit0915.RData")
load("commonans0915.RData")


## select the time period contains four countries
ukepi = uk.epi[15:39]
frepi = fr.epi[15:39]
swepi = sw.epi[1:25]
itepi = it.epi[3:27]
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

## the joint model without the default proxy
jointmodel = function(q1,b1,b2,b3,b4,a1,a2,a3,a4,p1){
  Tt = matrix(c(q1),1,1)
  Zt = rbind(b1,b2,b3,b4)
  ct = rbind(a1,a2,a3,a4)
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

#theta = c(q1 = c(0.3), b = c(0.3, 0.3, 0.3, 0.3), 
#          a = c(0.2, 0.2, 0.2, 0.2), p1 = c(0.7))

theta = c(q1 = c(0.2), b = c(0.2, 0.2, 0.2, 0.2), 
          a = c(0.3, 0.3, 0.3, 0.3), p1 = c(0.8))

theta = c(q1=c(0.5), b=c(0.6, 0.6, 0.6, 0.6), a=c(0.2,0.2,0.2,0.2), p1=c(0.8))

theta = c(q1=c(0.8), b=c(0.2, 0.2, 0.2, 0.2), a=c(0.2,0.2,0.2,0.2), p1=c(0.8))

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
comexp0709 = c(ans$at[-1])
lines(comexp0709, col="black", lwd=2)



comexp0714 = c(comexp0709, comexp0914)
comexpts1 = ts(comexp0714, frequency=12, start=c(2006,9))

commonfit0709 = fit
commonans0709 = ans
save(commonfit0709, file="commonfit0709.RData")
save(commonans0709, file="commonans0709.RData")
load("commonfit0709.RData")
load("commonans0709.RData")