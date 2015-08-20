# ------------------------------------------------------------------------------
# Project:     MTS - Modeling of Term Structure for Inflation Estimation
# ------------------------------------------------------------------------------
# Quantlet:    MTS_afns_france
# ------------------------------------------------------------------------------
# Description: The estimation results for France derived from the AFNS model in
#              multi-maturity term structue. Graphic showing the filtered
#              and predicted state variables.
# ------------------------------------------------------------------------------
# Keywords:    Kalman filter, optimization, MLE, maximum likelihood, bond, plot,
#              filter, estimation, extrapolation, dynamics, term structure,
#              interest-rate
# ------------------------------------------------------------------------------
# See also:    
# ------------------------------------------------------------------------------
# Author:      Shi Chen
# ------------------------------------------------------------------------------

# clear history
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("zoo", "FKF")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd("")

## read data of France
frdata1 = read.csv("frnom2.csv", header = F, sep = ";")
frdate = as.character(frdata1[, 2])
st = which(frdate == "30.06.2006")
et = which(frdate == "31.12.2014")
frdata11 = frdata1[(st:et), 3:14]

frdata2 = read.csv("frinf_bloom.csv", header = F, sep = ";")
frdate = as.character(frdata2[, 1])
st = which(frdate == "30.06.2006")
et = which(frdate == "31.12.2014")
frdata22 = frdata2[(st:et), 2:9]

frnom = cbind(frdata11[, 2], frdata11[, 4], frdata11[, 9])
frinf = cbind(frdata22[, 2], frdata22[, 4], frdata22[, 5])
frmat = c(3, 5, 10)

frjoi = cbind(frnom[, 1], frinf[, 1])
for (i in 2:length(frmat)) {
  frjoi = cbind(frjoi, frnom[, i], frinf[, i])
}

y51 = t(frjoi)

yieldadj_joint = function(sigma11, sigma12 = 0, sigma13 = 0, sigma21 = 0, 
                          sigma22, sigma23 = 0, sigma31 = 0, sigma32 = 0, sigma33, sigma44, lambda, 
                          time = maturity) {
  Atilde = sigma11^2 + sigma12^2 + sigma13^2 + sigma44^2
  Btilde = sigma21^2 + sigma22^2 + sigma23^2
  Ctilde = sigma31^2 + sigma32^2 + sigma33^2
  Dtilde = sigma11 * sigma21 + sigma12 * sigma22 + sigma13 * sigma23
  Etilde = sigma11 * sigma31 + sigma12 * sigma32 + sigma13 * sigma33
  Ftilde = sigma21 * sigma31 + sigma22 * sigma32 + sigma23 * sigma33
  
  adj1 = Atilde * time^2/6
  adj2 = Btilde * (1/(2 * lambda^2) - (1 - exp(-lambda * time))/(lambda^3 * 
                                                                   time) + (1 - exp(-2 * time * lambda))/(3 * lambda^3 * time))
  adj3 = Ctilde * (1/(2 * lambda^2) + exp(-lambda * time)/(lambda^2) - 
                     time * exp(-2 * lambda * time)/(4 * lambda) - 3 * exp(-2 * lambda * 
                                                                             time)/(4 * lambda^2) - 2 * (1 - exp(-lambda * time))/(lambda^3 * 
                                                                                                                                     time) + 5 * (1 - exp(-2 * lambda * time))/(8 * lambda^3 * time))
  adj4 = Dtilde * (time/(2 * lambda) + exp(-lambda * time)/(lambda^2) - 
                     (1 - exp(-lambda * time))/(lambda^3 * time))
  adj5 = Etilde * (3 * exp(-lambda * time)/(lambda^2) + time/(2 * lambda) + 
                     time * exp(-lambda * time)/lambda - 3 * (1 - exp(-lambda * time))/(lambda^3 * 
                                                                                          time))
  adj6 = Ftilde * (1/(lambda^2) + exp(-lambda * time)/(lambda^2) - exp(-2 * 
                                                                         lambda * time)/(2 * lambda^2) - 3 * (1 - exp(-lambda * time))/(lambda^3 * 
                                                                                                                                          time) + 3 * (1 - exp(-2 * lambda * time))/(4 * lambda^3 * time))
  
  return(adj1 + adj2 + adj3 + adj4 + adj5 + adj6)
}

Meloading_joint = function(lambda, alphaS, alphaC, time = maturity) {
  row1 = c(1, (1 - exp(-lambda * time))/(lambda * time), (1 - exp(-lambda * 
                                                                    time))/(lambda * time) - exp(-lambda * time), 0)
  row2 = c(0, alphaS * ((1 - exp(-lambda * time))/(lambda * time)), alphaC * 
             ((1 - exp(-lambda * time))/(lambda * time) - exp(-lambda * time)), 
           1)
  MatrixB = rbind(row1, row2)
  return(MatrixB)
}

afnsss = function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, 
                  t14, t15, t16, s1, s2, s3, s4, g1, g2, l1, h1, h2, h3, h4) {
  Tt = matrix(c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, 
                t14, t15, t16), nr = 4)
  Zt = rbind(Meloading_joint(lambda = l1, alphaS = g1, alphaC = g2, time = frmat[1]), 
             Meloading_joint(lambda = l1, alphaS = g1, alphaC = g2, time = frmat[2]), 
             Meloading_joint(lambda = l1, alphaS = g1, alphaC = g2, time = frmat[3]))
  ct = matrix(rep(c(yieldadj_joint(sigma11 = h1, sigma22 = h2, sigma33 = h3, 
                                   sigma44 = h4, lambda = l1, time = frmat[1]), yieldadj_joint(sigma11 = h1, 
                                                                                               sigma22 = h2, sigma33 = h3, sigma44 = h4, lambda = l1, time = frmat[2]), 
                    yieldadj_joint(sigma11 = h1, sigma22 = h2, sigma33 = h3, sigma44 = h4, 
                                   lambda = l1, time = frmat[1])), each = 2), nr = 6, nc = 1)
  dt = matrix(c(1 - t1, t2, t3, t4, t5, 1 - t6, t7, t8, t9, t10, 1 - 
                  t11, t12, t13, t14, t15, 1 - t16), nr = 4) %*% matrix(c(s1, s2, 
                                                                          s3, s4), nr = 4)
  GGt = matrix(0.1 * diag(6), nr = 6, nc = 6)
  H = diag(c(h1^2, h2^2, h3^2, h4^2), nr = 4)
  HHt = Tt %*% H %*% t(Tt)
  a0 = c(s1, s2, s3, s4)
  P0 = HHt * 10
  return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, 
              HHt = HHt))
}

objective = function(theta, yt) {
  sp = afnsss(theta["t1"], theta["t2"], theta["t3"], theta["t4"], theta["t5"], 
              theta["t6"], theta["t7"], theta["t8"], theta["t9"], theta["t10"], 
              theta["t11"], theta["t12"], theta["t13"], theta["t14"], theta["t15"], 
              theta["t16"], theta["s1"], theta["s2"], theta["s3"], theta["s4"], 
              theta["g1"], theta["g2"], theta["l1"], theta["h1"], theta["h2"], 
              theta["h3"], theta["h4"])
  ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, 
            Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
  return(-ans$logLik)
}

theta <- c(t = c(0.8, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0.8), s = c(0.2, 0.2, 0.2, 0.2), g = c(0.6, 0.6), l1 = c(0.7), h = c(0.2, 0.2, 0.2, 0.2))


fit = optim(theta, objective, yt = y51, hessian = TRUE)
sp = afnsss(fit$par["t1"], fit$par["t2"], fit$par["t3"], fit$par["t4"], 
            fit$par["t5"], fit$par["t6"], fit$par["t7"], fit$par["t8"], fit$par["t9"], 
            fit$par["t10"], fit$par["t11"], fit$par["t12"], fit$par["t13"], fit$par["t14"], 
            fit$par["t15"], fit$par["t16"], fit$par["s1"], fit$par["s2"], fit$par["s3"], 
            fit$par["s4"], fit$par["g1"], fit$par["g2"], fit$par["l1"], fit$par["h1"], 
            fit$par["h2"], fit$par["h3"], fit$par["h4"])
ans = fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt, 
          HHt = sp$HHt, GGt = sp$GGt, yt = y51)

res = matrix(rowMeans(ans$vt[, 2:103]), nr = 6)
joifr0915ans = ans
joifr0915fit = fit
save(joifr0915ans, file = "joifr0915ans.RData")
save(joifr0915fit, file = "joifr0915fit.RData")

## The plots of filtered and predicted state variables Another approach:
## plot.fkf(ans, CI=NA)
plot(ans$at[1, -1], type = "l", col = "red", ylab = "State variables", 
     xlab = "", ylim = c(-6, 6), lwd = 2)
lines(ans$att[1, -1], lty = 2, col = "red", lwd = 2)
lines(ans$at[2, -1], lty = 1, col = "purple", lwd = 2)
lines(ans$att[2, -1], lty = 2, col = "purple", lwd = 2)
lines(ans$at[3, -1], lty = 1, col = "grey3", lwd = 2)
lines(ans$att[3, -1], lty = 2, col = "grey3", lwd = 2)
lines(ans$at[4, -1], lty = 1, col = "blue", lwd = 2)
lines(ans$att[4, -1], lty = 2, col = "blue", lwd = 2)