## ----setup, message=FALSE---------------------------------------------------------------
library("knitr")
knitr::opts_chunk$set(fig.align="center", fig.width=6, fig.height=6)
options(width=90)

## ----gge--------------------------------------------------------------------------------
library(agridat)
data(yan.winterwheat)
dat1 <- yan.winterwheat

library(gge)
m1 <- gge(dat1, yield~gen*env, scale=FALSE)
biplot(m1, main="yan.winterwheat - GGE biplot",
       flip=c(1,0), origin=0, hull=TRUE)

## ---------------------------------------------------------------------------------------
m2 <- gge(dat1, yield~gen*env, scale=TRUE)
biplot(m2, main="yan.winterwheat - GGE biplot",
       flip=c(1,1), origin=0)

## ---------------------------------------------------------------------------------------
biplot(m2, main="yan.winterwheat - GGE biplot - PC 2 & 3",
       comps=c(2,3), flip=c(1,1), origin=0)

## ----mosaic-----------------------------------------------------------------------------
plot(m1, main="yan.winterwheat")

## ----ggb--------------------------------------------------------------------------------
library(agridat)
data(crossa.wheat)
dat2 <- crossa.wheat

# Define mega-environment groups of locations
dat2$eg <- ifelse(is.element(dat2$loc,
                             c("KN","NB","PA","BJ","IL","TC",
                               "JM","PI","AS","ID","SC","SS",
                               "SJ","MS","MG","MM")), "Grp1", "Grp2")

library(gge)
# Specify env.group as column in data frame
m3 <- gge(dat2, yield~gen*loc, env.group=eg, scale=FALSE)
biplot(m3, main="crossa.wheat - GGB biplot")

## ----eval=FALSE-------------------------------------------------------------------------
#  library(agridat)
#  library(reshape2)
#  library(nipals)
#  dat3 <- agridat::yan.winterwheat
#  dat3 <- acast(dat3, gen~env, value.var="yield")
#  dat3 <- scale(dat3, center=TRUE, scale=FALSE)
#  Xsvd <- svd(dat3)
#  Xnip <- nipals(dat3, center=FALSE, scale=FALSE)
#  U <- Xsvd$u
#  S <- diag(Xsvd$d)
#  V <- Xsvd$v
#  T <- Xnip$scores
#  Lam <- diag(Xnip$eig)
#  P <- Xnip$loadings

## ----eval=FALSE-------------------------------------------------------------------------
#  (U %*% S)[ , 1:2]
#  (T %*% Lam)[ , 1:2]

## ----eval=FALSE-------------------------------------------------------------------------
#  V[ , 1:2]
#  P[ , 1:2]

## ----eval=FALSE-------------------------------------------------------------------------
#  U[ , 1:2]
#  T[ , 1:2]

## ----eval=FALSE-------------------------------------------------------------------------
#  (S %*% V)[ , 1:2]
#  (Lam %*% P)[ , 1:2]

