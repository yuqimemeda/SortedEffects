## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("shuowencs/SortedEffects")

## ---- eval=FALSE---------------------------------------------------------
#  data("mortgage")

## ------------------------------------------------------------------------
fm <- deny ~ black + p_irat + hse_inc + ccred + mcred + pubrec + ltv_med + ltv_high + denpmi + selfemp + single + hischl

## ---- eval=FALSE---------------------------------------------------------
#  ex <- SPE(fm = fm, data = mortgage, var.T = "black", method = "probit", us = c(2:98)/100, B = 200)

## ---- eval=FALSE---------------------------------------------------------
#  plot.SPE(SE=ex$spe, AE=ex$ape, us=c(2:98)/100, alpha=0.1, main="APE and SPE of Being Black on the prob of Mortgage Denial", sub="Probit Model", ylab="Change in Probability")

## ---- eval=FALSE---------------------------------------------------------
#  t <- c(rep(1, 4), 0, rep(1, 7), 0, 0, 1, 1)

## ---- eval=FALSE---------------------------------------------------------
#  CA <- u.CA(fm=fm, data=mortgage, var.T="black", method="probit",
#             cl=matrix(c(1,0,0,1), nrow=2), t=t, B=200)

## ---- eval=FALSE---------------------------------------------------------
#  est <- matrix(CA$est, ncol=2)
#  se <- matrix(CA$bse, ncol=2)
#  Table <- matrix(0, ncol=4, nrow=13)
#  Table[, 1] <- est[, 1] # Least Affected Bias-corrected estimate
#  Table[, 2] <- se[, 1] # Corresponding SE
#  Table[, 3] <- est[, 2] # Most affected
#  Table[, 4] <- se[, 2] # Corresponding SE
#  rownames(Table) <- colnames(CA$est)[1:13] # assign names to each row
#  colnames(Table) <- rep(c("Estimate", "SE"), 2)
#  xtable(Table)

## ---- eval=FALSE---------------------------------------------------------
#  CAdiff <- u.CA(fm = fm, data = mortgage, var.T = "black", t = t,
#                 method = "logit", cl = matrix(c(1,-1), nrow=2),
#                 B = 200, bc = FALSE)
#  # Tabulate the results
#  est <- matrix(CAdiff$est, ncol = 1)
#  se <- matrix(CAdiff$bse, ncol = 1)
#  joint_p <- matrix(CAdiff$joint_p, ncol = 1)
#  Table2 <- matrix(0, ncol = 3, nrow = 13)
#  Table2[, 1] <- est
#  Table2[, 2] <- se
#  Table2[, 3] <- joint_p
#  rownames(Table2) <- colnames(CAdiff$est) # assign names to each row
#  colnames(Table2) <- c("Estimate", "SE", "JP-vals")
#  xtable(Table2)

## ---- eval=FALSE---------------------------------------------------------
#  data(wage2015)

## ---- eval=FALSE---------------------------------------------------------
#  <<eval=FALSE>>=
#  fmla1 <- lnw ~ female* (widowed + divorced + separated + nevermarried +
#                          exp1 + exp2 + exp3 + exp4 + educ + occ2 + ind2 +
#                          mw + so + we)

## ---- eval=FALSE---------------------------------------------------------
#  set <- u.Subpop(fm = fmla1, data = wage2015, var.T = "female",
#                  samp_weight = wage2015$weight, boot.type = "weighted",
#                  B = 200, seed = 88)
#  
#  Subpopplot(varx = "exp1", vary = "lnw", data = wage2015, u = 0.1,
#             interest = "subgroup", subgroup = wage2015[, "female"]==1,
#             result = set, main = "Projections of Exp-lnw", sub = "OLS",
#             xlab = "Exp", ylab = "Log Wages")
#  
#  Subpopplot(varx = "exp1", vary = "ms", data = wage2015, u = 0.1,
#             interest= "subgroup", subgroup = wage2015[, "female"]==1,
#             result = set, main = "Projections of Exp-MS", sub = "OLS",
#             xlab = "Experience", ylab = "Marital Status")

