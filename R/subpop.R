#' Inference on Most and Least Affected Groups
#'
#' \code{subpop} conducts set inference on the groups of most and least
#' affected. When \code{subgroup = NULL}, output is for whole sample. Otherwise
#' the results are subgroup. The output of \code{subpop} is a list
#' containing five components: \code{most}, \code{least}, \code{u} and
#' \code{sub}. As the names indicate, \code{most} and \code{least}
#' denote the confidence sets for the most and least affected units. \code{u}
#' stores the u-th most and least affected index, and \code{sub} stores the
#' indicators for subpopulations. The results can be visualized using the
#' \code{\link{plot.subpop}} command.
#'
#' @param fm          Regression formula
#' @param data        The data in use
#' @param method      Models to be used for estimating partial effects. Four
#'                    options: \code{"logit"} (binary response),
#'                    \code{"probit"} (binary response), \code{"ols"}
#'                    (interactive linear with additive errors), \code{"QR"}
#'                    (linear model with non-additive errors). Default is
#'                    \code{"ols"}.
#' @param var_type    The type of parameter in interest. Three options:
#'                    \code{"binary"}, \code{"categorical"},
#'                    \code{"continuous"}. Default is \code{"binary"}.
#' @param var         Variable T in interset. Should be a character.
#' @param compare     If parameter in interest is categorical, then user needs
#'                    to specify which two category to compare with. Should be
#'                    a 1 by 2 character vector. For example, if the two levels
#'                    to compare with is 1 and 3, then \code{c=("1", "3")},
#'                    which will calculate partial effect from 1 to 3. To use
#'                    this option, users first need to specify \code{var} as a
#'                    factor variable.
#' @param subgroup    Subgroup in interest. Default is \code{NULL}.
#'                    Specifcation should be a logical variable. For example,
#'                    suppose data contains indicator variable for women
#'                    (female if 1, male if 0). If users are interested in
#'                    women SPE, then users should specify
#'                    \code{subgroup = data[, "female"] == 1}.
#' @param samp_weight Sampling weight of data. Input should be a n by 1 vector,
#'                    where n denotes sample size. Default is \code{NULL}.
#' @param taus        Indexes for quantile regression.
#'                    Default is \code{c(1:9)/10}.
#' @param u           Percentile of most and least affected. Default is set to
#'                    be 0.1.
#' @param alpha       Size for confidence interval. Shoule be between 0 and 1.
#'                    Default is 0.1
#' @param b           Number of bootstrap draws. Default is set to be 500.
#' @param parallel    Whether the user wants to use parallel computation.
#'                    The default is \code{no} and only 1 CPU will be used.
#'                    The other option is \code{yes}, and user can specify
#'                    the number of CPUs in the \code{ncores} option.
#' @param ncores      Number of cores for computation. Default is set to be
#'                    \code{detectCores()}, which is a function from package
#'                    \code{parallel} that detects the number of CPUs on the
#'                    current host. For large dataset, parallel computing is
#'                    highly recommended since bootstrap is time-consuming.
#' @param seed        Pseudo-number generation for reproduction. Default is 1.
#' @param boot_type   Type of bootstrap. Default is \code{"nonpar"}, and the
#'                    package implements nonparametric bootstrap. The
#'                    alternative is \code{"weighted"}, and the package
#'                    implements weighted bootstrap.
#'
#' @examples
#' data("mortgage")
#' fm <- deny ~ black + p_irat + hse_inc
#' result <- subpop(fm = fm, data = mortgage, var = "black", method = "logit")
#'
#' @importFrom Hmisc wtd.quantile
#' @importFrom boot boot
#' @importFrom stats quantile rexp qnorm
#' @export
subpop <- function(fm, data, method = c("ols", "logit", "probit", "QR"),
                   var_type = c("binary", "continuous", "categorical"),
                   var, compare, subgroup = NULL, samp_weight = NULL,
                   taus = c(1:9)/10, u = 0.1, alpha = 0.1, b = 500, seed = 1,
                   parallel = c("no", "yes"), ncores = detectCores(),
                   boot_type = c("nonpar", "weighted")) {
  #---------------------- Stopping Conditions ---------------------------
  if (alpha >= 1 || alpha <= 0) stop("Please specify a correct size for
                                     hypothesis testing between 0 and 1.")
  if (u >= 1 || u <= 0) stop("Please provide a group classification
                             quantile between 0 and 1.")
  # ------ Replace Null samp_weight specification
  if (is.null(samp_weight)) samp_weight <- rep(1, dim(data)[1])
  # ------ Matching Arguments
  method <- match.arg(method)
  var_type <- match.arg(var_type)
  boot_type <- match.arg(boot_type)
  parallel <- match.arg(parallel)
  # ------ 1. Call to estimate PE and PEsub
  output <- peestimate(fm, data, samp_weight, var_type, var, compare, method,
                       subgroup, taus)
  pe_est <- output$pe_est
  # ----- 2. Threshold Values for u-most/least Affected
  # Full sample
  if (method == "QR") {
    effect_high <- wtd.quantile(pe_est, matrix(samp_weight, ncol = 1,
                                               nrow = nrow(pe_est),
                                               byrow = FALSE), 1 - u)
    effect_low <- wtd.quantile(pe_est, matrix(samp_weight, ncol = 1,
                                              nrow = nrow(pe_est),
                                              byrow = FALSE), u)
  } else {
    effect_high <- wtd.quantile(pe_est, samp_weight, 1 - u)
    effect_low <- wtd.quantile(pe_est, samp_weight, u)
  }
  # Subgroup sample
  if (!is.null(subgroup)) {
    pesub_est <- output$pesub_est
    pesub_w <- output$samp_weight_sub
    effect_sub_high <- wtd.quantile(pesub_est, pesub_w, 1 - u)
    effect_sub_low <- wtd.quantile(pesub_est, pesub_w, u)
  }
  # ---------------------------- 3. Bootstrap Samples -------------------------
  # set a bootstrap counting variable for the purpose of showing a progress bar
  rep_count <- 1
  # 1. Statistics in one boot if no weight is specifed
  boot_stat_noweight <- function(data, indices){
    data$.w <- samp_weight
    data <- data[indices, ]
    # set up a progress bar to document the bootstrap progress
    setpb(pb, rep_count)
    rep_count <<- rep_count + 1
    out_bs <- peestimate(fm, data, samp_weight = data$.w, var_type, var,
                         compare, method, subgroup, taus)
    pe_est_bs <- out_bs$pe_est
    effect_high_bs <- wtd.quantile(pe_est_bs, data$.w, 1 - u) # scalar
    effect_low_bs <- wtd.quantile(pe_est_bs, data$.w, u) # scalar
    if (!is.null(subgroup)) {
      pesub_est_bs <- out_bs$pesub_est
      pesub_w <- out_bs$samp_weight_sub
      effect_sub_high_bs <- wtd.quantile(pesub_est_bs, pesub_w, 1 - u)
      effect_sub_low_bs <- wtd.quantile(pesub_est_bs, pesub_w, u)
      return(c(effect_sub_high_bs, effect_sub_low_bs, pesub_est_bs))
    } else {
      return(c(effect_high_bs, effect_low_bs, pe_est_bs))
    }
  }
  # 2. Stats in one boot if weight is specified
  data_rg <- function(data, mle){
    n <- dim(data)[1]
    # Exponential weights
    multipliers  <- rexp(n)
    # Sampling weight of data
    weight <- samp_weight * multipliers / sum(multipliers) * 20000
    data$.w <- weight
    return(data)
  }
  # Implements nonparametric bootstrap for quantile regression
  data_non <- function(data, mle){
    n <- dim(data)[1]
    multipliers <- as.vector(table(factor(sample(n,n,replace = T),
                                          levels = c(1:n))))
    # Sampling weight of data.bs
    weight <- (multipliers/sum(multipliers)) * samp_weight * 20000
    data$.w <- weight
    return(data)
  }

  boot_stat_weight <- function(data){
    # set up a progress bar to document the bootstrap progress
    setpb(pb, rep_count)
    rep_count <<- rep_count + 1
    out_bs <- peestimate(fm, data, samp_weight = data$.w, var_type, var,
                         compare, method, subgroup, taus)
    pe_est_bs <- out_bs$pe_est
    if (method == "QR") {
      effect_high_bs <- wtd.quantile(pe_est_bs, matrix(data$.w, ncol = 1,
                                                       nrow = nrow(pe_est),
                                                       byrow = FALSE), 1 - u)
      effect_low_bs <- wtd.quantile(pe_est_bs, matrix(data$.w, ncol = 1,
                                                      nrow = nrow(pe_est),
                                                      byrow = FALSE), u)
    } else {
      effect_high_bs <- wtd.quantile(pe_est_bs, data$.w, 1 - u)
      effect_low_bs <- wtd.quantile(pe_est_bs, data$.w, u)
    }
    if (!is.null(subgroup)) {
      pesub_est_bs <- out_bs$pesub_est
      pesub_w_bs <- out_bs$samp_weight_sub
      effect_sub_high_bs <- wtd.quantile(pesub_est_bs, pesub_w_bs, 1 - u)
      effect_sub_low_bs <- wtd.quantile(pesub_est_bs, pesub_w_bs, u)
      return(c(effect_sub_high_bs, effect_sub_low_bs, pesub_est_bs))
    } else {
      return(c(effect_high_bs, effect_low_bs, pe_est_bs))
    }
  }
  # ----- 4. Conduct Bootstrap
  set.seed(seed)
  if (parallel == "no") ncores <- 1
  if (boot_type == "nonpar") {
    if (method != "QR") {
      # print a message showing how many cores are used
      cat(paste("Using", ncores, "CPUs now.\n"))
      # set up a progress bar
      pb <- startpb(min = 0, max = b)
      result_boot <- boot(data = data, statistic = boot_stat_noweight,
                          parallel = "multicore", ncpus = ncores, R = b)
      closepb(pb)
    } else{
      data$.w <- samp_weight
      cat(paste("Using", ncores, "CPUs now.\n"))
      pb <- startpb(min = 0, max = b)
      result_boot <- boot(data = data, statistic = boot_stat_weight,
                          sim = "parametric", ran.gen = data_non, mle = 0,
                          parallel = "multicore", ncpus = ncores, R = b)
      closepb(pb)
      data$.w <- NULL
    }
  } else if (boot_type == "weighted") {
    data$.w <- samp_weight
    cat(paste("Using", ncores, "CPUs now.\n"))
    pb <- startpb(min = 0, max = b)
    result_boot <- boot(data = data, statistic = boot_stat_weight,
                        sim = "parametric", ran.gen = data_rg, mle = 0,
                        parallel = "multicore", ncpus = ncores, R = b)
    closepb(pb)
    data$.w <- NULL
  }
  # -----5. Analysis for the subpopulation -=
  if (is.null(subgroup)) {
    # (a) PE
    # MOST Affected Group confidence set
    is_he <- submost(pe_est, result_boot$t[, 3:(length(pe_est) + 2)],
                     result_boot$t[, 1], effect_high, alpha, b)
    # LEAST Affected Group confidence set
    is_le <- subleast(pe_est, result_boot$t[, 3:(length(pe_est) + 2)],
                      result_boot$t[, 2], effect_low, alpha, b)
    output <- list(most = is_he, least = is_le, u = u, sub = subgroup)
  } else{
    # (b) PEsub
    is_he_sub <- submost(pesub_est, result_boot$t[, 3:(length(pesub_est) + 2)],
                         result_boot$t[, 1], effect_sub_high, alpha, b)
    is_le_sub <- subleast(pesub_est, result_boot$t[, 3:(length(pesub_est) + 2)],
                          result_boot$t[, 2], effect_sub_low, alpha, b)
    output <- list(most = is_he_sub, least = is_le_sub, u = u, sub = subgroup)
  }
  # claim output as a class
  output <- structure(output, class = "subpop")
  return(output)
}
# -----Two Auxiliary Functions
# Implementing algorithm: Output is confidence set indicator
# Most affected group
submost <- function(est_pe, bs_pe, bs_u, est_u, alpha, b){
  # Find the min (to implement the sup condition as in the paper)
  is_0 <- rank(abs(est_pe - est_u)) == min(rank(abs(est_pe - est_u)))
  draws <- bs_pe - matrix(bs_u, nrow = b, ncol = length(est_pe)) -
    matrix(est_pe - est_u, nrow = b, ncol = length(est_pe), byrow = TRUE)
  bse <- (apply(draws, 2, quantile, .75, na.rm = TRUE) -
            apply(draws, 2, quantile, .25, na.rm = TRUE))/(qnorm(0.75) - qnorm(.25))
  #bm <- apply(draws, 2, mean) # bias estimator
  zs <- apply(-draws[, is_0] / matrix(bse[is_0], nrow = b,
                                      ncol = length(bse[is_0]), byrow = TRUE),
              1, max, na.rm = TRUE)
  crt <- quantile(zs, 1 - alpha)  #critical value
  is_he <- -(est_pe - est_u) / bse <= crt
  return(is_he)
}
# Least affected group
subleast <- function(est_pe, bs_pe, bs_u, est_u, alpha, b) {
  # Find the min (to implement the sup condition as in the paper)
  is_0 <- rank(abs(est_pe - est_u)) == min(rank(abs(est_pe - est_u)))
  draws <- bs_pe - matrix(bs_u, nrow = b, ncol = length(est_pe)) -
    matrix(est_pe - est_u, nrow = b, ncol = length(est_pe), byrow = TRUE)
  bse <- (apply(draws, 2, quantile, .75, na.rm = TRUE) -
            apply(draws, 2, quantile, .25, na.rm = TRUE))/(qnorm(0.75) - qnorm(.25))
  #bm <- apply(draws, 2, mean) # bias estimator
  zs <- apply(draws[, is_0] / matrix(bse[is_0], nrow = b,
                                     ncol = length(bse[is_0]), byrow = TRUE),
              1, max, na.rm = TRUE)
  crt <- quantile(zs, 1 - alpha)  #critical value
  is_le <- (est_pe - est_u) / bse <= crt
  return(is_le)
}

# ----- Plotting (2-dimensional projection plots of two specified variables)
#'
#' Plot 2-dimensional projections of variables in interest.
#'
#' Takes output from \code{\link{subpop}} command as inputs and plots
#' 2-dimensional projection plots of two specified variables. If a
#' variable in interest is of type factor, then the user must put it on
#' the y-axis.The range of x-axis is set to be the range of variable on the
#' x-coordinate. If the variable on the y-coordinate is a factor, range of
#' y-axis is set to be the factor level; otherwise it is set to be the range of
#' the variable.
#'
#' @param x           Output of \code{\link{subpop}} command.
#' @param varx        The name of the variable to be plotted on the x-axis.
#' @param vary        The name of the variable name to be plotted on the
#'                    y-axis.
#' @param main        Main title of the plot. Default is \code{NULL}.
#' @param sub         Sub title of the plot. Default is NULL.
#' @param xlab        x-axis label. Default is \code{NULL}.
#' @param ylab        y-axis label. Default is \code{NULL}.
#' @param ...         Graphics parameters to be passed to the plotting
#'                    routines.
#' @examples
#' data("mortgage")
#' fm <- deny ~ black + p_irat + hse_inc
#' result <- subpop(fm = fm, data = mortgage, var = "black", method = "logit")
#' plot(x = result, varx = mortgage$p_irat, vary = mortgage$ccred)
#'
#' @importFrom graphics plot polygon lines legend
#' @export
plot.subpop <- function(x, varx, vary, main = NULL, sub = NULL,
                        xlab = NULL, ylab = NULL, ...) {
  if (is.factor(varx)) stop("Variable on the x-axis cannot be a factor.")
  is.he <- x$most
  is.le <- x$least
  subgroup <- x$sub
  u <- x$u
  if (!is.factor(vary)) {
    plot(varx, vary, type = "n", xlim = range(varx), ylim = NULL,
         log = "", main, sub, xlab, ylab, col = 4, pch = 20, lwd = 2)
  } else {
    plot(varx, vary, type = "n", xlim = range(varx), ylim = NULL,
         log = "", main, sub, xlab, ylab, col = 4, pch = 20, lwd = 2,
         yaxt = 'n')
    axis(side = 2, at = c(1:nlevels(vary)), labels = levels(vary))
  }
  if (is.null(subgroup)) {
    # Full sample
    points(varx[is.he & !is.le], vary[is.he & !is.le], col = 'lightblue1', pch = 1, lwd = 2)
    points(varx[is.le & !is.he], vary[is.le & !is.he], col = 4, pch = 20, lwd = 2)
  } else {
    # Sub sample
    subx <- varx[subgroup]
    suby <- vary[subgroup]
    points(subx[is.he & !is.le], suby[is.he & !is.le], col = 'lightblue1', pch = 1, lwd = 2)
    points(subx[is.le & !is.he], suby[is.le & !is.he], col = 4, pch = 20, lwd = 2)
  }
  legend('topleft', c(paste0(u*100,"% Most"), paste0(u*100,"% Least")),
         col = c(4, 'lightblue1'), pch = c(20, 1), horiz = F, bty = 'n')
}
