## Project: deemdat

#' Denoising Ensemble Empirical Mode Decomposition and Adaptive Thresholding
#' 
#' random noise removal by band-pass filtering in 
#' the time domain based on ensemble empirical mode 
#' decomposition (EEMD) combined with adaptive thresholding.
#' 
#' @param x time serie (ts)
#' @param sigma main parameter to be set
#' @param m1 the first (m1 - 1) IMFs are removed
#' @param m2 applied between the m1-th and (M - m2)th IMFs
#' @param ... additional arguments to be passed to \code{\link[Rlibeemd]{eemd}} function
#' 
#'  
#' @details thresholding is only applied between the m1-th 
#' and (M - m2)th IMFs, and the first (m1 - 1) IMFs are removed,
#' where IMFk is the kth IMF andMis the total number of IMFs of 
#' the input signal. If m2 is set to 0, we apply the thresholding
#' from the m1th IMF to the last IMF.
#' 
#' @references HAN, J.; VAN DER BAAN, M. Microseismic and seismic denoising via ensemble empirical mode decomposition and adaptive thresholding. Geophysics, v. 80, n. 6, p. KS69-KS80, 2015. 
#'  
#' @export


deemdat <- function(x, sigma, m1, m2, ...){
  imfs <- Rlibeemd::eemd(x, ...)
  n <- dim(imfs)[1]
  M <- dim(imfs)[2]
  E <- Th <-  vector()
  # the variance the first IMF estimated using a estimator based on median(Donoho and Johnstone, 1994; Herrera et al., 2014)
  E[1] <- (median(abs(imfs[,1]))/0.6755)^2
  Th[1] <- sigma*sqrt(2*log(n))*E[1]
  for(k in 2:(M-1)){
    # exponential relationship Kopsinis and McLaughlin(2009)
    E[k] <- (E[1]/0.719)*((2.01)^(-k))
    # universal threshold for removing thewhite Gaussian noise in the wavelet domain (Donoho and Johnstone, 1994; Donoho, 1995)
    Th[k] <- sigma*sqrt(2*log(n))*E[k]
  }
  # reconstructed signal
  s <- NULL
  for(k in m1:(M-m2)){
    s <- cbind(s, iht(imfs[,k], Th[k]))
  }
  for(k in (M-m2+1):M){
    s <- cbind(s,imfs[,k])
  }
  s <- apply(s, 1, sum)
  attributes(s) <- attributes(x)
  return(s)
}

#' interval hard thresholding
#' 
#' The idea of IMF interval thresholding is maintaining the
#'  whole interval between two zero crossings in each IMF, 
#'  when the absolute value of local extrema in this interval 
#'  is larger than the threshold.
#' 
#' @param h input signal
#' @param Th universal threshold
#' 
#' @return hz thresholded signal
#' 
#' 
#' @importFrom stats median
#' @importFrom utils head
#' @importFrom utils tail
#' 
#' @references HAN, J.; VAN DER BAAN, M. Microseismic and seismic denoising via ensemble empirical mode decomposition and adaptive thresholding. Geophysics, v. 80, n. 6, p. KS69-KS80, 2015. 
#' 
#' @export

iht <- function(h, Th){
  ext <- EMD::extrema(h)
  id <- cbind(c(1,ext$cross[,2]), c(ext$cross[,1],length(h)))
  extremes <- c(ext$minindex[,1], ext$maxindex[,1])
  extremes <- extremes[order(extremes)]
  hr <- h[extremes]
  # start
  if(head(extremes,1) > id[1,2])
    hr <- c(Inf, hr)
  # end
  if(tail(extremes,1) < id[dim(id)[1],1])
    hr <- c(hr, Inf)
  # thresholded output
  hz <- h
  for(j in 1:dim(id)[1]){
    if(abs(hr[j]) > Th)
      hz[id[j,1]:id[j,2]] <- h[id[j,1]:id[j,2]]
    else
      hz[id[j,1]:id[j,2]] <- 0
  }
  return(hz)
}