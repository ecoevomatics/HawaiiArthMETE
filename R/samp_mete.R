#' Sample from a \code{meteDist} object
#'
#' @description
#' A function to draw random samples from a \code{meteDist} object while keeping
#' the state variables of the samples and original data in reasonable
#' agreement
#'
#' @param x the \code{meteDist} object
#' @param n number of random samples
#' 
#' @returns 
#' a list, usually of length \code{n} with each element containing a 
#' sample. if length is less than \code{n} a warning is printed.
#'
#' @export


samp_mete <- function(x, n) {
    # state variables
    n0 <- length(x$data)
    v0 <- sum(x$data)
    
    samp0 <- samp_good(x$r, n, n0, v0)
    nkeep <- length(samp0)
    
    # browser()
    
    # loop counter
    i <- 1
    while(nkeep < n) {
        # only try 100 times
        if(i == 100) break
        
        # prob of keeping a sample
        p <- nkeep / n
        if(p < 0.00001) p <- 0.00001
        
        # approx of the 95% binomial quantile = n, solved for size
        n_need <- ceiling(((sqrt(n + 1 - p) - sqrt(1 - p))^2) / p)
        if(!is.finite(n_need) | n_need > 50000) n_need <- 50000
        
        # make more samples
        samp1 <- samp_good(x$r, n_need, n0, v0)
        
        if(length(samp1) > 0) {
            samp0 <- c(samp0, samp1)
            nkeep <- length(samp0)
            
            if(nkeep > n) {
                samp0 <- samp0[1:n]
            }
        }
        
        i <- i + 1
    }
    
    if(length(samp0) < n) {
        warning(sprintf("only %s samples produced", length(samp0)))
    }
    
    if(length(samp0) == 0) {
        stop("no samples produced")
    }
    
    names(samp0) <- NULL
    return(samp0)
}

# helper function to make samples that satisfy state vars
samp_good <- function(rfun, n_rep, n_samp, target) {
    samp <- rfun(n_samp * n_rep) |> 
        split(rep(1:n_rep, each = n_samp))
    
    bad <- sapply(samp, function(rnew) {
        abs(sum(rnew) - target) > 0.001 * target
    })
    
    samp[!bad]
}
