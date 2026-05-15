#' Summarize output from \code{samp_mete}
#'
#' @description
#' create a data frame with mean and 95% CI calculated for each rank
#' across samples produced by \code{samp_mete}
#'
#' @param x output of \code{samp_mete}
#' 
#' @details
#' if the upper bound of the CI collapses to 1 then a small wiggle is added
#' so the upper and lower bounds remain distinguishable 
#' 
#' 
#' @returns 
#' a data frame with columns \code{rank}, \code{m} for mean, and 
#' \code{lo} and \code{hi} for 95% CI bounds
#' 
#' @importFrom dplyr bind_rows summarize mutate
#'
#' @export

summarize_samp_mete <- function(x) {
    lapply(x, function(y) data.frame(rank = 1:length(y), 
                                  abund = sort(y, decreasing = TRUE))) |> 
        bind_rows(.id = "rep") |> 
        summarize(m = mean(abund), 
                  lo = quantile(abund, probs = 0.025), 
                  hi = quantile(abund, probs = 0.975), 
                  .by = rank) |> 
        mutate(hi = ifelse(lo == hi & hi == 1, hi + 0.05, hi))
}

