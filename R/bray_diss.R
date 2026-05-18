#' Partition Bray-Curtis dissimilarity
#'
#' @description
#' Partition Bray-Curtis dissimilarity across two different groups of species
#'
#' @param x a site by species matrix as used by \code{vegan::vegdist}
#' @param grp logical, \code{TRUE} for one group and \code{FALSE} for the other
#' 
#' @details
#' only works for a logical \code{grp} though the same partitioning logic 
#' applies to greater than two groups
#' 
#' @returns 
#' a single number, the log of the ratio of contributions of each group to
#' the overall dissimilarity; a negative number means a greater contribution 
#' of the group represented by \code{grp = FALSE}, a positive number means a 
#' greater contribution of the group represented by \code{grp = TRUE}.
#' 
#' \code{bray_part_z} returns a permutation-based z-value (using \code{cswap})
#' of the same log ratio. negative and positive sign of z-value is same as for 
#' \code{bray_part} but is now a "greater contribution of the group ..., 
#' relative to chance."
#'
#' @rdname bray_part
#' 
#' @export

bray_part <- function(x, grp) {
    ma <- outer(rowSums(x[, grp]), rowSums(x[, grp]), "+")
    mb <- outer(rowSums(x[, !grp]), rowSums(x[, !grp]), "+")
    m <- outer(rowSums(x), rowSums(x), "+")
    
    lt <- lower.tri(m)
    
    va <- as.matrix(vegan::vegdist(x[, grp])) * ma / m
    vb <- as.matrix(vegan::vegdist(x[, !grp])) * mb / m
    
    c(mean(va[lt]), mean(vb[lt])) |> 
        log() |> 
        diff()
}

#' @rdname bray_part
#' 
#' @export

bray_part_z <- function(x, grp, B = 1000) {
    obs <- bray_part(x, grp)
    sim <- sapply(1:B, function(i) bray_part(x, cswap(x, grp)))
    
    (obs - mean(sim)) / sd(sim)
}


#' Sequential column permutation algorithm
#'
#' @description
#' Create a randomization of column groupings using a sequential algorithm
#'
#' @param x a site by species matrix as used by \code{vegan::vegdist}
#' @param g logical, \code{TRUE} for one group and \code{FALSE} for the other
#' 
#' @details
#' seeks to probabilistically constrain column sums within groups meaning
#' column sums will not be strictly preserved, but the distributed of column
#' sums within groups should be similar between observed and permuted data. 
#' the algorithm does this by sequentially swapping columns with probability
#' inversely proportional to the relative magnitude of difference between
#' column totals
#' 
#' @returns 
#' permuted group vector of \code{TRUE}s and \code{FALSE}s ready to be used in
#' \code{bray_part}
#'
#' @export

cswap <- function(x, g) {
    # column constraints
    n <- length(g)
    xs <- colSums(x)
    
    # a new column grouping vector that will be permuted
    new_g <- g
    
    # begin permuting groups
    for(r in 1:1000) {
        # start by picking a random column
        i <- sample(n, 1)
        
        # swap considering original groups to more closely emulate 
        # purely random permuting, i.e., `sample(g)`
        othr <- which(g == !g[i])
        
        # pick a swap
        j <- sample(othr, 1, 
                    prob = 1 - abs(xs[i] - xs[othr]) / (xs[i] + xs[othr]))
        
        # do the swap
        g_i <- new_g[i]
        g_j <- new_g[j]
        new_g[i] <- g_j
        new_g[j] <- g_i
    }
    
    new_g
}

#' Dissimilarity z-score
#'
#' @description
#' Calculate z-score of Bray-Curtis dissimilarity compared to null model
#' generated with quasi-swap algorithm
#'
#' @param x a site by species matrix as used by \code{vegan::vegdist}
#' @param B number of permutation replicates
#' 
#' 
#' @returns 
#' a single numeric value representing the z-score
#'
#' @export

bray_diss_z <- function(x, B = 500) {
    obs_dist <- x |> vegdist(method = "bray") |> mean()
    
    sim <- nullmodel(x, "quasiswap_count") |>
        simulate(nsim = B)
    
    sim_mean_dist <- sapply(1:dim(sim)[3], function(i) {
        vegdist(sim[, , i]) |> mean()
    })
    
    
    (obs_dist - mean(sim_mean_dist)) / sd(sim_mean_dist)
    
}
