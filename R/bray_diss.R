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
#' applies to greater than two groups. Null model permutation is done by 
#' quasiswap_count algorithm but run separately for the submatrices of each
#' group indicated by \code{grp}.
#' 
#' @returns 
#' \code{bray_part} a vector of length 2 with the Bray-Curtis partitions for 
#' each group indicated by \code{grp}.
#' 
#' \code{bray_part_z} returns a vector of length 2 with permutation-based 
#' z-values the same partition. Interpretation is "contribution of group i 
#' compared to the null." Note that output of \code{bray_part_z} will not sum
#' exactly to values of \code{bray_diss_z} because the null model used here is
#' more constrained.
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
    
    c(mean(va[lt]), mean(vb[lt]))
}

#' @rdname bray_part
#' @param B number of permutation replicates
#' 
#' @export

bray_part_z <- function(x, grp, B = 500) {
    # browser()
    obs <- bray_part(x, grp)
    
    sim_a <- vegan::nullmodel(x[, grp], "quasiswap_count") |>
        simulate(nsim = B)
    
    sim_b <- vegan::nullmodel(x[, !grp], "quasiswap_count") |>
        simulate(nsim = B)
    
    # weʻre splitting groups to then cbind them, so need new grouping
    # vector reflecting this re-ordering
    new_grp <- c(rep(TRUE, sum(grp)), 
                 rep(FALSE, sum(!grp)))
    
    # this will be a B x 2 matrix with columns for reps, rows for groups
    sim_part <- sapply(1:dim(sim_a)[3], function(i) {
        bray_part(cbind(sim_a[, , i], 
                        sim_b[, , i]), 
                  new_grp)
    })
    
    # mean per row
    m_diss <- rowMeans(sim_part)
    
    # overall sd
    s_diss <- colSums(sim_part) |> sd()
    
    # z-value for each group
    (obs - m_diss) / s_diss
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
    obs_dist <- x |> vegan::vegdist(method = "bray") |> mean()
    
    sim <- vegan::nullmodel(x, "quasiswap_count") |>
        simulate(nsim = B)
    
    sim_mean_dist <- sapply(1:dim(sim)[3], function(i) {
        vegan::vegdist(sim[, , i]) |> mean()
    })
    
    
    (obs_dist - mean(sim_mean_dist)) / sd(sim_mean_dist)
    
}
