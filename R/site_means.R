#' Site means meta regression
#'
#' @description
#' run a meta regression on raw data. group means will be computed internally 
#'
#' @param formula a formula where random effects notation is used to indicate
#'     grouping variable (i.e. site)
#' @param data the data frame where variables in the formula live
#' 
#' @details
#' only works when provided both a formula and data. formula must include a
#' grouping term in random effects style syntax, \code{(1 | group)}. only a 
#' fixed effects rma is calculated, i.e. \code{metafor::rma(..., method = "FE")}
#' 
#' @returns 
#' a \code{metafor::rma} object
#' 
#' @importFrom dplyr group_by summarize across
#' @importFrom tidyselect where
#'
#' @export

site_means_lm <- function(formula, data) {
    # "fixed" terms
    fe <- reformulas::nobars(formula)
    
    # check for intercept-only model
    if(length(fe) < 3) {
        fe <- as.formula(paste(deparse(fe), "~ 1"))
    }
    
    # name on rhs of formula
    y_term <- deparse(fe[[2]])
    
    # value of response variable
    mf <- model.frame(fe, data)
    
    # adding copy of response variable to data
    data$`.y` <- mf[[y_term]]
    
    # name of grouping term
    grp <- reformulas::findbars(formula) |> 
        (\(x) deparse(x[[1]][[3]]))()
    
    # group data by grouping term and summarize
    dat <- group_by(data, .data[[grp]]) |> 
        summarize(n = n(), 
                  y_sd = sd(.y),
                  across(where(is.numeric), mean), 
                  across(where(\(x) !is.numeric(x)), first)) |> 
        ungroup()
    
    # run meta regression
    metafor::rma(yi = as.vector(dat[[".y"]]), 
                 vi = as.vector(dat$y_sd^2 / dat$n), 
                 mods = update(fe, NULL ~ .), 
                 data = dat,
                 method = "FE")
    
}

#' Predict site means from meta regression
#'
#' @description
#' predict response and confidence intervals from a \code{metafor::rma} object
#'
#' @param mod a \code{metafor::rma} object
#' @param newdata optional data frame with values for explanatory variable(s)
#'     column names must match those used when making \code{mod}
#' 
#' @details
#' assumes a model with an intercept term. assumes \code{newdata} has already
#' been grouped and summarized by site (i.e. rows represent group means)
#' 
#' @returns 
#' a \code{metafor::predict.rma} object, similar to a data frame or list with 
#' elements for \code{pred}, \code{se}, \code{ci.lb}, and \code{ci.ub}
#'
#' @export

site_means_predict <- function(mod, newdata) {
    # terms (as strings) of rhs of formula
    rhs <- formula(mod) |> 
        terms() |> 
        attr("term.labels")
    
    # default to existing data
    if(missing(newdata)) newdata <- mod$data
    
    # evaluate terms with newdata
    newx <- lapply(rhs, function(x) {
        eval(parse(text = x), envir = newdata)
    }) |> 
        do.call(cbind, args = _)
    
    # add intercept
    newx <- cbind(1, newx)

    # return prediction
    predict(mod, newx)
}

