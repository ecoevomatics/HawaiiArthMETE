#' Custom plotting parameters
#'
#' @description
#' A set of plotting choices to keep consisten plots across the project
#'
#' @param log_x boolean, if the x-axis is to be logged, default is FALSE
#' @param log_y boolean, if the y-axis is to be logged, default is FALSE
#'
#' @export

this_ms_look <- function(log_x = FALSE, log_y = FALSE) {
    l <- list(
        scale_color_viridis_d(option = "magma", 
                              begin = 0.2, end = 0.9), 
        cowplot::theme_cowplot()
    )
    
    
    lfun <- function(x) {
        10^seq(floor(log10(x[1])), ceiling(log10(x[2])))
    }
    
    lmfun <- function(x) {
        outer(2:9, lfun(x)) |> as.numeric()
    }
    
    if(log_x) {
        l <- c(
            l, 
            list(
                scale_x_log10(
                    breaks = lfun,
                    labels = scales::label_log(), 
                    minor_breaks = lmfun
                ), 
                guides(x = guide_axis_logticks(long = 1.75,
                                               mid = 0.75, 
                                               short = 0.75))
            )
        )
    }
    
    if(log_y) {
        l <- c(
            l, 
            list(
                scale_y_log10(
                    breaks = lfun,
                    labels = scales::label_log(), 
                    minor_breaks = lmfun
                ), 
                guides(y = guide_axis_logticks(long = 1.75, 
                                               mid = 0.75, 
                                               short = 0.75))
            )
        )
    }
    
    return(l)
}
