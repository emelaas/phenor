#' Model comparison  plotting routine to faciliate model development
#' and quick comparisons of the skill of various models.
#'
#' @param comparison: list returned by model_comparison()
#' @param data: data driving model_comparison()
#' @keywords phenology, model, data, comparison, plotting
#' @export
#' @examples
#'
#' \dontrun{
#' plot_model_comparison()
#' }

plot_model_comparison = function(comparison = NULL,
                                 ylab = "RMSE (days)",
                                 names = TRUE,
                                 ...){

  # trap lack of data
  if (is.null(comparison)){
    stop("No comparison or reference data ")
  }

  # colours => ugly find solution
  colours = as.data.frame(matrix(
    c("NULL","black",
    "LIN","black",
    "TT","red",
    "TTs","red",
    "PTT","red",
    "PTTs","red",
    "M1","red",
    "M1s","red",
    "AT","blue",
    "SQ","blue",
    "SQb","blue",
    "SM1","blue",
    "SM1b","blue",
    "PA","blue",
    "PAb","blue",
    "PM1","blue",
    "PM1b","blue",
    "UN","blue",
    "UM1","blue"
    ),19,2, byrow = TRUE))
  colnames(colours) = c("model","colour")

  # calculate mean / sd RMSE of all model runs
  # (different parameters - by different seeds)
  rmse_stats = lapply(comparison$modelled,function(x){
    rmse = apply(x$predicted_values,1,function(y){
      sqrt(mean((y - comparison$measured) ^ 2, na.rm = T))
    })
    return(rmse)
    list("rmse" = mean(rmse,na.rm=TRUE),"sd"=sd(rmse,na.rm=TRUE))
    })

  labels = names(rmse_stats)
  col_sel = as.character(colours[which(colours$model %in% labels),2])
  rmse_stats = do.call("cbind",rmse_stats)

  # calculate RMSE null model (single value)
  rmse_null = sqrt(mean((
    comparison$measured -  rep(round(mean(
      comparison$measured, na.rm = TRUE
    )), length(comparison$measured))
  ) ^ 2, na.rm = T))

  # tick settings
  par(tck = 0.03, lwd = 1.3)

  if (names == 'TRUE'){
    x_names = labels
  } else {
    x_names = rep(" ",ncol(rmse_stats))
  }

  # check for custom ylim values
  dots = list(...)
  if(length(dots)!=0){
    if(names(dots)=="ylim"){
      ylim_val = dots$ylim
    }
  } else {
    ylim_val = c(0, rmse_null + rmse_null * 0.25)
  }

  # create boxplot with stats
  boxplot(rmse_stats,
          las = 2,
          names = x_names,
          ylim = ylim_val,
          ylab = ylab,
          whiskcol = col_sel,
          staplecol = col_sel,
          boxcol = col_sel,
          medcol = col_sel,
          cex.lab = 1.5,
          cex.axis = 1.5,
          outline = FALSE)

  # set a horizontal marker for the baseline NULL model
  abline(h = rmse_null, lty = 2)
}
