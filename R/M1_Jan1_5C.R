#' M1 model as defined in
#' Basler et al. 2016 (Agr. For. Meteorlogy)
#' with fixed January 1 start date and 5 deg C base temperature
#'
#' @param data input data (see reference for detailed description),
#' data should be formatted using flat_format()
#' @param par a vector of parameter values, this is functions specific
#' @return raster or vector with estimated phenophase timing (in DOY)
#' @keywords phenology, model, sequential
#' @export
#' @examples
#'
#' \dontrun{
#' estimate = TT(data = data, par = par)
#'}

M1_Jan1_5C = function(par, data){

  # exit the routine as some parameters are missing
  if (length(par) != 2){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  k = par[1]
  F_crit = par[2]

  # create forcing/chilling rate vector
  # forcing
  Rf = data$Ti - 5
  Rf[Rf < 0] = 0
  Rf = ((data$Li / 10) ^ k) * Rf
  Rf[1:102,] = 0

  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    doy = data$doy[which(cumsum(xt) >= F_crit)[1]]
    doy[is.na(doy)] = 9999
    return(doy)
  })

  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}
