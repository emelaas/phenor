#' Alternating model as defined in
#' Basler et al. 2016 (Agr. For. Meteorlogy)
#' with fixed January 1 start date and 5 deg C base temperature, c = -0.01 (Jeong et al. 2012)
#'
#' @param data input data (see reference for detailed description),
#' data should be formatted using flat_format()
#' @param par a vector of parameter values, this is functions specific
#' @return raster or vector with estimated phenophase timing (in DOY)
#' @keywords phenology, model
#' @export
#' @examples
#'
#' \dontrun{
#' estimate = AT_Jan1_5C_2par(data = data, par = par)
#'}

AT_Jan1_5C_2par = function(par, data){

  # exit the routine as some parameters are missing
  if (length(par) != 2){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  a = par[1]
  b = par[2]

  # chilling
  Rc = data$Ti - 5
  Rc[Rc > 0] = 0
  Rc[Rc < 0] = 1
  Rc[1:102,] = 0
  Sc = apply(Rc, 2, cumsum)

  # forcing
  Rf = data$Ti - 5
  Rf[Rf <= 0] = 0
  Rf[1:102,] = 0
  Sf = apply(Rf, 2, cumsum)

  Sfc = Sf - (a + b * exp(-0.01 * Sc))

  doy = apply(Sfc, 2, function(x){
    doy = data$doy[which(x > 0)[1]]
    doy[is.na(doy)] = 9999
    return(doy)
  })

  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}
