#' Alternating model as defined in
#' Basler et al. 2016 (Agr. For. Meteorlogy)
#' with fixed November 1 start date
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
#' estimate = AT_Nov1(data = data, par = par)
#'}

AT_Nov1 = function(par, data){

  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  T_base = par[1]
  a = par[2]
  b = par[3]
  c = par[4]

  # chilling
  Rc = data$Ti - T_base
  Rc[Rc > 0] = 0
  Rc[Rc < 0] = 1
  Rc[1:41,] = 0
  Sc = apply(Rc, 2, cumsum)

  # forcing
  Rf = data$Ti - T_base
  Rf[Rf <= 0] = 0
  Rf[1:41,] = 0
  Sf = apply(Rf, 2, cumsum)

  Sfc = Sf - (a + b * exp(c * Sc))

  doy = apply(Sfc, 2, function(x){
    doy = data$doy[which(x > 0)[1]]
    doy[is.na(doy)] = 9999
    return(doy)
  })

  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}
