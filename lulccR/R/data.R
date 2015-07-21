#' Land use change dataset for Sibuyan Island
#'
#' Dataset containing land use map for 1997 and several explanatory variables for
#' Sibuyan Island derived from Verburg et al. (2002). Data are modified by Peter
#' Verburg to demonstrate the CLUE-s model; as such the dataset should not be
#' used for purposes other than demonstration.
#'
#' @format A list containing the following components:
#' \describe{
#'   \item{maps}{list containing the following RasterLayers:}
#'   \describe{
#'     \item{lu_sib_1997}{RasterLayer with land use in 1997 (forest, coconut,
#'     grassland, rice, other)}
#'     \item{ef_001}{RasterLayer showing distance to sea}
#'     \item{ef_002}{RasterLayer showing mean population density}
#'     \item{ef_003}{RasterLayer showing occurrence of diorite rock}
#'     \item{ef_004}{RasterLayer showing occurrence of ultramafic rock}
#'     \item{ef_005}{RasterLayer showing occurrence of sediments}
#'     \item{ef_006}{RasterLayer showing areas with no erosion}
#'     \item{ef_007}{RasterLayer showing areas with moderate erosion}
#'     \item{ef_008}{RasterLayer showing elevation}
#'     \item{ef_009}{RasterLayer showing slope}
#'     \item{ef_010}{RasterLayer showing aspect}
#'     \item{ef_011}{RasterLayer showing distance to roads in 1997}
#'     \item{ef_012}{RasterLayer showing distance to urban areas in 1997}
#'     \item{ef_013}{RasterLayer showing distance to streams}
#'     \item{restr1}{RasterLayer showing location of current national park}
#'     \item{restr2}{RasterLayer showing location of proposed national park}
#'   }
#'   \item{demand}{list of matrices with different demand scenarios:}
#'   \describe{
#'     \item{demand1}{data.frame with demand scenario representing slow growth
#'     scenario}
#'     \item{demand2}{data.frame with demand scenario representing fast growth
#'     scenario}
#'     \item{demand3}{data.frame with demand scenario representing land use
#'     change primarily for food production}
#'   }
#' }
#'
#' @references
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon,
#' V., Mastura, S.S (2002). Modeling the Spatial Dynamics of Regional Land Use:
#' The CLUE-S Model. Environmental Management 30(3): 391-405.
#'
#' @examples
#'
#' data(sibuyan)
"sibuyan"

#' Land use change dataset for Plum Island Ecosystem
#'
#' Dataset containing land use maps for 1985, 1991 and 1999 and several
#' explanatory variables derived from Pontius and Parmentier (2014).
#'
#' @format A list containing the following elements:
#' \describe{
#'   \item{lu_pie_1985}{RasterLayer showing land use in 1985 (forest, built,
#'   other)}
#'   \item{lu_pie_1991}{RasterLayer showing land use in 1991}
#'   \item{lu_pie_1999}{RasterLayer showing land use in 1999}
#'   \item{ef_001}{RasterLayer showing elevation}
#'   \item{ef_002}{RasterLayer showing slope}
#'   \item{ef_003}{RasterLayer showing distance to built land in 1985}
#' }
#'
#' @references
#' Pontius Jr, R. G., & Parmentier, B. (2014). Recommendations for using the
#' relative operating characteristic (ROC). Landscape ecology, 29(3), 367-382.
#'
#' @examples
#'
#' data(pie)
"pie"
