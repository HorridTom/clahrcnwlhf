#' link_bundles
#'
#' @param bundles the bundle data
#' @param episodes the episode data
#'
#' @return dataframe with new column linked.spell containing spellID of the spell associated with this bundle
#' @export
#'
link_bundles <- function(bundles, episodes) {
  bundles$linked.spell <- NA
  bundles
}
