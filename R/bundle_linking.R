#' link_bundles
#'
#' @param bundles the bundle data
#' @param episodes the episode data
#'
#' @return dataframe with new column linked.spell containing spell_number of the spell associated with this bundle
#' @export
#'
link_bundles <- function(bundles = clahrcnwlhf::bundle_data_clean,
                         episodes = clahrcnwlhf::emergency_adms,
                         show.working = TRUE,
                         bundle_date_col = "Admission.Datetime",
                         bundle_id_col = "PseudoID",
                         episode_id_col = "PseudoID",
                         episode_date_col = "CSPAdmissionTime",
                         episode_date_disch_col = "CSPDischargeTime",
                         episode_spell_col = "spell_number",
                         episode_new_spell = "new_spell") {
  bundles$linked.spell <- NA

  # Run preparatory analysis
  bundles <- bundle_in_spell(bundles = bundles, episodes = episodes,
                             bundle_date_col = bundle_date_col,
                             bundle_id_col = bundle_id_col,
                             episode_id_col = episode_id_col,
                             episode_date_col = episode_date_col,
                             episode_date_disch_col = episode_date_disch_col,
                             episode_spell_col = episode_spell_col,
                             episode_new_spell = episode_new_spell)

  # 1. First link all bundles "in" their previous admission to that admission.
  type1s <- which(bundles$bundle.in.spell == TRUE)
  bundles[type1s,"linked.spell"] <- bundles[type1s,"prev.spell"]

  # 2. Next link all bundles not already linked, with lag to next admission <= 3 days,
  #     to that next admission.
  type2s <- which((bundles$bundle.in.spell == FALSE | is.na(bundles$bundle.in.spell)) & bundles$lag.to.next.adm <= 3)
  bundles[type2s,"linked.spell"] <- bundles[type2s,"next.spell"]

  # 3. The remaining bundles cannot be linked at this time.

  if (!show.working) {
    wcs <- c("prev.spell", "next.spell", "lag.from.prev.adm", "lag.to.next.adm", "bundle.in.spell")
    bundles <- bundles[,!(colnames(bundles) %in% wcs)]
  }

  bundles
}


#' link_nicor
#'
#' @param nicor the NICOR data
#' @param episodes dataframe of hospital admissions from data warehouse
#' @param show.working include helper columns in output
#' @param bundle_date_col column in NICOR data containing date pertaining to admission
#' @param bundle_id_col column in NICOR data containing patient (pseudo-)ID
#' @param episode_id_col column in episodes containing patient (pseudo-)ID
#' @param episode_date_col column in episodes containing datetime of admission
#' @param episode_date_disch_col column in episodes containing datetime of discharge
#' @param episode_spell_col column in episodes containing spell ID to return
#' @param episode_new_spell column in episodes indicating first episodes in spell
#'
#' @return nicor with linked spells, and helper columns if requested
#' @export
#'
link_nicor <- function(nicor = clahrcnwlhf::nicor_data_clean,
                       episodes = clahrcnwlhf::emergency_adms,
                       show.working = TRUE,
                       bundle_date_col = "Date.of.Visit",
                       bundle_id_col = "PseudoID",
                       episode_id_col = "PseudoID",
                       episode_date_col = "CSPAdmissionTime",
                       episode_date_disch_col = "CSPDischargeTime",
                       episode_spell_col = "spell_number",
                       episode_new_spell = "new_spell") {

  nicor$linked.spell <- NA

  # Run preparatory analysis
  nicor <- bundle_in_spell(bundles = nicor, episodes = episodes,
                             bundle_date_col = bundle_date_col,
                             bundle_id_col = bundle_id_col,
                             episode_id_col = episode_id_col,
                             episode_date_col = episode_date_col,
                             episode_date_disch_col = episode_date_disch_col,
                             episode_spell_col = episode_spell_col,
                             episode_new_spell = episode_new_spell)

  # 1. First link all NICOR records with Date.of.Visit within a spell,
  # to that spell.
  type1s <- which(nicor$bundle.in.spell == TRUE)
  nicor[type1s, "linked.spell"] <- nicor[type1s,"prev.spell"]

  # 2. Next link all NICOR records with Date.of.Visit less than 2 days before
  # an admission, to that admission.
  type2s <- which((nicor$bundle.in.spell == FALSE | is.na(nicor$bundle.in.spell)) & nicor$lag.to.next.adm <= as.difftime(2, units = "days"))
  nicor[type2s,"linked.spell"] <- nicor[type2s,"next.spell"]

  nicor

}


#' nearest_spells
#'
#' @param bundles dataframe of care bundle audit sheets
#' @param episodes dataframe of hospital admissions from data warehouse
#'
#' @return copy of bundles with additional columns added giving information on adjacent admissions
#' @export
#'
nearest_spells <- function(bundles, episodes, bundle_date_col = "Admission.Datetime",
                           bundle_id_col = "PseudoID", episode_id_col = "PseudoID",
                           episode_date_col = "CSPAdmissionTime",
                           episode_spell_col = "spell_number", episode_new_spell = "new_spell") {

  bundles <- bundles[!is.na(bundles[,bundle_date_col]),]

  #TODO: consider refactoring these two apply calls into one.

  bundles_prv_nxt_spells <- apply(bundles, 1, function(x) {

    # Extract the patient id and admission datetime for this bundle
    pt_id <- x[bundle_id_col]
    bun_dt <- x[bundle_date_col]

    # Identify the most recent admission for this patient
    # prior to or on the bundle admission datetime
    pt_eps <- episodes[episodes[,episode_id_col] == pt_id,]
    date_logical <- as.POSIXct(pt_eps[,episode_date_col]) <= as.POSIXct(bun_dt)
    date_logical[is.na(date_logical)] <- FALSE
    pt_eps <- pt_eps[date_logical,]
    if(nrow(pt_eps)==0) {prv_adm <- NA} else {
      prv_adm <- pt_eps[which.max(as.POSIXct(pt_eps[,episode_date_col])),episode_spell_col]
    }

    # Identify the first subsequent admission for this patient
    # after the bundle admission datetime
    pt_eps <- episodes[episodes[,episode_id_col] == pt_id,]
    date_logical <- as.POSIXct(pt_eps[,episode_date_col]) > as.POSIXct(bun_dt)
    date_logical[is.na(date_logical)] <- FALSE
    pt_eps <- pt_eps[date_logical,]
    if(nrow(pt_eps)==0) {nxt_adm <- NA} else {
      nxt_adm <- pt_eps[which.min(as.POSIXct(pt_eps[,episode_date_col])),episode_spell_col]
    }
    list('prev.spell'=prv_adm, 'next.spell'=nxt_adm)

  })

  bundles <- cbind(bundles, do.call(rbind.data.frame, bundles_prv_nxt_spells))

  bundles_lags <- apply(bundles, 1, function(x) {

    p_sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    n_sp_id <- as.numeric(trimws(x["next.spell"],which = "both"))
    bun_dt <- x[bundle_date_col]

    # Get lag from previous admission
    if (is.na(p_sp_id)) {
      lfpa <- NA_integer_
      pa.dt <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))
    } else {

      spell.start <- episodes[episodes[,episode_spell_col] == p_sp_id & episodes[,episode_new_spell] == TRUE,episode_date_col]
      lfpa <- difftime(as.POSIXct(bun_dt), as.POSIXct(spell.start), units = "days")
      pa.dt <- as.POSIXct(spell.start, origin="1970-01-01")
    }

    # Get lag to next admission
    if (is.na(n_sp_id)) {
      ltna <- NA_integer_
      na.dt <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))
    } else {

      spell.start <- episodes[episodes[,episode_spell_col] == n_sp_id & episodes[,episode_new_spell] == TRUE,episode_date_col]
      ltna <- difftime(as.POSIXct(spell.start), as.POSIXct(bun_dt), units = "days")
      na.dt <- as.POSIXct(spell.start, origin="1970-01-01")
    }

    list('lag.from.prev.adm'=lfpa, 'lag.to.next.adm'=ltna, 'prev.adm.dt'=pa.dt, 'next.adm.dt'=na.dt)
    #lfpa
  })

  bundles <- cbind(bundles, do.call(rbind.data.frame, bundles_lags))

  bundles$lag.from.prev.adm <- as.difftime(bundles$lag.from.prev.adm, units = "days")
  bundles$lag.to.next.adm <- as.difftime(bundles$lag.to.next.adm, units = "days")
  bundles$prev.adm.dt <- as.POSIXct(bundles$prev.adm.dt, origin="1970-01-01")
  bundles$next.adm.dt <- as.POSIXct(bundles$next.adm.dt, origin="1970-01-01")

  bundles
}


#' bundle_in_spell
#'
#' @param bundles dataframe of care bundle audit sheets
#' @param episodes dataframe of hospital admissions from data warehouse
#'
#' @return copy of bundles with additional columns added giving information on adjacent admissions
#' @export
#'
bundle_in_spell <- function(bundles, episodes = clahrcnwlhf::emergency_adms,
                            bundle_date_col = "Admission.Datetime",
                            bundle_id_col = "PseudoID",
                            episode_id_col = "PseudoID",
                            episode_date_col = "CSPAdmissionTime",
                            episode_date_disch_col = "CSPDischargeTime",
                            episode_spell_col = "spell_number",
                            episode_new_spell = "new_spell") {

  bundles <- nearest_spells(bundles = bundles, episodes = episodes,
                            bundle_date_col = bundle_date_col,
                            bundle_id_col = bundle_id_col,
                            episode_id_col = episode_id_col,
                            episode_date_col = episode_date_col,
                            episode_spell_col = episode_spell_col,
                            episode_new_spell = episode_new_spell)

  bundles <- bundles[!is.na(bundles[,bundle_date_col]),]

  bundles$bundle.in.spell <- apply(bundles, 1, function(x) {
    # Extract the patient id and admission datetime for this bundle
    sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    bun_dt <- x[bundle_date_col]

    # If there is no previous spell, return NA
    if (is.na(sp_id)) {
      bis <- NA
    } else {
      # Otherwise, establish whether or not the bundle_date_col from the bundle
      # lies inside the spell
      spell.start <- episodes[episodes[,episode_spell_col] == sp_id & episodes[,episode_new_spell] == TRUE,episode_date_col]
      spell.end <- episodes[episodes[,episode_spell_col] == sp_id & episodes[,episode_new_spell] == TRUE,episode_date_disch_col]
      bis <- (as.POSIXct(spell.start) <= as.POSIXct(bun_dt)) && (as.POSIXct(bun_dt) <= as.POSIXct(spell.end))
    }
    bis
  })

  bundles

}


#' plot_lag_dist
#'
#' @param bundles dataframe of care bundle audit sheets
#' @param episodes dataframe of hospital admissions from data warehouse
#' @param bis optional pre-analysed output of bundle_in_spell
#' @param prev toggle between lags from previous admission and to next
#' @param cumulative show cumulative distributions vs distributions
#'
#' @return plot of the lag distribution
#' @export
#'
plot_lag_dist <- function(bundles = clahrcnwlhf::bundle_data_clean, episodes = clahrcnwlhf::emergency_adms, bis = NULL, prev = TRUE, cumulative = FALSE, facet = TRUE, max_lag = NULL) {

  if (is.null(bis)) {
    bis <- clahrcnwlhf::bundle_in_spell(bundles = bundles, episodes = episodes)
  }

  if (!is.null(max_lag)) {
    if (prev) {
      bis <- bis[which(bis$lag.from.prev.adm <= max_lag),]
    } else {
      bis <- bis[which(bis$lag.to.next.adm <= max_lag),]
    }
  }

  if (prev) {
    p <- ggplot2::ggplot(bis, ggplot2::aes(lag.from.prev.adm))
  } else {
    p <- ggplot2::ggplot(bis, ggplot2::aes(lag.to.next.adm))
  }

  if (facet) {
    p <- p + ggplot2::facet_wrap(~bundle.in.spell, scales = "free_x")
  }

  if (cumulative) {
    p <- p + ggplot2::stat_ecdf()
  } else {
    p <- p + ggplot2::geom_histogram()
  }
  p
}


#' link_diag_table
#'
#' @param bis output of bundle_in_spell
#' @param lag_cutoff cutoff for classifying lags
#'
#' @return table of lag diagnostic information
#' @export
#'
link_diag_table <- function(bis, lag_cutoff = 3) {

  table(bis$bundle.in.spell, bis$lag.to.next.adm <= as.difftime(lag_cutoff, units = "days"), useNA = "always")

}


#' plot_linking_venn
#'
#' @param episodes dataframe of hospital admissions from data warehouse
#' @param linked_bundles output of link_bundles
#' @param linked_nicor output of link_nicor
#' @param plot_vd logical to indicate whether to actually draw the plot
#'
#' @return venn diagram showing linked data sources
#' @export
#'
plot_linking_venn <- function(episodes = clahrcnwlhf::emergency_adms,
                              linked_bundles, linked_nicor, plot_vd = TRUE) {

  allspells <- unique(episodes[which(!is.na(episodes$spell_number)),"spell_number"])
  bundlespells <- unique(linked_bundles[which(!is.na(linked_bundles$linked.spell)),"linked.spell"])
  nicorspells <- unique(linked_nicor[which(!is.na(linked_nicor$linked.spell)),"linked.spell"])

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  venn.plot <- VennDiagram::venn.diagram(x = list(admissions = allspells, bundles = bundlespells, nicor = nicorspells), filename = NULL)

  if (!plot_vd) {
    venn.plot
  } else {
    grid::grid.newpage()
    grid::grid.draw(venn.plot)
  }

}


#' duplicated_links
#'
#' @param linked_bundles
#'
#' @return output of link_bundles
#' @export
#'
duplicated_links <- function(linked_bundles) {

  bls <- linked_bundles[,"linked.spell"]
  duplicated_bls <- unique(bls[duplicated(bls)])
  duplicated_bls <- duplicated_bls[!is.na(duplicated_bls)]
  duplicated_bls_data <- linked_bundles[which(linked_bundles$linked.spell %in% duplicated_bls),]

  duplicated_bls_data

}
