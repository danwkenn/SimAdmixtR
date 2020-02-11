#' Function which simulates inheritance for a set of genes for a single simulated person.
#' @param ancestor_pop_label A vector of integers giving the highest level of the family tree, which consists only of ancestors from pure genetic populations. Integers index the population, with order given by the order in which the population dominant allele frequencies are provided in the input allele frequencies file.
#' @param allele_freq_list A list of allele frequencies for the ancestor populations. The upper level must be SNP, and the lower level corresponds to population frequencies in order of the population indexes in \code{ancestor_pop_label}.
sim_single_sample <- function(
  ancestor_pop_label,
  allele_freq_list
){

  J <- length(allele_freq.list)

  return(
    sapply(
      1:J,
      function(x) sim_single_locus(
        ancestor_pop_label = ancestor_pop_label,
        allele_freq = allele_freq.list[[x]])
    )
  )
}
