#' Internal function which finds possible genotypes from available SNP variants.
#' @param allele_freq_data Processed allele frequencies data-set read in using \link{read_in_af_data}.
#' @return A list of possible genotypes based on allele variants.
genotype_finder <- function(allele_freq_data){

  genotype_finder.Internal <- function(x){
    genotypes <- c()
    for(i in 1:length(x)){
      for(j in 1:length(x)){
        genotypes <- c(genotypes,paste0(x[i],x[j]))
      }
    }
    return(genotypes)
  }

  return(
    lapply(allele_freq_data$variants,genotype_finder.Internal)
  )
}
