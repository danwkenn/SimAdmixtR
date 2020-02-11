#' Internal function which finds possible genotypes from available SNP variants.
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
