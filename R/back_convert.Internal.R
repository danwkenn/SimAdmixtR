#' Internal function, not for use. Used to convert from numeric output to AGCT.
#'
#' The output of the \link{sim_single_sample} function is integers, which index the variants of each of the SNPs. This function uses the allele frequency data-frame to convert these integers back to the variants (A,G,C,T).
#' @param sim_samples list outputted from the \link{sim_single_sample} function.
#' @param allele_freq_data tibble ouputted from the \link{read_in_af_data} function.
#' @return A list of \code{n_samples} elements, each corresponding to a sample. Each element is a matrix with a row for each allele, and a column for each SNP. The elements of the matrix are characters for nucleotides (A,C,G,T).
back_convert.Internal <- function(sim_samples,allele_freq_data){
  for(i in 1:length(sim_samples)){
    temp <- sim_samples[[i]]
    for(j in 1:ncol(sim_samples[[1]])){
      temp[,j] <- allele_freq_data$variants[[j]][sim_samples[[i]][,j]]
    }
    colnames(temp) <- allele_freq_data$snp_id
    sim_samples[[i]] <- temp
  }
  return(sim_samples)
}
