#' Function finds the genotype frequencies for a simulated data-list, and the most likely genotype for each SNP.
#' @param data_list Simulated data output from \link{simulate_admixture}.
#' @param pop_allele_file Path to CSV file containing the population allele frequencies.
#' @return A list containing:
#' \itemize{
#' \item{\code{frequencies}: A list of vectors, with one element for each SNP. The vectors have a single element for each possible genotype, and the value is the observed frequency of that genotype in the simulated data.}
#' \item{\code{most_common}: a matrix of the most common genotype found in the sample.}
#' }
#' @export
summarise_admixture <- function(data_list, pop_allele_file){

  #Load data:
  allele_freq_data <- read_in_af_data(file = pop_allele_file)

  #For each gene:
  #Find possibilities:
  genotype_list <- genotype_finder(allele_freq_data)

  #Count possibilities:
  sim_data <- t(sapply(data_list,function(x) apply(x,2,paste0,collapse = "")))
  sim_data <- lapply(1:ncol(sim_data),function(x) table(factor(sim_data[,x],levels = genotype_list[[x]])))

  #Combine heterozygotes:
  #Remove heterozygote replicates:
  for(gene in 1:length(genotype_list)){
    n <- sqrt(length(genotype_list[[gene]]))
    indexes <- expand.grid(i = 1:n,j = 1:n)

    indexes$keep = !(indexes$i < indexes$j)

    for(ind in 1:(n^2)){
      newind <- n * (indexes$i[ind]-1) + indexes$j[ind]
      if(!indexes$keep[ind]){
        sim_data[[gene]][newind] <- sim_data[[gene]][newind] + sim_data[[gene]][ind]
      }
    }

    sim_data[[gene]] <- sim_data[[gene]][indexes$keep]
  }

  #normalise:
  genotype_frequencies <- lapply(sim_data,function(x) x/sum(x))
  most_common_genotype <- lapply(genotype_frequencies,function(x) names(x)[which.max(x)])
  most_common_genotype <- sapply(most_common_genotype,function(x) strsplit(x,"")[[1]])
  colnames(most_common_genotype) <- allele_freq_data$snp_id
  names(genotype_frequencies) <- allele_freq_data$snp_id

  return(
    list(
      frequencies = genotype_frequencies,
      most_common = most_common_genotype
    )
  )
}
