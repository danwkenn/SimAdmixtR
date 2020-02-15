#' Function which simulates mendelian inheritance for a single allele, beginning with a random draws of variants based on the population allele frequencies.
#'
#' The function proceeds as follows:
#' \enumerate{
#' \item{For each of the ancestors, using the ancestor's population index, draw two alleles randomly from the ancestor's population allele frequency.}
#' \item{Based on Mendelian Inheritance, for each generation, randomly and with equal probability draw one allele from the mother and one allele from the father. Therefore, each generation there will half the number of people/allele pairs.}
#' \item{Proceed until there is only one sample left.}
#' }
#' @param ancestor_pop_label A vector of integers giving the highest level of the family tree, which consists only of ancestors from pure genetic populations. Integers index the population, with order given by the order in which the population dominant allele frequencies are provided in the input allele frequencies file.
#' @param allele_freq A list of vectors of the population allele frequencies. Index must match the index of \code{ancestor_pop_label}.
#' @return A matrix with two rows and a single column. The elements correspond to the two alleles for the resulting genotype at the given SNP. The values are either 1 or 2, and correspond to the two variants in order of the supplied allele frequency vectors.
#' @examples
#' #Single grandparent:
#' sim_single_locus(
#' ancestor_pop_label = c(2,1,1,1),
#' allele_freq = list(
#' c(0.1,0.9),c(0.9,0.1)
#' ))
#' @export
sim_single_locus <- function(
  ancestor_pop_label,
  allele_freq
){

  # Check inputs:
  n_generations <- log(length(ancestor_pop_label),base = 2)
  if(n_generations != abs(round(n_generations))){
    stop("Ancestors improperly specified. Number of ancestors must be a power of 2.")
  }
  if(!dplyr::near(sum(allele_freq[[1]]), 1)){
    stop("Allele frequencies improperly specified. Allele frequencies must sum to 1.")
  }

  #Calculate number of generations:
  G <- log(length(ancestor_pop_label),base = 2)

  #Calculate the number of possible alleles.
  K <- length(allele_freq[[1]])

  #Simulate ancestor alleles:
  gen = G

  #For each ancestor, access the correct population allele frequencies, then randomly draw:
  ancestor_alleles <-
    sapply(ancestor_pop_label,
           function(x)
             sample(1:K,size = 2,
                    replace = TRUE,
                    prob = allele_freq[[x]]))
  previous_alleles <- ancestor_alleles

  # For each generation until the final generation:
  while(gen > 0){
    gen = gen - 1
    #Mendelian inheritance:
    current_alleles <- matrix(,2,2^gen)
    for(i in 1:(2^gen)){
      #Randomly draw the new alleles from the mother and father.
      current_alleles[1,i] <- sample(previous_alleles[,2*i-1],size = 1)
      current_alleles[2,i] <- sample(previous_alleles[,2*i],size = 1)
    }
    previous_alleles <- current_alleles
  }
  return(current_alleles)
}
