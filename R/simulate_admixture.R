#' Simulate genetic inheritance of SNPs based on population allele frequencies and family tree structure.
#'
#' @param n_samples The number of samples to simulate.
#' @param ancestor_pop_label A vector of integers giving the highest level of the family tree, which consists only of ancestors from pure genetic populations. Integers index the population, with order given by the order in which the population dominant allele frequencies are provided in the input allele frequencies file.
#' @param file File with input allele frequencies of ancestor populations. This file must be in a specific format as exemplified in `input-af-example.csv`. For a complete description of the format see \code{\link{read_in_af_data}}.
#' @return A list of \code{n_samples} elements, each corresponding to a sample. Each element is a matrix with a row for each allele, and a column for each SNP. The elements of the matrix are characters for nucleotides (A,C,G,T).
#' @examples
#' #Download file to temporary directory:
#' temp_dir <- tempdir()
#' download.file(url = XXXX,destfile = paste0(temp_dir,"/input-af-example.csv"))
#' sim_data <- simulate_admixture(
#' n_samples = 10,
#' ancestor_pop_label = c(1,2,2,2),
#' file = paste0(temp_dir,"/input-af-example.csv")
#' )
simulate_admixture <- function(
  n_samples,
  ancestor_pop_label,
  file
){

# Read in population allele frequencies data:
allele_freq_data <- read_in_af_data(file)

# Create a paired list of the allele frequencies for hte
allele_freq.list = stitch(
  allele_freq_data[,3],
  allele_freq_data[,4])

sim_samples <-
  lapply(1:n_samples,
         function(x){
           sim_single_sample.Internal(
             ancestor_pop_label = ancestor_pop_label,
             allele_freq_list = allele_freq.list
           )
         }
  )

sim_data <- back_convert.Internal(sim_samples,allele_freq_data)
return(sim_data)
}
