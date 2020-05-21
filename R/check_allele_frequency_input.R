#' Internal file which checks the input file for errors and misspecifications.
#'
#' This function checks the input file of allele frequencies and makes sure the required columns are both present in the correct format.
#' @param allele_freq_data Direct read-in from the input data .csv file.
#' @param ancestor_pop_label A vector of integers giving the highest level of the family tree, which consists only of ancestors from pure genetic populations. Integers index the population, with order given by the order in which the population dominant allele frequencies are provided in the input allele frequencies file.

check_allele_frequency_input <- function(
  allele_freq_data,
  ancestor_pop_label = NULL
){

  # Check the first and third columns are non-missing:
  if(
    any(!complete.cases(allele_freq_data[,c(1,3)]))
  ){
    stop("Missing values found in the SNP ID (column 1) and/or allele variants column (column 3) of the input file.\nRun ?read_in_af_data for full input file specification details.")
  }

  # Check the SNP column:
  if(
    !is.character(allele_freq_data[,c(1)]) |
    length(unique(allele_freq_data[,c(1)])) != length(allele_freq_data[,c(1)])
  ){
    stop("The SNP ID column (column 1) does not contain unique character values. The SNP ID column must contain unique names for each SNP.\nRun ?read_in_af_data for full input file specification details.")
  }

  # Check the Chromosome column:
  if(
    sum(which(!grepl(allele_freq_data[,2],pattern = "^\\d+ \\(\\d+\\)$"))) > 0
  ){
    stop("Chromosome position column improperly specified. The second column in the input file must have the form \"chromosome (position)\". e.g. 6 (43253).\nRun ?read_in_af_data for full input file specification details.")
  }

  # Check the variants column:
  if(
    sum(which(!grepl(allele_freq_data[,3],pattern = "^((A|G|C|T)/)*(A|G|C|T)$"))) > 0
  ){
    stop("Allele variants column improperly specified. The third column in the input file must only include nucleotides (A,G,C,T) separated by backslashes. e.g. \"A/T\".\nRun ?read_in_af_data for full input file specification details.")
  }

  # Check that all values in the population allele frequency columns are between 0 and 1.
  pop_freq_columns <- grepl(colnames(allele_freq_data),pattern = "\\.D\\.$")
  if(any(!complete.cases(allele_freq_data[,pop_freq_columns]))){
    stop("Missing values found in the population allele frequency columns.")
  }

  # Check there are population frequency columns.
  n_pop_freq_columns <- sum(pop_freq_columns,na.rm = TRUE)
  if(sum(pop_freq_columns,na.rm = TRUE) == 0){
    stop("No population allele frequencies found. Check the input file, and ensure population allele frequency columns have \"(D)\" at the end of their name.\nRun ?read_in_af_data for full input file specification details.")
  }

  if(!is.null(ancestor_pop_label)){
  # Check ancestor pop label is properly specified:
  n_generations <- log(length(ancestor_pop_label),base = 2)
  if(n_generations != abs(round(n_generations))){
    stop("Ancestors improperly specified. Number of ancestors must be a power of 2.")
  }

  # Check that the ancestor labels are in the population frequencies:
  if(
    any(!(ancestor_pop_label %in% 1:n_pop_freq_columns))
    ){
      stop(paste0("At least one ancestor has a label which does not correspond to a population. \nMake sure each ancestor's label is an integer between 1 and the number of population allele frequency columns in the input file (",
                  n_pop_freq_columns,").
                  \nAncestor Population Labels: ",
                  paste0(ancestor_pop_label,collapse = ",")))
    }
  }
}
