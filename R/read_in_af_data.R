#' Read in the population allele frequencies from prespecified CSV file format.
#'
#' This function is used internally to read in allele frequency information and complete basic cleaning. To ensure that the correct probabilities are used in simulation, care should be taken to ensure the input file complies with the required format (see Details below), and that the order of the populations given in the input file is kept consistent with the input \code{ancestor_pop_label} in the simulation functions.
#' @details The CSV file must comply with the following format rules:
#' \itemize{
#' \item{Each row corresponds to a SNP.}
#' \item{The file's first 3 columns must be the SNP ID, The chromosome and position, and the SNP variants.}
#' \item{ The variants must be specified by a list of nucleotides separated by backslashes '/', where the most frequent allele is first. E.g. 'A/G' specifies a SNP with the most common variant being 'A' and the least common being 'G'.}
#' \item{ In the following columns the derived allele frequencies in each pure population. The column names must have "(D)" at the end of their name to be recognised.}
#' }
#' @param file Path to CSV file containing the population allele frequencies.
#' @return A \link[tibble]{tibble} data-frame with a column for the SNP ID called \code{snp_id}, a column for the variants called \code{variants} which contain character vectors for each SNP, and a column for each population of allele frequencies as sum-to-one constrained vectors.
#' @examples
#' #Download file to temporary directory:
#' temp_dir <- tempdir()
#' download.file(url = XXXX,destfile = paste0(temp_dir,"/input-af-example.csv"))
#' allele_frequencies.tbl <- read_in_af_data(file = paste0(temp_dir,"/input-af-example.csv"))
#' allele_frequencies.tbl
read_in_af_data <- function(file){

  #Read in raw:
  allele_freq_data <- read.csv(file = file,stringsAsFactors = FALSE)
  allele_freq_data <- apply(allele_freq_data,2,as.character)

  #Convert to tidyverse object "tibble".
  allele_freq_data <- as.tibble(allele_freq_data)

  #Identify the population frequencies based on the ".D." string.
  pop_freq_columns <- grepl(colnames(allele_freq_data),pattern = "\\.D\\.$")

  #Rename columns:
  colnames(allele_freq_data)[1:3] <- c("snp_id","chr_id","variants")
  colnames(allele_freq_data)[4:(4+sum(pop_freq_columns)-1)] <- paste0("pop",1:sum(pop_freq_columns),"_af")

  #Subset by columns:
  allele_freq_data <- allele_freq_data[,c("snp_id","variants",paste0("pop",1:sum(pop_freq_columns),"_af"))]

  #Melt into a long form:
  allele_freq_data <- allele_freq_data %>% melt(id = c("snp_id","variants"))

  #Convert allele frequencies to usable vectors:
  allele_freq_data <-
    allele_freq_data %>%
    mutate(variants = strsplit(variants,"/")) %>%
    mutate(value = map2(.x = value,.y = variants,.f = function(x,y) af_split(x,length(y))))

  #Remove white space:
  allele_freq_data$snp_id <- str_replace_all(allele_freq_data$snp_id,pattern = " ",replacement = "")

  #Convert to wide format:
  allele_freq_data <- allele_freq_data %>% spread(variable,value)
  return(allele_freq_data)
}
