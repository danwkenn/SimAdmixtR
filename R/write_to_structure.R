#' Take simulated genotypes and write to a STRUCTURE input file format.
#' @param sim_data Output from simulate_admixture
#' @param structure_snp_order (Optional) The desired order of SNPs for columns of output file.
#' @param example_structure_file (Optional) A sample STRUCTURE input file to extract the desired order of SNPs from. If both \code{example_structure_file} and \code{structure_snp_order} are specified, the order given by this will take precedence.
#' @param sample_names (Optional) vector of names for the simulated samples.
#' @param output Name for the output file.
#' @param type Options for txt or csv file outputs.
#' @param keep_extra_snps If there are SNPs present in the simulated data, but not in the SNP order provided by \code{example_structure_file} or \code{structure_snp_order}, should the extra SNPs be kept and appended to the table (on the right)?
#' @return \code{NULL}
#' @examples
#' #Download files to temporary directory:
#' temp_dir <- tempdir()
#' download.file(url = XXXX,destfile = paste0(temp_dir,"/input-af-example.csv"))
#' download.file(url = XXXX,destfile = paste0(temp_dir,"/example-structure-file.csv"))
#'
#' sim_data <- simulate_admixture(
#' n_samples = 10,
#' ancestor_pop_label = c(1,2,2,2),
#' file = paste0(temp_dir,"/input-af-example.csv")
#' )
#'
#' write_to_structure(
#' sim_data = sim_data,
#' example_structure_file = paste0(temp_dir,"/example-structure-file.csv"),
#' output = "example-output",
#' type = c("txt","csv"))

write_to_structure <- function(
  sim_data,
  structure_snp_order = NULL,
  example_structure_file = NULL,
  sample_names = NULL,
  output = "test",
  type = c("txt","csv"),
  keep_extra_snps = TRUE
){

  #If an example structure file is given, then the structure from this file is used:
  if(!is.null(example_structure_file)){
    example_data <- read.table(
      file = example_structure_file,
      sep = ",",
      header = TRUE)

    structure_snp_order <- colnames(example_data)[seq(4,2*floor(ncol(example_data)/2),2)]
  }

  #If structure SNP order is not given, then use the order of the SNPs in the simulated data:
  if(is.null(structure_snp_order)){
    structure_snp_order = colnames(sim_data[[1]])
  }

  #Check Structure SNP order contains all sim_data SNPs:
  structure_snp_order <- structure_snp_order[
    structure_snp_order %in% colnames(sim_data[[1]])
    ]

  if(keep_extra_snps){
    structure_snp_order <- c(
      structure_snp_order,
      colnames(sim_data[[1]])[
        !(colnames(sim_data[[1]]) %in% structure_snp_order)
        ]
    )
  }

  #Create object to convert to numeric (1:4) from ATCG
  structure_code <- c("A","C","T","G")

  #convert simulated data to long data-frame format:
  sim_data_long <- melt(sim_data,varnames = c("chromosome","snp_id"),level = "subject")

  #Convert data to numeric:
  sim_data_long <-
    sim_data_long %>% mutate(
      value = factor(as.character(value),levels = structure_code)) %>%
    mutate(num_value = as.numeric(value))

  #Ensure order of SNPs are correct:
  sim_data_long$snp_id <- factor(as.character(sim_data_long$snp_id),levels = structure_snp_order)
  if(sum(is.na(sim_data_long$snp_id)) > 0){
    warning("At least 1 SNP found in simulated data which was not found in the sample structure file and have been removed.
            Use `keep_extra_snps = TRUE` to keep these extraneous SNPs in simulated data.")
  }

  sim_data_long <- sim_data_long[!is.na(sim_data_long$snp_id),]
  #Convert to matrix form:
  allele_data <- sim_data_long %>% dcast(Lsubject ~ snp_id + chromosome,value.var = "num_value")

  colnames(allele_data) <- NULL
  allele_data <- as.matrix(allele_data[,-1])
  if(is.null(sample_names)){
    allele_data <- cbind(paste0("SIM",1:nrow(allele_data)),1,1,allele_data)
  }else{
    allele_data <- cbind(sample_names,1,1,allele_data)
  }

  #Convert matrix to txt file:
  if("txt" %in% type){
    sink(file = paste0(output,".txt"))
    head <- paste0(structure_snp_order,collapse = "\t\t")
    cat(head)
    for(i in 1:nrow(allele_data)){
      cat("\n")
      for(j in 1:ncol(allele_data)){
        cat(allele_data[i,j])
        cat("\t")
      }
    }
    sink(file = NULL)
  }
  #Convert matrix to csv file:
  if("csv" %in% type){
    colnames(allele_data) <- c(
      "SAMPLE",
      "POP",
      "POPFLAG",
      {p <- rep("",2*length(structure_snp_order))
      for(i in 1:length(structure_snp_order)){
        p[2*i - 1] <- structure_snp_order[i]
      }
      p}
    )

    write.table(allele_data,file = paste0(output,".csv"),sep = ",",row.names = FALSE)
  }
}
