% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_single_locus.R
\name{sim_single_locus}
\alias{sim_single_locus}
\title{Function which simulates mendelian inheritance for a single allele, beginning with a random draws of variants based on the population allele frequencies.}
\usage{
sim_single_locus(ancestor_pop_label, allele_freq)
}
\arguments{
\item{ancestor_pop_label}{A vector of integers giving the highest level of the family tree, which consists only of ancestors from pure genetic populations. Integers index the population, with order given by the order in which the population dominant allele frequencies are provided in the input allele frequencies file.}

\item{allele_freq}{A list of vectors of the population allele frequencies. Index must match the index of \code{ancestor_pop_label}.}
}
\value{
A matrix with two rows and a single column. The elements correspond to the two alleles for the resulting genotype at the given SNP. The values are either 1 or 2, and correspond to the two variants in order of the supplied allele frequency vectors.
}
\description{
The function proceeds as follows:
\enumerate{
\item{For each of the ancestors, using the ancestor's population index, draw two alleles randomly from the ancestor's population allele frequency.}
\item{Based on Mendelian Inheritance, for each generation, randomly and with equal probability draw one allele from the mother and one allele from the father. Therefore, each generation there will half the number of people/allele pairs.}
\item{Proceed until there is only one sample left.}
}
}
\examples{
#Single grandparent:
sim_single_locus(
ancestor_pop_label = c(2,1,1,1),
allele_freq = list(
c(0.1,0.9),c(0.9,0.1)
))
}
