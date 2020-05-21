navbarPage("Admixture Simulation",
           tabPanel("Data upload",
                    fileInput("file1", "Upload the allele frequencies data file:",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                    "These data is used to simulate the ancestor genotypes.",
                    tableOutput("contents_af"),
                    fileInput("file2", "Upload the simulation details:",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    ),
                    "The first column is a string of integers describing the population origins of the ancestors. 1 corresponds to the first population listed in the allele frequencies, 2 to the second, and so on.",
                    tableOutput("contents_sim"),
                    fileInput("file3", "Upload some example STRUCTURE input data:",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                    "An example of the STRUCTURE input is required so the output files containing simulated samples are correctly formatted for input to STRUCTURE."
           ),
           tabPanel("Simulate",
                    "Below is the temporary folder for the simulated data, currently included for testing purposes.",
                    verbatimTextOutput("anc_vec"),
                    "Press the button below to simulate data.\n",
                    actionButton("goButton", "Simulate!"),
                    verbatimTextOutput("distPlot"),
                    downloadButton('downloadData.zip', 'Download')
           )
)
