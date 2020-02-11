function(input, output, session) {

  tmpdir <- tempdir()


  output$contents_af <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    head(read.csv(inFile$datapath, header = TRUE))
  })

  output$contents_sim <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file2

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = TRUE)
  })

  output$anc_vec <- renderText(
    {
      tmpdir}
  )

  output$distPlot <- renderText({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton

    sim_details <- read.csv(input$file2$datapath, header = TRUE)
    sim_details[,1] <- as.character(sim_details[,1])

    example_data <- read.table(
      file = input$file3$datapath,
      sep = ",",
      header = TRUE)
    structure_snp_order <- colnames(example_data)[seq(4,92,2)]

    for(i in 1:nrow(sim_details)){
      anc_vec <- string_to_vector(sim_details[i,1])
      sim_data <-
        simulate_admixture(
          n_samples = sim_details[i,2],
          ancestor_pop_label = anc_vec,
          file = input$file1$datapath
        )
      output_name <- paste0("SIM",i)

      write_to_structure(
        sim_data = sim_data,
        example_structure_file = input$file3$datapath,
        output = paste0(tmpdir,"/",output_name),
        type = c("txt","csv"))
    }
    # Use isolate() to avoid dependency on input$obs
    paste0("Completed at: ",Sys.time())
  })

  output$downloadData.zip <- downloadHandler(
    filename = function() {
      paste('simulated-data.zip', sep='')
    },
    content = function(fname) {
      # fs <- c()
      setwd(tmpdir)
      # for (i in c(1,2,3,4,5)) {
      #   path <- paste0("sample_", i, ".csv")
      #   fs <- c(fs, path)
      #   write(i*2, path)
      # }
      sim_details <- read.csv(input$file2$datapath, header = TRUE)

      output_files <- paste0("SIM",1:nrow(sim_details))
      sink(file = paste0(tmpdir,"/","blah.txt"))
      print(paste0(tmpdir,"/",output_files,".csv"))
      sink(file = NULL)

      return(
        zip(zipfile=fname, files=c(paste0(output_files,".csv")))
      )
    }
  )

}
