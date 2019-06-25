shinyApp(
  
  ui = fluidPage(
    textInput(inputId = "form_id", label = "Form ID", value = "", width = NULL,
              placeholder = "Input the Ona ID of the form your data lives in"),
    textInput(inputId = "username", label = "Username", value = "", width = NULL,
              placeholder = "Input your Ona username"),
    textInput(inputId = "password", label = "Password", value = "", width = NULL,
              placeholder = "Input your Ona password (this will not be stored)"),
    downloadButton("report", "Generate report")
  ),
  
  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.docx",
      content = function(file) {
        # Copy the report RMD and all reference docs to a temporary directory before processing 
        # it, in case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "run_report.Rmd")
        tempStylesdoc <- file.path(tempdir(), "wordStylesReferenceMC.docx")
        tempDatadict <- file.path(tempdir(), "AutoPDMReport_datadictionary.csv")
        tempCodebook <- file.path(tempdir(), "AutoPDMReport_codebook.csv")
        file.copy("run_report.Rmd", tempReport, overwrite = TRUE)
        file.copy("wordStylesReferenceMC.docx", tempStylesdoc, overwrite = TRUE)
        file.copy("AutoPDMReport_datadictionary.csv", tempDatadict, overwrite = TRUE)
        file.copy("AutoPDMReport_codebook.csv", tempCodebook, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(form_id = input$form_id,
                       username = input$username, 
                       password = input$password)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)