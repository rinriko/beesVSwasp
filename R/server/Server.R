sapply(list.files(pattern="[.]R$", path="R/server/function", full.names=TRUE), source);
#Ref :https://github.com/benmarwick/Interactive_PCA_Explorer
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
Server <- function(input, output, session) {
    observe({
    cat("\ninput$directory value:\n\n")
    print(input$directory)
  })

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session)
  # by setting `allowDirCreate = FALSE` a user will not be able to create a new directory
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

    
  output$directorypath <- renderPrint({
    if (is.integer(input$directory)) {
      cat("No directory has been selected (shinyDirChoose)")
    } else {
      parseDirPath(volumes, input$directory)
    }
  })
}
