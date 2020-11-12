sapply(list.files(pattern="[.]R$", path="R/ui", full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path="R/server", full.names=TRUE), source);

# install.packages(c('shiny','bs4Dash','DT','shinyWidgets','shinyjs','GGally','ggplot2','ggfortify'))

# if(interactive()){
  library(shiny)
  library(shinyFiles)
  library(bs4Dash)
  library(DT)
  library(shinyWidgets)
  library(shinyjs)
  library(GGally)
  library(ggplot2)
  library(ggfortify)
  
  shiny::shinyApp(
    ui = DashBoard(),
    server = Server
  )
# }
