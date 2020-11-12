Navbar <- function(..., skin = "dark", status = "pimary", border = TRUE,
                          sidebarIcon = "bars", compact = FALSE, controlbarIcon = "th",
                          leftUi = NULL, rightUi = NULL, fixed = FALSE){
  shiny::tags$nav(
    `data-fixed` = tolower(fixed),
    class = paste0(
      "main-header navbar navbar-expand navbar-", status,
      " navbar-", skin, if (isTRUE(border)) " border-bottom-0" else NULL,
      if (compact) " text-sm" else NULL
    ),
    
    # left sidebar elements
    shiny::tags$ul(
      class = "navbar-nav",
      
      # sidebar toggle (left)
      shiny::tags$li(
        class = "nav-item",
        shiny::tags$a(
          class = "nav-link",
          `data-widget` = "pushmenu",
          href = "#",
          "Choose your technique"
          
        )
      ),
      leftUi
    ),
    
    # in between content
    ...,
    

  )
}
