Body <- function() {
  bs4DashBody(
    useShinyjs(),
    br(),
    bs4TabItems(
      bs4TabItem(
        tabName = "test1",
        bs4TabCard(
          id = "tabcard",
          side = "left",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          bs4TabPanel(
            tabName = "item1",
            p("Here is ..."),
          ),
          bs4TabPanel(
            tabName = "item2",
            p("Here is ..."),
          )
        )
      ),
      bs4TabItem(
        tabName = "test2",
      )
    )
  )
}