sapply(list.files(pattern="[.]R$", path="R/ui/bs4function", full.names=TRUE), source);
DashBoard <- function(){
  bs4DashPage(
    # enable_preloader = TRUE,
    navbar = Navbar(),
    sidebar = Sidebar(),
    controlbar = Controlbar(),
    footer = Footer(),
    title = "Project",
    body = Body()
  )
}
