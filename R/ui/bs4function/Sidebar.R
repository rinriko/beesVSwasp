Sidebar <- function() {
  bs4DashSidebar(
       skin = "dark",
       bs4SidebarMenu(
         id = "sidebar_menu",
         bs4SidebarMenuItem(
           tabName = "test1",
           text = "TEST1",
           icon = "chart-bar"
         ),
         bs4SidebarMenuItem(
           tabName = "test2",
           text = "TEST2",
           icon = "grin-beam"
         )
       )
     )
}