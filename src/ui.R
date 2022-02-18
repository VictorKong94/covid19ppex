dashboardPage(
  header = dashboardHeader(
    title = "PPE Exchange",
    titleWidth = "325px",
    uiOutput("navigation_bar", style = "width: auto; margin: 0 auto;")
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    tags$head(
      tags$script(src = "js.cookie.js"),
      tags$script(src = "script.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ),
    useShinyjs(),
    extendShinyjs(text = jsCode),
    uiOutput("body")
  ),
  skin = "red"
)
