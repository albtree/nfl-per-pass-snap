library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(reactable)
library(hrbrthemes)

bcl <- read.csv("rec_per_snap_data.csv", stringsAsFactors = FALSE)
reactable_df <- bcl %>% select(full_name, position, posteam, season, years_exp, pass_snaps, rec_yards_total,
                               targets_total, receiving_epa_total, yards_per_pass_snap, targets_per_pass_snap, rec_epa_per_pass_snap)
ui <- fluidPage(
  titlePanel("Yards, Receiving EPA, and Targets per Pass snap"),
  sidebarLayout(
    sidebarPanel(h4("Data via nflverse. App by TAlbTree"),
      uiOutput("full_nameOutput"),
                 sliderInput("years_exp", "Year in League", min = 1, max = 20,
                             value = c(1, 20)),
                 sliderInput("season", "Season", min = 2016, max = 2022,
                             value = c(2022, 2022)),
                 sliderInput("pass_snaps", "Pass Snaps", min = 0, max = 700,
                             value = c(150, 700))),
    mainPanel(plotOutput("ypps_plot"),
              br(),
              plotOutput("epapps_plot"),
              br(),
              reactableOutput("table"))
  )
)
server <- function(input, output, session){output$full_nameOutput <- renderUI({
  sliderInput("pass_snaps", "Pass Snaps", min = 0, max = 700, value = c(150, 700))
  sliderInput("years_exp", "Year in League", min = 1, max = 20, value = c(1, 20))
  sliderInput("season", "Season", min = 2016, max = 2022, value = c(2022, 2022))
  selectInput("full_name", "Player",
              sort(unique(bcl$full_name)), multiple = TRUE,
              selected = "Keenan Allen")
    })
{observeEvent}
filtered <- reactive({if (is.null(input$full_name)) {return(NULL)
    }
  bcl %>%
    filter(pass_snaps >= input$pass_snaps[1],
           pass_snaps <= input$pass_snaps[2],
           years_exp >= input$years_exp[1],
           years_exp <= input$years_exp[2],
           season >= input$season[1],
           season <= input$season[2],
           full_name %in% unique(input$full_name))})
  
output$ypps_plot <- renderPlot({if (is.null(filtered())) {return()}
  ggplot(filtered(), aes(x = targets_per_pass_snap, y = yards_per_pass_snap)) +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.06, alpha = 0.65, position = "jitter") +
    geom_label_repel(aes(label = name_season, colour = position), max.overlaps = 15, size = 3) +
    labs(title = "Targets & Yards per Pass Snap",
         x = "Targets per Pass Snap",
         y = "Receiving Yards per Pass Snap",
         caption = "Data = nflverse. Author = @TAlbTree")+xlim(0.1,0.35)+ylim(0.5,3.5)+
    theme_ipsum_rc()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 12))
})

output$epapps_plot <- renderPlot({if (is.null(filtered())) {return()}
  ggplot(filtered(), aes(x = targets_per_pass_snap, y = rec_epa_per_pass_snap)) +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.06, alpha = 0.65, position = "jitter") +
    geom_label_repel(aes(label = name_season, color = position), max.overlaps = 15, size = 3) +

    labs(title = "Targets & Receiving EPA per Pass Snap",
         x = "Targets per Pass Snap",
         y = "Receiving EPA per Pass Snap",
         caption = "Data = nflverse. Author = @TAlbTree Link: https://albtree.shinyapps.io/routes_shiny/")+xlim(0.1,0.35)+ylim(-0.25,0.35)+
    theme_ipsum_rc()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 12))
})

dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
  function(values, name) {
    dataListId <- sprintf("%s-%s-list", tableId, name)
    tagList(
      tags$input(
        type = "text",
        list = dataListId,
        oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      ),
      tags$datalist(
        id = dataListId,
        lapply(unique(values), function(value) tags$option(value = value))
      )
    )
  }
}

output$table <- renderReactable({reactable(reactable_df, 
                                           columns = list(
                                             full_name = colDef(name = "Name"),
                                             position = colDef(name = "Position"),
                                             posteam = colDef(name = "Team"),
                                             season = colDef(name = "Season"),
                                             years_exp = colDef(filterMethod = JS("function(rows, columnId, filterValue) {
                                                                                  return rows.filter(function(row) {
                                                                                  return row.values[columnId] == filterValue
                                                                                  })
                                                                                  }"),
                                                                name = "Years Exp.",),
                                             pass_snaps = colDef(name = "Pass Snaps (min)",
                                                                 filterMethod = JS("function(rows, columnId, filterValue) {
                                                                                  return rows.filter(function(row) {
                                                                                  return row.values[columnId] >= filterValue
                                                                                  })
                                                                                  }")),
                                             rec_yards_total = colDef(name = "Rec. Yards"),
                                             targets_total = colDef(name = "Targets"),
                                             receiving_epa_total = colDef(name = "Rec. EPA"),
                                             yards_per_pass_snap = colDef(name = "Yards PPS"),
                                             targets_per_pass_snap = colDef(name = "Targets PPS"),
                                             rec_epa_per_pass_snap = colDef(name = "Rec. EPA PPS")),
                                           filterable = TRUE, showPageSizeOptions = TRUE, minRows = 10)})
}

shinyApp(ui = ui, server = server)

#xlim(0, 0.4) +
 # ylim(-0.3, 0.35)+
#    xlim(0, 0.4) +
#ylim(0, 4.5)+
