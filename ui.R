# ui Fantasy Football
library(rCharts)

shinyUI(pageWithSidebar(
  headerPanel("",
              tags$head(
                tags$link(rel="shortcut icon", href="http://dl.dropboxusercontent.com/u/14343406/football.png"),
                tags$link(rel="stylesheet", type="text/css", href="fantasyFootball.css"),
                tags$link(rel="stylesheet", type="text/css", href="http://fonts.googleapis.com/css?family=Cinzel+Decorative"),
                HTML('
<div style="padding: 10px 0px;">
<h1>Premier League</h1>
<br>
<h1 style="font-size:400%">Fantasy Football 14</h1>
</div>
<hr style="background-color: yellow">
<title>Premier FF14</title>
                     '
                )
              )),
  sidebarPanel(
    htmlOutput("ui_scoring_period_id"),
    htmlOutput("ui_selected_team")
    #     ,
    #     hr(),
    #     br(),
    #     actionButton("update_data","Update Data",icon=icon("bar-chart-o"))
  ),
  mainPanel(
    tabsetPanel(
      id="tabs",
      tabPanel(
        "Results",
        h2("Results"),
        showOutput("actual_rchart","nvd3"),
        h2("Power Rankings"),
        showOutput("power_ranking","nvd3"),
        h2("Current Standings"),
        htmlOutput("standings_table"),
        br(),
        br()
        # p("Come back next year!")
        
      ),
      tabPanel(
        "Team Charts",
        h2("Points Above Minimal Start"),
        p("This shows how many points were scored above or below the lowest valued starter at each position"),
        showOutput("above_start_rchart","nvd3"),
        h2("Optimal"),
        p("This shows which players on the roster could have provided more value during this scoring period"),
        plotOutput("optimal_switch",width=500,height=500),
        h2("Add/Drop"),
        p("Shows Add/Drops, Trades, etc. during scoring period"),
        dataTableOutput("add_drop_table"),
        showOutput("team_add_drop_period","d3_sankey")
      ),
      tabPanel(
        "Matchup Charts",
        htmlOutput("matchup_header"),
        plotOutput("matchup_prob_chart",width="400px", height="600px")
      ),
      tabPanel(
        "Top POS",
        h2("Top Scorers by Position"),
        htmlOutput("ui_selected_position"),
        showOutput("pos_rchart","nvd3"),
        h2("Top FAs"),
        dataTableOutput("top_FAs")
      ),
      tabPanel(
        "Trophy Corner",
        dataTableOutput("trophy"))
    )
  )
))