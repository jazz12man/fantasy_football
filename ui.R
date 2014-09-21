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
      ### Scoring Period Summary ####
      tabPanel(
        "Scoring Period Summary",
        htmlOutput("results_header"),
        div(class="row",
            div(class="span6", showOutput("actual_rchart","nvd3"))),
        h2("Power Rankings"),
        div(class="row",
            div(class="span6", showOutput("power_ranking","nvd3"))),
        h2("Current Standings"),
        htmlOutput("standings_table"),
        br(),
        br()
        # p("Come back next year!")
        
      ),
      ### Scoring Period Team Details ####
      tabPanel(
        "Scoring Period Team Details",
        h2("Points Above Minimal Start"),
        p("How many points were scored above or below the lowest valued starter at each position"),
        div(class="row",
            div(class="span6", showOutput("above_start_rchart","nvd3"))),
        h2("Optimal Lineup"),
        p("Which players on the roster could have provided more value during this scoring period"),
        div(class="row",
            div(class="span6", plotOutput("optimal_switch",width=500,height=500))),
        h2("Add/Drop Summary"),
        p("Team Add/Drops, Trades, etc. during scoring period"),
        dataTableOutput("add_drop_table")#,
        #         showOutput("team_add_drop_period","d3_sankey")
      ),
      ### Scoring Period Top POS ####
      tabPanel(
        "Scoring Period Top POS",
        h2("Top Scorers by Position"),
        htmlOutput("ui_selected_position"),
        div(class="row",
            div(class="span6", showOutput("pos_rchart","nvd3"))),
        h2("Top FAs"),
        dataTableOutput("top_FAs")
      ),
      ### Scoring Period Trophy Corner ####
      tabPanel(
        "Scoring Period Trophy Corner",
        h2("Best and Worst of the Week"),
        dataTableOutput("trophy")
      ),
      ### Matchup Odds ####
      tabPanel(
        "Matchup Odds",
        htmlOutput("matchup_header"),
        plotOutput("matchup_prob_chart",width="400px", height="600px"),
        h2("All Matchups Odds"),
        div(class="row",
            div(class="span6",showOutput("season_win_probs","nvd3")))
      ),
      ### Playoffs Odds ####
      tabPanel(
        "Playoffs Odds",
        h2("Playoff Odds"),
        div(class="row",
            div(class="span6", plotOutput("bracket_chart", width = "600px", height = "400px")),
            div(class="span6", showOutput("playoff_donut","nvd3"))
        ),
        h2("Likely Final Standings"),
        div(class="row",
            div(class="span6", showOutput("final_standings_probs","nvd3"))
        ),
        h2("All Teams Odds"),
        plotOutput("playoff_champ_odds_all")
      ),
      ### Methodology ####
      tabPanel(
        "Methodology",
        h2("Coming Soon"),
        HTML('<a target="_blank" href="https://github.com/jazz12man/fantasy_football"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67" alt="Fork me on GitHub" data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"></a>')
      )
    )
  )
))
