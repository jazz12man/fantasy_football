### Setup ####
league_id = "1193762"
season_id = "2014"
shiny_directory = "/home/jazz12man/ShinyApps/ff14_premier_league/"
shared_football_directory = "/home/jazz12man/fantasy_football/"

### Source Librarys ####
library(shiny)
library(rCharts)
library(png)
library(RCurl)
library(XML)
library(data.table)
library(reshape2)

### Source Files - set league values ####
source("/home/jazz12man/fantasy_football/team_owners_table.R")
teams_table_in = teams_table_fun(league_id,season_id)
team_vals = teams_table_in$team_vals
teams_table = teams_table_in$teams_table
div_table_html = teams_table_in$div_table_html
source("/home/jazz12man/fantasy_football/get_settings.R")
settings = get_settings(league_id,season_id)
slots = settings$slots
slot_pos = settings$slot_pos
starters_table = settings$starters_table
positions_table = settings$positions_table
all_matchups = settings$all_matchups
scoring_period_table = settings$scoring_period_table
draft_date = settings$draft_date

scoring_period = which(scoring_period_table$end_date >= Sys.Date() & scoring_period_table$start_date <= Sys.Date()) - 1

google.colors = c("#3366cc","#dc3912","#ff9900","#109618","#990099","#0099c6","#dd4477","#66aa00","#b82e2e","#316395","#994499","#22aa99","#aaaa11","#6633cc","#e67300","#8b0707","#651067","#329262","#5574a6","#3b3eac","#b77322","#16d620","#b91383","#f4359e","#9c5935","#a9c413","#2a778d","#668d1c","#bea413","#0c5922","#743411")

### Updates ####
source(paste0(shared_football_directory,"scoring_leaders_table.R"))
source(paste0(shared_football_directory,"team_scoring_period_tables.R"))
source(paste0(shared_football_directory,"scoring_projections_table.R"))
source(paste0(shared_football_directory,"transactions_crawler.R"))