### Setup ##########
## Source Librarys
library(shiny)
library(rCharts)
library(png)
library(RCurl)
library(XML)
library(data.table)
library(reshape2)

## Set League ID
# league_id = "1193762"
league_id = "1193762"
season_id = "2014"

## Set Directorys
shiny_directory = "/home/jazz12man/ShinyApps/ff14_premier_league/"
shared_football_directory = "/home/jazz12man/fantasy_football/"

## Source Files - set league values
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
<<<<<<< HEAD
no_playoff_teams = settings$no_playoff_teams
byes = settings$byes
weeks_per_round = settings$weeks_per_round
no_playoff_rounds = settings$no_playoff_rounds
source("/home/jazz12man/fantasy_football/image_points.R")
source("/home/jazz12man/fantasy_football/other_functions.R")
source(paste0(shared_football_directory,"bracket_creator.R"))
=======
source("/home/jazz12man/fantasy_football/image_points.R")
source("/home/jazz12man/fantasy_football/other_functions.R")
>>>>>>> 6df677896335089a14190efbc967c493299df873

## load data
load(file=paste0(shiny_directory,"www/data_files/team_tables_all.Rdata"))
load(file=paste0(shiny_directory,"www/data_files/team_lookup.Rdata"))
load(file=paste0(shiny_directory,"/www/data_files/team_tables_all.Rdata"))

<<<<<<< HEAD
load(file=paste0(shiny_directory,"/www/data_files/playoff_results_all.Rdata"))
load(file=paste0(shiny_directory,"/www/data_files/final_standings_all.Rdata"))
load(file=paste0(shiny_directory,"/www/data_files/results_all.Rdata"))

=======
>>>>>>> 6df677896335089a14190efbc967c493299df873
scoring_leaders_all = read.csv(file=paste0(shiny_directory,"www/data_files/scoring_leaders_all.csv"),
                               stringsAsFactors=F)
projections_all = read.csv(file=paste0(shiny_directory,"www/data_files/projections_all.csv"),
                           stringsAsFactors=F)
actual_all = read.csv(file=paste0(shiny_directory,"www/data_files/team_tables_actual_all.csv"),
                      stringsAsFactors=F)
bench_all = read.csv(file=paste0(shiny_directory,"www/data_files/team_tables_bench_all.csv"),
                     stringsAsFactors=F)
optimal_all = read.csv(file=paste0(shiny_directory,"www/data_files/team_tables_optimal_all.csv"),
                       stringsAsFactors=F)
modified_all = read.csv(file = paste0(shiny_directory,"www/data_files/modifed_roster_moves.csv"),
                        stringsAsFactors=F)
transactions_all = read.csv(file = paste0(shiny_directory,"www/data_files/transactions_summary.csv"),
                            stringsAsFactors=F)
scoring_period_matchups = read.csv(file = paste0(shiny_directory,"www/data_files/scoring_period_matchups.csv"),
                                   stringsAsFactors=F)

## google colors
google.colors = c("#3366cc","#dc3912","#ff9900","#109618","#990099","#0099c6","#dd4477","#66aa00","#b82e2e","#316395","#994499","#22aa99","#aaaa11","#6633cc","#e67300","#8b0707","#651067","#329262","#5574a6","#3b3eac","#b77322","#16d620","#b91383","#f4359e","#9c5935","#a9c413","#2a778d","#668d1c","#bea413","#0c5922","#743411")

## set current scoring period
current_scoring_period = which(scoring_period_table$end_date >= Sys.Date() & scoring_period_table$start_date <= Sys.Date()) - 1

## Set Shiny Options
options(shiny.reactlog=FALSE)

### Start Shiny Server ##########
shinyServer(function(input, output, session) {
  ### ~~~ UI ##########
  output$ui_scoring_period_id = renderUI(
    selectInput("scoring_period_id","Scoring Period:",all_matchups,all_matchups[current_scoring_period])
  )
  
  output$ui_selected_team = renderUI(
    radioButtons("selected_team","Select Team:",team_vals)
  )
  
  output$ui_selected_position = renderUI(
    selectInput("selected_pos","Select Position:",slot_pos,selected=slot_pos[1])
  )
  
  
  
  ### ~~ Reactives ##########
  
  ### add_drop reactive ##########
  add_drop = reactive({
    trans = transactions_all
    trans = trans[trans$team_id==input$selected_team,]
    in_dates = scoring_period_table[scoring_period_table$scoring_period_id==input$scoring_period_id,c("start_date","end_date")]
    
    trans_period = trans[which(as.numeric(as.Date(trans$date_time))>=as.numeric(in_dates[1]) &
                                 as.numeric(as.Date(trans$date_time))<=as.numeric(in_dates[2])),]
    
    trans_period = data.frame(lapply(trans_period,as.character),stringsAsFactors=F)
    team_abbr = trans_period$abbr[1]
    ufrom =  unique(trans_period$from)
    ufrom = ufrom[ufrom!=team_abbr]
    uto = unique(trans_period$to)
    uto = uto[uto!=team_abbr]
    
    from_to1 = 
      lapply(ufrom, function(x) {
        players = trans_period[trans_period$from==x,"player_name"]
        values = scoring_leaders_all$PTS[match(players,scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id,"PLAYER"])]
        values[is.na(values)] = 0.5
        data.frame(source=players,
                   target=paste("Added from",x),
                   value=values,
                   stringsAsFactors=F)
      })
    from_to2 = 
      lapply(ufrom, function(x) {
        data.frame(source=paste("Added from",x),
                   target=team_abbr,
                   value=sum(from_to1[[1]]$value[from_to1[[1]]$target==paste("Added from",x)]),
                   stringsAsFactors=F)
      })
    from_to4 = 
      lapply(uto, function(x) {
        players=trans_period[trans_period$to==x,"player_name"]
        values = scoring_leaders_all$PTS[match(players,scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id,"PLAYER"])]
        values[is.na(values)] = 0.5
        data.frame(source=paste("Dropped to",x),
                   target=players,
                   value=values,
                   stringsAsFactors=F)
      })
    from_to3 = 
      lapply(uto, function(x) {
        data.frame(source=team_abbr,
                   target=paste("Dropped to",x),
                   value = sum(from_to4[[1]]$value[from_to4[[1]]$source==paste("Dropped to",x)]),
                   stringsAsFactors=F)
      })
    from_to_all = rbind(
      do.call(rbind,from_to1),
      do.call(rbind,from_to2),
      do.call(rbind,from_to3),
      do.call(rbind,from_to4))
    
    return(list(
      from_to_all=from_to_all,
      trans_period=trans_period))
  })
  
  ### Reactve Top Transactions #########
  top_transactions = reactive({
    trans = transactions_all
    trans$date_time = as.Date(trans$date_time)
    
    in_dates = scoring_period_table[scoring_period_table$scoring_period_id==input$scoring_period_id,c("start_date","end_date")]
    
    trans_period = trans[which(as.numeric(trans$date_time)>=as.numeric(in_dates[1]) &
                                 as.numeric(trans$date_time)<=as.numeric(in_dates[2])),]
    
    scoring_leaders_period = scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id,]
    trans_period = merge(trans_period,
                         scoring_leaders_period[,c("PLAYER","TEAM","POS","PTS")],
                         by.x=c("player_name","player_team","player_pos"),
                         by.y=c("PLAYER","TEAM","POS"),
                         all.x=T)
    top_add = trans_period[trans_period$player_trans=="added",]
    top_add = top_add[order(top_add$PTS,decreasing=T),]
    top_drop = trans_period[trans_period$player_trans=="dropped",]
    top_drop = top_drop[order(top_drop$PTS,decreasing=T),]
    return(list(top_add=top_add,
                top_drop=top_drop))
  })
  
  ### ~~ Observes ##########
  observe({
    #     input$update_data
    #     A = isolate({
    #       if(!is.null(input$scoring_period_id)) {
    #         cat("OBSERVE ISOLATE IS RUNNING!\n")
    #         scoring_period_id <<- gsub("scoring_period_","",input$scoring_period_id)
    #         source("/home/jazz12man/fantasy_football/scoring_leaders_table.R")
    #         source("/home/jazz12man/fantasy_football/team_scoring_period_tables.R")
    #         cat("OBSERVE ISOLATE IS DONE!\n")
    #       }
    #     })
  })
  
  
  ### ~~ Outputs ##########
  ### Output actual_rchart ##########
  output$actual_rchart = renderChart({
<<<<<<< HEAD
    if(!is.null(input$scoring_period_id)) {
      actual_all_i = actual_all[actual_all$scoring_period==input$scoring_period_id,]
      actual_all_i$summary2 = as.character(actual_all_i$summary2)
      actual_all_i$summary3 = as.character(actual_all_i$summary3)
      actual_all_i$summary2[is.na(actual_all_i$summary2)] = ""
      actual_all_i$summary3[is.na(actual_all_i$summary3)] = ""
      
      #     cat(paste0(colnames(actual_all_i),collapse="~|~"))
      actual_all_i$OWNER = teams_table$nicknames[match(actual_all_i$team_id,teams_table$team_no)]
      #       if(sum(is.na(actual_all_i$PTS))!=nrow(actual_all_i)) {
      n3 = nPlot(PTS~OWNER, group="SLOT", data=actual_all_i, type="multiBarChart")
      n3$addParams(dom = "actual_rchart")
      n3$chart(tooltipContent = "#! function(key, x, y, e){
               return '<h5 style=\"background-color: '+ e.point.color +'; color: white;\">' + e.point.PLAYER + '</h5>' +
               '<table><td>' +
               '<p><img src=\"player_images/' + e.point.player_id + '.png\" width=40px></p>' +
               '<div style=\"text-align:center;font-weight:bold;font-size:90%;\">' + e.point.PTS + ' PTS' + '</div>' +
               '</td><td>' +
               '<div word-wrap: break-word;>' + e.point.summary1 +
               '</div><div>' + e.point.summary2 +
               '</div><div>' + e.point.summary3 +
               '</div>' +
               '<div style=\"font-size:75%;text-align:left\">' + e.point.OWNER +
               '</div></td></table>';
    } !#")
      n3$chart(reduceXTicks = FALSE)
      n3$chart(stacked = ifelse(sum(is.na(dcast(actual_all_i,SLOT~OWNER,value.var="OWNER")))==0,TRUE,FALSE))
      n3$chart(showControls = TRUE)
      n3$chart(color=google.colors[1:16])
      return(n3)            
    } else {
      data_out = data.frame(None="No Data Available",PTS = 1)
      n3 = nPlot(PTS~None,data=data_out, type="multiBarChart")
      n3$addParams(dom = "actual_rchart")
      n3$chart(color = "#! function(d){ return '#272487'} !#")
    }
  })
  
  ### Output power ranking ##########
  output$power_ranking = renderChart({
    actual_pts = dcast(actual_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
    optimal_pts = dcast(optimal_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
    actual_pts_i = actual_pts[,input$scoring_period_id]
    optimal_pts_i = optimal_pts[,input$scoring_period_id]
    names(actual_pts_i) = teams_table$nicknames[match(actual_pts$team_id,teams_table$team_no)]
    names(optimal_pts_i) = teams_table$nicknames[match(optimal_pts$team_id,teams_table$team_no)]
    power_rankings = actual_pts_i*2/3 + optimal_pts_i*1/3
    power_rankings = round(power_rankings/max(power_rankings)*100)
    power_data = data.frame(Rank=power_rankings,
                            TEAM=names(power_rankings),
                            Optimal = optimal_pts_i,
                            Actual = actual_pts_i)
    power_data = power_data[order(power_rankings),]
    cat(nrow(power_data),"nrow(power_data)\n")
    cat(unlist(power_data),"unlist(power_data)\n")
    
    #     if(sum(is.na(power_data$Rank))!=nrow(power_data)) {
    n1 = nPlot(Rank~TEAM, data=power_data, type="multiBarChart")
    n1$addParams(dom = "power_ranking")
    n1$chart(tooltipContent = "#! function(key, x, y, e){
=======
    #     if(!is.null(input$scoring_period_id)) {
    actual_all_i = actual_all[actual_all$scoring_period==input$scoring_period_id,]
    actual_all_i$summary2 = as.character(actual_all_i$summary2)
    actual_all_i$summary3 = as.character(actual_all_i$summary3)
    actual_all_i$summary2[is.na(actual_all_i$summary2)] = ""
    actual_all_i$summary3[is.na(actual_all_i$summary3)] = ""
    
    #     cat(paste0(colnames(actual_all_i),collapse="~|~"))
    actual_all_i$OWNER = teams_table$nicknames[match(actual_all_i$team_id,teams_table$team_no)]
    #       if(sum(is.na(actual_all_i$PTS))!=nrow(actual_all_i)) {
    n3 = nPlot(PTS~OWNER, group="SLOT", data=actual_all_i, type="multiBarChart")
    n3$addParams(dom = "actual_rchart")
    n3$chart(tooltipContent = "#! function(key, x, y, e){
             return '<h5 style=\"background-color: '+ e.point.color +'; color: white;\">' + e.point.PLAYER + '</h5>' +
             '<table><td>' +
             '<p><img src=\"player_images/' + e.point.player_id + '.png\" width=40px></p>' +
             '<div style=\"text-align:center;font-weight:bold;font-size:90%;\">' + e.point.PTS + ' PTS' + '</div>' +
             '</td><td>' +
             '<div word-wrap: break-word;>' + e.point.summary1 +
             '</div><div>' + e.point.summary2 +
             '</div><div>' + e.point.summary3 +
             '</div>' +
             '<div style=\"font-size:75%;text-align:left\">' + e.point.OWNER +
             '</div></td></table>';
  } !#")
    n3$chart(reduceXTicks = FALSE)
    n3$chart(stacked = ifelse(sum(is.na(dcast(actual_all_i,SLOT~OWNER,value.var="OWNER")))==0,TRUE,FALSE))
    n3$chart(showControls = TRUE)
    n3$chart(color=google.colors[1:16])
    #       } else {
    #         data_out = data.frame(None="No Data Available",PTS = 1)
    #         n3 = nPlot(PTS~None,data=data_out, type="multiBarChart")
    #         n3$addParams(dom = "actual_rchart")
    #         n3$chart(color = "#! function(d){ return '#272487'} !#")
    #       }
    return(n3)            
    #     }
})

### Output power ranking ##########
output$power_ranking = renderChart({
  actual_pts = dcast(actual_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
  optimal_pts = dcast(optimal_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
  actual_pts_i = actual_pts[,input$scoring_period_id]
  optimal_pts_i = optimal_pts[,input$scoring_period_id]
  names(actual_pts_i) = teams_table$nicknames[match(actual_pts$team_id,teams_table$team_no)]
  names(optimal_pts_i) = teams_table$nicknames[match(optimal_pts$team_id,teams_table$team_no)]
  power_rankings = actual_pts_i*2/3 + optimal_pts_i*1/3
  power_rankings = round(power_rankings/max(power_rankings)*100)
  power_data = data.frame(Rank=power_rankings,
                          TEAM=names(power_rankings),
                          Optimal = optimal_pts_i,
                          Actual = actual_pts_i)
  power_data = power_data[order(power_rankings),]
  cat(nrow(power_data),"nrow(power_data)\n")
  cat(unlist(power_data),"unlist(power_data)\n")
  
  #     if(sum(is.na(power_data$Rank))!=nrow(power_data)) {
  n1 = nPlot(Rank~TEAM, data=power_data, type="multiBarChart")
  n1$addParams(dom = "power_ranking")
  n1$chart(tooltipContent = "#! function(key, x, y, e){
>>>>>>> 6df677896335089a14190efbc967c493299df873
           return '<h5 style=\"background-color: #272487; color: white;\">' + e.point.TEAM + '</h5>' +
           '<div>Power Ranking: ' + e.point.Rank +
           '</div><div>Actual Points: ' + e.point.Actual +
           '</div><div>Optimal Points: ' + e.point.Optimal + 
           '</div>';
} !#")
<<<<<<< HEAD
    n1$chart(color=rep("#272487",nrow(power_data)))
    n1$chart(reduceXTicks = FALSE)
    n1$chart(stacked = FALSE)
    n1$chart(showControls = FALSE)
    #     n1$xAxis(staggerLabels = TRUE) 
    #     } else {
    #       data_out = data.frame(None="No Data Available",PTS = 1)
    #       n1 = nPlot(PTS~None,data=data_out, type="multiBarChart")
    #       n1$addParams(dom = "power_ranking")
    #       n1$chart(color = "#! function(d){ return '#272487'} !#")
    #     }
    return(n1)
  })
  
  ### Output above_start_rchart ##########
  output$above_start_rchart = renderChart({
    actual_all_i_team = actual_all[actual_all$scoring_period==input$scoring_period_id &
                                     actual_all$team_id==input$selected_team,]
    actual_all_i_team$color = ifelse(actual_all_i_team$PTS_abv_repl>0,"#248727","#872724")
    actual_all_i_team$PTS_abv_repl[is.na(actual_all_i_team$PTS_abv_repl)] = 0
    actual_all_i_team$summary2 = as.character(actual_all_i_team$summary2)
    actual_all_i_team$summary3 = as.character(actual_all_i_team$summary3)
    actual_all_i_team$summary2[is.na(actual_all_i_team$summary2)] = ""
    actual_all_i_team$summary3[is.na(actual_all_i_team$summary3)] = ""
    if(sum(is.na(actual_all_i_team$PTS))!=nrow(actual_all_i_team)) {
      n2 = nPlot(PTS_abv_repl~SLOT, data=actual_all_i_team, type="multiBarChart")
      n2$addParams(dom = "above_start_rchart")
      n2$chart(tooltipContent = "#! function(key, x, y, e){
=======
  n1$chart(color=rep("#272487",nrow(power_data)))
  n1$chart(reduceXTicks = FALSE)
  n1$chart(stacked = FALSE)
  n1$chart(showControls = FALSE)
  #     n1$xAxis(staggerLabels = TRUE) 
  #     } else {
  #       data_out = data.frame(None="No Data Available",PTS = 1)
  #       n1 = nPlot(PTS~None,data=data_out, type="multiBarChart")
  #       n1$addParams(dom = "power_ranking")
  #       n1$chart(color = "#! function(d){ return '#272487'} !#")
  #     }
  return(n1)
  })

### Output above_start_rchart ##########
output$above_start_rchart = renderChart({
  actual_all_i_team = actual_all[actual_all$scoring_period==input$scoring_period_id &
                                   actual_all$team_id==input$selected_team,]
  actual_all_i_team$color = ifelse(actual_all_i_team$PTS_abv_repl>0,"#248727","#872724")
  actual_all_i_team$PTS_abv_repl[is.na(actual_all_i_team$PTS_abv_repl)] = 0
  actual_all_i_team$summary2 = as.character(actual_all_i_team$summary2)
  actual_all_i_team$summary3 = as.character(actual_all_i_team$summary3)
  actual_all_i_team$summary2[is.na(actual_all_i_team$summary2)] = ""
  actual_all_i_team$summary3[is.na(actual_all_i_team$summary3)] = ""
  if(sum(is.na(actual_all_i_team$PTS))!=nrow(actual_all_i_team)) {
    n2 = nPlot(PTS_abv_repl~SLOT, data=actual_all_i_team, type="multiBarChart")
    n2$addParams(dom = "above_start_rchart")
    n2$chart(tooltipContent = "#! function(key, x, y, e){
>>>>>>> 6df677896335089a14190efbc967c493299df873
             return '<h5 style=\"background-color: '+ e.point.color +'; color: white;\">' + e.point.PLAYER + '</h5>' +
             '<table><td>' +
             '<p><img src=\"player_images/' + e.point.player_id + '.png\" width=40px></p>' +
             '<div style=\"text-align:center;font-weight:bold;font-size:90%;\">' + e.point.PTS + ' PTS' + '</div>' +
             '</td><td>' +
             '<div word-wrap: break-word;>' + e.point.summary1 +
             '</div><div>' + e.point.summary2 +
             '</div><div>' + e.point.summary3 +
             '</div>' +
             '<div style=\"font-size:75%;text-align:left\">' + e.point.TEAM +
             '</div></td></table>';
  } !#")
<<<<<<< HEAD
      n2$chart(reduceXTicks = FALSE)
      n2$chart(stacked = FALSE)
      n2$chart(showControls = FALSE)
      n2$chart(color=ifelse(actual_all_i_team$PTS_abv_repl>0,"#248727","#872724"))
    } else {
      data_out = data.frame(None="No Data Available",PTS = 1)
      n2 = nPlot(PTS~None,data=data_out, type="multiBarChart")
      n2$addParams(dom = "above_start_rchart")
      n2$chart(color = "#! function(d){ return '#272487'} !#")
    }
    return(n2)      
  })
  
  ### Output pos_rChart ##########
  output$pos_rchart = renderChart({
    scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id &
                                              scoring_leaders_all$POS==input$selected_pos,]
    if(nrow(scoring_leaders_i)>0) {
      scoring_leaders_i$color = "#272487"
      scoring_laders_i = scoring_leaders_i[scoring_leaders_i$PTS>0,]
      n4 = nPlot(PTS~PLAYER, data=scoring_leaders_i, type="multiBarChart")
      n4$addParams(dom = "pos_rchart")
      n4$chart(tooltipContent = "#! function(key, x, y, e){
=======
    n2$chart(reduceXTicks = FALSE)
    n2$chart(stacked = FALSE)
    n2$chart(showControls = FALSE)
    n2$chart(color=ifelse(actual_all_i_team$PTS_abv_repl>0,"#248727","#872724"))
} else {
  data_out = data.frame(None="No Data Available",PTS = 1)
  n2 = nPlot(PTS~None,data=data_out, type="multiBarChart")
  n2$addParams(dom = "above_start_rchart")
  n2$chart(color = "#! function(d){ return '#272487'} !#")
}
return(n2)      
})

### Output pos_rChart ##########
output$pos_rchart = renderChart({
  scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id &
                                            scoring_leaders_all$POS==input$selected_pos,]
  if(nrow(scoring_leaders_i)>0) {
    scoring_leaders_i$color = "#272487"
    scoring_laders_i = scoring_leaders_i[scoring_leaders_i$PTS>0,]
    n4 = nPlot(PTS~PLAYER, data=scoring_leaders_i, type="multiBarChart")
    n4$addParams(dom = "pos_rchart")
    n4$chart(tooltipContent = "#! function(key, x, y, e){
>>>>>>> 6df677896335089a14190efbc967c493299df873
             return '<h5 style=\"background-color: '+ e.point.color +'; color: white;\">' + e.point.PLAYER + '</h5>' +
             '<table><td>' +
             '<p><img src=\"player_images/' + e.point.player_id + '.png\" width=40px></p>' +
             '<div style=\"text-align:center;font-weight:bold;font-size:90%;\">' + e.point.PTS + ' PTS' + '</div>' +
             '</td><td>' +
             '<div word-wrap: break-word;>' + e.point.summary1 +
             '</div><div>' + e.point.summary2 +
             '</div><div>' + e.point.summary3 +
             '</div>' +
             '<div style=\"font-size:75%;text-align:left\">' + e.point.TEAM +
             '</div></td></table>';
  } !#")
<<<<<<< HEAD
      n4$chart(reduceXTicks = FALSE)
      n4$chart(stacked = FALSE)
      n4$chart(showControls = FALSE)
      #       n4$xAxis(NULL, replace = T)
      #       n4$xAxis(tickFormat = 
      #                  "#!function (d) {
      #             return NULL;
      #           }!#"
      #       )
      n4$chart(color=rep("#272487",nrow(scoring_leaders_i)))
    } else {
      data_out = data.frame(None="No Data Available",PTS = 1)
      n4 = nPlot(PTS~None,data=data_out, type="multiBarChart")
      n4$addParams(dom = "pos_rchart")
      n4$chart(color = "#! function(d){ return '#272487'} !#")
    }
    return(n4)
  })
  
  ### Output top_FAs #########
  output$top_FAs = renderDataTable({
    scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id,]
    top_FAs = scoring_leaders_i[which(scoring_leaders_i$TYPE %in% c("FA","WA (Wed)", "WA (Tue)", "WA (Thur)")),]
    top_FAs = top_FAs[,c("PLAYER","TEAM","POS","PTS","PTS_abv_repl")]
    top_FAs = top_FAs[order(top_FAs$PTS_abv_repl,decreasing=T),]
    names(top_FAs) = c("Player","Team","Pos","PTS","PTS Above Replacement")
    return(top_FAs)
  })
  
  ### Output Optimal Switch ##########
  output$optimal_switch = renderPlot({
    actual_ij = actual_all[actual_all$scoring_period==input$scoring_period_id &
                             actual_all$team_id==input$selected_team,]
    bench_ij = bench_all[bench_all$scoring_period==input$scoring_period_id &
                           bench_all$team_id==input$selected_team,]
    optimal_ij = optimal_all[optimal_all$scoring_period==input$scoring_period_id &
                               optimal_all$team_id==input$selected_team,]
    w_act_out = which(!actual_ij$PLAYER %in% optimal_ij$PLAYER)
    w_opt_in = which(!optimal_ij$PLAYER %in% actual_ij$PLAYER)
    if(length(w_act_out)>0) {
      actual_ij = actual_ij[w_act_out,]
      optimal_ij = optimal_ij[w_opt_in,]
      optimal_ij$player_id = bench_ij$player_id[match(optimal_ij$PLAYER,bench_ij$PLAYER)]
      actual_ij$player_id[is.na(actual_ij$player_id)] = "nfl"
      optimal_ij$player_id[is.na(optimal_ij$player_id)] = "nfl"
      
      max_val = max(c(6,nrow(actual_ij)))
      par(mar=c(0,0,0,0))
      par(bg = "#ffffff")
      
      plot(0,type="n",ylab="",xlab="",axes=F,xlim=c(.8,2.2),ylim=c(0,max_val+0.2))
      text(c(1,1.5,2),rep(max_val,3),c("Started","Points Lost","Bench"),cex=1.5)
      for(i in 1:nrow(actual_ij)) {
        a=readPNG(paste0("~/ShinyApps/fantasy_football_14/www/player_images/",actual_ij$player_id[i],".png"))
        b=readPNG(paste0("~/ShinyApps/fantasy_football_14/www/player_images/",optimal_ij$player_id[i],".png"))
        image_points(a,1,max_val-i,cex=1)
        image_points(b,2,max_val-i,cex=1)
        text(1,max_val-.5-i,paste0(actual_ij$PLAYER[i],", ",actual_ij$POS[i]))
        text(2,max_val-.5-i,paste0(optimal_ij$PLAYER[i],", ",optimal_ij$POS[i]))
        text(1.2,max_val-i,paste0(actual_ij$PTS[i]," PTS"),pos=4)
        text(1.8,max_val-i,paste0(optimal_ij$PTS[i]," PTS"),pos=2)
        text(1.5,max_val-i,paste0(actual_ij$PTS[i]-optimal_ij$PTS[i]," PTS"),cex=1.2,col="red")
      }
    } else {
      par(mar=c(0,0,0,0))
      plot(0,type="n",ylab="",xlab="",axes=F,xlim=c(.8,2.2),ylim=c(0,6.2))
      text(c(1,1.5,2),rep(6,3),c("Started","Points Lost","Bench"),cex=1.5)
      text(1.5,5,"Roster Optimally Allocated")
    }
  })
  
  ### Output standings_table ##########
  output$standings_table = renderText({
    out = saveXML(div_table_html)
    out = gsub('href="','href="http://games.espn.go.com',out)
    gsub('<a ','<a target="_blank" ',out)
  })
  
  ### Output trophy ##########
  output$trophy = renderDataTable({
    ## actual/optimal points
    actual_pts = dcast(actual_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
    optimal_pts = dcast(optimal_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
    actual_pts_i = actual_pts[,input$scoring_period_id]
    optimal_pts_i = optimal_pts[,input$scoring_period_id]
    names(actual_pts_i) = teams_table$teams[match(actual_pts$team_id,teams_table$team_no)]
    names(optimal_pts_i) = teams_table$teams[match(optimal_pts$team_id,teams_table$team_no)]
    sub_optimal = actual_pts_i - optimal_pts_i
    
    ## top scorers
    scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period==input$scoring_period_id,]
    top_scorer_row = scoring_leaders_i[1,]
    top_scorer = top_scorer_row$PTS
    names(top_scorer) = teams_table$teams[match(top_scorer_row$TYPE,teams_table$nicknames)]
    top_scorer_name = top_scorer_row$PLAYER
    
    ## transactions
    top_add = top_transactions()$top_add[1,]
    top_add_value = top_add$PTS
    names(top_add_value) = teams_table$teams[match(top_add$abbr,teams_table$nicknames)]
    top_drop = top_transactions()$top_drop[1,]
    top_drop_value = top_drop$PTS
    names(top_drop_value) = teams_table$teams[match(top_drop$abbr,teams_table$nicknames)]
    
    ## team scoring
    highest_scoring = actual_pts_i[which.max(actual_pts_i)]
    most_optimal = sub_optimal[which.max(sub_optimal)]
    lowest_scoring = actual_pts_i[which.min(actual_pts_i)]
    most_sub_optimal = sub_optimal[which.min(sub_optimal)]
    
    Category = c("Highest Scoring Team", "Most Optimal Team",
                 "Lowest Scoring Team", "Most Sub-Optimal Team",
                 "Top Scorer",  "Best Add from FA", "Worst Drop to FA")
    Player = c("","","","",top_scorer_name,
               top_add$player_name,top_drop$player_name)
    Value = c(highest_scoring,most_optimal,lowest_scoring,most_sub_optimal,top_scorer,
              top_add_value,top_drop_value)
    Team = names(Value)
    
    data_out = data.frame(Category,Team,Player,Value)
    return(data_out)
  })
  
  ### Output add_drop_table ###########
  output$add_drop_table = renderDataTable({
    add_drop()$trans_period
  })
  
  ### Output team_add_drop_period ##########
  output$team_add_drop_period = renderChart({
    add_drop_table = add_drop()$from_to_all
    if(is.null(add_drop_table)) {
      add_drop_table = data.frame(source=NA, target=NA, value=NA,stringsAsFactors=NULL)
    }
    actual_all_i = actual_all[actual_all$team_id == input$selected_team & 
                                actual_all$scoring_period_id ==
                                input$scoring_period_id,]
    if(nrow(actual_all_i)>0) {
      actual_players = actual_all_i$PLAYER[!actual_all_i$PLAYER %in% add_drop_table$source]
      actual_values = actual_all_i$PTS[!actual_all_i$PLAYER %in% add_drop_table$source]
      actual_all_temp = data.frame(source = actual_players,
                                   target = "Owned",
                                   value = actual_values,
                                   stringsAsFactors=F)      
      owned_temp = data.frame(source = "Owned",
                              target = teams_table$nickname[match(input$selected_team,teams_table$team_no)],
                              value = sum(actual_values),
                              stringsAsFactors=F)
      add_drop = rbind(add_drop_table,actual_all_temp,owned_temp)
      
    } else {
      add_drop = add_drop_table
    }
    add_drop = add_drop[complete.cases(add_drop),]
    
    sankeyPlot <- rCharts$new()
    sankeyPlot$addParams(dom = "team_add_drop_period")
    sankeyPlot$setTemplate(script = "/home/jazz12man/rCharts/rCharts_d3_sankey-gh-pages/libraries/widgets/d3_sankey/layouts/chart.html")
    sankeyPlot$setLib('d3_sankey')
    sankeyPlot$set(
      data = add_drop,
      nodeWidth = 15,
      nodePadding = 10,
      layout = 32,
      width = 960,
      height = 500
    )
    #     sankeyPlot$print(chartId = 'sankey2')
    return((sankeyPlot))
  })
  
  ### Output matchup_prob_chart ####
  output$matchup_prob_chart = renderPlot({
    projections_sp = projections_all[projections_all$scoring_period_id==input$scoring_period_id,]
    
    team1 = input$selected_team
    team2 = scoring_period_matchups[scoring_period_matchups$scoring_period==input$scoring_period_id & scoring_period_matchups$team1==team1, "team2"]
    
    teamA = actual_all[actual_all$team_id==team1 & actual_all$scoring_period_id==input$scoring_period_id,]
    w_actualA = which(!is.na(teamA$PTS))
    teamA$PLAYER = name_cleanup(teamA$PLAYER)
    
    teamB = actual_all[actual_all$team_id==team2 & actual_all$scoring_period_id==input$scoring_period_id,]
    w_actualB = which(!is.na(teamB$PTS))
    teamB$PLAYER = name_cleanup(teamB$PLAYER)
    
    projections_A = projections_sp[projections_sp$team_id==team1,]
    projections_A = projections_A[match(teamA$PLAYER,projections_A$PLAYER),]
    if(sum(is.na(projections_A$PLAYER))>0) {
      w_NA = which(is.na(projections_A$PLAYER))
      projections_A[w_NA,] = 0
      projections_A[w_NA,c("PLAYER","TEAM","POS","player_id")] =
        teamA[w_NA,c("PLAYER","TEAM","POS","player_id")]
      projections_A[w_NA,c("TYPE","scoring_period_id")] = projections_A[1,c("TYPE","scoring_period_id")]
    }
    
    projections_B = projections_sp[projections_sp$team_id==team2,]
    projections_B = projections_B[match(teamB$PLAYER,projections_B$PLAYER),]
    if(sum(is.na(projections_B$PLAYER))>0) {
      w_NA = which(is.na(projections_B$PLAYER))
      projections_B[w_NA,] = 0
      projections_B[w_NA,c("PLAYER","TEAM","POS","player_id")] =
        teamB[w_NA,c("PLAYER","TEAM","POS","player_id")]
      projections_B[w_NA,c("TYPE","scoring_period_id")] = projections_B[1,c("TYPE","scoring_period_id")]
    }
    
    rootMSE = sapply(c("PASS_YARDS","RUSH_YARDS","REC_YARDS"),function(x) NULL)
    rootMSE[["PASS_YARDS"]] = 77
    rootMSE[["RUSH_YARDS"]] = 25
    rootMSE[["REC_YARDS"]] = 25
    k_stdev=4.64
    dst_stdev=6.07
    
    projections_fun = function(projections = projections_A, team = teamA) {
      proj_pts_all = sapply(team$PLAYER, function(x) NULL)
      for(i in 1:nrow(projections)) {
        x = projections[i,]
        if(is.na(x$POS)) {
          total_pts = rep(0,1000)
        } else {
          if(x$POS %in% c("TE","RB","WR","QB")) {
            ### Pass
            if(x$PASS_YARDS>10) {
              # Yards
              pass_yards = rnorm(1000,x$PASS_YARDS,rootMSE[["PASS_YARDS"]])
              pass_yards[pass_yards<0] = 0
              
              # TDs
              if(x$PASS_TD==0) {
                pass_tds = rep(0,1000)
              } else if(x$PASS_TD==1) {
                pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(.05,.39,.5,.05,.01))
              } else if(x$PASS_TD==2) {
                pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(0.01,.35,.6,.25,.09))
              } else if(x$PASS_TD>2) {
                pass_tds = sample(c(0,1,2,3,4,5),1000,replace=T,prob=c(0.0,.18,.35,.4,.05,.02))
              }  
              pass_pts = floor(pass_yards/25) + pass_tds*4
            } else {
              pass_pts = 0
            }
            
            ### Rush
            if(x$RUSH_YARD>10) {
              rush_yards = rnorm(1000,x$RUSH_YARDS,rootMSE[["RUSH_YARDS"]])
              rush_yards[rush_yards<0] = 0
              if(x$RUSH_TD==0) {
                rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.75,.24,.01))
              } else if(x$RUSH_TD==1) {
                rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.65,.33,.02))
              } else if (x$RUSH_TD>1) {
                rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
              }
              rush_pts = floor(rush_yards/10) + rush_tds*6
              
            } else {
              rush_pts = 0
            }
            
            ### REC
            if(x$REC_YARD>10) {
              rec_yards = rnorm(1000,x$REC_YARDS,rootMSE[["REC_YARDS"]])
              rec_yards[rec_yards<0] = 0
              if(x$REC_TD==0) {
                rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.79,.20,.01))
              } else if(x$REC_TD==1) {
                rec_tds = sample(c(0,1,2,3),1000,replace=T,prob=c(.7,.23,.05,.02))
              } else if (x$REC_TD>1) {
                rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
              }
              rec_pts = floor(rec_yards/10) + rec_tds*6
              
            } else {
              rec_pts = 0
            }
            
            total_pts = pass_pts + rush_pts + rec_pts    
          } else if(x$POS=="D/ST") {
            total_pts = floor(rnorm(1000,x$PTS,dst_stdev))
          } else if(x$POS=="K") {
            total_pts = floor(rnorm(1000,x$PTS,k_stdev))
            total_pts[total_pts<1] = 1
          }
        }
        proj_pts_all[[x$PLAYER]] = total_pts
      }
      return(proj_pts_all)
    }
    
    proj_pts_allA = projections_fun(projections_A,teamA)
    proj_pts_allB = projections_fun(projections_B,teamB)
    
    all_projections_fun = function(proj_pts_all = proj_pts_allA, w_actual = w_actualA,
                                   team = teamA, projections = projections_A) {
      if(length(w_actual)>0) {
        for(wa in 1:length(w_actual)) {
          proj_pts_all[[w_actual[wa]]] = rep(team[w_actual[wa],"PTS"],length(proj_pts_all[[w_actual[wa]]]))      
        }
        proj_pts = sum(projections$PTS[-w_actual])
        act_pts = sum(team$PTS[w_actual])
      } else {
        proj_pts = sum(projections$PTS)
        act_pts = 0
      }
      all_projections = do.call(rbind,proj_pts_all)
      projections_tots = apply(all_projections,2,sum)
      return(list(proj_pts=proj_pts, act_pts=act_pts,
                  all_projections=all_projections,
                  projections_tots=projections_tots))
    }
    
    AA = all_projections_fun(proj_pts_allA,w_actualA,teamA,projections_A)
    BB = all_projections_fun(proj_pts_allB,w_actualB,teamB,projections_B)
    
    hA = hist(AA$projections_tots,plot=F,breaks=seq(min(AA$projections_tots)-.5,max(AA$projections_tots)+.5,1))
    hB = hist(BB$projections_tots,plot=F,breaks=seq(min(BB$projections_tots)-.5,max(BB$projections_tots)+.5,1))
    heightA = 2*10*hA$counts/1000
    heightB = -2*10*hB$counts/1000
    
    ##
    par(mar=c(3,4,1,4))
    plot(0,type="n",xlim=c(-1.2,1.2),ylim=c(0,max(AA$projections_tots,BB$projections_tots)),
         xlab="",ylab="",axes=F)
    axis(2,las=2)
    axis(4,las=2)
    # abline(h=axTicks(2),lty=2,col="gray80")
    abline(h=0)
    mtext(paste0(teams_table[teams_table$team_no==team1,"nicknames"],
                 "\nP Win: ",round(100*mean(AA$projections_tots > BB$projections_tots),1),"%"),
          side=1,line=1,at=0.5)
    mtext(paste0(teams_table[teams_table$team_no==team2,"nicknames"],
                 "\nP Win: ",round(100*mean(AA$projections_tots < BB$projections_tots),1),"%"),
          side=1,line=1,at=-0.5)
    
    #     heatA = array(1,dim=c(max(AA$projections_tots),1,3))
    #     heatA[hA$mids,1,] = 1-hA$count/max(hA$count)
    # rasterImage(heatA,0,max(AA$projections_tots),1,0,interpolate=T)
    
    #     heatB = array(1,dim=c(max(BB$projections_tots),1,3))
    #     heatB[hB$mids,1,] = 1-hB$count/max(hB$count)
    # rasterImage(heatB,1.8,max(BB$projections_tots),2.2,0,interpolate=T)
    polygon(c(1,par()$usr[2],par()$usr[2],1),
            c(0,0,AA$act_pts,AA$act_pts),
            col=rgb(39/255,26/255,135/255),border=F)
    polygon(c(1,par()$usr[2],par()$usr[2],1),
            c(AA$act_pts,AA$act_pts,AA$proj_pts,AA$proj_pts),
            border=rgb(.8,.8,.8),lty=2)
    
    polygon(-c(1,par()$usr[2],par()$usr[2],1),
            c(0,0,BB$act_pts,BB$act_pts),
            col=rgb(36/255,135/255,39/255),border=F)
    polygon(-c(1,par()$usr[2],par()$usr[2],1),
            c(BB$act_pts,BB$act_pts,BB$proj_pts,BB$proj_pts),
            border=rgb(.8,.8,.8),lty=2)
    
    for(aa in 2:length(hA$breaks)-1) {
      polygon(c(0,0,heightA[aa],heightA[aa]),
              c(hA$breaks[aa],hA$breaks[aa+1],hA$breaks[aa+1],hA$breaks[aa]),
              border=F,col=rgb(1-(hA$count[aa]/max(hA$count)*(1-39/255)),
                               1-(hA$count[aa]/max(hA$count)*(1-36/255)),
                               1-(hA$count[aa]/max(hA$count)*(1-135/255))))
    }
    for(bb in 2:length(hB$breaks)-1) {
      polygon(c(0,0,heightB[bb],heightB[bb]),
              c(hB$breaks[bb],hB$breaks[bb+1],hB$breaks[bb+1],hB$breaks[bb]),
              border=F,col=rgb(1-(hB$count[bb]/max(hB$count)*(1-36/255)),
                               1-(hB$count[bb]/max(hB$count)*(1-135/255)),
                               1-(hB$count[bb]/max(hB$count)*(1-39/255))))
    }
    
    running_tot = sum(apply(AA$all_projections,1,mean))
    #     for(i in nrow(projections_A):1) {
    #       a=readPNG(paste0(shiny_directory,"/www/player_images/",projections_A$player_id[i],".png"))
    #       if(i %% 2 == 1) {
    #         image_points(a,1.2,running_tot,pos=1)
    #       } else {
    #         image_points(a,1.1,running_tot,pos=1)
    #       }
    #       running_tot = running_tot - mean(AA$all_projections[i,])
    #     }
    running_tot = sum(apply(BB$all_projections,1,mean))
    #     for(i in nrow(projections_B):1) {
    #       b=readPNG(paste0(shiny_directory,"/www/player_images/",projections_B$player_id[i],".png"))
    #       if(i %% 2 == 1) {
    #         image_points(b,-1.2,running_tot,pos=1)
    #       } else {
    #         image_points(b,-1.1,running_tot,pos=1)
    #       }
    #       running_tot = running_tot - mean(BB$all_projections[i,])
    #     }
  })
  
  ### Output matchup_header ####
  output$matchup_header = renderText({
    paste0("<h2>Matchup Odds: ",gsub("_"," ",input$scoring_period_id,"</h2>"))
  })
  
  ### Output results_header ####
  output$results_header = renderText({
    paste0("<h2>Results: ",gsub("_"," ",input$scoring_period_id,"</h2>"))
  })
  ### Output season_win_probs ####
  output$season_win_probs = renderChart({
    resultsx = results_all[match(input$selected_team,rownames(results_all)),]
    pdata = data.frame(wins = 100*unlist(resultsx),
                       losses = 100-100*unlist(resultsx))
    colnames(pdata) = c("P(Win)","P(Loss)")
    pdata$scoring_period_id = rownames(pdata)
    matchup_teams = scoring_period_matchups[scoring_period_matchups$team1==input$selected_team,c("scoring_period_id","team2")]
    matchup_teams$nickname = teams_table$nickname[match(matchup_teams$team2,teams_table$team_no)]
    pdata_m = melt(pdata,id.var="scoring_period_id")
    pdata_m$nickname = matchup_teams$nickname[match(pdata_m$scoring_period_id,matchup_teams$scoring_period_id)]
    n5 = nPlot(value~nickname, group="variable", data=pdata_m, type="multiBarChart")
    n5$addParams(dom = "season_win_probs")
    n5$chart(reduceXTicks = FALSE)
    n5$chart(stacked = TRUE)
    n5$chart(showControls = FALSE)
    n5$chart(color=rep(c("#248727","#872724"),nrow(pdata)))
    return(n5)
  })
  
  ### Output final_standings_probs ####
  output$final_standings_probs = renderChart({
    final_standings = do.call(rbind,final_standings_all)
    final_standingx = data.frame(standing = 1:nrow(teams_table),
                                 pstanding = 100*apply(final_standings,2,function(x) sum(x==input$selected_team))/nrow(final_standings))
    colnames(final_standingx) = c("Final Standing","P(Final Standing)")
    
    n6 = nPlot(x="Final Standing",y="P(Final Standing)",final_standingx, type="multiBarChart")
    n6$addParams(dom = "final_standings_probs")
    n6$chart(reduceXTicks = FALSE)
    n6$chart(showControls = FALSE)
    n6$chart(color=rep("#272487",nrow(final_standingx)))
    return(n6)
  })
  
  ### Output playoff_donut ####
  output$playoff_donut = renderChart({
    final_standings = do.call(rbind,final_standings_all)
    px = final_standings[,1:no_playoff_teams]
    p_playoffs = sum(px==input$selected_team)/nrow(final_standings)
    p_no_playoffs = 1-p_playoffs
    playoff_data = data.frame(Playoffs = c("Make Playoffs","Miss Playoffs"),
                              Odds = c(p_playoffs,p_no_playoffs),
                              stringsAsFactors=F)
    cat(paste0(playoff_data$Odds,collapse="~"),"odds\n")
    cat(paste0(playoff_data$Playoffs,collapse="~"),"playoffs\n")
    pdonut1 = nPlot(x="Playoffs", y="Odds", data = playoff_data, type = 'pieChart')
    pdonut1$chart(donut = TRUE,
                  color=c("#248727","#872724"),
                  #                   width="400px",
                  #                   height="400px",
                  labelType="percent"
    )
    pdonut1$addParams(dom="playoff_donut")
    return(pdonut1)
  })
  
  ### Output bracket_chart ####
  output$bracket_chart = renderPlot({
    prob_pos = bracket_creator(byes=byes,
                               no_playoff_teams=no_playoff_teams,
                               no_playoff_rounds=no_playoff_rounds)
    final_standings = do.call(rbind,final_standings_all)
    px = final_standings[,1:no_playoff_teams]
    psf = do.call(rbind,lapply(playoff_results_all,function(x) x[[1]]))
    pf = do.call(rbind,lapply(playoff_results_all,function(x) x[[2]]))
    pc = do.call(rbind,lapply(playoff_results_all,function(x) x[[3]]))
    
    p_playoffs = sum(px==input$selected_team)/nrow(final_standings)
    p_semi_finals = sum(psf==input$selected_team)/nrow(final_standings)
    p_finals = sum(pf==input$selected_team)/nrow(final_standings)
    p_champion = sum(pc==input$selected_team)/nrow(final_standings)
    
    if(byes>0) {
      pb = final_standings[,1:byes]
      p_bye = sum(pb==input$selected_team)/nrow(final_standings)
      prob_pos$val = c(p_playoffs,p_bye,p_semi_finals,p_finals,p_champion)  
    } else {
      prob_pos$val = c(p_playoffs,p_semi_finals,p_finals,p_champion)  
    }
    
    for(q in 1:nrow(prob_pos)) {
      text(prob_pos[q,"x"],prob_pos[q,"y"],
           paste0(round(100*prob_pos[q,"val"]),"%"),cex = 1+prob_pos[q,"val"],pos=3)
    }
  })
  ### Output playoff_champ_odds_all ####
  output$playoff_champ_odds_all = renderPlot({
    prob_pos = bracket_creator(byes=byes,
                               no_playoff_teams=no_playoff_teams,
                               no_playoff_rounds=no_playoff_rounds)
    final_standings = do.call(rbind,final_standings_all)
    px_tab = table(unlist(final_standings[,1:no_playoff_teams]))/nrow(final_standings)
    psf_tab = table(unlist(do.call(rbind,lapply(playoff_results_all,function(x) x[[1]]))))/nrow(final_standings)
    pf_tab = table(unlist(do.call(rbind,lapply(playoff_results_all,function(x) x[[2]]))))/nrow(final_standings)
    pc_tab = table(unlist(do.call(rbind,lapply(playoff_results_all,function(x) x[[3]]))))/nrow(final_standings)
    
    px_tab = px_tab[order(px_tab)]
    pf_tab = pf_tab[order(pf_tab)]
    psf_tab = psf_tab[order(psf_tab)]
    pc_tab = pc_tab[order(pc_tab)]
    names(px_tab) = teams_table$nickname[match(names(px_tab),teams_table$team_no)]
    names(pf_tab) = teams_table$nickname[match(names(pf_tab),teams_table$team_no)]
    names(psf_tab) = teams_table$nickname[match(names(psf_tab),teams_table$team_no)]
    names(pc_tab) = teams_table$nickname[match(names(pc_tab),teams_table$team_no)]
    
    for(q in 1:nrow(px_tab)) {
      if(!is.na(px_tab[q])) {
        text(prob_pos[prob_pos$col=="Playoff Odds","x"],
             prob_pos[prob_pos$col=="Playoff Odds","y"]+.3*q,
             paste0(names(px_tab)[q]," ",round(100*px_tab[q]),"%"),cex = 0.5+px_tab[q])
      }
      if(!is.na(psf_tab[q])) {
        text(prob_pos[prob_pos$col=="Semi-Finals Odds","x"],
             prob_pos[prob_pos$col=="Semi-Finals Odds","y"]+.3*q,
             paste0(names(psf_tab)[q]," ",round(100*psf_tab[q]),"%"),cex = 0.5+psf_tab[q])
      }
      if(!is.na(pf_tab[q])) {
        text(prob_pos[prob_pos$col=="Finals Odds","x"],
             prob_pos[prob_pos$col=="Finals Odds","y"]+.3*q,
             paste0(names(pf_tab)[q]," ",round(100*pf_tab[q]),"%"),cex = 0.5+pf_tab[q])
      }
      if(!is.na(pc_tab[q])) {
        text(prob_pos[prob_pos$col=="Champion Odds","x"],
             prob_pos[prob_pos$col=="Champion Odds","y"]+.3*q,
             paste0(names(pc_tab)[q]," ",round(100*pc_tab[q]),"%"),cex = 0.5+pc_tab[q])
        
      }
    }
    
  })
  
=======
    n4$chart(reduceXTicks = FALSE)
    n4$chart(stacked = FALSE)
    n4$chart(showControls = FALSE)
    #       n4$xAxis(NULL, replace = T)
    #       n4$xAxis(tickFormat = 
    #                  "#!function (d) {
    #             return NULL;
    #           }!#"
    #       )
    n4$chart(color=rep("#272487",nrow(scoring_leaders_i)))
} else {
  data_out = data.frame(None="No Data Available",PTS = 1)
  n4 = nPlot(PTS~None,data=data_out, type="multiBarChart")
  n4$addParams(dom = "pos_rchart")
  n4$chart(color = "#! function(d){ return '#272487'} !#")
}
return(n4)
})

### Output top_FAs #########
output$top_FAs = renderDataTable({
  scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period_id==input$scoring_period_id,]
  top_FAs = scoring_leaders_i[which(scoring_leaders_i$TYPE %in% c("FA","WA (Wed)", "WA (Tue)", "WA (Thur)")),]
  top_FAs = top_FAs[,c("PLAYER","TEAM","POS","PTS","PTS_abv_repl")]
  top_FAs = top_FAs[order(top_FAs$PTS_abv_repl,decreasing=T),]
  names(top_FAs) = c("Player","Team","Pos","PTS","PTS Above Replacement")
  return(top_FAs)
})

### Output Optimal Switch ##########
output$optimal_switch = renderPlot({
  actual_ij = actual_all[actual_all$scoring_period==input$scoring_period_id &
                           actual_all$team_id==input$selected_team,]
  bench_ij = bench_all[bench_all$scoring_period==input$scoring_period_id &
                         bench_all$team_id==input$selected_team,]
  optimal_ij = optimal_all[optimal_all$scoring_period==input$scoring_period_id &
                             optimal_all$team_id==input$selected_team,]
  w_act_out = which(!actual_ij$PLAYER %in% optimal_ij$PLAYER)
  w_opt_in = which(!optimal_ij$PLAYER %in% actual_ij$PLAYER)
  if(length(w_act_out)>0) {
    actual_ij = actual_ij[w_act_out,]
    optimal_ij = optimal_ij[w_opt_in,]
    optimal_ij$player_id = bench_ij$player_id[match(optimal_ij$PLAYER,bench_ij$PLAYER)]
    actual_ij$player_id[is.na(actual_ij$player_id)] = "nfl"
    optimal_ij$player_id[is.na(optimal_ij$player_id)] = "nfl"
    
    max_val = max(c(6,nrow(actual_ij)))
    par(mar=c(0,0,0,0))
    par(bg = "#ffffff")
    
    plot(0,type="n",ylab="",xlab="",axes=F,xlim=c(.8,2.2),ylim=c(0,max_val+0.2))
    text(c(1,1.5,2),rep(max_val,3),c("Started","Points Lost","Bench"),cex=1.5)
    for(i in 1:nrow(actual_ij)) {
      a=readPNG(paste0("~/ShinyApps/fantasy_football_14/www/player_images/",actual_ij$player_id[i],".png"))
      b=readPNG(paste0("~/ShinyApps/fantasy_football_14/www/player_images/",optimal_ij$player_id[i],".png"))
      image_points(a,1,max_val-i,cex=1)
      image_points(b,2,max_val-i,cex=1)
      text(1,max_val-.5-i,paste0(actual_ij$PLAYER[i],", ",actual_ij$POS[i]))
      text(2,max_val-.5-i,paste0(optimal_ij$PLAYER[i],", ",optimal_ij$POS[i]))
      text(1.2,max_val-i,paste0(actual_ij$PTS[i]," PTS"),pos=4)
      text(1.8,max_val-i,paste0(optimal_ij$PTS[i]," PTS"),pos=2)
      text(1.5,max_val-i,paste0(actual_ij$PTS[i]-optimal_ij$PTS[i]," PTS"),cex=1.2,col="red")
    }
  } else {
    par(bg = "#E5FFCC")
    par(mar=c(0,0,0,0))
    plot(0,type="n",ylab="",xlab="",axes=F,xlim=c(.8,2.2),ylim=c(0,6.2))
    text(c(1,1.5,2),rep(6,3),c("Started","Points Lost","Bench"),cex=1.5)
    text(1.5,5,"Roster Optimally Allocated")
  }
})

### Output standings_table ##########
output$standings_table = renderText({
  out = saveXML(div_table_html)
  out = gsub('href="','href="http://games.espn.go.com',out)
  gsub('<a ','<a target="_blank" ',out)
})

### Output trophy ##########
output$trophy = renderDataTable({
  ## actual/optimal points
  actual_pts = dcast(actual_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
  optimal_pts = dcast(optimal_all,team_id~scoring_period_id,fun.aggregate=sum,value.var="PTS",na.rm=T)
  actual_pts_i = actual_pts[,input$scoring_period_id]
  optimal_pts_i = optimal_pts[,input$scoring_period_id]
  names(actual_pts_i) = teams_table$teams[match(actual_pts$team_id,teams_table$team_no)]
  names(optimal_pts_i) = teams_table$teams[match(optimal_pts$team_id,teams_table$team_no)]
  sub_optimal = actual_pts_i - optimal_pts_i
  
  ## top scorers
  scoring_leaders_i = scoring_leaders_all[scoring_leaders_all$scoring_period==input$scoring_period_id,]
  top_scorer_row = scoring_leaders_i[1,]
  top_scorer = top_scorer_row$PTS
  names(top_scorer) = teams_table$teams[match(top_scorer_row$TYPE,teams_table$nicknames)]
  top_scorer_name = top_scorer_row$PLAYER
  
  ## transactions
  top_add = top_transactions()$top_add[1,]
  top_add_value = top_add$PTS
  names(top_add_value) = teams_table$teams[match(top_add$abbr,teams_table$nicknames)]
  top_drop = top_transactions()$top_drop[1,]
  top_drop_value = top_drop$PTS
  names(top_drop_value) = teams_table$teams[match(top_drop$abbr,teams_table$nicknames)]
  
  ## team scoring
  highest_scoring = actual_pts_i[which.max(actual_pts_i)]
  most_optimal = sub_optimal[which.max(sub_optimal)]
  lowest_scoring = actual_pts_i[which.min(actual_pts_i)]
  most_sub_optimal = sub_optimal[which.min(sub_optimal)]
  
  Category = c("Highest Scoring Team", "Most Optimal Team",
               "Lowest Scoring Team", "Most Sub-Optimal Team",
               "Top Scorer",  "Best Add from FA", "Worst Drop to FA")
  Player = c("","","","",top_scorer_name,
             top_add$player_name,top_drop$player_name)
  Value = c(highest_scoring,most_optimal,lowest_scoring,most_sub_optimal,top_scorer,
            top_add_value,top_drop_value)
  Team = names(Value)
  
  data_out = data.frame(Category,Team,Player,Value)
  return(data_out)
})

### Output add_drop_table ###########
output$add_drop_table = renderDataTable({
  add_drop()$trans_period
})

### Output team_add_drop_period ##########
output$team_add_drop_period = renderChart({
  add_drop_table = add_drop()$from_to_all
  if(is.null(add_drop_table)) {
    add_drop_table = data.frame(source=NA, target=NA, value=NA,stringsAsFactors=NULL)
  }
  actual_all_i = actual_all[actual_all$team_id == input$selected_team & 
                              actual_all$scoring_period_id ==
                              input$scoring_period_id,]
  if(nrow(actual_all_i)>0) {
    actual_players = actual_all_i$PLAYER[!actual_all_i$PLAYER %in% add_drop_table$source]
    actual_values = actual_all_i$PTS[!actual_all_i$PLAYER %in% add_drop_table$source]
    actual_all_temp = data.frame(source = actual_players,
                                 target = "Owned",
                                 value = actual_values,
                                 stringsAsFactors=F)      
    owned_temp = data.frame(source = "Owned",
                            target = teams_table$nickname[match(input$selected_team,teams_table$team_no)],
                            value = sum(actual_values),
                            stringsAsFactors=F)
    add_drop = rbind(add_drop_table,actual_all_temp,owned_temp)
    
  } else {
    add_drop = add_drop_table
  }
  add_drop = add_drop[complete.cases(add_drop),]
  
  sankeyPlot <- rCharts$new()
  sankeyPlot$addParams(dom = "team_add_drop_period")
  sankeyPlot$setTemplate(script = "/home/jazz12man/rCharts/rCharts_d3_sankey-gh-pages/libraries/widgets/d3_sankey/layouts/chart.html")
  sankeyPlot$setLib('d3_sankey')
  sankeyPlot$set(
    data = add_drop,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 960,
    height = 500
  )
  #     sankeyPlot$print(chartId = 'sankey2')
  return((sankeyPlot))
})

### Output matchup_prob_chart ####
output$matchup_prob_chart = renderPlot({
  projections_sp = projections_all[projections_all$scoring_period_id==input$scoring_period_id,]
  
  team1 = input$selected_team
  team2 = scoring_period_matchups[scoring_period_matchups$scoring_period==input$scoring_period_id & scoring_period_matchups$team1==team1, "team2"]
  
  teamA = actual_all[actual_all$team_id==team1 & actual_all$scoring_period_id==input$scoring_period_id,]
  w_actualA = which(!is.na(teamA$PTS))
  teamA$PLAYER = name_cleanup(teamA$PLAYER)
  
  teamB = actual_all[actual_all$team_id==team2 & actual_all$scoring_period_id==input$scoring_period_id,]
  w_actualB = which(!is.na(teamB$PTS))
  teamB$PLAYER = name_cleanup(teamB$PLAYER)
  
  projections_A = projections_sp[projections_sp$team_id==team1,]
  projections_A = projections_A[match(teamA$PLAYER,projections_A$PLAYER),]
  if(sum(is.na(projections_A$PLAYER))>0) {
    w_NA = which(is.na(projections_A$PLAYER))
    projections_A[w_NA,] = 0
    projections_A[w_NA,c("PLAYER","TEAM","POS","player_id")] =
      teamA[w_NA,c("PLAYER","TEAM","POS","player_id")]
    projections_A[w_NA,c("TYPE","scoring_period_id")] = projections_A[1,c("TYPE","scoring_period_id")]
  }
  
  projections_B = projections_sp[projections_sp$team_id==team2,]
  projections_B = projections_B[match(teamB$PLAYER,projections_B$PLAYER),]
  if(sum(is.na(projections_B$PLAYER))>0) {
    w_NA = which(is.na(projections_B$PLAYER))
    projections_B[w_NA,] = 0
    projections_B[w_NA,c("PLAYER","TEAM","POS","player_id")] =
      teamB[w_NA,c("PLAYER","TEAM","POS","player_id")]
    projections_B[w_NA,c("TYPE","scoring_period_id")] = projections_B[1,c("TYPE","scoring_period_id")]
  }
  
  rootMSE = sapply(c("PASS_YARDS","RUSH_YARDS","REC_YARDS"),function(x) NULL)
  rootMSE[["PASS_YARDS"]] = 77
  rootMSE[["RUSH_YARDS"]] = 25
  rootMSE[["REC_YARDS"]] = 25
  k_stdev=4.64
  dst_stdev=6.07
  
  projections_fun = function(projections = projections_A, team = teamA) {
    proj_pts_all = sapply(team$PLAYER, function(x) NULL)
    for(i in 1:nrow(projections)) {
      x = projections[i,]
      if(is.na(x$POS)) {
        total_pts = rep(0,1000)
      } else {
        if(x$POS %in% c("TE","RB","WR","QB")) {
          ### Pass
          if(x$PASS_YARDS>10) {
            # Yards
            pass_yards = rnorm(1000,x$PASS_YARDS,rootMSE[["PASS_YARDS"]])
            pass_yards[pass_yards<0] = 0
            
            # TDs
            if(x$PASS_TD==0) {
              pass_tds = rep(0,1000)
            } else if(x$PASS_TD==1) {
              pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(.05,.39,.5,.05,.01))
            } else if(x$PASS_TD==2) {
              pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(0.01,.35,.6,.25,.09))
            } else if(x$PASS_TD>2) {
              pass_tds = sample(c(0,1,2,3,4,5),1000,replace=T,prob=c(0.0,.18,.35,.4,.05,.02))
            }  
            pass_pts = floor(pass_yards/25) + pass_tds*4
          } else {
            pass_pts = 0
          }
          
          ### Rush
          if(x$RUSH_YARD>10) {
            rush_yards = rnorm(1000,x$RUSH_YARDS,rootMSE[["RUSH_YARDS"]])
            rush_yards[rush_yards<0] = 0
            if(x$RUSH_TD==0) {
              rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.75,.24,.01))
            } else if(x$RUSH_TD==1) {
              rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.65,.33,.02))
            } else if (x$RUSH_TD>1) {
              rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
            }
            rush_pts = floor(rush_yards/10) + rush_tds*6
            
          } else {
            rush_pts = 0
          }
          
          ### REC
          if(x$REC_YARD>10) {
            rec_yards = rnorm(1000,x$REC_YARDS,rootMSE[["REC_YARDS"]])
            rec_yards[rec_yards<0] = 0
            if(x$REC_TD==0) {
              rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.79,.20,.01))
            } else if(x$REC_TD==1) {
              rec_tds = sample(c(0,1,2,3),1000,replace=T,prob=c(.7,.23,.05,.02))
            } else if (x$REC_TD>1) {
              rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
            }
            rec_pts = floor(rec_yards/10) + rec_tds*6
            
          } else {
            rec_pts = 0
          }
          
          total_pts = pass_pts + rush_pts + rec_pts    
        } else if(x$POS=="D/ST") {
          total_pts = floor(rnorm(1000,x$PTS,dst_stdev))
        } else if(x$POS=="K") {
          total_pts = floor(rnorm(1000,x$PTS,k_stdev))
          total_pts[total_pts<1] = 1
        }
      }
      proj_pts_all[[x$PLAYER]] = total_pts
    }
    return(proj_pts_all)
  }
  
  proj_pts_allA = projections_fun(projections_A,teamA)
  proj_pts_allB = projections_fun(projections_B,teamB)
  
  all_projections_fun = function(proj_pts_all = proj_pts_allA, w_actual = w_actualA,
                                 team = teamA, projections = projections_A) {
    if(length(w_actual)>0) {
      for(wa in 1:length(w_actual)) {
        proj_pts_all[[w_actual[wa]]] = rep(team[w_actual[wa],"PTS"],length(proj_pts_all[[w_actual[wa]]]))      
      }
      proj_pts = sum(projections$PTS[-w_actual])
      act_pts = sum(team$PTS[w_actual])
    } else {
      proj_pts = sum(projections$PTS)
      act_pts = 0
    }
    all_projections = do.call(rbind,proj_pts_all)
    projections_tots = apply(all_projections,2,sum)
    return(list(proj_pts=proj_pts, act_pts=act_pts,
                all_projections=all_projections,
                projections_tots=projections_tots))
  }
  
  AA = all_projections_fun(proj_pts_allA,w_actualA,teamA,projections_A)
  BB = all_projections_fun(proj_pts_allB,w_actualB,teamB,projections_B)
  
  hA = hist(AA$projections_tots,plot=F,breaks=seq(min(AA$projections_tots)-.5,max(AA$projections_tots)+.5,1))
  hB = hist(BB$projections_tots,plot=F,breaks=seq(min(BB$projections_tots)-.5,max(BB$projections_tots)+.5,1))
  heightA = 2*10*hA$counts/1000
  heightB = -2*10*hB$counts/1000
  
  ##
  par(mar=c(3,4,1,4))
  plot(0,type="n",xlim=c(-1.2,1.2),ylim=c(0,max(AA$projections_tots,BB$projections_tots)),
       xlab="",ylab="",axes=F)
  axis(2,las=2)
  axis(4,las=2)
  # abline(h=axTicks(2),lty=2,col="gray80")
  abline(h=0)
  mtext(paste0(teams_table[teams_table$team_no==team1,"nicknames"],
               "\nP Win: ",round(100*mean(AA$projections_tots > BB$projections_tots),1),"%"),
        side=1,line=1,at=0.5)
  mtext(paste0(teams_table[teams_table$team_no==team2,"nicknames"],
               "\nP Win: ",round(100*mean(AA$projections_tots < BB$projections_tots),1),"%"),
        side=1,line=1,at=-0.5)
  
  #     heatA = array(1,dim=c(max(AA$projections_tots),1,3))
  #     heatA[hA$mids,1,] = 1-hA$count/max(hA$count)
  # rasterImage(heatA,0,max(AA$projections_tots),1,0,interpolate=T)
  
  #     heatB = array(1,dim=c(max(BB$projections_tots),1,3))
  #     heatB[hB$mids,1,] = 1-hB$count/max(hB$count)
  # rasterImage(heatB,1.8,max(BB$projections_tots),2.2,0,interpolate=T)
  polygon(c(1,par()$usr[2],par()$usr[2],1),
          c(0,0,AA$act_pts,AA$act_pts),
          col=rgb(39/255,26/255,135/255),border=F)
  polygon(c(1,par()$usr[2],par()$usr[2],1),
          c(AA$act_pts,AA$act_pts,AA$proj_pts,AA$proj_pts),
          border=rgb(.8,.8,.8),lty=2)
  
  polygon(-c(1,par()$usr[2],par()$usr[2],1),
          c(0,0,BB$act_pts,BB$act_pts),
          col=rgb(36/255,135/255,39/255),border=F)
  polygon(-c(1,par()$usr[2],par()$usr[2],1),
          c(BB$act_pts,BB$act_pts,BB$proj_pts,BB$proj_pts),
          border=rgb(.8,.8,.8),lty=2)
  
  for(aa in 2:length(hA$breaks)-1) {
    polygon(c(0,0,heightA[aa],heightA[aa]),
            c(hA$breaks[aa],hA$breaks[aa+1],hA$breaks[aa+1],hA$breaks[aa]),
            border=F,col=rgb(1-(hA$count[aa]/max(hA$count)*(1-39/255)),
                             1-(hA$count[aa]/max(hA$count)*(1-36/255)),
                             1-(hA$count[aa]/max(hA$count)*(1-135/255))))
  }
  for(bb in 2:length(hB$breaks)-1) {
    polygon(c(0,0,heightB[bb],heightB[bb]),
            c(hB$breaks[bb],hB$breaks[bb+1],hB$breaks[bb+1],hB$breaks[bb]),
            border=F,col=rgb(1-(hB$count[bb]/max(hB$count)*(1-36/255)),
                             1-(hB$count[bb]/max(hB$count)*(1-135/255)),
                             1-(hB$count[bb]/max(hB$count)*(1-39/255))))
  }
  
  running_tot = sum(apply(AA$all_projections,1,mean))
  #     for(i in nrow(projections_A):1) {
  #       a=readPNG(paste0(shiny_directory,"/www/player_images/",projections_A$player_id[i],".png"))
  #       if(i %% 2 == 1) {
  #         image_points(a,1.2,running_tot,pos=1)
  #       } else {
  #         image_points(a,1.1,running_tot,pos=1)
  #       }
  #       running_tot = running_tot - mean(AA$all_projections[i,])
  #     }
  running_tot = sum(apply(BB$all_projections,1,mean))
  #     for(i in nrow(projections_B):1) {
  #       b=readPNG(paste0(shiny_directory,"/www/player_images/",projections_B$player_id[i],".png"))
  #       if(i %% 2 == 1) {
  #         image_points(b,-1.2,running_tot,pos=1)
  #       } else {
  #         image_points(b,-1.1,running_tot,pos=1)
  #       }
  #       running_tot = running_tot - mean(BB$all_projections[i,])
  #     }
})

### Output matchup_header ####
output$matchup_header = renderText({
  paste0("<h2>Matchup Odds: ",gsub("_"," ",input$scoring_period_id,"</h2>"))
})

>>>>>>> 6df677896335089a14190efbc967c493299df873
})

