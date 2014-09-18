modified_out = NULL
trans_all_out = NULL
for(team_id_i in teams_table$team_no) {
  url = paste0("http://games.espn.go.com/ffl/recentactivity?leagueId=",league_id,
               "&seasonId=2014&activityType=2&startDate=",gsub("-","",as.character(draft_date)),
               "&endDate=",gsub("-","",as.character(Sys.Date())),
               "&teamId=",team_id_i,
               "&tranType=-1")
  
  url_in = getURL(url)
  doc = htmlTreeParse(url_in, useInternalNodes=T)
  root = xmlRoot(doc)
  body = root[["body"]]
  
  # parse through body to find table of interest for team names
  trans_table = xpathSApply(body,"//*/table[@class= 'tableBody']")[[1]]
  trans_rows = xpathSApply(trans_table,"//table[@class='tableBody']/tr")
  trans_out = lapply(3:length(trans_rows),function(x) {
    a = trans_rows[[x]]
    b = xmlChildren(a)
    
    #1 Date/Time
    datetime = sapply(xmlChildren(b[[1]]),xmlValue)
    date = datetime[1]
    date = as.Date(date,"%a, %b %d") #at some point fix for year
    time = datetime[3]
    #   date_time = ymd_hms(paste(date,time))
    date_time = date
    
    #2 Type
    type = xmlValue(b[[2]])
    types = strsplit(type,"\u00A0\u00A0")[[1]]
    type1 = types[1]
    type2 = types[2]
    
    #4 Action
    action = xmlValue(b[[4]])
    actions = strsplit(action,"\u0020")[[1]]
    abbr = actions[1]
    action_type = actions[2]
    
    #3 Players Involved
    player_vals = sapply(xmlChildren(b[[3]]),xmlValue)
    l3 = length(player_vals)
    no_trans = 1 + sum(names(player_vals)=="br")
    
    player_trans = 
      player_name = 
      player_team = 
      player_pos = 
      from = to = rep(NA,no_trans)
    
    if(l3>1) {
      w_pv = which(names(player_vals)!="br")
      if(length(w_pv) %% 3 == 1 ) w_pv = w_pv[-length(w_pv)]
      names(player_vals)[w_pv] = paste0(c("player_trans","player_name","split3"),rep(1:no_trans,each=3))
      for(trans_i in 1:no_trans) {
        player_trans[trans_i] = strsplit(player_vals[paste0("player_trans",trans_i)],"\u0020")[[1]][[2]]
        player_name[trans_i] = player_vals[paste0("player_name",trans_i)]
        split3 = strsplit(player_vals[paste0("split3",trans_i)],"\u0020")[[1]]
        player_team[trans_i] = split3[2]
        player_pos[trans_i] = split3[3]
        from3 = split3[which(split3=="from")+1]
        if(length(from3)==0) {
          from[trans_i] = abbr
        } else {
          from[trans_i] = from3
        }
        to3 = split3[which(split3=="to")+1]
        if(to3=="Bench" | player_trans[trans_i]=="drafted") {
          to[trans_i] = abbr
        } else {
          to[trans_i] = to3
        }
      }
    } else {
      if(grep("modified active roster",player_vals)==1) player_trans = "modified active roster"
    }
    out = data.frame(abbr,
                     #action_type,
                     type1,
                     #type2,
                     player_trans,
                     player_name,
                     player_team,
                     player_pos,
                     from,
                     to,
                     date_time
    )
    return(out)
  })
  
  trans_all = data.frame(do.call(rbind,trans_out),
                         stringsAsFactors=F)
  
  modified = trans_all[is.na(trans_all$player_name),]
  trans_all = trans_all[!is.na(trans_all$player_name),]
  if(nrow(modified)>0) modified$team_id = team_id_i
  if(nrow(trans_all)>0) trans_all$team_id = team_id_i
  
  modified_out[[team_id_i]] = modified
  trans_all_out[[team_id_i]] = trans_all
}

modified_all = do.call(rbind,modified_out)
transactions_all = do.call(rbind,trans_all_out)

write.csv(modified_all,
          file=paste0(shiny_directory,"www/data_files/modifed_roster_moves.csv"))
write.csv(transactions_all,
          file=paste0(shiny_directory,"www/data_files/transactions_summary.csv"))

