stat_columns = c("COMP","ATT","PASS_YARDS","PASS_TD","INT","RUSH","RUSH_YARDS","RUSH_TD",
                 "TAR","REC","REC_YARDS","REC_TD","FUML","MISC_TD",
                 "TT","SCK","FF","FR","DEF_INT","ITD","FTD",
                 "MADE_1_39","ATT_1_39","MADE_40_49","ATT_40_49","MADE_50P","ATT_50P","MADE_XP","ATT_XP",
                 "SFTY","BLK","PA")

## placeholder - I think this will be an iniput; need to think about how to update
# scoring_period_id=input$scoring_period_id
load(file=paste0(shiny_directory,"/www/data_files/scoring_leaders_all.Rdata"))

## initialize team_tables_all (only at start of season, then load)
# team_tables_all = sapply(team_no,function(x) NULL)
# team_tables_all = lapply(team_tables_all,function(x) sapply(all_matchups,function(y) NULL))
load(file=paste0(shiny_directory,"/www/data_files/team_tables_all.Rdata"))

## Iterate through 
for(team_id in team_no) {
  url = paste0("http://games.espn.go.com/ffl/boxscorefull?leagueId=",league_id,
               "&teamId=",team_id,
               "&scoringPeriodId=",scoring_period_id,
               "&seasonId=",season_id,
               "&view=scoringperiod&version=full")
  url_in = getURL(url)
  
  # Pull in and manipulate URL
  url_in = getURL(url)
  doc = htmlTreeParse(url_in, useInternalNodes=T)
  root = xmlRoot(doc)
  body = root[["body"]]
  
  ### Starters Tables #########
  player_tables_in = xpathSApply(body,"//*/table[@class='playerTableTable tableBody']")
  player_tables_all = NULL 
  for(i in 1:length(player_tables_in)) player_tables_all[[i]] = readHTMLTable(player_tables_in[[i]],stringsAsFactors=F)
  player_tables_all = lapply(player_tables_all, function(x) {
    x[x[,2]=="",2] = NA
    name_split = do.call(rbind,strsplit(x[,2],","))
    name = gsub("\\*","",name_split[,1])
    if(length(name_split)>1) {
      team_pos = do.call(rbind,strsplit(name_split[,2],"\u00A0"))
      team = gsub(" ","",team_pos[,1])
      pos = team_pos[,2]
      team[team=="TEAMPOS"] = "TEAM"
      pos[pos==" TEAM POS"] = "POS"
    } else {
      name = team = strsplit(name[[1]]," ")[[1]][1]
      pos = "DST"
    }
    slot = x[,1]
    player_tables_out = x[,-c(1:4)]
    table_out = cbind(data.frame(PLAYER=name,
                                 TEAM=team,
                                 POS=pos,
                                 SLOT=slot,
                                 stringsAsFactors=F),
                      player_tables_out)
    
    data_start = which(table_out$SLOT=="SLOT")
    colnames_i = table_out[data_start,]
    colnames_i[colnames_i==""] = "remove"
    table_out = table_out[-c(1:data_start),]
    colnames(table_out) = colnames_i
    table_out = table_out[,-which(colnames_i=="remove")]
    
    return(table_out)
  })
  names(player_tables_all) = lapply(player_tables_all,function(x) {
    starters_table$lookup_table[match(x[nrow(x),"POS"],starters_table$abbr)]
  })
  
  ### Bench Tables ##########
  bench_tables_in = xpathSApply(body,"//*/table[@class='playerTableTable tableBody hideableGroup']")
  bench_tables_all = NULL 
  for(i in 1:length(bench_tables_in)) bench_tables_all[[i]] = readHTMLTable(bench_tables_in[[i]],stringsAsFactors=F)
  bench_tables_all = lapply(bench_tables_all, function(x) {
    x[x[,2]=="",2] = NA
    name_split = do.call(rbind,strsplit(x[,2],","))
    name = gsub("\\*","",name_split[,1])
    if(length(name_split)>1) {
      team_pos = do.call(rbind,strsplit(name_split[,2],"\u00A0"))
      team = gsub(" ","",team_pos[,1])
      pos = team_pos[,2]
      team[team=="TEAMPOS"] = "TEAM"
      pos[pos==" TEAM POS"] = "POS"
    } else {
      name = team = strsplit(name[[1]]," ")[[1]][1]
      pos = "DST"
    }
    slot = x[,1]
    player_tables_out = x[,-c(1:4)]
    table_out = cbind(data.frame(PLAYER=name,
                                 TEAM=team,
                                 POS=pos,
                                 SLOT=slot,
                                 stringsAsFactors=F),
                      player_tables_out)
    
    data_start = which(table_out$SLOT=="SLOT")
    colnames_i = table_out[data_start,]
    colnames_i[colnames_i==""] = "remove"
    table_out = table_out[-c(1:data_start),]
    colnames(table_out) = colnames_i
    table_out = table_out[,-which(colnames_i=="remove")]
    
    return(table_out)
  })
  names(bench_tables_all) = lapply(bench_tables_all,function(x) {
    starters_table$lookup_table[match(x[1,"POS"],starters_table$abbr)]
  })
  
  ## identify which are for the team of interest
  w_start_tables = 1:(which(names(player_tables_all)=="OFFENSE")[2]-1)
  w_bench_tables = 1:(which(names(bench_tables_all)=="OFFENSE")[2]-1)
  
  
  ### iterate through starter tables ##########
  starters_out = NULL
  for(i in w_start_tables) {
    player_tables_i = player_tables_all[[i]]
    
    if(names(player_tables_all)[[i]]=="OFFENSE") {
      COMP_ATT = data.frame(do.call(rbind,strsplit(player_tables_i[,"C/A"],"/")),stringsAsFactors=F)
      colnames(COMP_ATT) = c("COMP","ATT")
      w_ca = which(colnames(player_tables_i)=="C/A")
      player_tables_i = cbind(player_tables_i[,1:(w_ca-1)],
                              COMP_ATT,
                              player_tables_i[,(w_ca+1):ncol(player_tables_i)])
      colnames(player_tables_i)[match(c("YDS","TD","YDS.1","TD.1","YDS.2","TD.2","TD.3"),colnames(player_tables_i))] = c('PASS_YARDS','PASS_TD','RUSH_YARDS','RUSH_TD','REC_YARDS','REC_TD',"MISC_TD")
    } else if(names(player_tables_all)[[i]]=="KICKERS") {
      kick_cols = c("1-39","40-49","50+","TOT","XP")
      splits = lapply(kick_cols,function(x) strsplit(player_tables_i[,x],"/"))
      kick_vals = data.frame(matrix(unlist(lapply(splits,function(x) x[[1]])),
                                    nrow=nrow(player_tables_i),
                                    byrow=T),stringsAsFactors=F)
      names(kick_vals) = c("MADE_1_39","ATT_1_39","MADE_40_49","ATT_40_49",
                           "MADE_50P","ATT_50P","MADE_TOT","ATT_TOT","MADE_XP","ATT_XP")
      player_tables_i[,names(kick_vals)] = kick_vals
      player_tables_i = player_tables_i[,-which(colnames(player_tables_i) %in% kick_cols)]
      player_tables_i = player_tables_i[,c(colnames(player_tables_i)[1:4],names(kick_vals),"PTS")] 
    }
    
    player_tables_i[player_tables_i=="--"] = NA
    player_tables_i[,-c(1:4)] = as.data.frame(lapply(player_tables_i[,-c(1:4)],as.numeric),stringsAsFactors=F)
    
    starters_out[[i]] = player_tables_i
  }
  names(starters_out) = names(player_tables_all)[w_start_tables]
  
  ### iterate through bench tables ##########
  bench_out = NULL
  for(i in w_bench_tables) {
    bench_tables_i = bench_tables_all[[i]]
    
    if(names(bench_tables_all)[[i]]=="OFFENSE") {
      COMP_ATT = data.frame(do.call(rbind,strsplit(bench_tables_i[,"C/A"],"/")),stringsAsFactors=F)
      colnames(COMP_ATT) = c("COMP","ATT")
      w_ca = which(colnames(bench_tables_i)=="C/A")
      bench_tables_i = cbind(bench_tables_i[,1:(w_ca-1)],
                             COMP_ATT,
                             bench_tables_i[,(w_ca+1):ncol(bench_tables_i)])
      colnames(bench_tables_i)[match(c("YDS","TD","YDS.1","TD.1","YDS.2","TD.2","TD.3"),colnames(bench_tables_i))] = c('PASS_YARDS','PASS_TD','RUSH_YARDS','RUSH_TD','REC_YARDS','REC_TD',"MISC_TD")
    } else if(names(bench_tables_all)[[i]]=="KICKERS") {
      kick_cols = c("1-39","40-49","50+","TOT","XP")
      splits = lapply(kick_cols,function(x) strsplit(bench_tables_i[,x],"/"))
      kick_vals = data.frame(matrix(unlist(lapply(splits,function(x) x[[1]])),
                                    nrow=nrow(bench_tables_i),
                                    byrow=T),stringsAsFactors=F)
      names(kick_vals) = c("MADE_1_39","ATT_1_39","MADE_40_49","ATT_40_49",
                           "MADE_50P","ATT_50P","MADE_TOT","ATT_TOT","MADE_XP","ATT_XP")
      bench_tables_i[,names(kick_vals)] = kick_vals
      bench_tables_i = bench_tables_i[,-which(colnames(bench_tables_i) %in% kick_cols)]
      bench_tables_i = bench_tables_i[,c(colnames(bench_tables_i)[1:4],names(kick_vals),"PTS")] 
    }
    
    bench_tables_i[bench_tables_i=="--"] = NA
    bench_tables_i[,-c(1:4)] = as.data.frame(lapply(bench_tables_i[,-c(1:4)],as.numeric),stringsAsFactors=F)
    
    bench_out[[i]] = bench_tables_i
  }
  names(bench_out) = names(bench_tables_all)[w_bench_tables]
  
  ### Create Position Tables ##########
  u_pos = unique(starters_table$abbr)
  
  starter_summary = do.call(rbind,lapply(starters_out,function(x) x[,c("PLAYER","POS","SLOT","PTS")]))
  bench_summary = do.call(rbind,lapply(bench_out,function(x) x[,c("PLAYER","POS","SLOT","PTS")]))
  
  players_summary = rbind(starter_summary, bench_summary)
  
  slots = rep(starters_table$abbr,starters_table$no_starters)
  slots[slots=="RB/WR/TE"] = "FLEX"
  slot_pos = slots[!slots %in% c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX","DB","DP","DL")]
  slot_pos = unique(slot_pos)
  
  pos_tables = lapply(slot_pos,function(x) {
    all_x = players_summary[which(players_summary$POS==x),]
    all_x = all_x[order(all_x$PTS,decreasing=T),]
    return(all_x)
  })
  names(pos_tables) = slot_pos
  
  if("DB" %in% slots) {
    pos_x = players_summary[which(players_summary$POS %in% c("CB","FS","SS","S")),]
    pos_tables[["DB"]] = pos_x[order(pos_x$PTS,decreasing=T),]
  }
  
  if("DL" %in% slots) {
    pos_x = players_summary[which(players_summary$POS %in% c("DE","DL","DT")),]
    pos_tables[["DL"]] = pos_x[order(pos_x$PTS,decreasing=T),]
  }
  
  if("DP" %in% slots) {
    pos_x = players_summary[which(players_summary$POS %in% c("CB","FS","SS","S","LB","DE","DT","DL")),]
    pos_tables[["DP"]] = pos_x[order(pos_x$PTS,decreasing=T),]
  }
  
  optimal_starters = lapply(slot_pos,function(x) {
    out = pos_tables[[x]][1:starters_table$no_starters[match(x,starters_table$abbr)],]
    if(nrow(out)==1) {
      out$SLOT = x
    } else {
      out$SLOT = paste0(x,1:length(out$SLOT))
    }
    return(out)
  })
  names(optimal_starters) = slot_pos
  
  if(sum(c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX") %in% slots)>0) {
    flex_pos = slots[slots %in% c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX")]
    for(flexi in 1:length(flex_pos)) {
      flexpi = flex_pos[flexi]
      if(flexpi=="FLEX") flexpi = "RB/WR/TE"
      pos_i = strsplit(flexpi,"/")[[1]]
      flex_playersi = do.call(rbind,lapply(pos_i,function(x) return(pos_tables[[x]])))
      flex_playersi = flex_playersi[order(flex_playersi$PTS,decreasing=T),]
      flex_playersi = flex_playersi[!flex_playersi$PLAYER %in% do.call(rbind,optimal_starters)$PLAYER,][
        1:starters_table$no_starters[match(flexpi,starters_table$abbr)],]
      if(nrow(flex_playersi)==1) {
        flex_playersi$SLOT = flex_pos
      } else {
        flex_playersi$SLOT = paste0(flex_pos,1:nrow(flex_playersi))
      }
      optimal_starters[[flex_pos]] = flex_playersi
    }
  }
  
  if("DL" %in% slots) {
    dl_sub = players_summary[players_summary$POS %in% c("DL","DT","DE"),]
    dl_sub = dl_sub[!dl_sub$PLAYER %in% do.call(rbind,optimal_starters)$PLAYER,]
    dl_sub = dl_sub[1:starters_table$no_starters[match("DL",starters_table$abbr)],]
    if(nrow(dl_sub)==1) {
      dl_sub$SLOT = "DL"
    } else {
      dl_sub$SLOT = paste0("DL",1:nrow(dl_sub))
    }
    optimal_starters[["DL"]] = dl_sub[order(dl_sub$PTS,decreasing=T),]
  }
  if("DB" %in% slots) {
    db_sub = players_summary[players_summary$POS %in% c("CB","FS","SS","S"),]
    db_sub = db_sub[!db_sub$PLAYER %in% do.call(rbind,optimal_starters)$PLAYER,]
    db_sub = db_sub[1:starters_table$no_starters[match("DB",starters_table$abbr)],]
    if(nrow(db_sub)==1) {
      db_sub$SLOT = "DB"
    } else {
      db_sub$SLOT = paste0("DB",1:nrow(db_sub))
    }
    optimal_starters[["DB"]] = db_sub[order(db_sub$PTS,decreasing=T),]
  }
  if("DP" %in% slots) {
    dp_sub = players_summary[players_summary$POS %in% c("CB","FS","SS","S","LB","DE","DT","LB"),]
    dp_sub = dp_sub[-which(dp_sub$PLAYER %in% do.call(rbind,optimal_starters)$PLAYER),]
    dp_sub = dp_sub[1:starters_table$no_starters[match("DP",starters_table$abbr)],]
    if(nrow(dp_sub)==1) {
      dp_sub$SLOT = "DP"
    } else {
      dp_sub$SLOT = paste0("DP",1:nrow(dp_sub))
    }
    optimal_starters[["DP"]] = dp_sub[order(dp_sub$PTS,decreasing=T),]
  }
  
  starter_table = starter_summary
  duplicated_starters = starter_table$SLOT[duplicated(starter_table$SLOT)]
  lapply(duplicated_starters,function(x) starter_table[starter_table$SLOT==x,"SLOT"] <<- paste0(x,1:sum(starter_table$SLOT==x)))
  optimal_table = do.call(rbind,optimal_starters)
  optimal_table$SLOT = gsub("DST","D/ST",optimal_table$SLOT)
  optimal_table = optimal_table[match(starter_table$SLOT,optimal_table$SLOT),]
  
  ### ID optimal ##########
  opt_subs = which(!optimal_table$PLAYER %in% starter_table$PLAYER)
  
  optimal_table[opt_subs,]
  starter_table[opt_subs,]
  
  starter_table$team_id = team_id
  starter_table$scoring_period_id = scoring_period_id
  
  ### Cycle through Stat Categories  ##########
  stat_cats = unlist(lapply(starters_out,colnames))
  stat_cats[which(stat_cats=="INT")[-1]] = "DEF_INT"
  stat_cats = unique(stat_cats)
  starters_all_stats = lapply(starters_out,function(x) {
    x[,stat_cats[!stat_cats %in% colnames(x)]] = 0
    x = x[,stat_cats]
  })
  starters_all_stats = do.call(rbind,starters_all_stats)
  ## Cycle through for bench
  bench_all_stats = lapply(bench_out, function(x) {
    x[,stat_cats[!stat_cats %in% colnames(x)]] = 0
    x = x[,stat_cats]
  })
  bench_all_stats = do.call(rbind,bench_all_stats)
  
  ## Add in Points Above Replacement
  starters_all_stats = merge(starters_all_stats,
                             scoring_leaders_table[,c("PLAYER","TEAM","POS","PTS_abv_repl")],
                             by=c("PLAYER","TEAM","POS"),all.x=T)
  bench_all_stats = merge(bench_all_stats,
                          scoring_leaders_table[,c("PLAYER","TEAM","POS","PTS_abv_repl")],
                          by=c("PLAYER","TEAM","POS"),all.x=T)
    
  ## add in Targets
  starters_all_stats = merge(starters_all_stats,
                             scoring_leaders_table[,c("PLAYER","TEAM","POS","TAR")],
                             by=c("PLAYER","TEAM","POS"),all.x=T)
  bench_all_stats = merge(bench_all_stats,
                          scoring_leaders_table[,c("PLAYER","TEAM","POS","TAR")],
                          by=c("PLAYER","TEAM","POS"),all.x=T)
  
  stat_cats = stat_cats[-c(1:4)]
  stat_cats = c(stat_cats,"TAR")
  
  ## Add in ID
  starters_all_stats = merge(starters_all_stats,
                             scoring_leaders_table[,c("PLAYER","TEAM","POS","player_id")],
                             by=c("PLAYER","TEAM","POS"),all.x=T)
  bench_all_stats = merge(bench_all_stats,
                             scoring_leaders_table[,c("PLAYER","TEAM","POS","player_id")],
                             by=c("PLAYER","TEAM","POS"),all.x=T)
  
  
  ### Add starter Stat summaries ##########
  starters_all_stats$summary4 =
    starters_all_stats$summary3 =
    starters_all_stats$summary2 = 
    starters_all_stats$summary1 = ""
  for(j in 1:nrow(starters_all_stats)) {
    actual_all_ij = starters_all_stats[j,]
    ww = which(starters_all_stats[j,stat_cats]!=0)
    stats_j = actual_all_ij[,stat_cats[ww]]
    
    if(length(stats_j)==1) {
      names(stats_j) = stat_columns[ww]
      stats_j = as.data.frame(t(stats_j))
    }
    
    if(ncol(stats_j)==0) {
      starters_all_stats$summary1[j] = NA
      starters_all_stats$summary2[j] = NA
      starters_all_stats$summary3[j] = NA
      starters_all_stats$summary4[j] = NA
    } else {
      stats_list = NULL
      if("ATT" %in% colnames(stats_j)) {
        group1 = NULL
        group1[[1]] = paste0(stats_j$COMP,"/",stats_j$ATT,", ",stats_j$PASS_YARDS," YDs")
        if("PASS_TD" %in% colnames(stats_j))
          group1[[2]] = paste(stats_j$PASS_TD,ifelse(stats_j$PASS_TD==1,"TD","TDs"))
        if("INT" %in% colnames(stats_j))
          group1[[3]] = paste(stats_j$INT,ifelse(stats_j$INT==1,"INT","INTs"))
        group1 = na.omit(group1)
        stats_list[[1]] = paste0(group1,collapse=", ")
      }
      if("RUSH" %in% colnames(stats_j)) {
        group2 = NULL
        group2[[1]] = paste(stats_j$RUSH,"Rush,",stats_j$RUSH_YARDS,"YDs")
        if("RUSH_TD" %in% colnames(stats_j))
          group2[[2]] = paste(stats_j$RUSH_TD,ifelse(stats_j$RUSH_TD==1,"TD","TDs"))
        group2 = na.omit(group2)
        stats_list[[2]] = paste0(group2,collapse=", ")
      }
      if("REC" %in% colnames(stats_j) | "TAR" %in% colnames(stats_j)) {
        group3 = NULL
        group3[[1]] = paste(stats_j$TAR,ifelse(stats_j$TAR==1,"TAR","TARs,"),
                            stats_j$REC,ifelse(stats_j$REC==1,"REC","RECs"))
        if("REC_YARDS" %in% colnames(stats_j))
          group3[[2]] = paste(stats_j$REC_YARDS,"YDs")
        if("REC_TD" %in% colnames(stats_j))
          group3[[3]] = paste(stats_j$REC_TD,ifelse(stats_j$REC_TD==1,"TD","TDs"))
        group3 = na.omit(group3)
        stats_list[[3]] = paste0(group3,collapse=", ")
      }
      if("FUML" %in% colnames(stats_j))
        stats_list[[4]] = paste(stats_j$FUML,ifelse(stats_j$FUML==1,"FUML","FUMLs"))
      if("MISC_TD" %in% colnames(stats_j))
        stats_list[[5]] = paste(stats_j$MISC_TD,ifelse(stats_j$MISC_TD==1,"Misc. TD","Misc. TDs"))
      if(sum(c("TT","SCK","FF","FR","DEF_INT","ITD","FTD") %in% colnames(stats_j))>0) {
        group6 = NULL
        if("TT" %in% colnames(stats_j))
          group6[[1]] = paste(stats_j$TT,ifelse(stats_j$TT==1,"TT","TTs"))
        if("SCK" %in% colnames(stats_j))
          group6[[2]] = paste(stats_j$SCK,ifelse(stats_j$SCK==1,"SCK","SCKs"))
        if("FF" %in% colnames(stats_j))
          group6[[3]] = paste(stats_j$FF,ifelse(stats_j$FF==1,"FF","FFs"))
        if("FR" %in% colnames(stats_j))
          group6[[4]] = paste(stats_j$FR,ifelse(stats_j$FR==1,"FR","FRs"))
        if("FTD" %in% colnames(stats_j))
          group6[[5]] = paste(stats_j$FTD,ifelse(stats_j$FTD==1,"Fum. TD","Fum. TDs"))
        if("DEF_INT" %in% colnames(stats_j))
          group6[[6]] = paste(stats_j$DEF_INT,ifelse(stats_j$DEF_INT==1,"INT","INTs"))
        if("ITD" %in% colnames(stats_j))
          group6[[7]] = paste(stats_j$ITD,ifelse(stats_j$ITD==1,"INT TD","INT TDs"))
        group6 = na.omit(group6)
        stats_list[[6]] = paste0(group6,collapse=", ")
      }
      if(sum(c("ATT_1_39","ATT_40_49","ATT_50P","ATT_XP") %in% colnames(stats_j))>0) {
        group7 = NULL
        if("ATT_1_39" %in% colnames(stats_j))
          group7[[1]] = paste0(stats_j$MADE_1_39,"/",stats_j$ATT_1_39," 1-39")
        if("ATT_40_49" %in% colnames(stats_j))
          group7[[2]] = paste0(stats_j$MADE_40_49,"/",stats_j$ATT_40_49," 40-49")
        if("ATT_50P" %in% colnames(stats_j))
          group7[[3]] = paste0(stats_j$MADE_50P,"/",stats_j$ATT_50P," 50+")
        if("ATT_XP" %in% colnames(stats_j))
          group7[[4]] = paste0(stats_j$MADE_XP,"/",stats_j$ATT_XP," XP")
        group7 = na.omit(group7)
        stats_list[[7]] = paste0(group7,collapse=", ")
      }
      if(sum(c("PA","SFTY","BLK") %in% colnames(stats_j))>0) {
        group8 = NULL
        if("PA" %in% colnames(stats_j))
          group8[[1]] = paste(stats_j$PA,"PA")
        if("SFTY" %in% colnames(stats_j))
          group8[[2]] = paste(stats_j$SFTY,ifelse(stats_j$SFTY==1,"SFTY","SFTYs"))
        if("BLK" %in% colnames(stats_j))
          group8[[3]] = paste(stats_j$BLK,ifelse(stats_j$SFTY==1,"BLK","BLKs"))
        group8 = na.omit(group8)
        stats_list[[8]] = paste0(group8,collapse=", ")
      }
      
      stats_list = unlist(stats_list)
      if(sum(is.na(stats_list))>0)
        stats_list = stats_list[!is.na(stats_list)]
      
      starters_all_stats$summary1[j] = stats_list[1]
      starters_all_stats$summary2[j] = stats_list[2]
      starters_all_stats$summary3[j] = stats_list[3]
      starters_all_stats$summary4[j] = stats_list[4]
    }
  }
  
  ### Add Bench Stat summaries ##########
  bench_all_stats$summary4 =
    bench_all_stats$summary3 =
    bench_all_stats$summary2 = 
    bench_all_stats$summary1 = NA
  for(j in 1:nrow(bench_all_stats)) {
    bench_all_ij = bench_all_stats[j,]
    ww = which(bench_all_stats[j,stat_cats]!=0)
    stats_j = bench_all_ij[,stat_cats[ww]]
    
    if(length(stats_j)==1) {
      names(stats_j) = stat_columns[ww]
      stats_j = as.data.frame(t(stats_j))
    }
    
    if(ncol(stats_j)==0) {
      bench_all_stats$summary1[j] = NA
      bench_all_stats$summary2[j] = NA
      bench_all_stats$summary3[j] = NA
      bench_all_stats$summary4[j] = NA
    } else {
      stats_list = NULL
      if("ATT" %in% colnames(stats_j)) {
        group1 = NULL
        group1[[1]] = paste0(stats_j$COMP,"/",stats_j$ATT,", ",stats_j$PASS_YARDS," YDs")
        if("PASS_TD" %in% colnames(stats_j))
          group1[[2]] = paste(stats_j$PASS_TD,ifelse(stats_j$PASS_TD==1,"TD","TDs"))
        if("INT" %in% colnames(stats_j))
          group1[[3]] = paste(stats_j$INT,ifelse(stats_j$INT==1,"INT","INTs"))
        group1 = na.omit(group1)
        stats_list[[1]] = paste0(group1,collapse=", ")
      }
      if("RUSH" %in% colnames(stats_j)) {
        group2 = NULL
        group2[[1]] = paste(stats_j$RUSH,"Rush,",stats_j$RUSH_YARDS,"YDs")
        if("RUSH_TD" %in% colnames(stats_j))
          group2[[2]] = paste(stats_j$RUSH_TD,ifelse(stats_j$RUSH_TD==1,"TD","TDs"))
        group2 = na.omit(group2)
        stats_list[[2]] = paste0(group2,collapse=", ")
      }
      if("REC" %in% colnames(stats_j) | "TAR" %in% colnames(stats_j)) {
        group3 = NULL
        group3[[1]] = paste(stats_j$TAR,ifelse(stats_j$TAR==1,"TAR","TARs,"),
                            stats_j$REC,ifelse(stats_j$REC==1,"REC","RECs"))
        if("REC_YARDS" %in% colnames(stats_j))
          group3[[2]] = paste(stats_j$REC_YARDS,"YDs")
        if("REC_TD" %in% colnames(stats_j))
          group3[[3]] = paste(stats_j$REC_TD,ifelse(stats_j$REC_TD==1,"TD","TDs"))
        group3 = na.omit(group3)
        stats_list[[3]] = paste0(group3,collapse=", ")
      }
      if("FUML" %in% colnames(stats_j))
        stats_list[[4]] = paste(stats_j$FUML,ifelse(stats_j$FUML==1,"FUML","FUMLs"))
      if("MISC_TD" %in% colnames(stats_j))
        stats_list[[5]] = paste(stats_j$MISC_TD,ifelse(stats_j$MISC_TD==1,"Misc. TD","Misc. TDs"))
      if(sum(c("TT","SCK","FF","FR","DEF_INT","ITD","FTD") %in% colnames(stats_j))>0) {
        group6 = NULL
        if("TT" %in% colnames(stats_j))
          group6[[1]] = paste(stats_j$TT,ifelse(stats_j$TT==1,"TT","TTs"))
        if("SCK" %in% colnames(stats_j))
          group6[[2]] = paste(stats_j$SCK,ifelse(stats_j$SCK==1,"SCK","SCKs"))
        if("FF" %in% colnames(stats_j))
          group6[[3]] = paste(stats_j$FF,ifelse(stats_j$FF==1,"FF","FFs"))
        if("FR" %in% colnames(stats_j))
          group6[[4]] = paste(stats_j$FR,ifelse(stats_j$FR==1,"FR","FRs"))
        if("FTD" %in% colnames(stats_j))
          group6[[5]] = paste(stats_j$FTD,ifelse(stats_j$FTD==1,"Fum. TD","Fum. TDs"))
        if("DEF_INT" %in% colnames(stats_j))
          group6[[6]] = paste(stats_j$DEF_INT,ifelse(stats_j$DEF_INT==1,"INT","INTs"))
        if("ITD" %in% colnames(stats_j))
          group6[[7]] = paste(stats_j$ITD,ifelse(stats_j$ITD==1,"INT TD","INT TDs"))
        group6 = na.omit(group6)
        stats_list[[6]] = paste0(group6,collapse=", ")
      }
      if(sum(c("ATT_1_39","ATT_40_49","ATT_50P","ATT_XP") %in% colnames(stats_j))>0) {
        group7 = NULL
        if("ATT_1_39" %in% colnames(stats_j))
          group7[[1]] = paste0(stats_j$MADE_1_39,"/",stats_j$ATT_1_39," 1-39")
        if("ATT_40_49" %in% colnames(stats_j))
          group7[[2]] = paste0(stats_j$MADE_40_49,"/",stats_j$ATT_40_49," 40-49")
        if("ATT_50P" %in% colnames(stats_j))
          group7[[3]] = paste0(stats_j$MADE_50P,"/",stats_j$ATT_50P," 50+")
        if("ATT_XP" %in% colnames(stats_j))
          group7[[4]] = paste0(stats_j$MADE_XP,"/",stats_j$ATT_XP," XP")
        group7 = na.omit(group7)
        stats_list[[7]] = paste0(group7,collapse=", ")
      }
      if(sum(c("PA","SFTY","BLK") %in% colnames(stats_j))>0) {
        group8 = NULL
        if("PA" %in% colnames(stats_j))
          group8[[1]] = paste(stats_j$PA,"PA")
        if("SFTY" %in% colnames(stats_j))
          group8[[2]] = paste(stats_j$SFTY,ifelse(stats_j$SFTY==1,"SFTY","SFTYs"))
        if("BLK" %in% colnames(stats_j))
          group8[[3]] = paste(stats_j$BLK,ifelse(stats_j$SFTY==1,"BLK","BLKs"))
        group8 = na.omit(group8)
        stats_list[[8]] = paste0(group8,collapse=", ")
      }
      
      stats_list = unlist(stats_list)
      if(sum(is.na(stats_list))>0)
        stats_list = stats_list[!is.na(stats_list)]
      
      bench_all_stats$summary1[j] = stats_list[1]
      bench_all_stats$summary2[j] = stats_list[2]
      bench_all_stats$summary3[j] = stats_list[3]
      bench_all_stats$summary4[j] = stats_list[4]
    }
  }
  

  ### Add to Results ##########
  starter_table$team_id = 
    optimal_table$team_id = 
    bench_summary$team_id = 
    starters_all_stats$team_id = 
    bench_all_stats$team_id = team_id
  
  starter_table$scoring_period_id = 
    optimal_table$scoring_period_id = 
    bench_summary$scoring_period_id = 
    starters_all_stats$scoring_period_id = 
    bench_all_stats$scoring_period_id = scoring_period_id
  
  starters_all_stats$SLOT = starter_table$SLOT[match(starters_all_stats$PLAYER,starter_table$PLAYER)]
  starters_all_stats = starters_all_stats[match(starter_table$SLOT,starters_all_stats$SLOT),]
  starters_all_stats$color = google.colors[1:nrow(starters_all_stats)]
  
  team_tables_all[[team_id]][[paste0("scoring_period_",scoring_period_id)]][["starter_table"]] = starter_table
  team_tables_all[[team_id]][[paste0("scoring_period_",scoring_period_id)]][["optimal_table"]] = optimal_table
  team_tables_all[[team_id]][[paste0("scoring_period_",scoring_period_id)]][["bench_summary"]] = bench_summary
  team_tables_all[[team_id]][[paste0("scoring_period_",scoring_period_id)]][["starters_all_stats"]] = starters_all_stats
  team_tables_all[[team_id]][[paste0("scoring_period_",scoring_period_id)]][["bench_all_stats"]] = bench_all_stats
}


save(team_tables_all,file=paste0(shiny_directory,"www/data_files/team_tables_all.Rdata"))

actual_all = do.call(rbind,lapply(team_tables_all,function(x) {
  out = do.call(rbind,lapply(x, function(y) {y[["starters_all_stats"]]}))
}))

bench_all = do.call(rbind,lapply(team_tables_all,function(x) {
  out = do.call(rbind,lapply(x, function(y) {y[["bench_all_stats"]]}))
}))

optimal_list = lapply(team_tables_all,function(x) {
  out = do.call(rbind,lapply(x, function(y) {y[["optimal_table"]]}))
})
optimal_all = do.call(rbind,optimal_list)

write.csv(actual_all, file=paste0(shiny_directory,"www/data_files/team_tables_actual_all.csv"), row.names=F)
write.csv(bench_all, file=paste0(shiny_directory,"www/data_files/team_tables_bench_all.csv"), row.names=F)
write.csv(optimal_all, file=paste0(shiny_directory,"www/data_files/team_tables_optimal_all.csv"), row.names=F)


