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

## Starters
player_tables_in = xpathSApply(body,"//*/table[@class='playerTableTable tableBody']")
player_tables_all = NULL 
for(i in 1:length(player_tables_in)) player_tables_all[[i]] = readHTMLTable(player_tables_in[[i]],stringsAsFactors=F)
## Benches
bench_tables_in = xpathSApply(body,"//*/table[@class='playerTableTable tableBody hideableGroup']")
bench_tables_all = NULL 
for(i in 1:length(bench_tables_in)) bench_tables_all[[i]] = readHTMLTable(bench_tables_in[[i]],stringsAsFactors=F)
## identify which are for the team of interest
w_start_tables = 1:(which(unlist(lapply(player_tables_all,function(x) ifelse(length(grep(gsub(" Box Score","",x[1,1]),teams_table$teams))>0,1,0)))>0)[2]-1)
w_bench_tables = 1:(which(unlist(lapply(bench_tables_all,function(x) ifelse(length(grep("BENCH: OFFENSIVE PLAYERS",names(x)))>0,1,0)))>0)[2]-1)
## iterate through starter tables
starters_out = NULL
for(i in w_start_tables) {
  player_tables_i = player_tables_all[[i]]
  col_name1 = colnames(player_tables_i)[1]
  if(col_name1=="V1") {
    pos_type_i = player_tables_i[2,1]
    colnames_i = player_tables_i[3,]
    colnames_i[colnames_i==""] = "remove"
    player_tables_i = player_tables_i[-c(1:3),]
    colnames(player_tables_i) = colnames_i
    COMP_ATT = data.frame(do.call(rbind,strsplit(player_tables_i[,"C/A"],"/")),stringsAsFactors=F)
    colnames(COMP_ATT) = c("COMP","ATT")
    player_tables_i = player_tables_i[,-grep("remove",colnames(player_tables_i))]
    w_ca = which(colnames(player_tables_i)=="C/A")
    player_tables_i = cbind(player_tables_i[,1:(w_ca-1)],
                            COMP_ATT,
                            player_tables_i[,(w_ca+1):ncol(player_tables_i)])
    colnames(player_tables_i)[match(c("YDS","TD","YDS.1","TD.1","YDS.2","TD.2","TD.3"),colnames(player_tables_i))] = c('PASS_YARDS','PASS_TD','RUSH_YARDS','RUSH_TD','REC_YARDS','REC_TD',"MISC_TD")
  } else {
    pos_type_i = col_name1
    colnames_i = player_tables_i[1,]
    colnames_i[colnames_i==""] = "remove"
    player_tables_i = player_tables_i[-c(1),]
    colnames(player_tables_i) = colnames_i    
    player_tables_i = player_tables_i[,-grep("remove",colnames(player_tables_i))]
    if(col_name1=="STARTERS: KICKERS") {
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
  }
  
  player_tables_i
  player_tables_i[player_tables_i[,2]=="",2] = NA
  
  name_split = do.call(rbind,strsplit(player_tables_i[,2],","))
  name = gsub("\\*","",name_split[,1])
  if(length(name_split)>1) {
    team_pos = do.call(rbind,strsplit(name_split[,2],"\u00A0"))
    team = gsub(" ","",team_pos[,1])
    pos = team_pos[,2]
  } else {
    name = team = strsplit(name[[1]]," ")[[1]][1]
    pos = "DST"
  }
  
  #
  player_tables_out = player_tables_i[,-c(1:4)]
  player_tables_out[player_tables_out=="--"] = NA
  player_tables_out = as.data.frame(lapply(player_tables_out,as.numeric),stringsAsFactors=F)
  
  table_out = cbind(data.frame(PLAYER=name,
                               TEAM=team,
                               POS=pos,
                               SLOT=player_tables_i$SLOT,
                               stringsAsFactors=F),
                    player_tables_out)
  starters_out[[i]] = table_out
}

## iterate through bench tables
bench_out = NULL
for(i in w_bench_tables) {
  bench_tables_i = bench_tables_all[[i]]
  col_name1 = colnames(bench_tables_i)[1]
  if(col_name1=="BENCH: OFFENSIVE PLAYERS") {
    colnames_i = bench_tables_i[1,]
    colnames_i[colnames_i==""] = "remove"
    bench_tables_i = bench_tables_i[-1,]
    colnames(bench_tables_i) = colnames_i
    COMP_ATT = data.frame(do.call(rbind,strsplit(bench_tables_i[,"C/A"],"/")),stringsAsFactors=F)
    colnames(COMP_ATT) = c("COMP","ATT")
    bench_tables_i = bench_tables_i[,-grep("remove",colnames(bench_tables_i))]
    w_ca = which(colnames(bench_tables_i)=="C/A")
    bench_tables_i = cbind(bench_tables_i[,1:(w_ca-1)],
                           COMP_ATT,
                           bench_tables_i[,(w_ca+1):ncol(bench_tables_i)])
    colnames(bench_tables_i)[match(c("YDS","TD","YDS.1","TD.1","YDS.2","TD.2","TD.3"),colnames(bench_tables_i))] = c('PASS_YARDS','PASS_TD','RUSH_YARDS','RUSH_TD','REC_YARDS','REC_TD',"MISC_TD")
  } else {
    pos_type_i = col_name1
    colnames_i = bench_tables_i[1,]
    colnames_i[colnames_i==""] = "remove"
    bench_tables_i = bench_tables_i[-c(1),]
    colnames(bench_tables_i) = colnames_i    
    bench_tables_i = bench_tables_i[,-grep("remove",colnames(bench_tables_i))]
    if(col_name1=="STARTERS: KICKERS") {
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
  }
  
  bench_tables_i
  
  bench_tables_i[bench_tables_i[,2]=="",2] = NA
  name_split = do.call(rbind,strsplit(bench_tables_i[,2],","))
  
  name = gsub("\\*","",name_split[,1])
  if(length(name_split)>1) {
    team_pos = do.call(rbind,strsplit(name_split[,2],"\u00A0"))
    team = gsub(" ","",team_pos[,1])
    pos = team_pos[,2]
  } else {
    name = team = strsplit(name[[1]]," ")[[1]][1]
    pos = "DST"
  }
  
  #
  bench_tables_out = bench_tables_i[,-c(1:4)]
  bench_tables_out[bench_tables_out=="--"] = NA
  bench_tables_out = as.data.frame(lapply(bench_tables_out,as.numeric),stringsAsFactors=F)
  
  
  
  table_out = cbind(data.frame(PLAYER=name,
                               TEAM=team,
                               POS=pos,
                               SLOT=bench_tables_i$SLOT,
                               stringsAsFactors=F),
                    bench_tables_out)
  bench_out[[i]] = table_out
}

## Create Position Tables
u_pos = unique(starters_table$abbr)

starter_summary = do.call(rbind,lapply(starters_out,function(x) x[,c("PLAYER","POS","SLOT","PTS")]))
bench_summary = do.call(rbind,lapply(bench_out,function(x) x[,c("PLAYER","POS","SLOT","PTS")]))

players_summary = rbind(starter_summary, bench_summary)

slots = rep(starters_table$abbr,starters_table$no_starters)
slots[slots=="RB/WR/TE"] = "FLEX"
slot_pos = slots[!slots %in% c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX")]
slot_pos = unique(slot_pos)

pos_tables = lapply(slot_pos,function(x) {
  all_x = players_summary[which(players_summary$POS==x),]
  all_x = all_x[order(all_x$PTS,decreasing=T),]
  return(all_x)
})
names(pos_tables) = slot_pos

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

starter_table = starter_summary
duplicated_starters = starter_table$SLOT[duplicated(starter_table$SLOT)]
lapply(duplicated_starters,function(x) starter_table[starter_table$SLOT==x,"SLOT"] <<- paste0(x,1:sum(starter_table$SLOT==x)))
optimal_table = do.call(rbind,optimal_starters)
optimal_table$SLOT = gsub("DST","D/ST",optimal_table$SLOT)
optimal_table = optimal_table[match(starter_table$SLOT,optimal_table$SLOT),]

