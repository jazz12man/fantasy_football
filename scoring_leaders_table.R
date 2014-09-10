
## initialize at start of season
# scoring_leaders_scoring_period = sapply(all_matchups,function(x) NULL)

load(file=paste0(shiny_directory,"/www/data_files/scoring_leaders_all.Rdata"))
load(file=paste0(shiny_directory,"www/data_files/team_lookup.Rdata"))
load(file=paste0(shiny_directory,"www/data_files/players_id_lookup.Rdata"))

start_index = seq(0,5000,by=50)
ind0 = 1
starti = 1
scoring_leaders_all = NULL
while(ind0==1) {
  url = paste0("http://games.espn.go.com/ffl/leaders?leagueId=",league_id,"&teamId=1&scoringPeriodId=",scoring_period_id,"&startIndex=",start_index[starti])
  url_in = getURL(url)
  doc = htmlTreeParse(url_in, useInternalNodes=T)
  root = xmlRoot(doc)
  body = root[["body"]]
  tabs = xpathSApply(body,"//*/table")
  player_table = readHTMLTable(xpathSApply(body,"//*/table")[[2]],stringsAsFactors=F)
  colnames_i = player_table[1,]
  player_table = data.frame(player_table[-1,],stringsAsFactors=F)
  colnames_i[colnames_i==""] = "drop"
  colnames(player_table) = colnames_i
  player_table = player_table[,-which(colnames(player_table)=="drop")]
  scoring_leaders_all[[starti]] = player_table
  if(player_table$PTS[nrow(player_table)] == "--" | is.na(player_table$PTS[nrow(player_table)])) {
    ind0 = 0
  } else {
    starti = starti + 1
  }
}


scoring_leaders_table = do.call(rbind,scoring_leaders_all)
last_relevant_row = min(which(scoring_leaders_table$PTS == "--" | is.na(scoring_leaders_table$PTS)))-1
scoring_leaders_table = scoring_leaders_table[1:last_relevant_row,]

scoring_leaders_table[scoring_leaders_table[,1]=="",1] = NA
name_split = do.call(rbind,strsplit(scoring_leaders_table[,1],","))
name = gsub("\\*","",name_split[,1])
name = gsub(" D/ST\u00A0D/ST","",name)
team_pos = do.call(rbind,strsplit(name_split[,2],"\u00A0"))
team = gsub(" ","",team_pos[,1])
pos = team_pos[,2]
type = scoring_leaders_table$TYPE
type[grep("WA \\(",type)] = "FA"
colnames(scoring_leaders_table) = gsub("\u00A0","",colnames(scoring_leaders_table))

COMP_ATT = data.frame(do.call(rbind,strsplit(scoring_leaders_table[,"C/A"],"/")),stringsAsFactors=F)
colnames(COMP_ATT) = c("COMP","ATT")
scoring_leaders_table = scoring_leaders_table[,-c(1:6)]
scoring_leaders_table = cbind( data.frame(PLAYER=name,
                                          TEAM=team,
                                          POS=pos,
                                          TYPE=type,
                                          stringsAsFactors=F),
                               scoring_leaders_table)
colnames(scoring_leaders_table)[match(c("YDS","TD","YDS.1","TD.1","YDS.2","TD.2","TD.3"),colnames(scoring_leaders_table))] = c('PASS_YARDS','PASS_TD','RUSH_YARDS','RUSH_TD','REC_YARDS','REC_TD',"MISC_TD")
scoring_leaders_table[,-c(1:4)] = apply(scoring_leaders_table[,-c(1:4)],2,as.numeric)

### Baseline Points ##########
pos_multiplier = data.frame(POS = slot_pos,
                            multiplier = starters_table$no_starters[match(slot_pos,starters_table$abbr)],
                            stringsAsFactors=F)

if(sum(c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX") %in% slots)>0) {
  flex_pos = slots[slots %in% c("RB/WR","RB/WR/TE","WR/TE","QB/RB/WR/TE","FLEX")]
  for(flexi in 1:length(flex_pos)) {
    flexpi = flex_pos[flexi]
    if(flexpi=="FLEX") flexpi = "RB/WR/TE"
    pos_i = strsplit(flexpi,"/")[[1]]
    pos_multiplier[pos_multiplier$POS %in% pos_i,"multiplier"] = 
      0.5 + pos_multiplier[pos_multiplier$POS %in% pos_i,"multiplier"]
  }
}

if("DL" %in% slots) {
  DL_data = data.frame(POS = c("DL","DT","DE"),
                       multiplier = rep(starters_table$no_starters[match("DL",starters_table$abbr)],3),
                       stringsAsFactors=F)
  pos_multiplier = rbind(pos_multiplier,DL_data)
}
if("DB" %in% slots) {
  DB_data = data.frame(POS = c("CB","FS","SS","S"),
                       multiplier = rep(starters_table$no_starters[match("DB",starters_table$abbr)],4),
                       stringsAsFactors=F)
  pos_multiplier = rbind(pos_multiplier,DB_data)
}
if("DP" %in% slots) {
  DP_data = data.frame(POS = c("LB","CB","FS","SS","S","DL","DT","DE"),
                       multiplier = rep(starters_table$no_starters[match("DP",starters_table$abbr)],8),
                       stringsAsFactors=F)
  dupe_vals = pos_multiplier[pos_multiplier$POS %in% DP_data$POS,"POS"]
  pos_multiplier[pos_multiplier$POS %in% dupe_vals,"multiplier"] = 
    pos_multiplier[pos_multiplier$POS %in% dupe_vals,"multiplier"] + DP_data[DP_data$POS %in% dupe_vals,"multiplier"]/2
  DP_data = DP_data[!DP_data$POS %in% dupe_vals,]
  pos_multiplier = rbind(pos_multiplier,DP_data)
}

pos_multiplier$baseline_no = pos_multiplier$multiplier*nrow(teams_table)

baseline_values = NULL
for(bi in 1:nrow(pos_multiplier)) {
  players_i = scoring_leaders_table[scoring_leaders_table$POS==pos_multiplier$POS[bi],]
  if(nrow(players_i)>0) {
    baseline_values[[bi]] = players_i$PTS[min(nrow(players_i),ceiling(pos_multiplier$baseline_no[bi]))]    
  } else {
    baseline_values[[bi]] = 0
  }
}
names(baseline_values) = pos_multiplier$POS

## Calculate points above baseline
scoring_leaders_table$PTS_abv_repl = 
  scoring_leaders_table$PTS -
  baseline_values[match(scoring_leaders_table$POS,names(baseline_values))]

## match with player ID
id_match = scoring_leaders_table[,c("PLAYER","TEAM")]
id_match$TEAM = tolower(id_match$TEAM)
id_match$TEAM[id_match$TEAM=="jac"] = "jax"
id_match$ORDER = 1:nrow(id_match)
matched_ids = merge(id_match,
                    players_id_lookup,
                    by.x=c("PLAYER","TEAM"),
                    by.y=c("NAME","TEAM"),
                    all.x=T,
                    sort=F,
                    incomparables=-1)
matched_ids = matched_ids[order(matched_ids$ORDER),]
scoring_leaders_table$player_id = matched_ids$ID
scoring_leaders_table$scoring_period_id = scoring_period_id

scoring_leaders_table$player_id[scoring_leaders_table$PLAYER %in% team_lookup$team_name] = 
  team_lookup$team_id[match(scoring_leaders_table$PLAYER[scoring_leaders_table$PLAYER %in% team_lookup$team_name],
        team_lookup$team_name)]
        

### Save scoring leaders table ##########
scoring_leaders_scoring_period[[paste0("scoring_period_",scoring_period_id)]] = scoring_leaders_table
save(scoring_leaders_scoring_period,
     file=paste0(shiny_directory,"/www/data_files/scoring_leaders_scoring_period.Rdata"))
scoring_leaders_all = do.call(rbind,scoring_leaders_scoring_period)
write.csv(scoring_leaders_all,file=paste0(shiny_directory,"www/data_files/scoring_leaders_all.csv"),
          row.names=F)



