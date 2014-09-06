library(data.table)
league_id = 1193762
start_index = seq(0,5000,by=50)
ind0 = 1
starti = 1
scoring_leaders_all = NULL
while(ind0==1) {
  url = paste0("http://games.espn.go.com/ffl/leaders?leagueId=",league_id,"&teamId=1&scoringPeriodId=1&startIndex=",start_index[starti])
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
pos[pos=="D/ST"] = "DST"
type = scoring_leaders_table$TYPE
type[grep("WA \\(",type)] = "FA"

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

scoring_leaders_table = data.table(scoring_leaders_table)

### Baseline Points ##########
pos_multiplier = data.frame(POS = slot_pos,
                            multiplier = starters_table$no_starters[match(slot_pos,starters_table$abbr)],
                            stringsAsFactors=F)
if(length(slot_pos)<nrow(starters_table)) {
  missing = starters_table[-which(starters_table$abbr %in% slot_pos),]
  for(missingi in 1:nrow(missing)) {
    missing_adds = strsplit(missing[missingi,"abbr"],"\\/")[[1]]
    pos_multiplier[match(missing_adds,pos_multiplier$POS),"multiplier"] = pos_multiplier[match(missing_adds,pos_multiplier$POS),"multiplier"] + 0.5
  }
}

pos_multiplier$baseline_no = pos_multiplier$multiplier*nrow(teams_table)

baseline_values = NULL
for(bi in 1:nrow(pos_multiplier)) {
  players_i = scoring_leaders_table[POS==slot_pos[bi]]
  baseline_values[[bi]] = players_i$PTS[min(nrow(players_i),ceiling(pos_multiplier$baseline_no[bi]))]
}
names(baseline_values) = slot_pos
