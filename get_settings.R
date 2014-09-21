### Settings ##########
# league_id = 731939
# league_id = 1193762
# season_id = 2014
get_settings = function(league_id, season_id) {
  url = paste0("http://games.espn.go.com/ffl/leaguesetup/settings?leagueId=",league_id,
               "&seasonId=",season_id)
  url_in = getURL(url)
  doc = htmlTreeParse(url_in, useInternalNodes=T)
  
  tables = xpathSApply(doc,"//*/table[@class= 'leagueSettingsTable tableBody viewable']")
  # Summary Stats
  roster_stats_in = xpathSApply(doc,"//*/td[@class= 'dataSummary settingLabel']/p")
  roster_stats_vals = lapply(roster_stats_in,xmlValue)
  roster_stats_table = do.call(rbind,lapply(roster_stats_vals,function(x) strsplit(x,": ")[[1]]))
  if(length(grep("\\(",roster_stats_table[,2]))>0) {
    roster_stats_table = rbind(roster_stats_table,roster_stats_table[1,])
    roster_stats_table[4,1] = "IR"
    bench_ir = strsplit(roster_stats_table[grep("\\(",roster_stats_table[,2]),2]," \\(")[[1]]
    roster_stats_table[3,2] = bench_ir[1]
    roster_stats_table[4,2] = strsplit(bench_ir[2]," ")[[1]][1]
  }
  roster_stats_table = data.frame(roster_stats_table,stringsAsFactors=F)
  roster_stats_table[,2] = as.numeric(roster_stats_table[,2])
  
  # positions and numbers
  positions_table_in = readHTMLTable(tables[[1]],stringsAsFactors=F)
  position_start = which(positions_table_in[,1]=="Position")+1
  pos_names = unlist(positions_table_in[position_start-1,])
  positions_table_sub = positions_table_in[position_start:nrow(positions_table_in),]
  pos_split1 = do.call(rbind,strsplit(positions_table_sub[,1]," \\("))
  max_own = positions_table_sub[,3]
  max_own[max_own=="N/A"] = NA
  positions_table = data.frame(position = pos_split1[,1],
                               abbr = substr(pos_split1[,2],1,nchar(pos_split1[,2])-1),
                               no_starters = as.numeric(positions_table_sub[,2]),
                               max_own = as.numeric(max_own),
                               stringsAsFactors = F)
  
  
  starters_table = positions_table[!positions_table$abbr %in% c("BE","IR"),]
  
  starters_table$lookup_table = NA
  starters_table$lookup_table[starters_table$abbr %in% c("QB","RB","RB/WR","RB/WR/TE","WR/TE","WR","TE")] = "OFFENSE"
  starters_table$lookup_table[starters_table$abbr %in% c("DT","DE","LB","DL","CB","S","DB","DP")] = "DEFENSE"
  starters_table$lookup_table[starters_table$abbr %in% c("K")] = "KICKERS"
  starters_table$lookup_table[starters_table$abbr %in% c("D/ST")] = "D/ST"
  
  slots = starters_table$abbr[starters_table$no_starters>0]
  slot_pos = slots[slots %in% c("QB","RB","WR","TE","D/ST","K","LB","DE","DT","CB","S")]
  
  ### no_matchups #########
  tables = xpathSApply(doc,"//*/table[@class= 'leagueSettingsTable tableBody']")
  # Summary Stats
  regular_season_weeks = readHTMLTable(tables[[7]],stringsAsFactors=F)
  no_weeks1 = regular_season_weeks$V2[regular_season_weeks$V1=="Regular Season Matchups"]
  no_matchups = as.numeric(strsplit(no_weeks1," ")[[1]][1])
  matchup_weeks = paste0("scoring_period_",1:no_matchups)
  # playoffs
  playoffs_weeks = readHTMLTable(tables[[8]],stringsAsFactors=F)
  no_playoffs1 = playoffs_weeks$V2[playoffs_weeks$V1=="Playoff Teams"]
  no_playoffs2 = strsplit(no_playoffs1,"\\(")[[1]][2]
  no_playoffs = as.numeric(strsplit(no_playoffs2," ")[[1]][1])
  playoff_weeks = paste0("playoff_",1:no_playoffs)
  # playoff teams
  no_playoff_teams = as.numeric(strsplit(no_playoffs1," ")[[1]][1])
  if(length(grep("Byes",no_playoffs2))>0) {
    byes1 = strsplit(no_playoffs2,",")[[1]][2]
    byes2 = gsub(" Byes)","", byes1)
    byes = as.numeric(byes2)
  } else {
    byes = 0
  }
  weeks_per_round1 = playoffs_weeks$V2[grep("weeks per round",playoffs_weeks$V2)]
  weeks_per_round = as.numeric(gsub(" weeks per round","",weeks_per_round1))
  
  # all matchups
  all_matchups = c(matchup_weeks,playoff_weeks)
  names(all_matchups) = gsub("_"," ",all_matchups)
  
  
  
  ### scoring_period_table ##########
  url = paste0("http://games.espn.go.com/ffl/leaguesetup/settings?leagueId=",league_id)
  
  url_in = getURL(url)
  doc = htmlTreeParse(url_in, useInternalNodes=T)
  root = xmlRoot(doc)
  body = root[["body"]]
  
  ##
  draft_table = readHTMLTable(xpathSApply(body,"//*/table[@id='draftSettingsTable']")[[1]],stringsAsFactors=F)
  draft_date1 = draft_table$V2[draft_table$V1=="Draft Date"]
  draft_date2 = strsplit(draft_date1,", ")[[1]]
  draft_date = paste(draft_date2[1:2],collapse=" ")
  draft_date = as.Date(draft_date,"%A %B %d")
  
  ##
  schedule_table = readHTMLTable(xpathSApply(body,"//*/table[@class='leagueSettingsTable tableBody']")[[7]],stringsAsFactors=F)
  no_matchups1 = schedule_table$V2[schedule_table$V1=="Regular Season Matchups"]
  no_matchups = as.numeric(strsplit(no_matchups1,"\u0020")[[1]][1])
  
  ##
  playoff_table = readHTMLTable(xpathSApply(body,"//*/table[@class='leagueSettingsTable tableBody']")[[8]],stringsAsFactors=F)
  playoff_team_rounds = playoff_table$V2[playoff_table$V1=="Playoff Teams"]
  no_playoff_teams = as.numeric(strsplit(playoff_team_rounds,"\u0020")[[1]][1])
  no_playoff_rounds = as.numeric(gsub("\\(","",strsplit(playoff_team_rounds,"\u0020")[[1]][2]))
  weeks_per_round = as.numeric(strsplit(playoff_table$V2[playoff_table$V1=="Weeks per Playoff Matchup"],"\u0020")[[1]][1])
  
  start_1 = draft_date + 1
  end_1 = as.Date("2014-09-08")
  starts = c(start_1,seq(end_1+1,by=7,length.out=no_matchups-1))
  ends = c(end_1,seq(end_1+7,by=7,length.out=no_matchups-1))
  
  start_playoffs1 = ends[length(ends)] + 1
  starts_playoffs = seq(start_playoffs1,by=7*weeks_per_round,length.out=no_playoff_rounds)
  end_playoffs1 = starts_playoffs[2] - 1
  ends_playoffs = seq(end_playoffs1,by=7*weeks_per_round,length.out=no_playoff_rounds)
  
  scoring_period_table = data.frame(scoring_period_id = c(paste0("scoring_period_",1:no_matchups),
                                                          paste0("playoff_period_",1:no_playoff_rounds)),
                                    start_date = c(starts,starts_playoffs),
                                    end_date = c(ends,ends_playoffs),
                                    stringsAsFactors=F)
  scoring_period_table = rbind(scoring_period_table[1,],scoring_period_table)
  scoring_period_table$scoring_period_id[1] = "draft_date"
  scoring_period_table$start_date[1] = draft_date
  scoring_period_table$end_date[1] = draft_date
  
  
  return(list(slots=slots,
              slot_pos=slot_pos,
              starters_table=starters_table,
              positions_table=positions_table,
              all_matchups=all_matchups,
              scoring_period_table=scoring_period_table,
              draft_date=draft_date,
              no_playoff_teams=no_playoff_teams,
              no_playoff_rounds=no_playoff_rounds,
              byes=byes,
              weeks_per_round=weeks_per_round))
}
