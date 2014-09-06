### Settings ##########
# league_id = 731939
# league_id = 1193762
# season_id = 2014
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
pos_names = unlist(positions_table_in[3,])
positions_table_sub = positions_table_in[4:nrow(positions_table_in),]
pos_split1 = do.call(rbind,strsplit(positions_table_sub[,1]," \\("))
positions_table = data.frame(position = pos_split1[,1],
                             abbr = substr(pos_split1[,2],1,nchar(pos_split1[,2])-1),
                             no_starters = as.numeric(positions_table_sub[,2]),
                             max_own = as.numeric(positions_table_sub[,3]),
                             stringsAsFactors = F)
positions_table$abbr = gsub("D/ST","DST",positions_table$abbr)


starters_table = positions_table[!positions_table$abbr %in% c("BE","IR"),]
