url = "http://games.espn.go.com/ffl/schedule?leagueId=1193762"

# Pull in and manipulate URL
url_in = getURL(url)
doc = htmlTreeParse(url_in, useInternalNodes=T)
root = xmlRoot(doc)
body = root[["body"]]

rows = xpathSApply(body,"//*/table[@class= 'tableBody']/tr")
rowAttrs = xpathSApply(body,"//*/table[@class= 'tableBody']/tr",xmlAttrs)

attrs = lapply(rowAttrs,function(x) if("class" %in% names(x)) x[["class"]])

w_head = which(attrs=="tableHead")
w_subhead = which(attrs=="tableSubHead")

starts = w_subhead + 1
starts = starts
ends = (w_head - 2)[-1][1:11]

scoring_period_matchups = do.call(rbind,lapply(1:length(starts), function(y) {   
  do.call(rbind,lapply(starts[y]:ends[y], function(x) {
    ch = xmlChildren(rows[[x]])
    split1 = strsplit(xpathSApply(ch[1]$td,"a/@href"),"teamId=")[[1]][2]
    team1 = strsplit(split1,"&")[[1]][1]
    split2 = strsplit(xpathSApply(ch[7]$td,"a/@href"),"teamId=")[[1]][2]
    team2 = strsplit(split2,"&")[[1]][1]
    team1 = c(team1,team2)
    team2 = c(team2,team1[1])
    #     cat(team1,team2,"\n")
    return(data.frame(team1,team2,
                      scoring_period_id=paste0("scoring_period_",y),
                      stringsAsFactors=F))
  })
  )
})
)

write.csv(scoring_period_matchups,paste0(shiny_directory,"www/data_files/scoring_period_matchups.csv"),row.names=F)

write.csv(matchup_dates,paste0(shiny_directory,"www/data_files/matchup_dates.csv"),row.names=F)
