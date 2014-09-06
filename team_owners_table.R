url = paste0("http://games.espn.go.com/ffl/leagueoffice?leagueId=",league_id,
             "&seasonId=",season_id)

# Pull in and manipulate URL
url_in = getURL(url)
doc = htmlTreeParse(url_in, useInternalNodes=T)
root = xmlRoot(doc)
body = root[["body"]]

# parse through body to find table of interest for team names
li = xpathSApply(body,"//*/li[@class= ' games-teams-btn']")[[1]]
ul = li[["ul"]]
n0 = length(names(ul))

team_no = lapply(1:n0,function(x)
  strsplit(strsplit(xmlGetAttr(ul[[x]][["a"]],"href"),"teamId=")[[1]][2],"&seasonId=")[[1]][1])
team_no = as.numeric(unlist(team_no))
teams = unlist(lapply(1:n0,function(x) xmlValue(ul[[x]][["a"]][["text"]])))
teams = sapply(teams,function(x) substr(x,1,nchar(x)-1))
nicknames = unlist(lapply(1:n0,function(x) xmlValue(ul[[x]][["a"]][["span"]][["text"]])))
nicknames = gsub("\\(","",gsub("\\)","",nicknames))

# DIVISION
div_table = readHTMLTable(url_in)[[2]]
divs = as.character(div_table[,1])
div_teams = as.character(div_table[,2])
div_teams = div_teams[!is.na(div_teams)]
div_u = divs[nchar(divs)>3]
divs_all = rep(div_u,each=length(teams)/length(div_u))
names(divs_all) = div_teams

# OWNER NAME
teams_table = data.frame(team_no, teams, nicknames,
                         division = divs_all[match(teams,names(divs_all))],
                         stringsAsFactors=F)

