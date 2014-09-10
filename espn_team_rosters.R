library(RCurl)
library(XML)

getLinks = function() { 
  links = character() 
  list(a = function(node, ...) { 
    links <<- c(links, xmlGetAttr(node, "href"))
    node 
  }, 
       links = function()links)
}
handler1 = getLinks()

nfl.teams = getURL("http://espn.go.com/nfl/teams")
htmlTreeParse(nfl.teams, handlers = handler1)
links = handler1$links()

w.roster.links = grep("/nfl/team/roster/_/name/",links)
roster.links = paste0("http://espn.go.com",links[w.roster.links])

players.list = NULL
for(i in 1:32) {
  team.i = getURL(roster.links[i])
  handler2 = getLinks()
  htmlTreeParse(team.i,handlers=handler2)
  players = handler2$links()
  doc = htmlTreeParse(team.i, useInternalNodes=T)
  root = xmlRoot(doc)
  body = root[["body"]]
  links = xpathSApply(body,"//*/a")
  w.players = grep("http://espn.go.com/nfl/player/_/id/",players)
  id.name = gsub("http://espn.go.com/nfl/player/_/id/","",players[w.players])
  player_names = xpathSApply(doc,"//*/td/a",xmlValue)
  player_names = player_names[!player_names %in% c("NO","NAME","POS","AGE","HT","WT","EXP")]
  id.name.data = as.data.frame(do.call(rbind,strsplit(id.name,"/")))
  id.name.data$team = strsplit(gsub("http://espn.go.com/nfl/team/roster/_/name/","",roster.links[i]),"/")[[1]][1]
  id.name.data$NAME = player_names
  players.list[[i]] = id.name.data
}
all.players = do.call(rbind,players.list)
colnames(all.players) = c("ID","ID_NAME","TEAM","NAME")

write.csv(all.players,c("/home/jazz12man/ShinyApps/fantasy_football_14/www/data_files/player_id_key.csv"),row.names=F)
players_id_lookup = data.frame(lapply(all.players,as.character),stringsAsFactors=F)
save(players_id_lookup,file=paste0(shiny_directory,"www/data_files/players_id_lookup.Rdata"))

for(i in 1:nrow(all.players)) {
  tryCatch(download.file(paste0("http://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",all.players$ID[i],".png&w=35&h=48&scale=crop&background=0xcccccc&transparent=true"),
                paste0("/home/jazz12man/ShinyApps/fantasy_football_14/www/player_images/",all.players$ID[i],".png")),
           error=function(e) NULL)
  if(i %% 100 == 0 ) cat(i,"~",100*i/nrow(all.players),"%")
#   Sys.sleep(sample(seq(.1,1,.1),1))
}

download.file("http://dl.dropboxusercontent.com/u/14343406/nophoto.PNG",
              "/home/jazz12man/ShinyApps/fantasy_football_14/www/player_images/no_photo.png")

# download.file("http://dl.dropboxusercontent.com/u/14343406/nophoto.PNG",
#               "/home/jazz12man/ShinyApps/fantasy_football_13/player_images/no_photo.png")




lapply(team.lookup$team.id,function(team)
download.file(paste0("http://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/",team,".png&w=80&h=80&scale=crop"),
              paste0("/home/jazz12man/ShinyApps/fantasy_football_14/www/player_images/",team,".png"))
)
download.file("http://bronxcapone.com/Quickstart/ImageLib/nflo.png",
              paste0("/home/jazz12man/ShinyApps/fantasy_football_14/www/player_images/nfl.png"))



team_lookup = data.frame(
  team_name = c(
    "Broncos",
    "Colts",
    "Bengals",
    "Saints",
    "Titans",
    "Steelers",
    "Chiefs",
    "49ers",
    "Ravens",
    "Redskins",
    "Lions",
    "Cowboys",
    "Patriots",
    "Seahawks",
    "Buccaneers",
    "Cardinals",
    "Texans",
    "Falcons",
    "Chargers",
    "Bears",
    "Raiders",
    "Bills",
    "Rams",
    "Giants",
    "Vikings",
    "Jets",
    "Dolphins",
    "Browns",
    "Jaguars",
    "Eagles",
    "Panthers",
    "Packers"),
  team_id = c(
    "den",
    "ind",
    "cin",
    "no",
    "ten",
    "pit",
    "kc",
    "sf",
    "bal",
    "wsh",
    "det",
    "dal",
    "ne",
    "sea",
    "tb",
    "ari",
    "hou",
    "atl",
    "sd",
    "chi",
    "oak",
    "buf",
    "stl",
    "nyg",
    "min",
    "nyj",
    "mia",
    "cle",
    "jac",
    "phi",
    "car",
    "gb"),stringsAsFactors=F)
write.csv(team_lookup,
          "/home/jazz12man/ShinyApps/fantasy_football_14/www/data_files/team_lookup.csv",
          row.names=F)
save(team_lookup,file=paste0(shiny_directory,"www/data_files/team_lookup.Rdata"))


