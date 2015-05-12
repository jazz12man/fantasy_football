actual_all = read.csv(file=paste0(shiny_directory,"www/data_files/team_tables_actual_all.csv"),
                      stringsAsFactors=F)
completed_scoring_periods = 1:(which(scoring_period_table$end_date >= Sys.Date() & scoring_period_table$start_date <= Sys.Date()) - 2)

## Get scores
scores = dcast(actual_all, team_id~scoring_period_id, fun.aggregate=sum, value.var = "PTS",na.rm=T)
scores = scores[,c("team_id",paste0("scoring_period_",completed_scoring_periods))]
scores_m = melt(scores, id.var="team_id")

scoring_period_matchups[scoring_period_matchups$scoring_period_id %in% colnames(scores),]

vals1 = merge(scoring_period_matchups,scores_m,
              by.x=c("team1","scoring_period_id"),
              by.y=c("team_id","variable"),
              all.x=T,
              sort=F)
vals2 = merge(scoring_period_matchups,scores_m,
              by.x=c("team2","scoring_period_id"),
              by.y=c("team_id","variable"),
              all.x=T,
              sort=F)
colnames(vals1)[colnames(vals1)=="value"] = "team1_score"
colnames(vals2)[colnames(vals2)=="value"] = "team2_score"
vals_all = merge(vals1,vals2,sort=F)
vals_all = vals_all[,c("scoring_period_id","team1","team2","team1_score","team2_score")]

# library(lme4,lib.loc="/home/jazz12man/R/library")

# m1 = lmer(value ~ (1 | team_id),data=scores_m)
# m1sum = summary(m1)
delta_means = (tapply(scores_m$value,scores_m$team_id,mean) - mean(scores_m$value))/3

scores_all = sapply(teams_table$team_no,function(x) NULL)
for(qq in teams_table$team_no) {
#   if(sum(ranef(m1)$team_id)<0.001) {
      scores_all[[qq]] = rnorm(100000,
                               mean(scores_m$value) + delta_means[qq],
                               sqrt(1 + sd(scores_m$value[scores_m$team_id==qq]))) +
        rnorm(100000,0,sd(scores_m$value))
#       scores_all[[qq]] = rnorm(100000,
#                              fixef(m1) + delta_means[qq],
#                              sqrt(1 + m1sum$coefficients[1,"Std. Error"]^2)) +
#       rnorm(100000,0,sigma(m1))
#   } else {
#     scores_all[[qq]] = rnorm(100000,
#                              fixef(m1) + ranef(m1)$team_id[qq,],
#                              sqrt(m1sum$varcor$team[1] + m1sum$coefficients[1,"Std. Error"]^2)) +
#       rnorm(100000,0,sigma(m1))
#   }
}

scores_matrix = matrix(NA,nrow(teams_table),nrow(teams_table))
for(qq in 1:nrow(teams_table)) scores_matrix[qq,] = sapply(scores_all,function(x) mean(scores_all[[qq]]>x))
scores2 = scores


regular_season_scoring_periods = all_matchups[grep("scoring_period",all_matchups)]
scores[,regular_season_scoring_periods[!regular_season_scoring_periods %in% colnames(scores)]] = NA
w_unplayed = names(which(is.na(apply(scores,2,sum))))
w_played = names(which(!is.na(apply(scores,2,sum))))

results_all = data.frame(matrix(NA,nrow(teams_table),length(regular_season_scoring_periods)))
rownames(results_all) = teams_table$team_no
colnames(results_all) = regular_season_scoring_periods
for(q in 1:length(regular_season_scoring_periods)) {
  spmq = scoring_period_matchups[scoring_period_matchups[,"scoring_period_id"]==regular_season_scoring_periods[q],]
  if(all_matchups[q] %in% w_unplayed) {
    results_all[match(spmq$team1,rownames(results_all)),all_matchups[q]] = diag(scores_matrix[as.numeric(spmq$team1),as.numeric(spmq$team2)])
  } else {
    scoresq = scores[,regular_season_scoring_periods[q]]
    names(scoresq) = scores[,"team_id"]
    score1 = scoresq[match(spmq$team1,names(scoresq))]
    score2 = scoresq[match(spmq$team2,names(scoresq))]
    wins = score1 > score2
    ties = score1 == score2
    results_all[match(names(wins),rownames(results_all)),regular_season_scoring_periods[q]] = ifelse(wins==T,1,ifelse(ties==T,1,0))
  }
}

nsims = 1000
nsamples = 1000
seasons_all = lapply(1:nsims, function(x) return(results_all))
final_standings_all = lapply(1:nsims, function(x) NULL)
playoff_results_all = lapply(1:nsims, function(x) NULL)
for(sim in 1:nsims) {
  for(q in 1:length(w_unplayed)) {
    spmq = scoring_period_matchups[scoring_period_matchups[,"scoring_period_id"]==w_unplayed[q],]
    samplesq = sapply(spmq$team1, function(x) sample(scores_all[[as.numeric(x)]],1))
    names(samplesq) = spmq$team1
    win = samplesq[spmq$team1] > samplesq[spmq$team2]
    win = win[order(as.numeric(names(win)))]
    seasons_all[[sim]][,w_unplayed[q]] = ifelse(win,1,0)
  }
  standings = apply(seasons_all[[sim]],1,sum)
  standings = standings[order(standings,decreasing=T)]
  
  playoff_bracket = data.frame(team = names(standings)[1:no_playoff_teams],
                               byes = 0,
                               stringsAsFactors=F)
  rownames(playoff_bracket) = paste0("seed_",1:no_playoff_teams)
  playoff_bracket[,all_matchups[grep("playoff",all_matchups)]] = NA
  
  playoff_bracket[,all_matchups[grep("playoff",all_matchups)]] = 
    do.call(rbind,sapply(as.list(playoff_bracket$team), function(x) {
      xout = data.frame(matrix(sample(scores_all[[as.numeric(x)]],no_playoff_rounds*weeks_per_round),
                               no_playoff_rounds,weeks_per_round,byrow=T))
      if(nrow(xout)>1) xout = data.frame(apply(xout,1,sum))
    }))
  
  if(byes>0) {
    playoff_bracket$byes[1:byes] = 1
    round1 = playoff_bracket$team[playoff_bracket$byes!=1]
    matchups1 = lapply(1:(length(round1)/2),function(x) round1[c(x,length(round1)-x+1)])
    winners1 = lapply(matchups1,function(x) {
      xout = playoff_bracket[match(x,playoff_bracket$team),"playoff_1"]
      names(xout) = playoff_bracket[match(x,playoff_bracket$team),"team"]
      return(names(xout)[order(xout,decreasing=T)][1])
    })
    winners2 = lapply(1:byes, function(x) {
      teamsx = c(playoff_bracket[playoff_bracket$byes==1,"team"][x],winners1[[length(winners1)-x+1]])
      xout = playoff_bracket[match(teamsx,playoff_bracket$team),"playoff_2"]
      names(xout) = playoff_bracket[match(teamsx,playoff_bracket$team),"team"]
      return(names(xout)[order(xout,decreasing=T)][1])
    })
    winners_out = NULL
    winners_out[[1]] = c(playoff_bracket$team[playoff_bracket$bye==1],unlist(winners1))
    winners_out[[2]] = unlist(winners2)
    winners_all = winners2
    sp = 3
    while(length(winners_all)>1) {
      winnersx = lapply(1:(length(winners_all)/2), function(x) {
        teamsx = c(winners_all[[x]],winners_all[[length(winners_all)-x+1]])
        xout = playoff_bracket[match(teamsx,playoff_bracket$team),paste0("playoff_",sp)]
        names(xout) = playoff_bracket[match(teamsx,playoff_bracket$team),"team"]
        return(names(xout)[order(xout,decreasing=T)][1])
      })
      winners_out[[sp]] = unlist(winnersx)
      winners_all = winnersx
      sp = sp+1
    }
  } else {
    winners_all = lapply(playoff_bracket$team,function(x) x)
    winners_out = NULL
    sp = 1
    while(length(winners_all)>1) {
      winnersx = lapply(1:(length(winners_all)/2), function(x) {
        teamsx = c(winners_all[[x]],winners_all[[length(winners_all)-x+1]])
        xout = playoff_bracket[match(teamsx,playoff_bracket$team),paste0("playoff_",sp)]
        names(xout) = playoff_bracket[match(teamsx,playoff_bracket$team),"team"]
        return(names(xout)[order(xout,decreasing=T)][1])
      })
      winners_out[[sp]] = unlist(winnersx)
      winners_all = winnersx
      sp = sp+1
    }
  }
  final_standings_all[[sim]] = names(standings)
  playoff_results_all[[sim]] = winners_out
}

save(playoff_results_all,
     file=paste0(shiny_directory,"/www/data_files/playoff_results_all.Rdata"))
save(final_standings_all,
     file=paste0(shiny_directory,"/www/data_files/final_standings_all.Rdata"))
save(results_all,
     file=paste0(shiny_directory,"/www/data_files/results_all.Rdata"))

