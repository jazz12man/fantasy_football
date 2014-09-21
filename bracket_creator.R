bracket_creator = function(byes=byes,
                           no_playoff_teams=no_playoff_teams,
                           no_playoff_rounds=no_playoff_rounds) {
  if(byes==2 & no_playoff_teams==6 & no_playoff_rounds==3) {
    par(mar = rep(0,4))
    plot(0,type="n",xlim=c(0.5,no_playoff_rounds+2.5),ylim=c(0.5,no_playoff_teams+1.5),axes=F)
    nl = no_playoff_teams-byes
    bye_ends = NULL
    for(xx in 1:nl) {
      lines(c(1,2),rep(xx+1,2))
      if(xx %% 2 == 0) {
        lines(rep(2,2),c(xx,xx+1))
        lines(c(2,3),rep(xx+0.5,2))
        bye_ends = c(bye_ends,xx+0.5)
      }
    }
    nb = byes
    bye_starts = c(1,nl+2)
    newmid = NULL
    for(xx in 1:nb) {
      lines(c(2,3),rep(bye_starts[xx],2))
      lines(rep(3,2),c(bye_starts[xx],bye_ends[xx]))
      newmid = c(newmid,mean(c(bye_starts[xx],bye_ends[xx])))
    }
    for(xx in 1:length(newmid)) {
      lines(c(3,4),rep(newmid[xx],2))
    }
    lines(c(4,4),newmid)
    lines(c(4,5),rep(mean(newmid),2))
    
    prob_pos = data.frame(col = c("Playoff Odds","Bye Odds","Semi-Finals Odds","Finals Odds","Champion Odds"),
                          x = c(1.5,2.5,2.5,3.5,4.5),
                          y = c(2,1,2.5,newmid[1],mean(newmid)),
                          stringsAsFactors=F)
    for(q in 1:nrow(prob_pos)) {
      text(prob_pos[q,"x"],prob_pos[q,"y"],prob_pos[q,"col"],pos=1)
    }
    
  } else if(byes==0 & no_playoff_teams==8 & no_playoff_rounds==3) {
    par(mar = rep(0,4))
    plot(0,type="n",xlim=c(0.5,no_playoff_rounds+2.5),ylim=c(0.5,no_playoff_teams+2.5),axes=F)
    nl = no_playoff_teams-byes
    nl_ends = NULL
    for(xx in 1:nl) {
      lines(c(1,2),rep(xx+1,2))
      if(xx %% 2 == 0) {
        lines(rep(2,2),c(xx,xx+1))
        lines(c(2,3),rep(xx+0.5,2))
        nl_ends = c(nl_ends,xx+0.5)
      }
    }
    lines(rep(3,2),nl_ends[1:2])
    lines(rep(3,2),nl_ends[3:4])
    lines(c(3,4),rep(mean(nl_ends[1:2]),2))
    lines(c(3,4),rep(mean(nl_ends[3:4]),2))
    lines(rep(4,2),c(mean(nl_ends[1:2]),mean(nl_ends[3:4])))
    lines(c(4,5),rep(mean(nl_ends),2))
    
    
    prob_pos = data.frame(col = c("Playoff Odds","Semi-Finals Odds","Finals Odds","Champion Odds"),
                          x = c(1.5,2.5,3.5,4.5),
                          y = c(1,1.5,2,4),
                          stringsAsFactors=F)
    for(q in 1:nrow(prob_pos)) {
      text(prob_pos[q,"x"],prob_pos[q,"y"],prob_pos[q,"col"],pos=1)
    } 
  }
  return(prob_pos)
}
