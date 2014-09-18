projections_all = read.csv(file=paste0(shiny_directory,"www/data_files/projections_all.csv"),
                           stringsAsFactors=F)
scoring_leaders_all = read.csv(file=paste0(shiny_directory,"www/data_files/scoring_leaders_all.csv"),
                               stringsAsFactors=F)

proj = projections_all[projections_all$scoring_period_id==1,]
scrl = scoring_leaders_all[scoring_leaders_all$scoring_period_id==1,]

proj = proj[proj$PLAYER %in% scrl$PLAYER,]
scrl = scrl[scrl$PLAYER %in% proj$PLAYER,]

proj = proj[match(scrl$player_id,proj$player_id),]

##
deltas = lapply(c("PASS_YARDS","INT","PASS_TD",
                  "RUSH","RUSH_YARDS","RUSH_TD",
                  "REC","REC_YARDS","REC_TD",
                  "PTS"), function(x) {
                    delta = scrl[,x] - proj[,x]
                    delta[scrl[,x]==0 & proj[,x]==0] = NA
                    ape = abs(delta/scrl[,x])
                    ape[ape==Inf] = NA
                    return(delta)
                  })
##
apes = lapply(c("PASS_YARDS","INT","PASS_TD",
                "RUSH","RUSH_YARDS","RUSH_TD",
                "REC","REC_YARDS","REC_TD",
                "PTS"), function(x) {
                  delta = scrl[,x] - proj[,x]
                  delta[scrl[,x]==0 & proj[,x]==0] = NA
                  ape = abs(delta/scrl[,x])
                  ape[ape==Inf] = NA
                  return(ape)
                })
##
sqerrs = lapply(c("PASS_YARDS","INT","PASS_TD",
                  "RUSH","RUSH_YARDS","RUSH_TD",
                  "REC","REC_YARDS","REC_TD",
                  "PTS"), function(x) {
                    delta = scrl[,x] - proj[,x]
                    delta[scrl[,x]==0 & proj[,x]==0] = NA
                    sqerr = delta^2
                    return(sqerr)
                  })
##
names(deltas) = names(apes) = names(sqerrs) = 
  c("PASS_YARDS","INT","PASS_TD",
    "RUSH","RUSH_YARDS","RUSH_TD",
    "REC","REC_YARDS","REC_TD",
    "PTS")

MSE = lapply(sqerrs,mean,na.rm=T)
rootMSE = lapply(MSE,sqrt)



pdf("MSE.pdf",width=10)
lapply(c("PASS_YARDS",
         "RUSH_YARDS",
         "REC_YARDS",
         "PTS"), function(x) {
           
           projx = proj[,x]
           scrlx = scrl[,x]
           
           w_remove = which(projx==0 & scrlx==0)
           if(length(w_remove)>0) {
             projx = projx[-w_remove]
             scrlx = scrlx[-w_remove]
             delta = deltas[-w_remove]
           }
           
           plot(projx,scrlx,main=x)
           
           new_order = order(projx,scrlx)
           projx = projx[new_order]
           scrlx = scrlx[new_order]
           
           stdev = rootMSE[[x]]
           
           ## 80%
           upper = projx+stdev*qnorm(0.9)
           lower = projx-stdev*qnorm(0.9)
           
           lower[lower<0] = 0
           lower[projx<10] = 0
           
           plot(1:length(scrlx),scrlx,col="blue",pch=16,ylim=c(min(lower),max(upper)))
           points(1:length(projx),projx,col="forestgreen",pch=15)
           lapply(1:length(lower),function(x) {
             lines(rep(x,2),c(lower[x],upper[x]),col="forestgreen",lwd=2)
           })
           title(paste0(x," 80% CI\nCoverage: ", round(100*mean(lower < scrlx & upper > scrlx)),"%"))

           ## 90%
           upper = projx+stdev*qnorm(0.95)
           lower = projx-stdev*qnorm(0.95)
           plot(1:length(scrlx),scrlx,col="blue",pch=16,ylim=c(min(lower),max(upper)))
           points(1:length(projx),projx,col="forestgreen",pch=15)
           lapply(1:length(lower),function(x) {
             lines(rep(x,2),c(lower[x],upper[x]),col="forestgreen",lwd=2)
           })
           title(paste0(x," 90% CI \nCoverage: ", round(100*mean(lower < scrlx & upper > scrlx)),"%"))
           
         })
dev.off()

### Probability Distributions of TDs ####
lapply(c("PASS_TD",
         "RUSH_TD",
         "REC_TD"), function(x) {
           table_out = table(proj[,x],scrl[,x])
           data_out = data.frame(
             names = c("P(>=1 | proj >=1)",
                       "sum(proj>=1)",
                       "P(>=2 | proj >=2)",
                       "sum(proj>=2)",
                       "P(>=3 | proj >=3)",
                       "sum(proj>=3)",
                       "P(actual>=proj+1)",
                       "P(actual>=proj+2)"),
             vals = c(mean((scrl[,x]>=1)[proj[,x]>=1]),
                      sum(proj[,x]>=1),
                      mean((scrl[,x]>=2)[proj[,x]>=2]),
                      sum(proj[,x]>=2),
                      mean((scrl[,x]>=3)[proj[,x]>=3]),
                      sum(proj[,x]>=3),
                      mean(scrl[,x]>proj[,x]),
                      mean(scrl[,x]>proj[,x]+1)))
           return(list(table_out=table_out,data_out=data_out))
         })

table(scrl$PASS_TD[proj$PASS_TD==1])/17
table(scrl$PASS_TD[proj$PASS_TD==2])
table(scrl$PASS_TD[proj$PASS_TD>=3])

table(scrl$RUSH_TD[proj$RUSH_YARDS>10 & proj$RUSH_TD==0])
table(scrl$RUSH_TD[proj$RUSH_TD==1])
table(scrl$RUSH_TD[proj$RUSH_TD>=2])

table(scrl$REC_TD[proj$REC_YARDS>10 & proj$REC_TD==0])/sum(proj$REC_YARDS>10 & proj$REC_TD==0)
table(scrl$REC_TD[proj$REC_TD==1])/sum(proj$REC_TD==1)
table(scrl$REC_TD[proj$REC_TD>=2])/sum(proj$REC_TD>=2)

#Kicker
plot(proj[proj$POS=="K" & !is.na(proj$PTS),"PTS"]-scrl[scrl$POS=="K","PTS"])
m1 = lm(scrl[scrl$POS=="K","PTS"]~proj[proj$POS=="K" & !is.na(proj$PTS),"PTS"])
k_stdev = summary(m1)$sigma

#D/ST
plot(proj[proj$POS=="D/ST" & !is.na(proj$PTS),"PTS"],scrl[scrl$POS=="D/ST","PTS"])
m2 = lm(scrl[scrl$POS=="D/ST","PTS"]~proj[proj$POS=="D/ST" & !is.na(proj$PTS),"PTS"])
dst_stdev = summary(m2)$sigma

### Projection Simulations ####
projections_sp = projections_all[projections_all$scoring_period_id==scoring_period_id,]

team1 = 1
team2 = 12
scoring_period_id = 2

teamA = actual_all[actual_all$team_id==team1 & actual_all$scoring_period_id==scoring_period_id,]
teamA$PLAYER = gsub(" D/ST\u00A0D/ST","",teamA$PLAYER)
w_actualA = which(!is.na(teamA$PTS))

teamB = actual_all[actual_all$team_id==team2 & actual_all$scoring_period_id==scoring_period_id,]
teamB$PLAYER = gsub(" D/ST\u00A0D/ST","",teamB$PLAYER)
w_actualB = which(!is.na(teamB$PTS))

projections_A = projections_sp[projections_sp$TYPE==teams_table$nicknames[match(team1,teams_table$team_no)],]
projections_A = projections_A[match(teamA$PLAYER,projections_A$PLAYER),]

projections_B = projections_sp[projections_sp$TYPE==teams_table$nicknames[match(team2,teams_table$team_no)],]
projections_B = projections_B[match(teamB$PLAYER,projections_B$PLAYER),]

projections_fun = function(projections = projections_A,team = teamA) {
  proj_pts_all = sapply(team$PLAYER, function(x) NULL)
  for(i in 1:nrow(projections)) {
    x = projections[i,]
    if(x$POS %in% c("TE","RB","WR","QB")) {
      ### Pass
      if(x$PASS_YARDS>10) {
        # Yards
        pass_yards = rnorm(1000,x$PASS_YARDS,rootMSE[["PASS_YARDS"]])
        pass_yards[pass_yards<0] = 0
        
        # TDs
        if(x$PASS_TD==0) {
          pass_tds = rep(0,1000)
        } else if(x$PASS_TD==1) {
          pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(.05,.39,.5,.05,.01))
        } else if(x$PASS_TD==2) {
          pass_tds = sample(c(0,1,2,3,4),1000,replace=T,prob=c(0.01,.35,.6,.25,.09))
        } else if(x$PASS_TD>2) {
          pass_tds = sample(c(0,1,2,3,4,5),1000,replace=T,prob=c(0.0,.18,.35,.4,.05,.02))
        }  
        pass_pts = floor(pass_yards/25) + pass_tds*4
      } else {
        pass_pts = 0
      }
      
      ### Rush
      if(x$RUSH_YARD>10) {
        rush_yards = rnorm(1000,x$RUSH_YARDS,rootMSE[["RUSH_YARDS"]])
        rush_yards[rush_yards<0] = 0
        if(x$RUSH_TD==0) {
          rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.75,.24,.01))
        } else if(x$RUSH_TD==1) {
          rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.65,.33,.02))
        } else if (x$RUSH_TD>1) {
          rush_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
        }
        rush_pts = floor(rush_yards/10) + rush_tds*6
        
      } else {
        rush_pts = 0
      }
      
      ### REC
      if(x$REC_YARD>10) {
        rec_yards = rnorm(1000,x$REC_YARDS,rootMSE[["REC_YARDS"]])
        rec_yards[rec_yards<0] = 0
        if(x$REC_TD==0) {
          rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.79,.20,.01))
        } else if(x$REC_TD==1) {
          rec_tds = sample(c(0,1,2,3),1000,replace=T,prob=c(.7,.23,.05,.02))
        } else if (x$REC_TD>1) {
          rec_tds = sample(c(0,1,2),1000,replace=T,prob=c(.3,.5,.2))
        }
        rec_pts = floor(rec_yards/10) + rec_tds*6
        
      } else {
        rec_pts = 0
      }
      
      total_pts = pass_pts + rush_pts + rec_pts    
    } else if(x$POS=="D/ST") {
      total_pts = floor(rnorm(1000,x$PTS,dst_stdev))
    } else if(x$POS=="K") {
      total_pts = floor(rnorm(1000,x$PTS,k_stdev))
      total_pts[total_pts<1] = 1
    }
    proj_pts_all[[x$PLAYER]] = total_pts
  }  
  return(proj_pts_all)
}

proj_pts_allA = projections_fun(projections_A,teamA)
proj_pts_allB = projections_fun(projections_B,teamB)

all_projections_fun = function(proj_pts_all = proj_pts_allA, w_actual = w_actualA,
                               team = teamA, projections = projections_A) {
  
  if(length(w_actual)>0) {
    for(wa in 1:length(w_actual)) {
      proj_pts_all[[w_actual[wa]]] = rep(team[w_actual[wa],"PTS"],length(proj_pts_all[[w_actual[wa]]]))      
    }
    proj_pts = sum(projections$PTS[-w_actual])
    act_pts = sum(team$PTS[w_actual])
  } else {
    proj_pts = sum(projections$PTS)
    act_pts = 0
  }
  all_projections = do.call(rbind,proj_pts_all)
  projections_tots = apply(all_projections,2,sum)
  return(list(proj_pts=proj_pts, act_pts=act_pts,
              all_projections=all_projections,
              projections_tots=projections_tots))
}

AA = all_projections_fun(proj_pts_allA,w_actualA,teamA,projections_A)
BB = all_projections_fun(proj_pts_allB,w_actualB,teamB,projections_B)

hA = hist(projections_A_tots,plot=F,breaks=seq(min(projections_A_tots)-.5,max(projections_A_tots)+.5,1))
hB = hist(BB$projections_tots,plot=F,breaks=seq(min(BB$projections_tots)-.5,max(BB$projections_tots)+.5,1))
heightA = 10*hA$counts/1000
heightB = -10*hB$counts/1000

##
par(mar=c(4,4,0,4))
plot(0,type="n",xlim=c(-1.2,1.2),ylim=c(0,max(AA$projections_tots,BB$projections_tots)),
     xlab="",ylab="",axes=F)
axis(2,las=2)
axis(4,las=2)
# abline(h=axTicks(2),lty=2,col="gray80")
abline(h=0)
mtext(paste0(teams_table[teams_table$team_no==team1,"nicknames"],
             "\nP Win: ",round(100*mean(AA$projections_tots > BB$projections_tots),1),"%"),
      side=1,line=1,at=0.5)
mtext(paste0(teams_table[teams_table$team_no==team2,"nicknames"],
             "\nP Win: ",round(100*mean(AA$projections_tots < BB$projections_tots),1),"%"),
      side=1,line=1,at=-0.5)

heatA = array(1,dim=c(max(AA$projections_tots),1,3))
heatA[hA$mids,1,] = 1-hA$count/max(hA$count)
# rasterImage(heatA,0,max(AA$projections_tots),1,0,interpolate=T)

heatB = array(1,dim=c(max(BB$projections_tots),1,3))
heatB[hB$mids,1,] = 1-hB$count/max(hB$count)
# rasterImage(heatB,1.8,max(BB$projections_tots),2.2,0,interpolate=T)

polygon(c(1,par()$usr[2],par()$usr[2],1),
        c(0,0,AA$act_pts,AA$act_pts),
        col=rgb(1,0,0),border=F)
polygon(c(1,par()$usr[2],par()$usr[2],1),
        c(AA$act_pts,AA$act_pts,AA$proj_pts,AA$proj_pts),
        border=rgb(.8,.8,.8),lty=2)

polygon(-c(1,par()$usr[2],par()$usr[2],1),
        c(0,0,BB$act_pts,BB$act_pts),
        col=rgb(0,0,1),border=F)
polygon(-c(1,par()$usr[2],par()$usr[2],1),
        c(BB$act_pts,BB$act_pts,BB$proj_pts,BB$proj_pts),
        border=rgb(.8,.8,.8),lty=2)

for(aa in 2:length(hA$breaks)-1) {
  polygon(c(0,0,heightA[aa],heightA[aa]),
          c(hA$breaks[aa],hA$breaks[aa+1],hA$breaks[aa+1],hA$breaks[aa]),
          border=F,col=rgb(1,1-hA$count[aa]/max(hA$count),1-hA$count[aa]/max(hA$count)))
}
for(bb in 2:length(hB$breaks)-1) {
  polygon(c(0,0,heightB[bb],heightB[bb]),
          c(hB$breaks[bb],hB$breaks[bb+1],hB$breaks[bb+1],hB$breaks[bb]),
          border=F,col=rgb(1-hA$count[bb]/max(hA$count),1-hA$count[bb]/max(hA$count),1))
}

a















### Player Plots
histi = hist(proj_pts_all[[i]],breaks=seq(-10.5,50.5,1),plot=F)
new_y = seq(0,1,length.out=length(histi$breaks))

png("test.png",width=200,height=200)
plot(0,type="n",ylab="",xlab="",axes=F,xlim=c(0,1),ylim=c(0,1))
a=readPNG(paste0("~/ShinyApps/fantasy_football_14/www/player_images/",projections_A$player_id[i],".png"))
rasterImage(a,0,0,.73,1)
for(yy in 2:length(new_y)-1) {
  polygon(c(.73,.73,.73+histi$density[yy],.73+histi$density[yy]),
          c(new_y[yy],new_y[yy+1],new_y[yy+1],new_y[yy]))
}
dev.off()

### Coverage ###########

lapply(c("PASS_YARDS","INT","PASS_TD",
         "RUSH","RUSH_YARDS","RUSH_TD",
         "REC","REC_YARDS","REC_TD",
         "PTS"), function(x) {
           projx = proj[,x]
           scrlx = scrl[,x]
           
           #pct
           mape = mean(apes[[x]],na.rm=T)
           upper = proj[,x]*(1+mape)
           lower = proj[,x]*(1-mape)
           coverage = scrl[,x] >= lower & scrl[,x] <= upper
           
           plot(1:length(scrl[!is.na(apes[[x]]),x]),scrl[!is.na(apes[[x]]),x],col="blue",pch=16)
           points(1:length(scrl[!is.na(apes[[x]]),x]),proj[!is.na(apes[[x]]),x],col="forestgreen")
           lapply(1:length(scrl[!is.na(apes[[x]]),x]),function(y) {
             lines(rep(y,2),c(lower[!is.na(apes[[x]])][y],upper[!is.na(apes[[x]])][y]),col="forestgreen")
           })
           
           
           #CI
           stdev = mean(sqrt(sqerrs[[x]]),na.rm=T)
           pm = stdev*qnorm(.975)
           upper = proj[,x] + pm 
           lower = proj[,x] - pm 
           coverage = scrl[,x] >= lower & scrl[,x] <= upper  
           
           plot(1:length(scrl[!is.na(sqerrs[[x]]),x]),scrl[!is.na(sqerrs[[x]]),x],col="blue",pch=16)
           points(1:length(scrl[!is.na(sqerrs[[x]]),x]),proj[!is.na(sqerrs[[x]]),x],col="forestgreen")
           lapply(1:length(scrl[!is.na(sqerrs[[x]]),x]),function(y) {
             lines(rep(y,2),c(lower[!is.na(sqerrs[[x]])][y],upper[!is.na(sqerrs[[x]])[y]),col="forestgreen")
           })
           
         })

### RUSH YARDS ##########

x="RUSH_YARDS"
projx = proj[,x]
scrlx = scrl[,x]
lprojx = log(proj[,x])
lscrlx = log(scrl[,x])
plot(lprojx,lscrlx)
abline(lm(lscrlx~lprojx))

ldelta = lprojx-lscrlx
ldelta[ldelta==-Inf] = NA
ldelta[ldelta==Inf] = NA

lprojx = lprojx[!is.na(ldelta)]
lscrlx = lscrlx[!is.na(ldelta)]

new_order = order(lprojx)
lprojx = lprojx[new_order]
lscrlx = lscrlx[new_order]

stdev = sqrt(mean(ldelta^2,na.rm=T))
lupper = lprojx+stdev*qnorm(0.975)
llower = lprojx-stdev*qnorm(0.975)

# coverage
mean(llower < lscrlx & lupper > lscrlx)

plot(1:length(lscrlx),lscrlx,col="blue",pch=16,ylim=c(min(llower),max(lupper)))
points(1:length(lprojx),lprojx,col="forestgreen",pch=15)
lapply(1:length(llower),function(x) {
  lines(rep(x,2),c(llower[x],lupper[x]),col="forestgreen",lwd=2)
})

plot(1:length(lscrlx),exp(lscrlx),col="blue",pch=16,ylim=c(exp(min(llower)),exp(max(lupper))))
points(1:length(lprojx),exp(lprojx),col="forestgreen",pch=15)
lapply(1:length(llower),function(x) {
  lines(rep(x,2),c(exp(llower[x]),exp(lupper[x])),col="forestgreen",lwd=2)
})

## back to original
projx = proj[,x]
scrlx = scrl[,x]
plot(projx,scrlx)

delta = projx-scrlx
delta[delta==0 & projx==0 & scrlx==0] = NA
projx = projx[!is.na(delta)]
scrlx = scrlx[!is.na(delta)]
delta = delta[!is.na(delta)]

new_order = order(projx)
projx = projx[new_order]
scrlx = scrlx[new_order]

stdev = sqrt(mean(delta^2,na.rm=T))
upper = projx+stdev*qnorm(0.975)
lower = projx-stdev*qnorm(0.975)
lower[lower<0] = 0
upper[projx<=10] = 20

# coverage
mean(lower < scrlx & upper > scrlx)

plot(1:length(scrlx),scrlx,col="blue",pch=16,ylim=c(min(lower),max(upper)))
points(1:length(projx),projx,col="forestgreen",pch=15)
lapply(1:length(lower),function(x) {
  lines(rep(x,2),c(lower[x],upper[x]),col="forestgreen",lwd=2)
})
abline(lm(scrlx~projx))

plot(1:length(lscrlx),exp(lscrlx),col="blue",pch=16,ylim=c(exp(min(llower)),exp(max(lupper))))
points(1:length(lprojx),exp(lprojx),col="forestgreen",pch=15)
lapply(1:length(llower),function(x) {
  lines(rep(x,2),c(exp(llower[x]),exp(lupper[x])),col="forestgreen",lwd=2)
})
abline(lm(scrlx~projx))

### Rushing TDs ##########
x="RUSH_TD"
tabx = table(proj[proj$RUSH_YARDS>10,x],scrl[proj$RUSH_YARDS>10,x])
names(tabx) = c("proj", "scrl")

# P(1 TD | proj=0) = 0.25
# P(1 TD | proj=1) = 0.33

### RUSH ##########
x = "RUSH"

table(proj[,x],scrl[,x])
plot(proj[,x],scrl[,x])

summary(lm(scrl[,x]~proj[,x]))
