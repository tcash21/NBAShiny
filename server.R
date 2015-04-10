	
library(randomForest)
library(dplyr)
library(plyr)
library(RSQLite)
library(shiny)
library(rCharts)

options(shiny.trace=TRUE)

shinyServer(function(input, output, session){

newData <- reactive({

invalidateLater(30000, session)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "/home/ec2-user/sports/sports.db")

tables <- dbListTables(con)

lDataFrames <- vector("list", length=length(tables))


 ## create a data.frame for each table
for (i in seq(along=tables)) {
  if(tables[[i]] == 'NBASBHalfLines' | tables[[i]] == 'NBASBLines'){
   lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste0("SELECT n.away_team, n.home_team, n.game_date, n.line, n.spread, n.game_time from '", tables[[i]], "' n inner join
  (select game_date, away_team,home_team, max(game_time) as mgt from '", tables[[i]], "' group by game_date, away_team, home_team) s2 on s2.game_date = n.game_date and
  s2.away_team = n.away_team and s2.home_team = n.home_team and n.game_time = s2.mgt and n.game_date = '", format(as.Date(input$date),"%m/%d/%Y"),  "';"))
 # lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste0("SELECT * FROM ", tables[[i]]))

  } else if (tables[[i]] == 'NBAseasontotals' | tables[[i]] == 'NBAseasonstats') {
        lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "' where the_date = '", format(as.Date(input$date), "%m/%d/%Y"), "'", sep=""))
  } else if (tables[[i]] %in% c('NBAgames')) {
        lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "' where game_date = '", format(as.Date(input$date), "%m/%d/%Y"), "'", sep=""))
  } else {
        lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
  }
  cat(tables[[i]], ":", i, "\n")
}

halflines <- lDataFrames[[2]]
games <- lDataFrames[[6]]
lines <- lDataFrames[[3]]
teamstats <- lDataFrames[[8]]
boxscores <- lDataFrames[[10]]
lookup <- lDataFrames[[4]]
nbafinal <- lDataFrames[[5]]
seasontotals <- lDataFrames[[9]]


if(dim(halflines)[1] > 0 ){

b<-apply(boxscores[,3:5], 2, function(x) strsplit(x, "-"))
boxscores$fgm <- do.call("rbind",b$fgma)[,1]
boxscores$fga <- do.call("rbind",b$fgma)[,2]
boxscores$tpm <- do.call("rbind",b$tpma)[,1]
boxscores$tpa <- do.call("rbind",b$tpma)[,2]
boxscores$ftm <- do.call("rbind",b$ftma)[,1]
boxscores$fta <- do.call("rbind",b$ftma)[,2]
boxscores <- boxscores[,c(1,2,16:21,6:15)]

m1<-merge(boxscores, games, by="game_id")
m1$key <- paste(m1$team, m1$game_date)
teamstats$key <- paste(teamstats$team, teamstats$the_date)
m2<-merge(m1, teamstats, by="key")
lookup$away_team <- lookup$sb_team
lookup$home_team <- lookup$sb_team

## Total Lines
lines$game_time<-as.POSIXlt(lines$game_time)
lines<-lines[order(lines$home_team, lines$game_time),]
lines$key <- paste(lines$away_team, lines$home_team, lines$game_date)

# grabs the first line value after 'OFF'
#res2 <- tapply(1:nrow(lines), INDEX=lines$key, FUN=function(idxs) idxs[lines[idxs,'line'] != 'OFF'][1])
#first<-lines[res2[which(!is.na(res2))],]
#lines <- first[,1:6]

## Merge line data with lookup table
la<-merge(lookup, lines, by="away_team")
lh<-merge(lookup, lines, by="home_team")
la$key <- paste(la$espn_abbr, la$game_date)
lh$key <- paste(lh$espn_abbr, lh$game_date)
m3a<-merge(m2, la, by="key")
m3h<-merge(m2, lh, by="key")
colnames(m3a)[49] <- "CoversTotalLineUpdateTime"
colnames(m3h)[49] <- "CoversTotalLineUpdateTime"

## Halftime Lines
halflines$game_time<-as.POSIXlt(halflines$game_time)
halflines<-halflines[order(halflines$home_team, halflines$game_time),]
halflines$key <- paste(halflines$away_team, halflines$home_team, halflines$game_date)

# grabs first line value after 'OFF'
#res2 <- tapply(1:nrow(halflines), INDEX=halflines$key, FUN=function(idxs) idxs[halflines[idxs,'line'] != 'OFF'][1])
#first<-halflines[res2[which(!is.na(res2))],]
#halflines <- first[,1:6]

la2<-merge(lookup, halflines, by="away_team")
lh2<-merge(lookup, halflines, by="home_team")
la2$key <- paste(la2$espn_abbr, la2$game_date)
lh2$key <- paste(lh2$espn_abbr, lh2$game_date)
m3a2<-merge(m2, la2, by="key")
m3h2<-merge(m2, lh2, by="key")
colnames(m3a2)[49] <- "CoversHalfLineUpdateTime"
colnames(m3h2)[49] <- "CoversHalfLineUpdateTime"
l<-merge(m3a, m3a2, by=c("game_date.y", "away_team"))
#l<-l[match(m3a$key, l$key.y),]
m3a<-m3a[match(l$key.y, m3a$key),]
m3a<-cbind(m3a, l[,94:96])
l2<-merge(m3h, m3h2, by=c("game_date.y", "home_team"))
#l2<-l2[match(m3h$key, l2$key.y),]
m3h<-m3h[match(l2$key.y, m3h$key),]
m3h<-cbind(m3h, l2[,94:96])
colnames(m3h)[44:45] <- c("home_team.x", "home_team.y")
colnames(m3a)[40] <- "home_team"
if(dim(m3a)[1] > 0){
 m3a$hometeam <- FALSE
 m3h$hometeam <- TRUE
 m3h <- m3h[,1:53]
}

halftime_stats<-rbind(m3a,m3h)
if(length(which(halftime_stats$game_id %in% names(which(table(halftime_stats$game_id) != 2))) > 0)){
halftime_stats<-halftime_stats[-which(halftime_stats$game_id %in% names(which(table(halftime_stats$game_id) != 2)) ),]
}
#halftime_stats <- subset(halftime_stats, line.y != 'OFF')
halftime_stats<-halftime_stats[which(!is.na(halftime_stats$line.y)),]
halftime_stats<-halftime_stats[order(halftime_stats$game_id),]
halftime_stats$CoversTotalLineUpdateTime <- as.character(halftime_stats$CoversTotalLineUpdateTime)
halftime_stats$CoversHalfLineUpdateTime<-as.character(halftime_stats$CoversHalfLineUpdateTime)

#diffs<-ddply(halftime_stats, .(game_id), transform, diff=pts.x[1] - pts.x[2])
if(dim(halftime_stats)[1] > 0 ){
halftime_stats$half_diff <-  rep(aggregate(pts ~ game_id, data=halftime_stats, FUN=diff)[,2] * -1, each=2)
halftime_stats$line.y<-as.numeric(halftime_stats$line.y)
halftime_stats$line <- as.numeric(halftime_stats$line)
halftime_stats$mwt<-rep(aggregate(pts ~ game_id, data=halftime_stats, sum)[,2], each=2) + halftime_stats$line.y - halftime_stats$line
half_stats <- halftime_stats[seq(from=2, to=dim(halftime_stats)[1], by=2),]
} else {
  return(data.frame(results="No Results"))
}

all <- rbind(m3a, m3h)
all <- all[,-1]
all$key <- paste(all$game_id, all$team.y)
all<-all[match(unique(all$key), all$key),]

colnames(all) <- c("GAME_ID","TEAM","HALF_FGM", "HALF_FGA", "HALF_3PM","HALF_3PA", "HALF_FTM","HALF_FTA","HALF_OREB", "HALF_DREB", "HALF_REB",
"HALF_AST", "HALF_STL", "HALF_BLK", "HALF_TO", "HALF_PF", "HALF_PTS", "HALF_TIMESTAMP", "TEAM1", "TEAM2", "GAME_DATE","GAME_TIME",
"REMOVE2","REMOVE3","SEASON_FGM","SEASON_FGA", "SEASON_FGP","SEASON_3PM", "SEASON_3PA", "SEASON_3PP", "SEASON_FTM","SEASON_FTA","SEASON_FTP",
"SEASON_2PM", "SEASON_2PA", "SEASON_2PP","SEASON_PPS", "SEASON_AFG","REMOVE4", "REMOVE5", "REMOVE6", "REMOVE7","REMOVE8", "REMOVE9", "REMOVE10",
"LINE", "SPREAD", "COVERS_UPDATE","LINE_HALF", "SPREAD_HALF", "COVERS_HALF_UPDATE", "HOME_TEAM", "REMOVE11")
all <- all[,-grep("REMOVE", colnames(all))]

## Add the season total stats
colnames(seasontotals)[1] <- "TEAM"
colnames(seasontotals)[2] <- "GAME_DATE"
#today <- format(Sys.Date(), "%m/%d/%Y")
#seasontotals <- subset(seasontotals, GAME_DATE == today)
all$key <- paste(all$GAME_DATE, all$TEAM)
seasontotals$key <- paste(seasontotals$GAME_DATE, seasontotals$TEAM)

x<-merge(seasontotals, all, by=c("key"))
x<- x[,c(-1,-2, -16, -35)]
final<-x[,c(1:53)]
colnames(final)[2:12] <- c("SEASON_GP", "SEASON_PPG", "SEASON_ORPG", "SEASON_DEFRPG", "SEASON_RPG", "SEASON_APG", "SEASON_SPG", "SEASON_BGP",
"SEASON_TPG", "SEASON_FPG", "SEASON_ATO")
#final$GAME_DATE <- seasontotals$GAME_DATE[1]
#final$GAME_DATE<-games[match(final$GAME_ID, games$game_id),]$game_date
final<-final[order(final$GAME_DATE.x, decreasing=TRUE),]

## match half stats that have 2nd half lines with final set
f<-final[which(final$GAME_ID %in% half_stats$game_id),]
f$mwt <- half_stats[match(f$GAME_ID, half_stats$game_id),]$mwt
f$half_diff <- half_stats[match(f$GAME_ID, half_stats$game_id),]$half_diff
f[,2:12] <- apply(f[,2:12], 2, function(x) as.numeric(as.character(x)))
f[,14:28] <- apply(f[,14:28], 2, function(x) as.numeric(as.character(x)))
f[,33:48] <- apply(f[,33:48], 2, function(x) as.numeric(as.character(x)))
f[,50:51] <- apply(f[,50:51], 2, function(x) as.numeric(as.character(x)))

## Team1 and Team2 Halftime Differentials
f <- f[order(f$GAME_ID),]
f$fg_percent <- ((f$HALF_FGM / f$HALF_FGA) - (f$SEASON_FGM / f$SEASON_FGA))
f$FGM <- (f$HALF_FGM - (f$SEASON_FGM / f$SEASON_GP / 2))
f$TPM <- (f$HALF_3PM - (f$SEASON_3PM / f$SEASON_GP / 2))
f$FTM <- (f$HALF_FTM - (f$SEASON_FTM / f$SEASON_GP / 2 - 1))
f$TO <- (f$HALF_TO - (f$SEASON_ATO / 2))
f$OREB <- (f$HALF_OREB - (f$SEASON_ORPG / 2))

## Cumulative Halftime Differentials
f$COVERS_UPDATE<-as.character(f$COVERS_UPDATE)
f$COVERS_HALF_UPDATE <- as.character(f$COVERS_HALF_UPDATE)

f$chd_fg<-rep(aggregate(fg_percent ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)
f$chd_fgm <- rep(aggregate(FGM ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)
f$chd_tpm <- rep(aggregate(TPM ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)
f$chd_ftm <- rep(aggregate(FTM ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)
f$chd_to <- rep(aggregate(TO ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)
f$chd_oreb <- rep(aggregate(OREB ~ GAME_ID, data=f, function(x) sum(x) / 2)[,2], each=2)

## load nightly model trained on all previous data
load("~/sports/models/NBAhalftimeOversModel.Rdat")

f<-f[order(f$GAME_ID),]
#f<-ddply(f, .(GAME_ID), transform, half_diff=HALF_PTS[1] - HALF_PTS[2])
f$team <- ""
f[seq(from=1, to=dim(f)[1], by=2),]$team <- "TEAM1"
f[seq(from=2, to=dim(f)[1], by=2),]$team <- "TEAM2"
#f <- f[,c(13,1,30:32,47,48,50,51,53:68)]
#f<-f[order(f$GAME_ID),]
wide <- reshape(f, direction = "wide", idvar="GAME_ID", timevar="team")
#train <- wide[,c(4,6:21,24,29:35)]
#train<-as.matrix(train)
#train[which(is.na(train))] <- 0

#set.seed(21)
#p <- predict(m, newdata=data.frame(train), interval="predict", level=.75)
#preds <- p > .5
#result <- wide[,c(1:10,11,19:24)]
result <- wide
result$GAME_DATE<- strptime(paste(result$GAME_DATE.x.TEAM1, result$GAME_TIME.TEAM1), format="%m/%d/%Y %I:%M %p")
#result <- result[,c(-2,-5)]
#result <- result[,c(1,16,2,3,4:15)]


#if(Sys.Date() == input$date){
#result$projectedWinner <- "TEAM1"
#result$projectedWinner[which(as.character(preds) == TRUE)] <- "TEAM2"
#result$projectedWinner[which(result$projectedWinner == "TEAM1")] <- result$TEAM.x.TEAM1[which(result$projectedWinner == "TEAM1")]
#result$projectedWinner[which(result$projectedWinner == "TEAM2")] <- result$TEAM.x.TEAM2[which(result$projectedWinner == "TEAM2")]
#}

colnames(result)[54] <- "MWT"
colnames(result)[48] <- "SPREAD"
colnames(result)[62:67] <- c("chd_fg", "chd_fgm", "chd_tpm", "chd_ftm", "chd_to", "chd_oreb")

#colnames(result)[3:16] <- c("TEAM1", "TEAM2","LINE","SPREAD", "HALF_LINE", "HALF_SPREAD", "TEAM1_HOME", "MWT", "chd_fg","chd_fgm", "chd_tpm", "chd_ftm", "chd_to", "chd_oreb")
#result$SUM_FGP = result$FGP_T1 + result$FGP_T2
#result$SUM_FTM = result$FTM_T1 + result$FTM_T2

#result$prediction <- p[,1]
#result$lower <- p[,2]
#result$upper <- p[,3]
#result$pred2 <- result$prediction - (result$HALF_PTS.T1 - result$HALF_PTS.T2)

result$mwtO <- as.numeric(result$MWT < 7.1 & result$MWT > -3.9)
result$chd_fgO <- as.numeric(result$chd_fg < .15 & result$chd_fg > -.07)
result$chd_fgmO <- as.numeric(result$chd_fgm < -3.9)
result$chd_tpmO <- as.numeric(result$chd_tpm < -1.9)
result$chd_ftmO <- as.numeric(result$chd_ftm < -.9)
result$chd_toO <- as.numeric(result$chd_to < -1.9)

result$mwtO[is.na(result$mwtO)] <- 0
result$chd_fgO[is.na(result$chd_fgO)] <- 0
result$chd_fgmO[is.na(result$chd_fgmO)] <- 0
result$chd_tpmO[is.na(result$chd_tpmO)] <- 0
result$chd_ftmO[is.na(result$chd_ftmO)] <- 0
result$chd_toO[is.na(result$chd_toO)] <- 0
result$overSum <- result$mwtO + result$chd_fgO + result$chd_fgmO + result$chd_tpmO + result$chd_ftmO + result$chd_toO

result$fullSpreadU <- as.numeric(abs(result$SPREAD) > 10.9)
result$mwtU <- as.numeric(result$MWT > 7.1)
result$chd_fgU <- as.numeric(result$chd_fg > .15 | result$chd_fg < -.07)
result$chd_fgmU <- 0
result$chd_tpmU <- 0
result$chd_ftmU <- as.numeric(result$chd_ftm > -0.9)
result$chd_toU <- as.numeric(result$chd_to > -1.9)

result$mwtU[is.na(result$mwtU)] <- 0
result$chd_fgO[is.na(result$chd_fgU)] <- 0
result$chd_fgmU[is.na(result$chd_fgmU)] <- 0
result$chd_tpmU[is.na(result$chd_tpmU)] <- 0
result$chd_ftmU[is.na(result$chd_ftmU)] <- 0
result$chd_toU[is.na(result$chd_toU)] <- 0
result$underSum <- result$fullSpreadU + result$mwtU + result$chd_fgU + result$chd_fgmU + result$chd_tpmU + result$chd_ftmU + result$chd_toU

#result <- result[,c(1:10,23,31,17:22,24:30,11:16)]
result <- result[order(result$GAME_DATE),]
result$GAME_DATE <- as.character(result$GAME_DATE)
colnames(result)[62] <- 'chd_fg.TEAM1'
load("~/sports/models/NBAhalftimeOversModel.Rdat")
result$probOver<-predict(r, newdata=result, type="prob")[,2]
result <- result[,c("GAME_ID",  "GAME_DATE.x.TEAM1", "TEAM1.TEAM1", "TEAM2.TEAM1", "underSum", "overSum", "LINE_HALF.TEAM1", "HALF_PTS.TEAM1", "HALF_PTS.TEAM2", "probOver")]

}else{

return(data.frame(results="No Results"))

}

return(result)
#return(f[,c(1,2,30,31,54:67)])
dbDisconnect(con)

})



output$results <- renderChart2({
#  invalidateLater(5000, session)
#  dTable(newData(), bPaginate=F, aaSorting=list(c(1,"asc")))
  dTable(newData(), bPaginate=F, aaSorting=list(c(1,"asc")))


})

})


