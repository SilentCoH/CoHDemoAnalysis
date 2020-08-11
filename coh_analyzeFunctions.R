
## A set of summary functions to be used on the output of parseDemo

analyzeGroupDefense <- function(parseOut, teamNum,
                                defenseset = c("Phase Shift","Hibernate","Jaunt","Raptor or Fly","Burst of Speed","Green Insp")){
  if (teamNum==0){
    ent<-parseOut$entities0
    enemySpike<-parseOut$spikeSummary1
    thisSumm<-parseOut$powerSummary0
  }else{
    ent<-parseOut$entities1
    enemySpike<-parseOut$spikeSummary0
    thisSumm<-parseOut$powerSummary1
  }
  datOut <- thisSumm[,c("Name","ApproxTargeted","ApproxDeaths")]
  names(datOut)<-c("name","targeted","deaths")
  datOut$survival_rate <- (datOut$targeted - datOut$deaths)/datOut$targeted
  datOut[which(is.nan(datOut$survival_rate)),"survival_rate"] <- NA
  datOut2 <- datOut
  for (i in defenseset){
    if (i %in% names(thisSumm)){
      datOut2 <- cbind(datOut2,thisSumm[,i])
      names(datOut2)[ncol(datOut2)]<-i
    }
  }
  datOut$avg_players_on_spike<-NA
  datOut$avg_attack_count<-NA
  datOut$avg_spike_duration<-NA
  datOut$evaded<-NA
  datOut$healed<-NA
  datOut$used_greens<-NA
  totalEvaded<-0
  totalHealed<-0
  totalGreens<-0
  for (i in 1:nrow(datOut)){
    thisName<-datOut[i,"name"]
    thisSpike <- enemySpike[which(enemySpike$target==thisName),]
    if (nrow(thisSpike)>0){
      datOut[i,"avg_players_on_spike"]<-mean(thisSpike$players_on_spike)
      datOut[i,"avg_attack_count"]<-mean(thisSpike$attack_count)
      datOut[i,"avg_spike_duration"]<-mean(thisSpike$spike_duration)
      datOut[i,"evaded"]<-mean(thisSpike$evaded)
      datOut[i,"healed"]<-mean(thisSpike$healed)
      datOut[i,"used_greens"]<-mean(thisSpike$greens)
      totalEvaded <- totalEvaded + sum(thisSpike$evaded)
      totalHealed <- totalHealed + sum(thisSpike$healed)
      totalGreens <- totalGreens + sum(thisSpike$greens)
    }
  }
  return(list(powerUsage = datOut2,
              defenseData = datOut,
              team_survival_rate = (sum(datOut$targeted) - sum(datOut$deaths))/sum(datOut$targeted),
              team_evade_rate = totalEvaded/sum(datOut$targeted),
              team_heal_rate = totalHealed/sum(datOut$targeted),
              team_green_rate = totalGreens/sum(datOut$targeted)))
}

analyzePlayerHealing <- function(parseOut,name){
  ent<-rbind(parseOut$entities0,parseOut$entities1)
  if (!name %in% ent$name){
    stop("That player is not in this match's list of entities")
  }else{
    teamtemp<-ent[which(ent$name==name),"team"]
  }
  if (teamtemp==0){
    healDetails <- parseOut$healDetails0
    targset <- parseOut$powerSummary0[,c("Name","ApproxTargeted")]
  }else{
    healDetails <- parseOut$healDetails1
    targset <- parseOut$powerSummary1[,c("Name","ApproxTargeted")]
  }
  thisHeal<-healDetails[[1]]
  playerOut<-thisHeal[which(thisHeal$name==name),]
  if (length(healDetails)>1){
    for (i in 2:length(healDetails)){
      thisHeal<-healDetails[[i]]
      playerOut<-rbind(playerOut,thisHeal[which(thisHeal$name==name),])
    }
  }
  return(list(rawData = playerOut,
              targeted = targset[which(targset$Name==name),"ApproxTargeted"],
              on_heal_pct = mean(playerOut$on_heal),
              heals_per_spike = mean(playerOut$heal_count,na.rm=TRUE),
              first_heal_timing = mean(playerOut$first_heal_timing,na.rm=TRUE),
              chain_table = sort(table(playerOut$heal_combo,exclude=NULL), decreasing = TRUE)))
}


analyzePlayerOffense <- function(parseOut,name){
  ent<-rbind(parseOut$entities0,parseOut$entities1)
  if (!name %in% ent$name){
    stop("That player is not in this match's list of entities")
  }else{
    teamtemp<-ent[which(ent$name==name),"team"]
  }
  if (teamtemp==0){
    spikeDetails <- parseOut$spikeDetails0
    targset <- parseOut$powerSummary0[,c("Name","ApproxTargeted")]
  }else{
    spikeDetails <- parseOut$spikeDetails1
    targset <- parseOut$powerSummary1[,c("Name","ApproxTargeted")]
  }
  thisSpike<-spikeDetails[[1]]
  playerOut<-thisSpike[which(thisSpike$name==name),]
  if (length(spikeDetails)>1){
    for (i in 2:length(spikeDetails)){
      thisSpike<-spikeDetails[[i]]
      playerOut<-rbind(playerOut,thisSpike[which(thisSpike$name==name),])
    }
  }
  return(list(rawData = playerOut,
              targeted = targset[which(targset$Name==name),"ApproxTargeted"],
              on_spike_pct = mean(playerOut$on_spike),
              attacks_per_spike = mean(playerOut$attack_count,na.rm=TRUE),
              first_attack_timing = mean(playerOut$first_attack_timing,na.rm=TRUE),
              first_attack_rank = mean(playerOut$first_attack_rank,na.rm=TRUE),
              first_timing_distance = mean(playerOut$first_timing_distance,na.rm=TRUE),
              abs_timing_distance = mean(playerOut$abs_timing_distance,na.rm=TRUE),
              chain_duration = mean(playerOut$chain_duration,na.rm=TRUE),
              chain_table = sort(table(playerOut$attack_combo,exclude=NULL), decreasing = TRUE)))
}


analyzeCustomGroupOffense <- function(parseOut, name1, name2, name3=NULL, name4=NULL, name5=NULL, name6=NULL, name7=NULL, name8=NULL){
  validPlayers <- 2 + !is.null(name3) + !is.null(name4) + !is.null(name5) + !is.null(name6) + !is.null(name7) + !is.null(name8)
  datOut<-as.data.frame(matrix(NA,nrow=validPlayers,ncol=9))
  p1 <- analyzePlayerOffense(parseOut, name1)
  p2 <- analyzePlayerOffense(parseOut, name2)
  if (!is.null(name3)) { p3 <- analyzePlayerOffense(parseOut, name3) }
  if (!is.null(name4)) { p4 <- analyzePlayerOffense(parseOut, name4) }
  if (!is.null(name5)) { p5 <- analyzePlayerOffense(parseOut, name5) }
  if (!is.null(name6)) { p6 <- analyzePlayerOffense(parseOut, name6) }
  if (!is.null(name7)) { p7 <- analyzePlayerOffense(parseOut, name7) }
  if (!is.null(name8)) { p8 <- analyzePlayerOffense(parseOut, name8) }
  names(datOut)<-c("name",names(p1)[2:9])
  datOut[1,"name"] <- p1$rawData$name[1]
  datOut[1,2:9] <- as.numeric(p1[2:9])
  datOut[2,"name"] <- p2$rawData$name[1]
  datOut[2,2:9] <- as.numeric(p2[2:9])
  if (!is.null(name3)){ datOut[3,"name"] <- p3$rawData$name[1]; datOut[3,2:9] <- as.numeric(p3[2:9]) }
  if (!is.null(name4)){ datOut[4,"name"] <- p4$rawData$name[1]; datOut[4,2:9] <- as.numeric(p4[2:9]) }
  if (!is.null(name5)){ datOut[5,"name"] <- p5$rawData$name[1]; datOut[5,2:9] <- as.numeric(p5[2:9]) }
  if (!is.null(name6)){ datOut[6,"name"] <- p6$rawData$name[1]; datOut[6,2:9] <- as.numeric(p6[2:9]) }
  if (!is.null(name7)){ datOut[7,"name"] <- p7$rawData$name[1]; datOut[7,2:9] <- as.numeric(p7[2:9]) }
  if (!is.null(name8)){ datOut[8,"name"] <- p8$rawData$name[1]; datOut[8,2:9] <- as.numeric(p8[2:9]) }
  datOut$compactness <- datOut$chain_duration / datOut$attacks_per_spike
  return(datOut)
}



analyzeGroupOffense <- function(parseOut, teamNum){
  if (teamNum==0){
    ent<-parseOut$entities0
  }else{
    ent<-parseOut$entities1
  }
  validPlayers <- nrow(ent)
  datOut<-as.data.frame(matrix(NA,nrow=validPlayers,ncol=9))
  p1<-analyzePlayerOffense(parseOut,ent$name[1])
  p2<-analyzePlayerOffense(parseOut,ent$name[2])
  if (validPlayers>=3){ p3<-analyzePlayerOffense(parseOut,ent$name[3]) } else { p3 <- NULL }
  if (validPlayers>=4){ p4<-analyzePlayerOffense(parseOut,ent$name[4]) } else { p4 <- NULL }
  if (validPlayers>=5){ p5<-analyzePlayerOffense(parseOut,ent$name[5]) } else { p5 <- NULL }
  if (validPlayers>=6){ p6<-analyzePlayerOffense(parseOut,ent$name[6]) } else { p6 <- NULL }
  if (validPlayers>=7){ p7<-analyzePlayerOffense(parseOut,ent$name[7]) } else { p7 <- NULL }
  if (validPlayers>=8){ p8<-analyzePlayerOffense(parseOut,ent$name[8]) } else { p8 <- NULL }
  
  names(datOut)<-c("name",names(p1)[2:9])
  datOut[1,"name"] <- p1$rawData$name[1]
  datOut[1,2:9] <- as.numeric(p1[2:9])
  datOut[2,"name"] <- p2$rawData$name[1]
  datOut[2,2:9] <- as.numeric(p2[2:9])
  if (!is.null(p3)){ datOut[3,"name"] <- p3$rawData$name[1]; datOut[3,2:9] <- as.numeric(p3[2:9]) }
  if (!is.null(p4)){ datOut[4,"name"] <- p4$rawData$name[1]; datOut[4,2:9] <- as.numeric(p4[2:9]) }
  if (!is.null(p5)){ datOut[5,"name"] <- p5$rawData$name[1]; datOut[5,2:9] <- as.numeric(p5[2:9]) }
  if (!is.null(p6)){ datOut[6,"name"] <- p6$rawData$name[1]; datOut[6,2:9] <- as.numeric(p6[2:9]) }
  if (!is.null(p7)){ datOut[7,"name"] <- p7$rawData$name[1]; datOut[7,2:9] <- as.numeric(p7[2:9]) }
  if (!is.null(p8)){ datOut[8,"name"] <- p8$rawData$name[1]; datOut[8,2:9] <- as.numeric(p8[2:9]) }
  datOut$compactness <- datOut$chain_duration / datOut$attacks_per_spike
  return(datOut)
}




analyzeCustomGroupHealing <- function(parseOut, name1, name2, name3=NULL, name4=NULL, name5=NULL, name6=NULL, name7=NULL, name8=NULL){
  validPlayers <- 2 + !is.null(name3) + !is.null(name4) + !is.null(name5) + !is.null(name6) + !is.null(name7) + !is.null(name8)
  datOut<-as.data.frame(matrix(NA,nrow=validPlayers,ncol=5))
  p1 <- analyzePlayerHealing(parseOut, name1)
  p2 <- analyzePlayerHealing(parseOut, name2)
  if (!is.null(name3)) { p3 <- analyzePlayerHealing(parseOut, name3) }
  if (!is.null(name4)) { p4 <- analyzePlayerHealing(parseOut, name4) }
  if (!is.null(name5)) { p5 <- analyzePlayerHealing(parseOut, name5) }
  if (!is.null(name6)) { p6 <- analyzePlayerHealing(parseOut, name6) }
  if (!is.null(name7)) { p7 <- analyzePlayerHealing(parseOut, name7) }
  if (!is.null(name8)) { p8 <- analyzePlayerHealing(parseOut, name8) }
  names(datOut)<-c("name",names(p1)[2:5])
  datOut[1,"name"] <- p1$rawData$name[1]
  datOut[1,2:5] <- as.numeric(p1[2:5])
  datOut[2,"name"] <- p2$rawData$name[1]
  datOut[2,2:5] <- as.numeric(p2[2:5])
  if (!is.null(name3)){ datOut[3,"name"] <- p3$rawData$name[1]; datOut[3,2:5] <- as.numeric(p3[2:5]) }
  if (!is.null(name4)){ datOut[4,"name"] <- p4$rawData$name[1]; datOut[4,2:5] <- as.numeric(p4[2:5]) }
  if (!is.null(name5)){ datOut[5,"name"] <- p5$rawData$name[1]; datOut[5,2:5] <- as.numeric(p5[2:5]) }
  if (!is.null(name6)){ datOut[6,"name"] <- p6$rawData$name[1]; datOut[6,2:5] <- as.numeric(p6[2:5]) }
  if (!is.null(name7)){ datOut[7,"name"] <- p7$rawData$name[1]; datOut[7,2:5] <- as.numeric(p7[2:5]) }
  if (!is.null(name8)){ datOut[8,"name"] <- p8$rawData$name[1]; datOut[8,2:5] <- as.numeric(p8[2:5]) }
  return(datOut)
}



analyzeGroupHealing <- function(parseOut, teamNum){
  if (teamNum==0){
    ent<-parseOut$entities0
  }else{
    ent<-parseOut$entities1
  }
  validPlayers <- nrow(ent)
  datOut<-as.data.frame(matrix(NA,nrow=validPlayers,ncol=5))
  p1<-analyzePlayerHealing(parseOut,ent$name[1])
  p2<-analyzePlayerHealing(parseOut,ent$name[2])
  if (validPlayers>=3){ p3<-analyzePlayerHealing(parseOut,ent$name[3]) } else { p3 <- NULL }
  if (validPlayers>=4){ p4<-analyzePlayerHealing(parseOut,ent$name[4]) } else { p4 <- NULL }
  if (validPlayers>=5){ p5<-analyzePlayerHealing(parseOut,ent$name[5]) } else { p5 <- NULL }
  if (validPlayers>=6){ p6<-analyzePlayerHealing(parseOut,ent$name[6]) } else { p6 <- NULL }
  if (validPlayers>=7){ p7<-analyzePlayerHealing(parseOut,ent$name[7]) } else { p7 <- NULL }
  if (validPlayers>=8){ p8<-analyzePlayerHealing(parseOut,ent$name[8]) } else { p8 <- NULL }
  
  names(datOut)<-c("name",names(p1)[2:5])
  datOut[1,"name"] <- p1$rawData$name[1]
  datOut[1,2:5] <- as.numeric(p1[2:5])
  datOut[2,"name"] <- p2$rawData$name[1]
  datOut[2,2:5] <- as.numeric(p2[2:5])
  if (!is.null(p3)){ datOut[3,"name"] <- p3$rawData$name[1]; datOut[3,2:5] <- as.numeric(p3[2:5]) }
  if (!is.null(p4)){ datOut[4,"name"] <- p4$rawData$name[1]; datOut[4,2:5] <- as.numeric(p4[2:5]) }
  if (!is.null(p5)){ datOut[5,"name"] <- p5$rawData$name[1]; datOut[5,2:5] <- as.numeric(p5[2:5]) }
  if (!is.null(p6)){ datOut[6,"name"] <- p6$rawData$name[1]; datOut[6,2:5] <- as.numeric(p6[2:5]) }
  if (!is.null(p7)){ datOut[7,"name"] <- p7$rawData$name[1]; datOut[7,2:5] <- as.numeric(p7[2:5]) }
  if (!is.null(p8)){ datOut[8,"name"] <- p8$rawData$name[1]; datOut[8,2:5] <- as.numeric(p8[2:5]) }
  return(datOut)
}


analyzePlayerGreens <- function(parseOut, name, greenMax=20){
  ent<-rbind(parseOut$entities0,parseOut$entities1)
  if (!name %in% ent$name){
    stop("That player is not in this match's list of entities")
  }else{
    teamtemp<-ent[which(ent$name==name),"team"]
  }
  
  if (teamtemp==0){
    enemySpike <- parseOut$spikeSummary1
  }else{
    enemySpike <- parseOut$spikeSummary0
  }
  
  greenDat <- parseOut$greenUsage[which(parseOut$greenUsage$name==name),]
  targDat <- enemySpike[which(enemySpike$target==name),]
  
  greensLeft <- as.data.frame(matrix(NA,nrow=11,ncol=4))
  names(greensLeft) <- c("minutes_since_start","targeted","greens_used","greens_remaining")
  greensLeft$minutes_since_start <- c(0:10)
  for (i in 1:nrow(greensLeft)){
    greensLeft[i,"targeted"] <- nrow(targDat[which(targDat$begin_time/60 <= greensLeft[i,"minutes_since_start"]),])
    greensLeft[i,"greens_used"] <- nrow(greenDat[which(greenDat$timesec/60 <= greensLeft[i,"minutes_since_start"]),])
    greensLeft[i,"greens_remaining"] <- greenMax - greensLeft[i,"greens_used"]
  }
  
  greens_used <- nrow(greenDat)
  
  low_on_greens_time_minutes <- NA
  if (nrow(greenDat) >= greenMax - 2){ ## consider someone "low" after they use their 18th green
    low_on_greens_time_minutes <- greenDat[(greenMax-2),"time"]
  }
  
  spikes_while_low <- 0
  if (!is.na(low_on_greens_time_minutes)){
    spikes_while_low <- nrow(targDat[which(targDat$begin_time/60 >= low_on_greens_time_minutes),])
  }
  
  avg_greens_per_spike <- NA
  if (nrow(targDat)>0){
    avg_greens_per_spike <- nrow(greenDat) / nrow(targDat)
  }
  
  avg_greens_per_spike_before_low <- NA
  if (nrow(targDat)>0 & is.na(low_on_greens_time_minutes)){
    avg_greens_per_spike_before_low <- nrow(greenDat) / nrow(targDat)
  }else if (nrow(targDat)>0){
    avg_greens_per_spike_before_low <- nrow(greenDat[which(greenDat$time <= low_on_greens_time_minutes),]) /
      nrow(targDat[which(targDat$begin_time/60 < low_on_greens_time_minutes),])
  }
  
  return(list(greens_by_time = greensLeft,
              greens_used = greens_used,
              low_on_greens_time_minutes = low_on_greens_time_minutes,
              spikes_while_low = spikes_while_low,
              avg_greens_per_spike = avg_greens_per_spike,
              avg_greens_per_spike_before_low = avg_greens_per_spike_before_low))
}

analyzeGroupGreens <- function(parseOut, teamNum, greenMax=20){
  if (teamNum==0){
    ent<-parseOut$entities0
    enemySpike<-parseOut$spikeSummary1
    thisSumm<-parseOut$powerSummary0
  }else{
    ent<-parseOut$entities1
    enemySpike<-parseOut$spikeSummary0
    thisSumm<-parseOut$powerSummary1
  }
  validPlayers <- nrow(ent)
  datOut<-as.data.frame(matrix(NA,nrow=validPlayers,ncol=5))
  p1<-analyzePlayerGreens(parseOut,ent$name[1], greenMax=greenMax)
  p2<-analyzePlayerGreens(parseOut,ent$name[2], greenMax=greenMax)
  if (validPlayers>=3){ p3<-analyzePlayerGreens(parseOut,ent$name[3], greenMax=greenMax) } else { p3 <- NULL }
  if (validPlayers>=4){ p4<-analyzePlayerGreens(parseOut,ent$name[4], greenMax=greenMax) } else { p4 <- NULL }
  if (validPlayers>=5){ p5<-analyzePlayerGreens(parseOut,ent$name[5], greenMax=greenMax) } else { p5 <- NULL }
  if (validPlayers>=6){ p6<-analyzePlayerGreens(parseOut,ent$name[6], greenMax=greenMax) } else { p6 <- NULL }
  if (validPlayers>=7){ p7<-analyzePlayerGreens(parseOut,ent$name[7], greenMax=greenMax) } else { p7 <- NULL }
  if (validPlayers>=8){ p8<-analyzePlayerGreens(parseOut,ent$name[8], greenMax=greenMax) } else { p8 <- NULL }
  
  datOut <- thisSumm[,c("Name","ApproxTargeted","ApproxDeaths")]
  names(datOut)<-c("name","targeted","deaths")
  datOut$survival_rate <- (datOut$targeted - datOut$deaths)/datOut$targeted
  datOut[which(is.nan(datOut$survival_rate)),"survival_rate"] <- NA
  
  datOut$pct_spikes_greens_used <- 0
  datOut$total_greens_used <- 0
  datOut$low_on_greens_time <- NA
  datOut$spikes_while_low <- 0
  datOut$avg_greens_per_spike <- NA
  datOut$avg_greens_per_spike_before_low <- NA
  
  for (i in 1:nrow(datOut)){
    thisName<-datOut[i,"name"]
    thisSpike <- enemySpike[which(enemySpike$target==thisName),]
    if (nrow(thisSpike)>0){
      datOut[i,"pct_spikes_greens_used"]<-mean(thisSpike$greens)
    }
  }
  
  datOut[1,6:10] <- as.numeric(p1[2:6])
  datOut[2,6:10] <- as.numeric(p2[2:6])
  if (!is.null(p3)){ datOut[3,6:10] <- as.numeric(p3[2:6]) }
  if (!is.null(p4)){ datOut[4,6:10] <- as.numeric(p4[2:6]) }
  if (!is.null(p5)){ datOut[5,6:10] <- as.numeric(p5[2:6]) }
  if (!is.null(p6)){ datOut[6,6:10] <- as.numeric(p6[2:6]) }
  if (!is.null(p7)){ datOut[7,6:10] <- as.numeric(p7[2:6]) }
  if (!is.null(p8)){ datOut[8,6:10] <- as.numeric(p8[2:6]) }
  return(datOut)
}
