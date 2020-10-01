
#### Known issues:
## - only counts Strangler when it causes a hit reaction on target (not fixable but fairly reliable except if it misses)
## - always counts SSJ as being "on target" because it has no target data (not fixable), must still be within spike window
## - always counts projectile misses as being "on target" because they have no target data (not fixable), must still be within spike window
## - technically a particular evasion or a particular heal could be counted for TWO spikes if there is a back-to-back "recall" spike
##     and the heal or evasion occurs near the boundary of those two spikes
## - there's probably no reliable way to give someone credit for a pre-evasion that results in the enemy firing 0 attacks (likely not fixable)
## - this code is missing any ability that I haven't programmed into it, which is a long list. Fixable by adding more FX
## - extended / dirty recall spikes (e.g. target getting hit for a long period of time) will be split up into multiple spikes (as intended),
##     but the target might not get credit for an evasion if their response to the latter portion of the spike is simply to continue
##     moving / breaking LOS rather than using jaunt/fly/phase/hibernate
## - no guarantee that other powers (not yet added to my lists) don't use MOV Wall 0 which is what I use to detect SSJ. For example,
##     Block of Ice from Ice Control might use the same thing, so care would need to be taken to deduplicate MOV Wall 0's from Block of Ice.
## - Masterminds aren't counted as "on spike" simply by having pets target players... they only end up on spike by using their abilities.

## Also, a major to do item is that graphics could be generated in ggplot2, particularly for things like count of spikes over time,
##     match score over time, number of players on spike over time, etc. I've been too lazy to create these, but it wouldn't be difficult.

parseDemo <- function(x, 
                      keySkills = NULL,
                      exclNames = c("Dr","Phantasm","Blind","Decoy","Fiery Orb","Poison Trap",
                                    "Animated Stone","Coralax Blue Hybrid","Voltaic Sentinel",
                                    "Decoy Phantasm","Burst of Speed","Ice Storm",
                                    "Superior Vigilant Assault","Sticky Arrow", "Phoenix",
                                    "Voltaic Geyser","Water Spout","Mu Guardian","Dimension Shift",
                                    "Energy Font","Singularity","Superior Scourging Blast","Spectral Terror",
                                    "Architect Contact","Ticket Vendor","Architect Entertainment Instructor",
                                    "Architect Entertainment Greeter","17  ","Coralax Red Hybrid","Galvanic Sentinel",
                                    "Umbra Beast","Fortunata Mistress","Transference","Blizzard","Spirit Panther",
                                    "Superior Defender's Bastion", "Rain of Fire"),
                      attackset = c("Blaze", "Char", "Blazing Bolt", "Fire Blast",
                                    "Corrosive", "Envenom", "Bitter Ice Blast", "Dominate",
                                    "Blind", "Fossilize", "Strangler", "Stalagmites", "Lancer Shot","P Ray or Charged",
                                    "Disintegrate","Ice Blast", "Freeze Ray","Bitter Freeze Ray","Enervating Field",
                                    "Mesmerize","Zapp", "Spirit Shark Jaws","Shriek","Scream","Shout","Screech",
                                    "Power Push","Energy Snipe","Will Domination","Telekinetic Blast","Subdue",
                                    "Scramble Thoughts","Psionic Lance","Mental Blast","Force Bolt",
                                    "Cosmic Burst","Proton Volley","X-Ray Beam", "Melt Armor", "Gravity Distortion",
                                    "Lift","Wormhole","Dark Blast","Gloom","Moonbeam","Life Drain","Knockout Blow",
                                    "Electric Shackles","Suppressive Fire","Executioner Shot"),
                      healset = c("Absorb Pain","Heal Other","Aid Other","Spirit Ward","Insulating Circuit",
                                  "Rejuvenating Circuit", "Soothe", "Share Pain", "Cauterize"),
                      evadeset = c("Phase Shift","Hibernate","Jaunt","Raptor or Fly","Burst of Speed","Dim Shift"),
                      otherset = c("Crey Pistol","Net Arrow","Weaken","Confuse or Deceive","Regrowth",
                                   "Shock","Transfusion","Transference",
                                   "Siphon Speed","Inferno","Aid Self","Heat Exhaustion","Blackstar",
                                   "Embrace of Fire","Domination","Entangle","Web Grenade"),
                      buffset = c("Empowering Circuit",
                                   "Energizing Circuit","Amp Up","Healing Aura","Fort or AB","Clear Mind",
                                   "Speed Boost","Increase Density","Inertial Reduction",
                                   "Absorb Pain","Heal Other","Aid Other","Spirit Ward","Insulating Circuit",
                                   "Rejuvenating Circuit","Soothe","Share Pain","Thaw or Forge","Cauterize"),
                      spikeWindow=3.5, min_attacks_per_spike=2, max_time_per_spike_sec=11, hitSupportCredit=TRUE,
                      healWindow=2, preEvadeWindow=c(-1.5,1), greenMax=20,
                      smartStart=TRUE, smartEnd=TRUE, customStart=NULL, customEnd=NULL, checkEntities=TRUE,
                      suppsTeam0=2, suppsTeam1=2,
                      expectedPlayers=16,dropNPCentity=FALSE,
                      emotePolice=TRUE,returnRawDF=FALSE,
                      textset = c("FIREBALL.FX", "SOOT.FX", "INFERNOBOLT.FX",
                                   "FIREBLASTAIM.FX", "FIREBOLT.FX", "FLARES.FX","XXLARGEFIREBALL.FX", "INFERNO.FX",
                                   "INFERNO_ATTACK.FX","LEAPING.FX", "GEASTHEKINDONESCONTINUING.FX", "BURSTOFSPEED.FX",
                                   "BLAZE_ATTACK.FX", "BLAZINGBOLT_ATTACK.FX", "FLARES_ATTACK.FX","FIREBLAST_ATTACK.FX",
                                   "JAUNT_ATTACK.FX", "FIREBALL_ATTACK.FX",
                                   "SKYRAIDERJETS.FX", "AIM_ACTIVATION.FX", "/BUILDUP_ATTACK.FX",
                                   "SUPERJUMP_ACTIVATION.FX", "ALIGNMENT_JUSTICE.FX",
                                   "FOCUSCHI.FX", "PHASESHIFT_ATTACK.FX", "LEAPING_NOFX.FX",
                                   "ARROW_STICKY.FX","CORROSIVESAP.FX", "LASER_PISTOL_1.FX",
                                   "REGROWTH2.FX", "/POISONLIQUIDPROJECTILE.FX",
                                   "BITTERFREEZERAY.FX", "PALMPOISONLIQUIDPROJECTILE.FX",
                                   "ICEBLASTAIM.FX", "SPEEDOFSOUND_ACTIVATION.FX", "SUPERSPEED.FX",
                                   "COMMAND2.FX", "ABSORBPAIN.FX", "HEALINGHANDS.FX",
                                   "ILLUSBLIND.FX", "ILLUSDECOY.FX", "ILLUSPHANTASM.FX",
                                   "BUILDUPPOWERBOOST.FX", "INVISPHASE_FASTCAST.FX", "FOSSILIZEHANDS.FX",
                                   "MYSTRANGLER.FX",
                                   "STALAGMITESTOMP.FX","HIBERNATE_CONTINUING.FX","HIBERNATE.FX",
                                   "STOLLEN_IMOBILIZER_PISTOL.FX","BEAMRIFLE_LANCERSHOT.FX", "BEAMRIFLE_CHARGEDSHOT.FX",
                                   "BEAMRIFLE_DISINTEGRATE.FX","BEAMRIFLE_AIM.FX","ICEBOLT.FX", "ICEBLAST.FX",
                                   "/FREEZERAY.FX","_FREEZERAY.FX","BITTERICEBOLT.FX","ENERVATINGFIELDHAND.FX",
                                   "LINGERINGRADIATION.FX","FREEZE.FX","CONFUSE.FX","HYPNOTIZE.FX","MINDCONTROL.FX",
                                   "ILLUSDECIEVE.FX","THUNDEROUSBLAST.FX","ELECTRICITYCAGEBOLTS.FX","ZAPP_QUICK.FX",
                                   "MAID.FX","MYSTICFLIGHT.FX","NONCOMBATFLIGHT.FX","SPIRITWARD.FX",
                                   "INSULATINGCIRCUITATK.FX","REJUVENATINGCIRCUITATK.FX","EMPOWERINGCIRCUITATK.FX",
                                   "AMPUPATTACK.FX","GALVANICSENTINELSUMMON.FX","ENERGIZINGCIRCUITATK.FX",
                                   "WITCHESLIGHTNINGBOLTMEGA.FX","DEFIBRILLATE_ATTACK.FX","HEROSMALLBLAST.FX",
                                   "HEROSTANDARDBLAST2.FX","MASSIVESONICBLAST.FX","HEADSONICSCREECH.FX",
                                   "SCREAMPBAOE.FX","ENERGYBLAST_BUILDUP_AIM.FX","POWERPUSH.FX","SNIPERBLAST_QUICK.FX",
                                   "THORNS/BUILDUP_ATTACK.FX","SSJ.FX","RADIATIONEMISSION.FX","ENDURANCEHANDS.FX",
                                   "STRENGTHHANDS2.FX","NONCOMBATFLIGHT_NOFX.FX","/ARROW_NET.FX",
                                   "KINSIPHONSPEED.FX","KINSPEEDBOOST.FX","KININCREASEDENSITY.FX",
                                   "KININERTIALREDUCTIONS.FX","WILLDOMINATION.FX","TELEKINETICBLAST.FX",
                                   "SUBDUEPSIONICBLAST.FX","PSIBLAST_SLOWERCAST.FX","PSIONICLANCEBLASTQUICK.FX",
                                   "PSIONICBLAST_SLOWCAST.FX","KINTRANSFUSION.FX","KINTRANSFERENCE.FX",
                                   "SOOTH_ATTACK.FX","SHAREPAIN_ATTACK.FX","FORCEBOLT.FX","/STAMINA.FX",
                                   "RADIATIONBLASTAIM.FX","COSMICBLAST.FX","PROTONBLAST_QUICK.FX",
                                   "XRAYBEAM.FX","BURSTOFSPEED.FX","GCDIMENSIONSHIFT.FX","AIDSELF_ATTACK.FX",
                                   "AIM_ACTIVATION.FX","WILDBASTION.FX","OVERGROWTH.FX","WILD_GROWTH.FX",
                                   "CHILLINGHANDS.FX","GCCRUSH_SINGULARITY.FX","GCLIFT.FX","GCDISTORTION.FX",
                                   "GCCRUSHINGFIELD.FX","GCDISTORTIONFIELD.FX","GCWORMHOLE.FX",
                                   "ADRENALINEFLOW.FX","EMPATHYCUREWOUNDS.FX","PBAOE.FX","FIRESHIELDHANDS.FX",
                                   "FIREHEALSELFWITHHANDS.FX","FIRESHIELDHANDSBOTH.FX","MELTARMORCAST.FX",
                                   "DARKNESSBLAST2.FX","SOULDRAIN.FX","MOONBEAM_QUICK.FX","LIFEDRAIN.FX","BLACKSTAR.FX",
                                   "FOLLOWUPHIT4.FX","DOMINATIONACTIVATION.FX","STRENGTHSTREAKSKO.FX",
                                   "REDMU_CAGEBOLTS.FX","SUPPRESSIVEFIRE_ATTACK.FX","EXECUTIONERSSHOT_ATTACK.FX",
                                   "ENTANGLEPLANTSEEDTHROW.FX","WEBGRENADETHROW.FX"),
                      
                      powerset = c("Blaze", "Char", "Blazing Bolt", "Aim", "Fire Blast",
                                    "Flares", "Fire Ball", "Inferno", "Inferno","Super Jump", "Geas", "Burst of Speed",
                                    "Blaze","Blazing Bolt", "Flares","Fire Blast","Jaunt", "Fire Ball","Raptor or Fly",
                                    "Aim","Build Up","Super Jump", "Call to Justice", "Build Up", "Phase Shift",
                                    "Super Jump", "Glue Arrow", "Corrosive", "Crey Pistol", "Regrowth",
                                    "Envenom", "Bitter Ice Blast", "Weaken", "Aim", "Speed of Sound",
                                    "Super Speed", "Dominate", "Absorb Pain", "Heal Other",
                                    "Blind", "Phantom Army", "Phantasm", "Vanguard/PB", "Phase Shift",
                                    "Fossilize", "Strangler", "Stalagmites", "Hibernate", "Hibernate",
                                    "Crey Pistol", "Lancer Shot","P Ray or Charged","Disintegrate","Aim","Ice Bolt",
                                    "Ice Blast","Freeze Ray","Freeze Ray","Bitter Freeze Ray","Enervating Field",
                                    "Lingering Radiation","Total Domination","Mass Confusion","Confuse or Deceive",
                                    "Mesmerize","Confuse or Deceive","Thunderous Blast","Tesla Cage","Zapp",
                                    "Aid Other","Raptor or Fly","Raptor or Fly","Spirit Ward","Insulating Circuit",
                                    "Rejuvenating Circuit","Empowering Circuit","Amp Up","Galvanic Sentinel",
                                    "Energizing Circuit","Shock","Defibrillate","Shriek","Scream","Shout","Screech",
                                    "Dreadful Wail","Aim","Power Push","Energy Snipe","Toxins","Spirit Shark Jaws",
                                    "Healing Aura","Fort or AB","Clear Mind","Raptor or Fly",
                                    "Net Arrow","Siphon Speed","Speed Boost","Increase Density",
                                    "Inertial Reduction","Will Domination","Telekinetic Blast","Subdue",
                                    "Scramble Thoughts","Psionic Lance","Mental Blast","Transfusion",
                                    "Transference","Soothe","Share Pain","Force Bolt","Green Insp",
                                    "Aim","Cosmic Burst","Proton Volley","X-Ray Beam","Burst of Speed",
                                    "Dim Shift","Aid Self","Aim","Wild Bastion","Overgrowth","Wild Growth",
                                    "Ice Storm","Crush","Lift","Grav Distortion","Crushing Field",
                                    "Grav Field or Singularity","Wormhole","Recovery Aura","Regen Aura",
                                    "Warmth","Heat Exhaustion","Cauterize","Thaw or Forge","Melt Armor",
                                    "Dark Blast","Gloom","Moonbeam","Life Drain","Blackstar",
                                    "Embrace of Fire","Domination","Knockout Blow","Electric Shackles",
                                    "Suppressive Fire","Executioner Shot","Entangle","Web Grenade")){
  
  ## General note: a few times in this code I get lazy by assuming player entities will be < 1000
  ## instead of just using regex. Using this code outside of arena matches, or in crowded arena
  ## matches, runs the risk of having players with 4+ digit entities which might break some of the code.
  
  ## Always track green inspirations (shows up in defensive data later on)
  otherset <- unique(c(otherset,"Green Insp"))
  
  if (is.null(keySkills)){
    keySkills <- unique(c(attackset,healset,evadeset,otherset,buffset))
  }
  
  if (suppsTeam0>3 | suppsTeam1>3){
    warning("suppsTeam0 and suppsTeam1 only function up to a value of 3 (for now); more supports will not be counted")
  }
  
  warnTrigger<-FALSE
  mydataDF <- tryCatch(read.delim(paste0(x,".cohdemo"),sep="\r",
                         stringsAsFactors=FALSE,	na.strings=""), warning = function(w) w)
  if ("warning" %in% class(mydataDF)){
    mydataDF <- read.delim(paste0(x,".cohdemo"),sep="\r", quote="",
                           stringsAsFactors=FALSE,	na.strings="")
    warning("Unusual quote error while reading demo. Quote detection has been turned off. Output should still be fine (hopefully)")
    warnTrigger<-TRUE
  }
  names(mydataDF)[1] <- "string"
  mydata <- as.character(mydataDF[,1])
  mydataDF$timeIncr <- as.numeric(substr(mydata,1,4))
  mydataDF$time <- cumsum(mydataDF$timeIncr)/1000/60
  mydataDF$timesec <- mydataDF$time*60
  mydataDF$entity <- suppressWarnings(as.numeric(substr(mydata,5,7)))
  
  mydataDF <- mydataDF[1:(nrow(mydataDF)-1),] # drop footer row
  mydata <- mydata[1:(length(mydata)-1)] # drop footer row
  
  map <- "Map not added to code yet!"
  
  if (any(grepl("/Arena_Skyway_01.txt",mydataDF$string))) { map <- "Skyway City" }
  else if (any(grepl("/Arena_steel_01.txt",mydataDF$string))) { map <- "Steel Canyon" }
  else if (any(grepl("/Arena_OutbreakRuin_01.txt",mydataDF$string))) { map <- "Outbreak" }
  else if (any(grepl("/Arena_Atlas_01.txt",mydataDF$string))) { map <- "Atlas Park" }
  else if (any(grepl("/Arena_Striga_01.txt",mydataDF$string))) { map <- "Striga Isle" }
  else if (any(grepl("/Arena_Eden_01.txt",mydataDF$string))) { map <- "Eden" }
  else if (any(grepl("/Arena_Industrial_01.txt",mydataDF$string))) { map <- "Industrial/Siege" }
  else if (any(grepl("/Arena_Stadium_01.txt",mydataDF$string))) { map <- "Stadium" }
  else if (any(grepl("/Arena_Coliseum.txt",mydataDF$string))) { map <- "Coliseum" }
  else if (any(grepl("/Arena_Tech_01.txt",mydataDF$string))) { map <- "Tech Lab" }
  else if (any(grepl("/Arena_CargoShip_01.txt",mydataDF$string))) { map <- "Cargo Ship" }
  else if (any(grepl("/Arena_Graveyard_01.txt",mydataDF$string))) { map <- "Graveyard" }
  else if (any(grepl("/Arena_Perez_01.txt",mydataDF$string))) { map <- "Perez Park" }
  
  ## New entity detection: figure out the top expectedPlayers who used FX OneShot the most.
  ## This excludes cameras (spectators) and also probably eliminates the need for exclNames altogether, unless
  ## a player drops from the match early and does not rack up a ton of FX OneShots.
  mostOneShots <- table(mydataDF[which(grepl("FX OneShot",mydataDF$string)),"entity"])
  npcnums <- as.numeric(gsub("0   (\\d\\d*[\\d \\s]*) NPC ", "\\1",
                             regmatches(mydataDF$string,regexpr("0   (\\d\\d*[\\d \\s]*) NPC ",mydataDF$string))))
  playerNums <- as.numeric(names(sort(mostOneShots,decreasing=T)))
  if (dropNPCentity){
    playerNums <- playerNums[which(!playerNums %in% c(npcnums))][1:expectedPlayers]
  }else{
    playerNums <- playerNums[1:expectedPlayers]
  }
  
  ent_str <- gsub("0   ","",unique(mydata[which(grepl("NEW",mydata))]))
  ent_temp <- unlist(strsplit(ent_str,"NEW"))
  ent_nums <- as.numeric(ent_temp[seq(from=1,to=length(ent_temp),by=2)])
  ent_names <- trimws(ent_temp[seq(from=2,to=length(ent_temp),by=2)])
  entities <- unique(data.frame(num = ent_nums, name = ent_names))
  entities$name <- as.character(entities$name)
  entities <- entities[which(!duplicated(entities$num)),] ## for rare situations when a player drops and entity is reassigned
  
  ## New code:
  entities <- entities[which(entities$num %in% playerNums),]
  
  ## NO LONGER USED:
  # entities <- entities[which(entities$num < 100),] ## heuristic, but it should work. Players load first
  # entities <- entities[which(!entities$name %in% exclNames),]
  
  entities$team<-NA
  
  ## Team identification code... hat tip to xhiggy on this; teams can usually be inferred from buff casts/targets.
  ## Unfortunately, this requires moving some parts of the process to the front.
  
  buffFX <- textset[which(powerset %in% buffset)]
  mydataDF$buff<-0
  for (i in buffFX){
    mydataDF[which(grepl(i,mydataDF$string)),"buff"]<-1
  }
  buffRows<-which(mydataDF$buff==1)
  
  ## For every buff that was cast... try to find caster and target. Caster-target pairings (over a threshold) will be assumed to be teammates.
  
  buffdat <- as.data.frame(matrix(NA,nrow=length(buffRows),ncol=1))
  names(buffdat)[1] <- "buff"
  buffdat$caster <- NA
  buffdat$target <- NA
  buffdat$buff <- 1:length(buffRows)
  for (i in 1:nrow(buffdat)){
    thisStart<-buffRows[i]
    theseRows <- mydataDF[c(thisStart,thisStart+1,thisStart+2,thisStart+3,thisStart+4),]
    if (sum(theseRows$buff) != 1){
      stop("This shouldn't happen; error with buff detection for team assignment")
    }
    buffdat[i,"caster"] <- theseRows$entity[1]
    s2targ <- theseRows[which(grepl(" TARGET ENT",theseRows$string)),]
    thisTarg<-NA
    if (nrow(s2targ)>0){
      attackerPre<-substr(substr(s2targ$string,regexpr("ENT",s2targ$string),regexpr("ENT",s2targ$string)+6),5,7)
      if (length(attackerPre)>1){stop("This shouldn't happen... it means more than one ' TARGET ENT' for a given FX OneShot")}
      if (substr(attackerPre,2,2)==" "){
        attackerPre <- substr(attackerPre,1,2) ## hotfix
      }
      thisTarg <- as.numeric(attackerPre)
    }
    buffdat[i,"target"] <- thisTarg
  }
  
  entities[1,"team"] <- 0 ## assume the first player seen is on Team 0 (ally)
  firstEnt <- entities[1,"num"]
  
  firstTab1 <- as.data.frame(table(buffdat[which(buffdat$caster==firstEnt),"target"]))
  firstTab2 <- as.data.frame(table(buffdat[which(buffdat$target==firstEnt),"caster"]))
  firstAllies <- c()
  if (nrow(firstTab1)>0){
    firstTab1 <- firstTab1[which(firstTab1$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab1)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab1$Var1)))
    }
  }
  if (nrow(firstTab2)>0){
    firstTab2 <- firstTab2[which(firstTab2$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab2)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab2$Var1)))
    }
  }
  
  entities[which(entities$num %in% unique(firstAllies)),"team"] <- 0
  
  team0pl <- entities[which(entities$team==0),"num"]
  
  ## recycle the firstTab, firstAllies names... run this cycle twice more just to be safe
  firstTab1 <- as.data.frame(table(buffdat[which(buffdat$caster %in% team0pl),"target"]))
  firstTab2 <- as.data.frame(table(buffdat[which(buffdat$target %in% team0pl),"caster"]))
  firstAllies <- c()
  if (nrow(firstTab1)>0){
    firstTab1 <- firstTab1[which(firstTab1$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab1)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab1$Var1)))
    }
  }
  if (nrow(firstTab2)>0){
    firstTab2 <- firstTab2[which(firstTab2$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab2)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab2$Var1)))
    }
  }
  
  entities[which(entities$num %in% unique(firstAllies)),"team"] <- 0
  
  team0pl <- entities[which(entities$team==0),"num"]
  
  ## recycle the firstTab, firstAllies names... run this cycle once more just to be safe
  firstTab1 <- as.data.frame(table(buffdat[which(buffdat$caster %in% team0pl),"target"]))
  firstTab2 <- as.data.frame(table(buffdat[which(buffdat$target %in% team0pl),"caster"]))
  firstAllies <- c()
  if (nrow(firstTab1)>0){
    firstTab1 <- firstTab1[which(firstTab1$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab1)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab1$Var1)))
    }
  }
  if (nrow(firstTab2)>0){
    firstTab2 <- firstTab2[which(firstTab2$Freq > 2),] ## might discard bizarre situations like throwing 1-2 buffs while confused
    if (nrow(firstTab2)>0){
      firstAllies<-c(firstAllies,as.numeric(as.character(firstTab2$Var1)))
    }
  }
  
  entities[which(entities$num %in% unique(firstAllies)),"team"] <- 0
  
  if (length(which(entities$team==0)) != expectedPlayers/2){
    warning("Error in entity detection / team assignment. Reverting back to old team assignment code (may require inspection of teams)")
    ## Note: this might happen if teams run "full offense" or if this code is used for very small-scale matches.
    entities[1:expectedPlayers,"team"] <- c(rep(0,expectedPlayers/2),rep(1,expectedPlayers/2))
  }else{
    entities[which(is.na(entities$team)),"team"] <- 1
  }
  
  entities <- entities[which(!is.na(entities$name)),] ## This is probably no longer necessary but shouldn't hurt anything
  entities <- entities[order(entities$team),]
  
  if (checkEntities){
    tempent <- entities[,c("name","team")]
    if (warnTrigger){
      tempent$name <- gsub("\"","",tempent$name)
    }
    print(tempent)
    response1 <- readline("Does this team list look correct? Y or blank response = Yes, N = No (will enter correction mode): ")
    if (!toupper(response1) %in% c("Y","N","")) { stop("Unexpected response. Must be Y or N") }
    if (toupper(response1)=="N"){
      for (i in 1:nrow(entities)){
        response2 <- readline(paste0("Is ", entities[i,"name"], " a player character? Y or N: "))
        if (!toupper(response2) %in% c("Y","N","")) { stop("Unexpected response. Must be Y or N") }
        if (toupper(response2)=="N"){
          message(paste0("Removed. Consider adding ", entities[i,"name"], " to the set of exclNames in this function."))
          entities <- entities[which(entities$name != entities[i,"name"]),]
        }else{
          response3 <- readline(paste0("Is ", entities[i,"name"], " on Team 0 (ally) or Team 1 (enemy)? 0 or 1: "))
          if (!response3 %in% c(0,1)){ stop("Unexpected response. Must be 0 or 1") }
          entities[which(entities$name==entities[i,"name"]),"team"] <- response3
        }
      }
    }
    message("Entity check complete. Proceeding...")
  }
  
  if (nrow(entities) != expectedPlayers){
    warning(paste0("ERROR, inspect output: Unexpected number of players: ",nrow(entities)))
    return(entities)
  }
  
  mydataDF$sortind <- 1:nrow(mydataDF)
  mydataDF <- merge(x=mydataDF, y=entities, by.x="entity", by.y="num",
                    all.x=TRUE)
  mydataDF <- mydataDF[order(mydataDF$sortind),]
  mydataDF$sortind <- NULL
  
  ## smartStart begins tracking data 1 second before the first player jumps.
  ## While this hasn't been thoroughly vetted, it should work in most cases.
  
  firstJump <- min(mydataDF[which(grepl("JUMP",mydataDF$string) & !grepl("NEW ",mydataDF$string) &
                                    grepl("MOV ",mydataDF$string)),"timesec"],na.rm=TRUE)
  firstJumpT <- min(mydataDF[which(grepl("JUMP",mydataDF$string) & !grepl("NEW ",mydataDF$string) &
                                     grepl("MOV ",mydataDF$string)),"time"],na.rm=TRUE)
  
  ## customStart can also be used (now that entities have been captured):
  
  if (!is.null(customStart)){
    if (smartStart){
      warning("customStart supplied along with smartStart=TRUE. Note that customStart will override.")
    }
    mydataDF <- mydataDF[which(mydataDF$timesec >= customStart),]
  }else if (smartStart){
    mydataDF$timesec <- mydataDF$timesec - firstJump + 1
    mydataDF$time <- mydataDF$time - firstJumpT + 1/60
    firstJump <- 0
    firstJumpT <- 0
  }
  
  emoteUsage <- "Emote detection disabled"
  if (emotePolice){
    emoteRows <- which(grepl("EMOTE",mydataDF$string))
    if (length(emoteRows)>0){
      emoteUsage <- mydataDF[emoteRows,c("name","team","time","string")]
      emoteUsage$notes <- NA
      emoteUsage[which(grepl("EMOTE_HOLD_TORCH",emoteUsage$string)
                       & emoteUsage$time < 0.6),"notes"] <- "Probably just Silent at the start"
      emoteUsage[which(grepl("EMOTE_MALE_LOOKING_DOWN",emoteUsage$string)),"notes"] <- "ANIM CANCEL - em shocked"
      emoteUsage[which(grepl("EMOTE_FEM_BEING_HELD",emoteUsage$string)),"notes"] <- "ANIM CANCEL - em liedown"
    }else{
      emoteUsage <- "No emote usage"
    }
    if (class(emoteUsage) != "character"){
      if (length(which(grepl("ANIM CANCEL",emoteUsage$notes)))>0){
        warning("At least one animation canceling emote was used. Inspect emoteUsage output")
      }
      if (length(which(is.na(emoteUsage$notes)))>0){
        warning("Unclassified emotes were used. Not necessarily animation canceling, but inspect emoteUsage output")
      }
    }
  }
  
  mydataDF <- mydataDF[which(mydataDF$timesec >= 0),]
  
  if (smartEnd){
    ## When smartEnd is used, it stops recording 10 minutes + 1 second after the first jump occurs, regardless of whether
    ## smartStart is used. This means you can use smartEnd without smartStart if you want to capture pre-match buff stats, etc.
    matchEnd <- firstJump + 601
  }else{
    ## Someone on Team 0 leaving match after 10+ min of demo recording = heuristic end of match
    leaverow<-which(grepl("DEL",mydataDF$string) & mydataDF$team==0 & mydataDF$timesec>600)
    if (length(leaverow)>0){
      matchEnd <- min(mydataDF[which(grepl("DEL",mydataDF$string) & mydataDF$team==0 &
                                       mydataDF$timesec>600),"timesec"])
    }else{
      matchEnd <- Inf
    }
  }
  
  ## alternatively, if customEnd has been specified, then that overrides.
  ## Basically, advanced users can specify a custom ending time (in seconds) if they know exactly when a match ended
  ## If used in conjunction with smartStart, users should be aware that timesec gets recalibrated based on the first jump!
  if (!is.null(customEnd)){
    if (smartEnd){
      warning("customEnd supplied along with smartEnd=TRUE. Note that customEnd will override.")
    }
    if (smartStart & is.null(customStart)){
      warning("customEnd supplied along with smartStart=TRUE. customStart recalibrates time indices, so be cautious!")
    }
    matchEnd <- customEnd
  }
  
  mydataDF <- mydataDF[which(mydataDF$timesec < matchEnd),]
  
  runtimeMin <- round((max(mydataDF$timesec) - min(mydataDF$timesec)) / 60,0)
  runtimeSec <- round((max(mydataDF$timesec) - min(mydataDF$timesec)) %% 60,0)
  runtime <- paste0(runtimeMin,":", ifelse(nchar(runtimeSec)==1,paste0("0",runtimeSec),runtimeSec))
  
  
  deaths <- mydataDF[which(((grepl("AIR_DEATH",mydataDF$string) | grepl("HITDEATH",mydataDF$string)) &
                              !grepl("IMPACT",mydataDF$string)) &
                             !is.na(mydataDF$team)),]
  deaths <- deaths[order(deaths$name,deaths$timesec),]
  keepDeath <- 1 ## sometimes players will have a death animation appear multiple times in quick succession
  for (i in 2:nrow(deaths)){
    if (deaths[i,"name"]!=deaths[(i-1),"name"] | (deaths[i,"timesec"] - deaths[(i-1),"timesec"] >= 15)){
      keepDeath<-c(keepDeath,i)
    }
  }
  deaths <- deaths[keepDeath,]
  
  ## And sometimes deaths don't get picked up by that method but get picked up by HP 0.0 instead...
  
  deaths2 <- mydataDF[which(!is.na(mydataDF$team) & grepl("HP 0.0",mydataDF$string)),]
  
  for (i in 1:nrow(deaths2)){
    thisName<-deaths2[i,"name"]
    thisTime<-deaths2[i,"timesec"]
    thisWindowMin<-thisTime - 7.5
    thisWindowMax<-thisTime + 7.5
    thisDeath<-deaths[which(deaths$name==thisName),]
    found_indicator <- FALSE
    if (nrow(thisDeath)>0){
      found_indicator <- length(which(thisDeath$timesec > thisWindowMin & thisDeath$timesec < thisWindowMax)) > 0
    }
    if (nrow(thisDeath)==0 | !found_indicator){
      deaths <- rbind(deaths,deaths2[i,])
    }
  }
  
  kbs <- mydataDF[which(!is.na(mydataDF$team) & grepl("PLAYERKNOCKBACK", mydataDF$string)),]
  
  if (nrow(kbs)>0){
    kbs <- kbs[order(kbs$name, kbs$timesec),]
    
    keepKB <- 1 ## KB animations have several MOVs in succession with PLAYERKNOCKBACK; keep only the first, every 10 sec
    if (nrow(kbs)>1){
      for (i in 2:nrow(kbs)){
        if (kbs[i,"name"]!=kbs[(i-1),"name"] | (kbs[i,"timesec"] - kbs[(i-1),"timesec"] >= 10)){
          keepKB<-c(keepKB,i)
        }
      }
    }
    kbs <- kbs[keepKB,]
  }else{
    kbs <- "No player knockbacks"
  }
  
  
  ## For some bizarre reason, SSJ doesn't have an .FX animation. Instead, it shows up as MOV Wall 0.
  ## Might as well just "convert" those values into my own fake .FX.
  
  ## Note: SSJ will always have an NA target. Other NA targets are typically self-targeted powers (e.g. Jaunt) or projectile misses.
  ## There's really no choice but to give "on-spike" credit when an NA target occurs, because it could've just been
  ## a miss. However, off-spike targets can be removed.
  
  mydataDF$string <- gsub("MOV Wall 0","FX OneShot FAKE/SSJ.FX",mydataDF$string)
  
  
  ## Strangler is ridiculously messed up. It doesn't have an .FX for animation (like SSJ), but it also doesn't
  ## have a obvious MOV that gives it away either. It's basically a ghost, in the demos. It has one of
  ## two possible prep .FX's: QUILLSHTHDAMAGENOSPIKES.FX or THORNS_ACTIVATION.FX
  ## ...and the target has a hit .FX: PLANTCONTROLHIT.FX
  ## ...but there's no reliable way to tell if it was actually fired.
  ## There is a way to tell if it landed, but this is different from how all other powers are accounted,
  ## which is at the time of firing (i.e. regardless of whether they hit/miss, whether target is dead/phased...).
  ## Unfortunately this is the only way to count it: find PLANTCONTROLHIT.FX's, and see who the TARGET or PREVTARGET
  ## is from the subsequent lines. However, Entangle usages need to be removed.
  
  stranglerhits<-which(grepl("PLANTCONTROLHIT.FX",mydataDF$string) & !is.na(mydataDF$team))
  ## Get the next several rows of data, too:
  s1<-mydataDF[sort(c(stranglerhits,stranglerhits+1,stranglerhits+2,stranglerhits+3,stranglerhits+4,stranglerhits+5)),]
  s1targ <- s1[which(grepl(" TARGET ENT",s1$string)),]
  attackerPre<-substr(substr(s1targ$string,regexpr("ENT",s1targ$string),regexpr("ENT",s1targ$string)+6),5,7)
  for (i in 1:length(attackerPre)){
    if (substr(attackerPre[i],2,2)==" "){
      attackerPre[i] <- substr(attackerPre[i],1,2) ## hotfix
    }
  }
  s1targ$attacker <- as.numeric(attackerPre)
  s1targ$target <- NA
  
  entangRows<-c()
  for (i in 1:nrow(s1targ)){
    thisAttacker <- s1targ[i,"attacker"]
    thisTime <- s1targ[i,"timesec"]
    atkEnt <- mydataDF[which(mydataDF$entity==thisAttacker & grepl(textset[which(powerset=="Entangle")],mydataDF$string)),]
    if (nrow(atkEnt)>0){
      atkEntTimeDiff <- atkEnt$timesec - thisTime
      if (length(which(atkEntTimeDiff > -0.5 & atkEntTimeDiff <= 1.3))>0){
        entangRows<-c(entangRows,i)
      }
    }
  }
  
  if (length(entangRows)>0){
    s1targ <- s1targ[-entangRows,]
  }
  
  for (i in 1:nrow(s1targ)){
    enttemp<-s1targ[i,"entity"]
    s1targ[i,"entity"] <- s1targ[i,"attacker"]
    s1targ[i,"string"] <- paste0("0  ",s1targ[i,"attacker"],"  FX OneShot FAKE/MYSTRANGLER.FX 0")
    s1targ[i,"name"] <- entities[which(entities$num==s1targ[i,"attacker"]),"name"]
    s1targ[i,"team"] <- ifelse(s1targ[i,"team"]==1,0,1)
    s1targ[i,"target"]<-enttemp
  }
  s1targ$attacker<-NULL
  
  mydataDF$target<-NA
  
  mydataDF <- rbind(mydataDF,s1targ) ## add the fake Strangler attack rows to the dataset
  
  
  ## Now the task of figuring out who the target was for each power activation...
  ## Use a method similar to Strangler. All of the relevant skills (that need a target)
  ## should hopefully be OneShots, rather than Maintained. Only the OneShots have subsequent target data.
  
  
  
  fxrows <- which(grepl("FX OneShot", mydataDF$string) &
                    !grepl("FAKE/", mydataDF$string) &
                    ## the line above excludes SSJ (cannot find target) and Strangler (already has target)
                    !grepl("NEW ",mydataDF$string) &
                    !is.na(mydataDF$team))
  
  s2<-mydataDF[unique(sort(c(fxrows,fxrows+1,fxrows+2,fxrows+3,fxrows+4,fxrows+5))),]
  oneshot<-which(grepl("FX OneShot",s2$string))
  
  ## This isn't written very efficiently, but it'll suffice given that demos are typically not that large
  deciles <- round(quantile(1:length(oneshot), seq(from=0,to=1,by=.1)))
  for (i in 1:length(oneshot)){
    if (i %in% deciles){
      message(paste0("FX parse portion is ",names(deciles[which(deciles==i)])," complete"))
    }
    theseRows<-s2[c(oneshot[i],oneshot[i]+1,oneshot[i]+2,oneshot[i]+3,oneshot[i]+4,oneshot[i]+5),]
    thisOneShot<-which(grepl("FX OneShot",theseRows$string))[-1]
    if (length(thisOneShot)>0){
      theseRows<-theseRows[1:(thisOneShot[1]-1),]
    }
    s2targ <- theseRows[which(grepl(" TARGET ENT",theseRows$string)),]
    thisTarg<-NA
    if (nrow(s2targ)>0){
      attackerPre<-substr(substr(s2targ$string,regexpr("ENT",s2targ$string),regexpr("ENT",s2targ$string)+6),5,7)
      if (length(attackerPre)>1){stop("This shouldn't happen... it means more than one ' TARGET ENT' for a given FX OneShot")}
      if (substr(attackerPre,2,2)==" "){
        attackerPre <- substr(attackerPre,1,2) ## hotfix
      }
      thisTarg <- as.numeric(attackerPre)
    }
    thisString<-theseRows[1,"string"]
    thisTime<-theseRows[1,"timesec"]
    mydataDF[which(mydataDF$string==thisString & mydataDF$timesec==thisTime),"target"] <- thisTarg
  }
  
  mydataDF$targName <- NA
  for (i in 1:nrow(entities)){
    mydataDF[which(mydataDF$target==entities[i,"num"]),"targName"] <- entities[i,"name"]
  }
  
  greenUsage <- mydataDF[which(grepl(textset[which(powerset=="Green Insp")],mydataDF$string)),
                               c("entity","string","time","timesec","team","name")]
  
  ## Grab .FX data which forms the basis for most of the rest of the analysis
  fxteam0 <- mydataDF[which((grepl("FX OneShot", mydataDF$string) |
                               grepl("FX Maintained", mydataDF$string)) &
                              mydataDF$team==0),]
  
  fxteam1 <- mydataDF[which((grepl("FX OneShot", mydataDF$string) |
                               grepl("FX Maintained", mydataDF$string)) &
                              mydataDF$team==1),]
  
  ## being affected by confuse has a .fx instead of .FX for some reason
  # GenericContinuing/ConfuseContinuing.fx 0"
  fxteam0$string <- gsub(".fx",".FX",fxteam0$string)
  fxteam1$string <- gsub(".fx",".FX",fxteam1$string)
  
  fxteam0$power <- regmatches(fxteam0$string,
                              regexpr("/\\w+\\.FX",fxteam0$string))
  
  fxteam1$power <- regmatches(fxteam1$string,
                              regexpr("/\\w+\\.FX",fxteam1$string))
  
  ## Toxins can only actually be differentiated from Build Up via the prefix
  fxteam0[which(grepl(textset[which(powerset=="Toxins")],
                      fxteam0$string)),"power"] <- textset[which(powerset=="Toxins")]
  fxteam1[which(grepl(textset[which(powerset=="Toxins")],
                      fxteam1$string)),"power"] <- textset[which(powerset=="Toxins")]
  
  
  fxteam0$power2 <- NA
  fxteam0$attack <- 0
  fxteam0$heal <- 0
  fxteam0$evade <- 0
  
  fxteam1$power2 <- NA
  fxteam1$attack <- 0
  fxteam1$heal <- 0
  fxteam1$evade <- 0
  
  codePowers<-function(x,text,power,attacks,heals,evades){
    for (i in 1:length(text)){
      theseRows<-which(grepl(text[i],x$power))
      x[theseRows,"power2"] <- power[i]
      if (power[i] %in% attacks){
        x[theseRows,"attack"]<-1
      }
      if (power[i] %in% heals){
        x[theseRows,"heal"]<-1
      }
      if (power[i] %in% evades){
        x[theseRows,"evade"]<-1
      }
    }
    return(x[order(x$name,x$time),])
  }
  
  
  fxteam0 <- codePowers(fxteam0, textset, powerset, attackset, healset, evadeset)
  fxteam1 <- codePowers(fxteam1, textset, powerset, attackset, healset, evadeset)
  
  ## Heat Exhaustion shares an .FX with one of the therm shields, but the therm shields aren't really worth tracking,
  ## so simply set Heat Exhaustion detections to NA if they weren't used on an enemy player
  fxteam0[which(fxteam0$power2=="Heat Exhaustion" & !fxteam0$target %in% entities[which(entities$team==1),"num"]),"power2"] <- NA
  fxteam1[which(fxteam1$power2=="Heat Exhaustion" & !fxteam1$target %in% entities[which(entities$team==0),"num"]),"power2"] <- NA
  
  fxteam0good <- fxteam0[which(!is.na(fxteam0$power2)),c("name","time",
                                                         "timesec","team","power2","attack","heal","evade","target","targName")]
  fxteam1good <- fxteam1[which(!is.na(fxteam1$power2)),c("name","time",
                                                         "timesec","team","power2","attack","heal","evade","target","targName")]
  
  ## Blind actually comes in pairs a lot of the time, which is really annoying.
  ## That is, when X uses Blind on Y, X will cast ILLUSBLIND first, and then
  ##   shortly thereafter, Y will cast ILLUSBLIND. To figure out who
  ##   actually used Blind, "Blind pairs" need to be created whenever the FX
  ##   show up. The "hit" is ALWAYS less than a second apart.
  
  fxblind <- rbind(fxteam0good[which(fxteam0good$power2=="Blind"),],
                   fxteam1good[which(fxteam1good$power2=="Blind"),])
  if (nrow(fxblind)>0){
    fxblind <- fxblind[order(fxblind$timesec),]
    fxblind$removeThis <- 0
    for (i in 1:nrow(fxblind)){
      thisTeam <- fxblind$team[i]
      thisTime <- fxblind$timesec[i]
      fxblind[which(fxblind$team != thisTeam & fxblind$timesec > thisTime &
                      fxblind$timesec < (thisTime+1)),"removeThis"] <- 1
    }
    
    blindRemovals <- fxblind[which(fxblind$removeThis==1),c("name","power2","timesec","removeThis")]
    
    fxteam0 <- merge(x=fxteam0, y=blindRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam0good <- merge(x=fxteam0good, y=blindRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam1 <- merge(x=fxteam1, y=blindRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam1good <- merge(x=fxteam1good, y=blindRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    
    fxteam0 <- fxteam0[which(is.na(fxteam0$removeThis)),]; fxteam0$removeThis <- NULL
    fxteam0good <- fxteam0good[which(is.na(fxteam0good$removeThis)),]; fxteam0good$removeThis <- NULL
    fxteam1 <- fxteam1[which(is.na(fxteam1$removeThis)),]; fxteam1$removeThis <- NULL
    fxteam1good <- fxteam1good[which(is.na(fxteam1good$removeThis)),]; fxteam1good$removeThis <- NULL
  }
  
  ## Phase Shift sometimes appears multiple times in quick succession. This is probably
  ##   because the user tried it, instantly got mezzed, and then kept trying it. So as not
  ##   to count it multiple times, only count the LAST instance of Phase used when
  ##   it is used multiple times < 10 sec apart each.
  
  fxphase <- rbind(fxteam0good[which(fxteam0good$power2=="Phase Shift"),],
                   fxteam1good[which(fxteam1good$power2=="Phase Shift"),])
  
  if (nrow(fxphase)>0){
    fxphase <- fxphase[order(fxphase$name,fxphase$timesec),]
    fxphase$removeThis <- 0
    for (i in unique(fxphase$name)){
      thisDat <- fxphase[which(fxphase$name==i),]
      if (nrow(thisDat)>1){
        for (j in 1:(nrow(thisDat)-1)){
          if (thisDat[(j+1),"timesec"] - thisDat[j,"timesec"] < 10){
            thisDat[j,"removeThis"]<-1
          }
        }
      }
      if (sum(thisDat$removeThis)>0){
        theseTimes <- thisDat[which(thisDat$removeThis==1),"timesec"]
        fxphase[which(fxphase$name==i & fxphase$timesec %in% theseTimes),"removeThis"]<-1
      }
    }
    
    phaseRemovals <- fxphase[which(fxphase$removeThis==1),c("name","power2","timesec","removeThis")]
    
    fxteam0 <- merge(x=fxteam0, y=phaseRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam0good <- merge(x=fxteam0good, y=phaseRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam1 <- merge(x=fxteam1, y=phaseRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    fxteam1good <- merge(x=fxteam1good, y=phaseRemovals, by=c("name","power2","timesec"), all.x=TRUE)
    
    fxteam0 <- fxteam0[which(is.na(fxteam0$removeThis)),]; fxteam0$removeThis <- NULL
    fxteam0good <- fxteam0good[which(is.na(fxteam0good$removeThis)),]; fxteam0good$removeThis <- NULL
    fxteam1 <- fxteam1[which(is.na(fxteam1$removeThis)),]; fxteam1$removeThis <- NULL
    fxteam1good <- fxteam1good[which(is.na(fxteam1good$removeThis)),]; fxteam1good$removeThis <- NULL
  }
  
  
  ## could wrap this part in a function, but whatever
  fxteam0atk <- fxteam0[which(fxteam0$attack==1),c("name","time",
                                                   "timesec","team","power2","attack","target","targName")]
  fxteam1atk <- fxteam1[which(fxteam1$attack==1),c("name","time",
                                                   "timesec","team","power2","attack","target","targName")]
  
  ## Try to guess who the primary supports are...
  temptab0 <- data.frame(sort(table(fxteam0atk$name),decreasing = TRUE))
  temptab1 <- data.frame(sort(table(fxteam1atk$name),decreasing = TRUE))
  for (i in entities[which(entities$team==0),"name"]){
    if (!i %in% temptab0$Var1){
      temptab0 <- rbind(temptab0, data.frame(Var1=i,Freq=0))
    }
  }
  for (i in entities[which(entities$team==1),"name"]){
    if (!i %in% temptab1$Var1){
      temptab1 <- rbind(temptab1, data.frame(Var1=i,Freq=0))
    }
  }
  supp0_1 <- NULL
  supp0_2 <- NULL
  supp0_3 <- NULL
  supp1_1 <- NULL
  supp1_2 <- NULL
  supp1_3 <- NULL
  
  if (suppsTeam0>0){ supp0_1 <- as.character(temptab0[(expectedPlayers/2)-0,"Var1"]) }
  if (suppsTeam0>1){ supp0_2 <- as.character(temptab0[(expectedPlayers/2)-1,"Var1"]) }
  if (suppsTeam0>2){ supp0_3 <- as.character(temptab0[(expectedPlayers/2)-2,"Var1"]) }
  if (suppsTeam1>0){ supp1_1 <- as.character(temptab1[(expectedPlayers/2)-0,"Var1"]) }
  if (suppsTeam1>1){ supp1_2 <- as.character(temptab1[(expectedPlayers/2)-1,"Var1"]) }
  if (suppsTeam1>2){ supp1_3 <- as.character(temptab1[(expectedPlayers/2)-2,"Var1"]) }
  
  fxteam0atk$targEmp <- ifelse(fxteam0atk$targName %in% c(supp1_1,supp1_2,supp1_3),1,0)
  fxteam1atk$targEmp <- ifelse(fxteam1atk$targName %in% c(supp0_1,supp0_2,supp0_3),1,0)
  
  fxteam0ev <- fxteam0[which(fxteam0$evade==1),c("name","time",
                                                 "timesec","team","power2","evade","target","targName")]
  fxteam1ev <- fxteam1[which(fxteam1$evade==1),c("name","time",
                                                 "timesec","team","power2","evade","target","targName")]
  
  fxteam0heal <- fxteam0[which(fxteam0$heal==1),c("name","time",
                                                  "timesec","team","power2","heal","target","targName")]
  fxteam1heal <- fxteam1[which(fxteam1$heal==1),c("name","time",
                                                  "timesec","team","power2","heal","target","targName")]
  
  ent_0 <- entities[which(entities$team==0),]
  ent_1 <- entities[which(entities$team==1),]
  
  codeSpikes<-function(x, enemyEvade, enemyHeal, ent, ent1){
    x <- x[order(x$target,x$timesec),]
    x$newspike <- 0
    x$spike <- 1
    x[1,"newspike"] <- 1
    for (i in 2:nrow(x)){ ## there are more elegant ways to do this, but runtime should be low either way
      x[i,"spike"] <- x[(i-1),"spike"]
      if (!is.na(x[i,"targName"])){ ## will have to handle missed projectiles and things like SSJ later
        if (x[i,"targName"] != x[(i-1),"targName"] | x[i,"timesec"] - x[(i-1),"timesec"] > spikeWindow){
          x[i,"newspike"] <- 1
          x[i,"spike"] <- x[i,"spike"]+1
        }
      }
    }
    x[which(is.na(x$targName)),"newspike"]<-0
    x[which(is.na(x$targName)),"spike"]<-NA
    spikeDat <- cbind(aggregate(x$timesec,by=list(spike=x$spike),FUN=NROW),
                      aggregate(x$timesec,by=list(spike=x$spike),FUN=min)[,2],
                      aggregate(x$timesec,by=list(spike=x$spike),FUN=max)[,2],
                      aggregate(x$targEmp,by=list(spike=x$spike),FUN=max)[,2])
    names(spikeDat) <- c("spike","prelim_atks","starttime","endtime","targEmp")
    
    x <- merge(x, y=spikeDat[,c("spike","prelim_atks")],by="spike",all.x=TRUE)
    x <- x[order(x$target,x$timesec),]
    
    ## The goal here is to reassign NA target attacks (missed projectiles + SSJ) to the nearest spike if possible.
    
    ## One other set of attacks will be reassigned -- seemingly stray attacks (i.e. spikes with < min_attacks_per_spike)
    ## will be assigned "on spike" if they are hitting a support at the same time as another actual spike.
    ## This last bit is controlled by an option: hitSupportCredit.
    if (hitSupportCredit){
      naspikeRows<-which(is.na(x$targName) | (x$targEmp==1 & x$prelim_atks < min_attacks_per_spike))
    }else{
      naspikeRows<-which(is.na(x$targName))
    }
    naspikes<-x[naspikeRows,]
    spikeDat2 <- spikeDat[which(!spikeDat$spike %in% na.omit(naspikes$spike)),] ## temp version of spikeDat without these spikes
    x<-x[-naspikeRows,] ## temporarily remove these rows. They may be added back in if they can be assigned to a spike
    spikeDat2$startleeway <- spikeDat2$starttime - spikeWindow
    spikeDat2$endleeway <- spikeDat2$endtime + spikeWindow
    for (i in 1:nrow(naspikes)){
      thisTime<-naspikes[i,"timesec"]
      possibleSpikes<-spikeDat2[which(thisTime > spikeDat2$startleeway & thisTime < spikeDat2$endleeway),]
      if (nrow(possibleSpikes)>0){
        possibleSpikes <- possibleSpikes[which.max(possibleSpikes$prelim_atks),] ## generally assign to the possibility w/ most atks
        possibleSpikes$closestTime <- abs(possibleSpikes$starttime - thisTime)
        assignedSpike <- possibleSpikes[which.min(possibleSpikes$closestTime),"spike"] ## then to the one with closest time
        newRow <- naspikes[i,]
        newRow$spike <- assignedSpike
        newRow$newspike <- 0 # for now
        newRow$prelim_atks <- 999 ## so it doesn't get trimmed for being < min_attacks_per_spike
        thisTeam <- newRow$team
        if (is.na(newRow$target)){
          newRow$target <- names(sort(table(x[which(x$spike==assignedSpike),"target"]),decreasing = TRUE))[1]
          newRow$targName <- names(sort(table(x[which(x$spike==assignedSpike),"targName"]),decreasing = TRUE))[1]
          newRow$targEmp <- names(sort(table(x[which(x$spike==assignedSpike),"targEmp"]),decreasing = TRUE))[1]
          proceed<-TRUE
        }else{ ## meaning, this is an attack on a support
          if (max(possibleSpikes$prelim_atks==1)){
            proceed<-FALSE
            ## I found rare cases where there are two stray attacks at the same time, with one being on a support.
            ## That shouldn't be counted as a two-attack spike.
          }else{
            proceed<-TRUE
          }
        }
        if (newRow$target %in% entities[which(entities$team != thisTeam),"num"] & proceed){ ## make sure they're hitting an opposing player
          x <- rbind(x,newRow)
        }
      } ## else, nothing is done, because the attack appears to be a stray and should not be counted for spikes
    }
    ## At this point it's almost time to trim stray attacks, but there's one other case to cover:
    ## Number of attacks on spike < min_attacks_per_spike but the target took evasive action. That should still be
    ## counted as a spike, and as a successful evasion for the evader, even if they only took 1 attack.
    ## For this to count, the evasion has to occur extremely close to, or ideally shortly before, the attack in question.
    ## Getting held by a random Blind for 2 seconds and then needlessly Jaunting/Phasing afterward should not be counted
    ## as an evasion. This window is controlled by preEvadeWindow.
    
    ## Unfortunately this process will never be able to detect a FULLY successful pre-evasion
    ## i.e. the target was called but escaped/phased/etc. without taking a single attack. The only way to
    ## detect that would be to give credit for spamming evasion abilities during a period when the enemy team
    ## goes for something like 8+ seconds without a spike and without gathering for buffs, which probably has its
    ## own pitfalls and isn't worth the hassle.
    
    toBeRemoved <- which(x$prelim_atks < min_attacks_per_spike)
    
    if (length(toBeRemoved)>0){
      tbr <- x[toBeRemoved,]
      x <- x[-toBeRemoved,]
      for (i in 1:nrow(tbr)){
        if (!is.na(tbr[i,"targName"])){ ## the attack is just a lost cause if it's a stray w/ no target
          thisTime<-tbr[i,"timesec"]
          thisTarget<-tbr[i,"targName"]
          thisEvadeTime<-enemyEvade[which(enemyEvade$name==thisTarget),"timesec"]
          if (length(thisEvadeTime)>0){
            preEvade <- which(thisEvadeTime - thisTime > min(preEvadeWindow) & thisEvadeTime - thisTime < max(preEvadeWindow))
            if (length(preEvade)>0){
              x<-rbind(x,tbr[i,])
            }
          }
        }
      }
    }
    
    x <- x[order(x$timesec),]
    x$prelim_atks<-NULL
    x$oldspike<-x$spike
    x$tempspike<-as.numeric(as.factor(x$oldspike))
    recodeSpike<-as.data.frame(cbind(unique(x$tempspike),1:length(unique(x$tempspike))))
    for (i in 1:nrow(recodeSpike)){
      x[which(x$tempspike==recodeSpike[i,"V1"]),"spike"]<-recodeSpike[i,"V2"]
    }
    x$tempspike<-NULL
    x$oldspike<-NULL
    x$newspike<-NULL
    
    
    ## There's also a need to remove accidental follow-ups to spikes.
    ## If <= 3 attacks are fired at someone <= 2 sec from the firing of the last attack from the previous spike, then it's probably an accident.
    spiketemp<-cbind(aggregate(x$timesec,by=list(spike=x$spike),FUN=NROW),
                     aggregate(x$timesec,by=list(spike=x$spike),FUN=min)[,2],
                     aggregate(x$timesec,by=list(spike=x$spike),FUN=max)[,2])
    names(spiketemp)<-c("spike","atks","start","end")
    removeSpikes<-c()
    for (i in 2:nrow(spiketemp)){
      if (spiketemp[i,"atks"]<=3 & spiketemp[i,"start"] - spiketemp[(i-1),"end"] <= 2){
        removeSpikes<-c(removeSpikes,i)
      }
    }
    x <- x[which(!x$spike %in% removeSpikes),]
    
    x$oldspike<-x$spike
    x$tempspike<-as.numeric(as.factor(x$oldspike))
    recodeSpike<-as.data.frame(cbind(unique(x$tempspike),1:length(unique(x$tempspike))))
    for (i in 1:nrow(recodeSpike)){
      x[which(x$tempspike==recodeSpike[i,"V1"]),"spike"]<-recodeSpike[i,"V2"]
    }
    x$tempspike<-NULL
    x$oldspike<-NULL
    
    ## Finally, recall spikes sometimes occur, but they should be counted separately.
    ## Separate these by time rather than # of attacks. This is controlled by max_time_per_spike_sec
    spiketemp<-cbind(aggregate(x$timesec,by=list(spike=x$spike),FUN=NROW),
                     aggregate(x$timesec,by=list(spike=x$spike),FUN=min)[,2],
                     aggregate(x$timesec,by=list(spike=x$spike),FUN=max)[,2])
    names(spiketemp)<-c("spike","atks","start","end")
    spiketemp$duration <- spiketemp$end-spiketemp$start
    
    recalls<-spiketemp[which(spiketemp$duration > max_time_per_spike_sec),]
    if (nrow(recalls)>0){
      for (i in 1:nrow(recalls)){
        thisSpike<-recalls[i,"spike"]
        thisSpikeData<-x[which(x$spike==thisSpike),]
        ## Here I get lazy and just assume there won't be > 4 recalls in a row. Everything about this is super lazy
        starttime<-min(thisSpikeData$timesec)
        max1<-starttime+max_time_per_spike_sec
        max2<-starttime+2*max_time_per_spike_sec
        max3<-starttime+3*max_time_per_spike_sec
        spike1<-thisSpikeData[which(thisSpikeData$timesec<=max1),]
        spike2<-thisSpikeData[which(thisSpikeData$timesec<=max2 & thisSpikeData$timesec>max1),]
        spike3<-thisSpikeData[which(thisSpikeData$timesec<=max3 & thisSpikeData$timesec>max2),]
        spike4<-thisSpikeData[which(thisSpikeData$timesec>max3),]
        if (nrow(spike2)>0){ x[which(x$spike==thisSpike & x$timesec %in% spike2$timesec),"spike"] <- max(x$spike)+1 }
        if (nrow(spike3)>0){ x[which(x$spike==thisSpike & x$timesec %in% spike3$timesec),"spike"] <- max(x$spike)+1 }
        if (nrow(spike4)>0){ x[which(x$spike==thisSpike & x$timesec %in% spike4$timesec),"spike"] <- max(x$spike)+1 }
      }
      x$oldspike<-x$spike
      x$tempspike<-as.numeric(as.factor(x$oldspike))
      recodeSpike<-as.data.frame(cbind(unique(x$tempspike),1:length(unique(x$tempspike))))
      for (i in 1:nrow(recodeSpike)){
        x[which(x$tempspike==recodeSpike[i,"V1"]),"spike"]<-recodeSpike[i,"V2"]
      }
      x$tempspike<-NULL
      x$oldspike<-NULL
    }
    
    ## Number of targets
    enemyTargDatatemp<-as.data.frame(table(aggregate(x$targName,by=list(spike=x$spike),
                                                     FUN=function(x){names(sort(table(x),decreasing=TRUE))[1]})$x))
    names(enemyTargDatatemp)<-c("name","targeted")
    enemyTargData <- merge(x=ent1,y=enemyTargDatatemp, by="name", all.x=TRUE)
    enemyTargData[which(is.na(enemyTargData$targeted)),"targeted"]<-0
    enemyTargData$num<-NULL
    enemyTargData$team<-NULL
    enemyTargData <- enemyTargData[order(enemyTargData$targeted, decreasing = TRUE),]
    
    listOut<-list()
    listOutHeal<-list()
    datOutNames<-c("spike","target","approx_success","begin_time","end_time",
                   "players_on_spike","attack_count","spike_duration","avg_timing_distance","evaded","healed",
                   "greens_available","greens_used","greens")
    datOut<-as.data.frame(matrix(NA,nrow=max(x$spike),ncol=length(datOutNames)))
    names(datOut)<-datOutNames
    datOut[,"spike"]<-sort(unique(x$spike))
    
    for (i in 1:nrow(datOut)){
      thisSpike<-datOut[i,"spike"]
      thisSpikeDetail<-x[which(x$spike==thisSpike),]
      spikeDatNames<-c("name","target","approx_success","on_spike","attack_count","first_attack_timing","first_attack_rank",
                       "first_timing_distance","abs_timing_distance","chain_duration","attack_combo")
      spikeDat <- as.data.frame(matrix(NA,nrow=expectedPlayers/2,ncol=length(spikeDatNames)))
      names(spikeDat)<-spikeDatNames
      
      enemyHealNames<-c("name","target","on_heal","heal_count","first_heal_timing","heal_combo")
      healDat <- as.data.frame(matrix(NA,nrow=expectedPlayers/2,ncol=length(enemyHealNames)))
      names(healDat)<-enemyHealNames
      
      spikeDat$name <- unique(ent$name)
      spikeDat$target <- names(sort(table(thisSpikeDetail$targName),decreasing = TRUE))[1]
      
      healDat$name <- unique(ent1$name)
      healDat$target <- spikeDat$target
      
      thisSpikeDetail$mintime <- min(thisSpikeDetail$timesec)
      thisSpikeDetail$maxtime <- max(thisSpikeDetail$timesec)
      deathtemp <- deaths[which(deaths$name==spikeDat$target[1]),"timesec"]
      apprsuccess <- 0
      if (length(deathtemp)>0){
        apprsuccess <- ifelse(length(which(deathtemp >= thisSpikeDetail$mintime[1] &
                                             deathtemp <= (thisSpikeDetail$maxtime[1] + 4)))>0,1,0)
        ## Above: hard-coded to look for deaths between spike start and spike end + 4 sec.
        ## This doesn't really need to be a parameter IMO but could be converted to one.
      }
      spikeDat$approx_success <- apprsuccess
      spikeDat$on_spike <- as.numeric(spikeDat$name %in% thisSpikeDetail$name)
      for (j in 1:nrow(spikeDat)){
        if (spikeDat[j,"on_spike"]==1){
          spikeDat[j,"attack_count"] <- length(which(spikeDat[j,"name"]==thisSpikeDetail$name))
          spikeDat[j,"first_attack_timing"] <- min(thisSpikeDetail[which(thisSpikeDetail$name==spikeDat[j,"name"]),"timesec"]) -
            thisSpikeDetail$mintime[1]
          spikeDat[j,"attack_combo"] <- paste0(thisSpikeDetail[which(thisSpikeDetail$name==spikeDat[j,"name"]),"power2"],collapse="-")
          spikeDat[j,"chain_duration"] <- max(thisSpikeDetail[which(thisSpikeDetail$name==spikeDat[j,"name"]),"timesec"]) -
            min(thisSpikeDetail[which(thisSpikeDetail$name==spikeDat[j,"name"]),"timesec"])
        }
      }
      spikeDat[,"first_attack_rank"] <- rank(spikeDat$first_attack_timing,na.last="keep")
      for (j in 1:nrow(spikeDat)){
        thisPerson<-spikeDat[j,]
        others<-spikeDat[-j,]
        spikeDat[j,"first_timing_distance"] <- mean(others[,"first_attack_timing"],na.rm=TRUE) - thisPerson[,"first_attack_timing"]
        if (is.nan(spikeDat[j,"first_timing_distance"])){
          spikeDat[j,"first_timing_distance"] <- NA
        }
        spikeDat[j,"abs_timing_distance"] <- abs(spikeDat[j,"first_timing_distance"])
      }
      
      listOut[[i]] <- spikeDat
      datOut[i,"target"] <- spikeDat$target[1]
      datOut[i,"approx_success"] <- spikeDat$approx_success[1]
      datOut[i,"begin_time"] <- min(thisSpikeDetail$mintime)
      datOut[i,"end_time"] <- max(thisSpikeDetail$maxtime)
      datOut[i,"players_on_spike"] <- sum(spikeDat$on_spike)
      datOut[i,"attack_count"] <- sum(spikeDat$attack_count,na.rm=TRUE)
      datOut[i,"spike_duration"] <- max(thisSpikeDetail[,"timesec"]) - min(thisSpikeDetail[,"timesec"])
      datOut[i,"avg_timing_distance"] <- mean(abs(spikeDat$first_timing_distance),na.rm=TRUE)
      if (is.nan(datOut[i,"avg_timing_distance"])){ datOut[i,"avg_timing_distance"] <- NA }
      
      ## Capture whether there was an evasion or heal attempt
      datOut[i,"evaded"]<-0
      thisEv <- enemyEvade[which(enemyEvade$name==spikeDat$target[1]),]
      if (nrow(thisEv)>0){
        timelyEvade <- thisEv[which(thisEv$timesec >= (datOut[i,"begin_time"] +
                                                         ifelse(min(preEvadeWindow)<0,min(preEvadeWindow),0)) &
                                      thisEv$timesec <= (datOut[i,"end_time"] + 2)),"timesec"]
        if (length(timelyEvade)>0){
          datOut[i,"evaded"]<-1
        }
      }
      ## Above: hard-coded to only count evasions within 2 sec of the last attack
      
      datOut[i,"healed"]<-0
      thisHeal <- enemyHeal[which(enemyHeal$targName==spikeDat$target[1]),]
      if (nrow(thisHeal)>0){
        timelyHealRows<-which(thisHeal$timesec >= datOut[i,"begin_time"] &
                                thisHeal$timesec <= (datOut[i,"end_time"] + healWindow))
        ## Above: healWindow set to 2 sec by default
        timelyHeal <- thisHeal[timelyHealRows,"timesec"]
        timelyHealDat <- thisHeal[timelyHealRows,]
        if (length(timelyHeal)>0){
          datOut[i,"healed"]<-1
        }
      }else{
        timelyHealDat <- as.data.frame(matrix(NA,nrow=1,ncol=2))[-1,]
      }
      
      for (j in 1:nrow(healDat)){
        if (nrow(timelyHealDat)>0){
          healDat[j,"on_heal"] <- as.numeric(healDat$name[j] %in% timelyHealDat$name)
          if (healDat[j,"on_heal"]==1){
            healDat[j,"heal_count"] <- length(which(timelyHealDat$name==healDat[j,"name"]))
            healDat[j,"first_heal_timing"] <- min(timelyHealDat[which(timelyHealDat$name==healDat[j,"name"]),"timesec"]) -
              datOut[i,"begin_time"]
            healDat[j,"heal_combo"] <- paste0(timelyHealDat[which(timelyHealDat$name==healDat[j,"name"]),"power2"],collapse="-")
          }
        }else{
          healDat[j,"on_heal"] <- 0
        }
      }
      
      listOutHeal[[i]] <- healDat
      
      ## Look for green insp usage, hard-coded to count within 3 sec of the last attack
      ## Why 3 sec instead of 2, like healing? Arbitrary call; sometimes players top themselves off quickly after a spike
      datOut[i,"greens_available"] <- greenMax -
        nrow(greenUsage[which(greenUsage$name==datOut[i,"target"] & greenUsage$timesec < datOut[i,"begin_time"]),])
      datOut[i,"greens_used"] <- nrow(greenUsage[which(greenUsage$name==datOut[i,"target"] & greenUsage$timesec >= datOut[i,"begin_time"] &
                                                         greenUsage$timesec <= datOut[i,"end_time"] + 3),])
      ## Sometimes greens are picked up mid-match
      if (datOut[i,"greens_available"] < datOut[i,"greens_used"]) { datOut[i,"greens_available"] <- datOut[i,"greens_used"] }
      datOut[i,"greens"] <- ifelse(datOut[i,"greens_used"] > 0,1,0) ## binary indicator (for use in other functions later on)
    }
    return(list(spikeSummary = datOut,
                spikeDetails = listOut,
                healDetails = listOutHeal,
                enemyTargets = enemyTargData))
  }
  
  s0good <- codeSpikes(fxteam0atk, enemyEvade = fxteam1ev, enemyHeal = fxteam1heal, ent=ent_0, ent1=ent_1)
  s1good <- codeSpikes(fxteam1atk, enemyEvade = fxteam0ev, enemyHeal = fxteam0heal, ent=ent_1, ent1=ent_0)
  
  ## It's actually possible to get miss rates for every character who used an attack, but SSJ and Strangler have to be excluded
  ## from this. Why? SSJ has no target data and cannot be coded as hit or miss, while Strangler's weird definition (above)
  ## means that the only Stranglers I catch are the ones that hit. So these are hit/miss rates excluding SSJ and Strangler.
  
  ## THIS HAS BEEN COMMENTED OUT because it's actually only applicable to projectile attacks. Non-projectiles will still have a
  ## target even if they miss, so the denominator would not be fully accurate unless all non-projectile attacks are excluded.
  ## This code could be reused at some point but only for projectile attacks.
  
  # misstable0 <- as.data.frame(prop.table(table(fxteam0atk[which(!fxteam0atk$power2 %in% c("Spirit Shark Jaws", "Strangler")),"name"],
  #                                              is.na(fxteam0atk[which(!fxteam0atk$power2 %in% c("Spirit Shark Jaws","Strangler")),"target"])),1))
  # misstable1 <- as.data.frame(prop.table(table(fxteam0atk[which(!fxteam0atk$power2 %in% c("Spirit Shark Jaws", "Strangler")),"name"],
  #                                              is.na(fxteam0atk[which(!fxteam0atk$power2 %in% c("Spirit Shark Jaws","Strangler")),"target"])),1))
  # 
  # misstable0 <- misstable0[which(misstable1$Var2==TRUE),c("Var1","Freq")]
  # misstable1 <- misstable1[which(misstable1$Var2==TRUE),c("Var1","Freq")]
  # names(misstable0) <- c("name","approx_miss_rate")
  # names(misstable1) <- c("name","approx_miss_rate")
  
  
  summnames <- c("Name","ApproxTargeted","ApproxDeaths","TeamKills","TeamDeaths",
                 "TeamSpikes","TeamTargeted",keySkills)
  
  summ0 <- as.data.frame(matrix(NA,nrow=nrow(entities)/2,ncol=length(summnames)))
  names(summ0) <- summnames
  
  summ1 <- summ0
  
  generateSummary <- function(x, team, keySkillsList=keySkills){
    x$Name <- entities[which(entities$team==team),"name"]
    
    deathAgg <- aggregate(deaths$name,by=list(name=deaths$name,team=deaths$team),FUN=NROW)
    goodSkills <- rbind(fxteam0good,fxteam1good)
    gsAgg <- aggregate(goodSkills$name,by=list(name=goodSkills$name,skill=goodSkills$power2),FUN=NROW)
    for (i in 1:nrow(x)){
      
      tempd <- deathAgg[which(deathAgg$name==x[i,"Name"]),"x"]
      x[i,"ApproxDeaths"] <- ifelse(length(tempd)>0,tempd,0)
      x[i,"TeamKills"] <- sum(deathAgg[which(deathAgg$team != team),"x"])
      x[i,"TeamDeaths"] <- sum(deathAgg[which(deathAgg$team == team),"x"])
      
      ## TeamSpikes, TeamTargeted filled in later
      
      for (j in keySkillsList){
        tempd <- gsAgg[which(gsAgg$name==x[i,"Name"] & gsAgg$skill==j),"x"]
        x[i,j] <- ifelse(length(tempd)>0,tempd,0)
      }
    }
    return(x)
  }
  
  summ0 <- generateSummary(x=summ0, team=0)
  summ1 <- generateSummary(x=summ1, team=1)
  
  remove0<-c()
  for (i in keySkills){
    if (sum(summ0[,i])==0){
      remove0<-c(remove0,i)
    }
  }
  
  remove1<-c()
  for (i in keySkills){
    if (sum(summ1[,i])==0){
      remove1<-c(remove1,i)
    }
  }
  
  if (length(remove0)>0){summ0 <- summ0[,which(!names(summ0) %in% remove0)]}
  if (length(remove1)>0){summ1 <- summ1[,which(!names(summ1) %in% remove1)]}
  
  
  summ0$TeamSpikes <- max(s0good$spikeSummary$spike)
  summ1$TeamSpikes <- max(s1good$spikeSummary$spike)
  summ0$TeamTargeted <- max(s1good$spikeSummary$spike)
  summ1$TeamTargeted <- max(s0good$spikeSummary$spike)
  
  for (j in 1:nrow(summ0)){
    temp1<-s1good$enemyTargets
    summ0[j,"ApproxTargeted"] <- temp1[which(temp1$name==summ0[j,"Name"]),"targeted"]
  }
  for (j in 1:nrow(summ1)){
    temp1<-s0good$enemyTargets
    summ1[j,"ApproxTargeted"] <- temp1[which(temp1$name==summ1[j,"Name"]),"targeted"]
  }
  
  spikeComparison <-as.data.frame(matrix(NA,nrow=6,ncol=3))
  names(spikeComparison)<-c("Spike_Attribute","Ally","Enemy")
  spikeComparison[,1]<-c("Approx Kills","Total Spikes","Avg Players on Spike", "Avg Attacks Per Spike",
                         "Avg Spike Duration","Avg Timing Diff")
  spikeComparison[1,2:3]<-c(length(which(deaths$team==1)),length(which(deaths$team==0)))
  spikeComparison[2,2:3]<-c(max(s0good$spikeSummary$spike),max(s1good$spikeSummary$spike))
  spikeComparison[3,2:3]<-c(mean(s0good$spikeSummary$players_on_spike),mean(s1good$spikeSummary$players_on_spike))
  spikeComparison[4,2:3]<-c(mean(s0good$spikeSummary$attack_count),mean(s1good$spikeSummary$attack_count))
  spikeComparison[5,2:3]<-c(mean(s0good$spikeSummary$spike_duration),mean(s1good$spikeSummary$spike_duration))
  spikeComparison[6,2:3]<-c(mean(s0good$spikeSummary$avg_timing_distance,na.rm=TRUE),
                            mean(s1good$spikeSummary$avg_timing_distance,na.rm=TRUE))

  spikeComparison <- list(map = map, runtime = runtime, spikeComparison = spikeComparison)
  rawDF <- NULL
  if (returnRawDF){
    rawDF <- rbind(fxteam0,fxteam1)
    if (warnTrigger){
      rawDF$name <- gsub("\"","",rawDF$name)
      rawDF$targName <- gsub("\"","",rawDF$targName)
    }
  }
  
  if (warnTrigger){
    ## If quote detection had to be turned off, the main thing that is affected should be player names, so gsub
    ## any double quotes out of the player names before returning any data.
    summ0$Name <- gsub("\"","",summ0$Name)
    summ1$Name <- gsub("\"","",summ1$Name)
    s0good$spikeSummary$target <- gsub("\"","",s0good$spikeSummary$target)
    s1good$spikeSummary$target <- gsub("\"","",s1good$spikeSummary$target)
    for (i in 1:length(s0good$spikeDetails)){
      s0good$spikeDetails[[i]]$name <- gsub("\"","",s0good$spikeDetails[[i]]$name)
      s0good$spikeDetails[[i]]$target <- gsub("\"","",s0good$spikeDetails[[i]]$target)
    }
    for (i in 1:length(s1good$spikeDetails)){
      s1good$spikeDetails[[i]]$name <- gsub("\"","",s1good$spikeDetails[[i]]$name)
      s1good$spikeDetails[[i]]$target <- gsub("\"","",s1good$spikeDetails[[i]]$target)
    }
    for (i in 1:length(s0good$healDetails)){
      s0good$healDetails[[i]]$name <- gsub("\"","",s0good$healDetails[[i]]$name)
      s0good$healDetails[[i]]$target <- gsub("\"","",s0good$healDetails[[i]]$target)
    }
    for (i in 1:length(s1good$healDetails)){
      s1good$healDetails[[i]]$name <- gsub("\"","",s1good$healDetails[[i]]$name)
      s1good$healDetails[[i]]$target <- gsub("\"","",s1good$healDetails[[i]]$target)
    }
    ent_0$name <- gsub("\"","",ent_0$name)
    ent_1$name <- gsub("\"","",ent_1$name)
    greenUsage$name <- gsub("\"","",greenUsage$name)
    deaths$name <- gsub("\"","",deaths$name)
    if (class(emoteUsage) != "character"){
      emoteUsage$name <- gsub("\"","",emoteUsage$name)
    }
  }
  
  if (class(kbs) != "character"){
    knockbacks <- kbs[order(kbs$team,kbs$timesec),c("entity","name","team","time","timesec")]
  }else{
    knockbacks <- kbs
  }
  
  return(list(powerSummary0 = summ0,
              powerSummary1 = summ1,
              spikeSummary0 = s0good$spikeSummary,
              spikeSummary1 = s1good$spikeSummary,
              spikeDetails0 = s0good$spikeDetails,
              spikeDetails1 = s1good$spikeDetails,
              healDetails0 = s1good$healDetails, ## because it's opposite team healing
              healDetails1 = s0good$healDetails,
              spikeComparison = spikeComparison,
              entities0 = ent_0,
              entities1 = ent_1,
              greenUsage = greenUsage[order(greenUsage$team,greenUsage$timesec),c("entity","name","team","time","timesec")],
              deathTable = deaths[order(deaths$team,deaths$timesec),c("entity","name","team","time","timesec")],
              rawDF = rawDF,
              knockbacks = knockbacks,
              emoteUsage = emoteUsage))
}



## floatdmg code below. No longer used

# dmgteam0 <- mydataDF[which(grepl("floatdmg", mydataDF$string) &
#                              mydataDF$team==0 & !grepl("Preventive", mydataDF$string)),]
# dmgteam1 <- mydataDF[which(grepl("floatdmg", mydataDF$string) &
#                              mydataDF$team==1 & !grepl("Preventive", mydataDF$string)),]
# 
# head(strsplit(dmgteam0$string,"\\d+"))
# 
# matches0 <- regmatches(dmgteam0$string,
#                        gregexpr("-?[[:digit:]]+", dmgteam0$string))
# 
# matches1 <- regmatches(dmgteam0$string,
#                        gregexpr("-?[[:digit:]]+", dmgteam0$string))
# 
# for (i in 1:nrow(dmgteam0)){
#   dmgteam0[i,"target"] <- as.numeric(matches0[[i]][3])
#   dmgteam0[i,"dmg"] <- as.numeric(matches0[[i]][4])
#   if (as.numeric(matches0[[i]][3]) %in% entities$num){
#     dmgteam0[i,"target_name"] <- entities[which(entities$num==
#                                                   as.numeric(matches0[[i]][3])),"name"]}
# }
# 
# for (i in 1:nrow(dmgteam1)){
#   dmgteam1[i,"target"] <- as.numeric(matches1[[i]][3])
#   dmgteam1[i,"dmg"] <- as.numeric(matches1[[i]][4])
#   if (as.numeric(matches1[[i]][3]) %in% entities$num){
#     dmgteam1[i,"target_name"] <- entities[which(entities$num==
#                                                   as.numeric(matches1[[i]][3])),"name"]}
# }
# 
# dmgteam0_att <- dmgteam0[which(dmgteam0$dmg>0),]
# dmgteam1_att <- dmgteam1[which(dmgteam1$dmg>0),]

