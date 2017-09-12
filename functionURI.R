getByIDS <- function(list ){
  # setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4")
  library(jsonlite)
  library(curl)
  link <- "https://api.opendota.com/api/matches/"
  temp <- list()
  temp2 <- list()
  retorno2 <- list()
  data2 <- list()
  kills <- list()
  killsWrite <- list()
  for (i in 1:length(list)){
    
    tryCatch({
      data2 <- fromJSON(paste(link,list[i],sep=""), flatten=TRUE)
      Sys.sleep(2)
      temp2 = getVariables(data2)
      retorno2 <- rbind(retorno2,temp2)
      kills <- getKills(data2)
      killsWrite <- rbind(killsWrite,kills)
    },error = function(cond) {
      return (NA)
    }, warning = function(cond) {
      return (NULL)
    })
  }
  write.csv(retorno2, file="teste2.csv")
  write.csv(killsWrite, file="kills2.csv")
  
}

getVariables <- function(data2) {
  
  temp = list()
  id = NA
  for(i in 1:length(data2$players$hero_id)) { if(data2$players$hero_id[i]==4) id = i }
  if(!is.na(id)) {
    temp <- list()
    temp$match_id                                     <- data2$match_id
    temp$game_mode                                    <- data2$game_mode
    temp$dire_score                                   <- data2$dire_score
    temp$human_players                                <- data2$human_players
    temp$lobby_type                                   <- data2$lobby_type
    temp$engine                                       <- data2$engine
    temp$radiant_win                                  <- data2$radiant_win
    temp$radiant_score                                <- data2$radiant_score
    # temp$purchase_log                                 <- data2$purchase_log
    # temp$lane_pos                                   <- data2$lane_pos
    temp$player.player_slot                           <- data2$players$player_slot[id]
    # temp$player.ability_upgrades_arr                <- data2$players$ability_upgrades_arr[id]
    # temp$player.ability_uses                        <- data2$players$ability_uses[id]
    # temp$player.actions                             <- data2$players$actions[id]
    temp$player.assists                               <- data2$players$assists[id]
    # temp$player.backpack_0                            <- data2$players$backpack_0[id]
    # temp$player.backpack_1                            <- data2$players$backpack_1[id]
    # temp$player.backpack_2                            <- data2$players$backpack_2[id]
    # temp$player.damage                              <- data2$players$damage[id]
    # temp$player.damage_inflictor                    <- data2$players$damage_inflictor[id]
    # temp$player.damage_inflictor_received           <- data2$players$damage_inflictor_received[id]
    # temp$player.damage_taken                        <- data2$players$damage_taken[id]
    temp$player.deaths                                <- data2$players$deaths[id]
    temp$player.denies                                <- data2$players$denies[id]
    temp$player.hero_damage                           <- data2$players$hero_damage[id]
    temp$player.hero_healing                          <- data2$players$hero_healing[id]
    # temp$player.hero_hits                           <- data2$players$hero_hits[id]
    temp$player.hero_id                               <- data2$players$hero_id[id]
    temp$player.item_0                                <- data2$players$item_0[id]
    temp$player.item_1                                <- data2$players$item_1[id]
    temp$player.item_2                                <- data2$players$item_2[id]
    temp$player.item_3                                <- data2$players$item_3[id]
    temp$player.item_4                                <- data2$players$item_4[id]
    temp$player.item_5                                <- data2$players$item_5[id]
    # temp$player.item_uses                           <- data2$players$item_uses[id]
    # temp$player.kill_streaks                        <- data2$players$kill_streaks[id]
    # temp$player.killed                              <- data2$players$killed[id]
    # temp$player.killed_by                           <- data2$players$killed_by[id]
    temp$player.kills                                 <- data2$players$kills[id]
    # temp$player.kills_log                           <- data2$players$kills_log[id]
    temp$player.leaver_status                         <- data2$players$leaver_status[id]
    temp$player.level                                 <- data2$players$level[id]
    # temp$player.purchase                            <- data2$players$purchase[id]
    # temp$player.purchase_log                        <- data2$players$purchase_log[id]
    # temp$player.rune_pickups                        <- data2$players$rune_pickups[id]
    # temp$player.runes                               <- data2$players$runes[id]
    # temp$player.runes_log                           <- data2$players$runes_log[id]
    temp$player.tower_damage                          <- data2$players$tower_damage[id]
    # temp$player.towers_killed                       <- data2$players$towers_killed[id]
    temp$player.isRadiant                             <- data2$players$isRadiant[id]
    temp$player.win                                   <- data2$players$win[id]
    temp$player.lose                                  <- data2$players$lose[id]
    # temp$player.benchmarks.kills_per_min              <- data2$players$benchmarks.kills_per_min[id]
    temp$player.kda                                   <- data2$players$kda[id]
    temp$player.abandons                              <- data2$players$abandons[id]
    # temp$player.neutral_kills                         <- data2$players$neutral_kills[id]
    # temp$player.tower_kills                           <- data2$players$tower_kills[id]
    # temp$player.courier_kills                         <- data2$players$courier_kills[id]
    # temp$player.lane_kills                            <- data2$players$lane_kills[id]
    # temp$player.hero_kills                            <- data2$players$hero_kills[id]
    # temp$player.observer_kills                        <- data2$players$observer_kills[id]
    # temp$player.sentry_kills                          <- data2$players$sentry_kills[id]
    # temp$player.roshan_kills                          <- data2$players$roshan_kills[id]
    # temp$player.necronomicon_kills                    <- data2$players$necronomicon_kills[id]
    # temp$player.ancient_kills                         <- data2$players$ancient_kills[id]
    # temp$player.lane_efficiency                       <- data2$players$lane_efficiency[id]
    # # temp$player.lane                                <- data2$players$lane[id]da
    # temp$player.lane_role                             <- data2$players$lane_role[id]
    temp$player.benchmarks.gold_per_min.raw 			    <-data2$players$benchmarks.gold_per_min.raw[id]
    temp$player.benchmarks.gold_per_min.pct 			    <-data2$players$benchmarks.gold_per_min.pct[id]
    temp$player.benchmarks.hero_damage_per_min.raw 	  <-data2$players$benchmarks.gold_per_min.raw[id]
    temp$player.benchmarks.hero_damage_per_min.pct 	  <-data2$players$benchmarks.hero_damage_per_min.pct[id]
    temp$player.benchmarks.hero_healing_per_min.raw   <-data2$players$benchmarks.hero_healing_per_min.raw[id]
    temp$player.benchmarks.hero_healing_per_min.pct	  <-data2$players$benchmarks.hero_healing_per_min.pct[id]
    temp$player.benchmarks.tower_damage.raw			      <-data2$players$benchmarks.tower_damage.raw[id]
    temp$player.benchmarks.tower_damage.pct			      <-data2$players$benchmarks.tower_damage.pct[id]
    temp$player.benchmarks.xp_per_min.raw 				    <-data2$players$benchmarks.xp_per_min.raw [id]
    temp$player.benchmarks.xp_per_min.pct  			      <-data2$players$benchmarks.xp_per_min.pct  [id]
    temp$player.benchmarks.kills_per_min.raw 			    <-data2$players$benchmarks.kills_per_min.raw [id]
    temp$player.benchmarks.kills_per_min.pct			    <-data2$players$benchmarks.kills_per_min.pct[id]
    temp$player.benchmarks.hits_per_min.raw  			    <-data2$players$benchmarks.hits_per_min.raw [id] 
    temp$player.benchmarks.hits_per_min.pct 			    <-data2$players$benchmarks.hits_per_min.pct [id]
  }
  return(temp)
}

getKills <- function (data2) {
  kills = list()
  id = NA
  for(i in 1:length(data2$players$hero_id)) { if(data2$players$hero_id[i]==4) id = i }
  if(!is.na(id)) {
    kills$match_id                                      <- data2$match_id
    kills$player.neutral_kills                         <- data2$players$neutral_kills[id]
    kills$player.tower_kills                           <- data2$players$tower_kills[id]
    kills$player.courier_kills                         <- data2$players$courier_kills[id]
    kills$player.lane_kills                            <- data2$players$lane_kills[id]
    kills$player.hero_kills                            <- data2$players$hero_kills[id]
    kills$player.observer_kills                        <- data2$players$observer_kills[id]
    kills$player.sentry_kills                          <- data2$players$sentry_kills[id]
    kills$player.roshan_kills                          <- data2$players$roshan_kills[id]
    kills$player.necronomicon_kills                    <- data2$players$necronomicon_kills[id]
    kills$player.ancient_kills                         <- data2$players$ancient_kills[id]
    kills$player.lane_efficiency                       <- data2$players$lane_efficiency[id]
    # kills$player.lane                                <- data2$players$lane[id]da
    kills$player.lane_role                             <- data2$players$lane_role[id]
  }
  return(kills)
}

loadLista <- function() {
  return = c(3393050530,
             3393050542,
             3393050720,
             3393051213,
             3393051496,
             3393051586,
             3393051724,
             3393051862,
             3393052037,
             3393052305,
             3393052306,
             3393052480,
             3393052727,
             3393052797,
             3393053009,
             3393053149,
             3393053213,
             3393053403,
             3393054042,
             3393054155,
             3393054219,
             3393054254,
             3393054833,
             3393055240,
             3393055278,
             3393055320,
             3393055381,
             3393055465,
             3393055855,
             3393055867,
             3393056427,
             3393056622,
             3393057251,
             3393057556,
             3393057601,
             3393058448,
             3393058645,
             3393059049,
             3393059142,
             3393059205,
             3393059477,
             3393059503,
             3393059692,
             3393059877,
             3393059922,
             3393060091,
             3393060294,
             3393060633,
             3393060744,
             3393060872,
             3393060979,
             3393061579,
             3393061850,
             3393062236,
             3393062594,
             3393063107,
             3393063124,
             3393063279,
             3393063487,
             3393063518,
             3393063525,
             3393063556,
             3393063556,
             3393063561,
             3393063674,
             3393063781,
             3393063812,
             3393063893,
             3393063983,
             3393064078,
             3393064134,
             3393064291,
             3393064336,
             3393064391,
             3393064483,
             3393064642,
             3393064985,
             3393065010,
             3393065069,
             3393065105,
             3393065199,
             3393065228,
             3393065276,
             3393065295,
             3393065400,
             3393065722,
             3393066153,
             3393066197,
             3393066278,
             3393066420,
             3393066551,
             3393066551,
             3393066683,
             3393066722,
             3393066739,
             3393066848,
             3393066861,
             3393067045,
             3393067085,
             3393067175,
             3393067356,
             3393067360,
             3393067492,
             3393067696,
             3393067842,
             3393067855,
             3393067880,
             3393067965,
             3393068103,
             3393068225,
             3393068299,
             3393068299,
             3393068383,
             3393068473,
             3393068535,
             3393068748,
             3393068805,
             3393068856,
             3393068860,
             3393068906,
             3393068917,
             3393069089,
             3393069159,
             3393069342,
             3393069424,
             3393069476,
             3393069536,
             3393069620,
             3393069690,
             3393069724,
             3393069869,
             3393069869,
             3393069872,
             3393069898,
             3393069898,
             3393069901,
             3393070127,
             3393070228,
             3393070248,
             3393070298,
             3393070334,
             3393070368,
             3393070428,
             3393070428,
             3393070564,
             3393070688,
             3393070688,
             3393070752,
             3393070763,
             3393070763,
             3393070800,
             3393070814,
             3393070878,
             3393070989,
             3393070993,
             3393071079,
             3393071123,
             3393071134,
             3393071279,
             3393071283,
             3393071324,
             3393071333,
             3393071438,
             3393071562,
             3393071651,
             3393071651,
             3393071689,
             3393071745,
             3393071765,
             3393071781,
             3393071781,
             3393071782,
             3393071812,
             3393071894,
             3393071971,
             3393071971,
             3393072046,
             3393072218,
             3393072248,
             3393072471,
             3393072471,
             3393072482,
             3393072546,
             3393072640,
             3393072681,
             3393072687,
             3393072739,
             3393072739,
             3393072786,
             3393072878,
             3393072895,
             3393072895,
             3393072918,
             3393072918,
             3393073129,
             3393073129,
             3393073342,
             3393073353,
             3393073389,
             3393073485,
             3393073515,
             3393073604,
             3393073770,
             3393073804,
             3393073888,
             3393074201,
             3393074218,
             3393074219,
             3393074252,
             3393074252,
             3393074467,
             3393074605,
             3393074763,
             3393074793,
             3393074837,
             3393074857,
             3393074923,
             3393074933,
             3393075100,
             3393075102,
             3393075145,
             3393075145,
             3393075167,
             3393075312,
             3393075313,
             3393075320,
             3393075320,
             3393075395,
             3393075585,
             3393075585,
             3393075585,
             3393075642,
             3393075642,
             3393075741,
             3393075777,
             3393075963,
             3393075963,
             3393076214,
             3393076333,
             3393076428,
             3393076462,
             3393076773,
             3393076835,
             3393076981,
             3393077050,
             3393077307,
             3393077314,
             3393077385,
             3393077396,
             3393077414,
             3393077506,
             3393077608,
             3393077617,
             3393077617,
             3393077622,
             3393077624,
             3393077719,
             3393077769,
             3393077842,
             3393078099,
             3393078099,
             3393078158,
             3393078259,
             3393078259,
             3393078440,
             3393078717,
             3393078721,
             3393078919,
             3393079064,
             3393079064,
             3393079091,
             3393079091,
             3393079091,
             3393079147,
             3393079147,
             3393079532,
             3393079570,
             3393079816,
             3393079816,
             3393080213,
             3393080251,
             3393080672,
             3393080672,
             3393080808,
             3393080986,
             3393081093,
             3393081093,
             3393081093,
             3393081433,
             3393081618,
             3393081911,
             3393081999,
             3393082426,
             3393082426,
             3393082568,
             3393083424,
             3393083600,
             3393083721,
             3393083721,
             3393083721,
             3393083797,
             3393084062,
             3393084124,
             3393084143,
             3393084143,
             3393084219,
             3393084507,
             3393084676,
             3393084676,
             3393084676,
             3393084838,
             3393084838,
             3393085370,
             3393085485,
             3393085625,
             3393086301,
             3393086418,
             3393086418,
             3393086577,
             3393086782,
             3393086812,
             3393086989,
             3393087050,
             3393087061,
             3393087137,
             3393087760,
             3393087948,
             3393087987,
             3393087987,
             3393088079,
             3393088079,
             3393088416,
             3393088545,
             3393088863,
             3393088958,
             3393089252,
             3393089412,
             3393089753,
             3393089754,
             3393089882,
             3393090029,
             3393090627,
             3393091502,
             3393091572,
             3393092236,
             3393092515,
             3393092823,
             3393093159,
             3393093345,
             3393094041,
             3393094307,
             3393094646,
             3393094707,
             3393095090,
             3393095292,
             3393095435,
             3393095671,
             3393095840,
             3393095915,
             3393096028,
             3393096028,
             3393096186,
             3393096245,
             3393096249,
             3393096531,
             3393096531,
             3393097469,
             3393097472,
             3393098055,
             3393098527,
             3393099678,
             3393100973,
             3393101146,
             3393101420,
             3393101420,
             3393102072,
             3393102768,
             3393102768,
             3393104580,
             3393104967,
             3393105368,
             3393107612,
             3393108958,
             3393109440,
             3393110510,
             3393111752,
             3393112779,
             3393113806,
             3393113806,
             3393114623,
             3393115660,
             3393115660,
             3393117207,
             3393117254,
             3393117254,
             3393119257,
             3393119992,
             3393124296,
             3393124540,
             3393125527,
             3393245679,
             3393245969,
             3393246169,
             3393246284,
             3393246448,
             3393246595,
             3393246718,
             3393246794,
             3393246997,
             3393247126,
             3393247861,
             3393247993,
             3393248000,
             3393248042,
             3393248150,
             3393248152,
             3393248425,
             3393248660,
             3393248793,
             3393248987,
             3393249056,
             3393249136,
             3393249191,
             3393249247,
             3393249599,
             3393250005,
             3393250042,
             3393250112,
             3393250555,
             3393251018,
             3393251028,
             3393251421,
             3393251645,
             3393251696,
             3393251831,
             3393252400,
             3393252679,
             3393252827,
             3393253136,
             3393253758,
             3393253795,
             3393253871,
             3393254363,
             3393254422,
             3393254871,
             3393255173,
             3393255237,
             3393255252,
             3393256058,
             3393256495,
             3393256869,
             3393256934,
             3393256980,
             3393257025,
             3393257159,
             3393257170,
             3393257203,
             3393257204,
             3393257281,
             3393257388,
             3393257508,
             3393257530,
             3393257617,
             3393257780,
             3393257812,
             3393258027,
             3393258086,
             3393258280,
             3393258381,
             3393258408,
             3393258513,
             3393258657,
             3393258667,
             3393258849,
             3393258932,
             3393258967,
             3393258967,
             3393259021,
             3393259292,
             3393259328,
             3393259374,
             3393259428,
             3393259428,
             3393259497,
             3393259506,
             3393259533,
             3393259575,
             3393259580,
             3393259643,
             3393259648,
             3393259733,
             3393259733,
             3393259802,
             3393259846,
             3393259860,
             3393260006,
             3393260072,
             3393260239,
             3393260291
  )
}

getByIDS(loadLista())
debug(getByIDS)
 list <- loadLista()
 