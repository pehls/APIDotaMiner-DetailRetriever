setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4");
lista <- c(3393069424,3393100973,3393099678);
debug(getByIDS);
getByIDS(lista);


for (i in 1:length(list)){
       
     tryCatch({
         data <- fromJSON(paste( list[i],".json", sep=""), flatten=TRUE)
         temp = getVariables(data)
         retorno <- rbind(retorno,temp)
         },error = function(cond) {
           return (NA)
           }, warning = function(cond) {
              return (NULL)
             })
   }

temp$player.match_id <- data$players$match_id[id]
temp$player.chat <- data$players$chat[id]
chat <- rbind(chat,temp)

pegar index de onde esta o player com hero_id: 4,
data$players$match_id[1]
, pegar variaveis especificas do player

getByIDS <- function(list ){
  setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4")
  library(jsonlite)
  link <- "https://api.opendota.com/api/matches/"
  temp <- list()
  retorno <- list()
  chats <- list()
  for (i in 1:length(list)){
    
    tryCatch({
      data <- fromJSON(paste( list[i],".json", sep=""), flatten=TRUE)
      temp = getVariables(data)
      retorno <- rbind(retorno,temp)
    },error = function(cond) {
      return (NA)
    }, warning = function(cond) {
      return (NULL)
    })
  }
  write.csv(retorno, file="teste.csv")
}


getVariables <- function(data) {
  for(i in 1:length(data$players$hero_id)) { if(data$players$hero_id[i]==4) id = i}
  temp <- list()
  temp$match_id <- data$match_id
  temp$game_mode <- data$game_mode
  temp$dire_score <- data$dire_score
  temp$human_players <- data$human_players
  temp$lobby_type <- data$lobby_type
  temp$engine <- data$engine
  temp$radiant_win <- data$radiant_win
  temp$radiant_score <- data$radiant_score
  temp$purchase_log <- data$purchase_log
  # temp$lane_pos <- data$lane_pos
  temp$player.player_slot <- data$players$player_slot[id]
  temp$player.ability_upgrades_arr <- data$players$ability_upgrades_arr[id]
  temp$player.ability_uses <- data$players$ability_uses[id]
  temp$player.actions <- data$players$actions[id]
  temp$player.assists <- data$players$assists[id]
  temp$player.backpack_0 <- data$players$backpack_0[id]
  temp$player.backpack_1 <- data$players$backpack_1[id]
  temp$player.backpack_2 <- data$players$backpack_2[id]
  temp$player.damage <- data$players$damage[id]
  temp$player.damage_inflictor <- data$players$damage_inflictor[id]
  temp$player.damage_inflictor_received <- data$players$damage_inflictor_received[id]
  temp$player.damage_taken <- data$players$damage_taken[id]
  temp$player.deaths <- data$players$deaths[id]
  temp$player.denies <- data$players$denies[id]
  temp$player.hero_damage <- data$players$hero_damage[id]
  temp$player.hero_healing <- data$players$hero_healing[id]
  temp$player.hero_hits <- data$players$hero_hits[id]
  temp$player.hero_id <- data$players$hero_id[id]
  temp$player.item_0 <- data$players$item_0[id]
  temp$player.item_1 <- data$players$item_1[id]
  temp$player.item_2 <- data$players$item_2[id]
  temp$player.item_3 <- data$players$item_3[id]
  temp$player.item_4 <- data$players$item_4[id]
  temp$player.item_5 <- data$players$item_5[id]
  temp$player.item_uses <- data$players$item_uses[id]
  temp$player.kill_streaks <- data$players$kill_streaks[id]
  temp$player.killed <- data$players$killed[id]
  temp$player.killed_by <- data$players$killed_by[id]
  temp$player.kills <- data$players$kills[id]
  temp$player.kills_log <- data$players$kills_log[id]
  temp$player.leaver_status <- data$players$leaver_status[id]
  temp$player.level <- data$players$level[id]
  temp$player.purchase <- data$players$purchase[id]
  temp$player.purchase_log <- data$players$purchase_log[id]
  temp$player.rune_pickups <- data$players$rune_pickups[id]
  temp$player.runes <- data$players$runes[id]
  temp$player.runes_log <- data$players$runes_log[id]
  temp$player.tower_damage <- data$players$tower_damage[id]
  temp$player.towers_killed <- data$players$towers_killed[id]
  temp$player.isRadiant <- data$players$isRadiant[id]
  temp$player.win <- data$players$win[id]
  temp$player.lose <- data$players$lose[id]
  temp$player.benchmarks.kills_per_min <- data$players$benchmarks.kills_per_min[id]
  temp$player.kda <- data$players$kda[id]
  temp$player.abandons <- data$players$abandons[id]
  temp$player.neutral_kills <- data$players$neutral_kills[id]
  temp$player.tower_kills <- data$players$tower_kills[id]
  temp$player.courier_kills <- data$players$courier_kills[id]
  temp$player.lane_kills <- data$players$lane_kills[id]
  temp$player.hero_kills <- data$players$hero_kills[id]
  temp$player.observer_kills <- data$players$observer_kills[id]
  temp$player.sentry_kills <- data$players$sentry_kills[id]
  temp$player.roshan_kills <- data$players$roshan_kills[id]
  temp$player.necronomicon_kills <- data$players$necronomicon_kills[id]
  temp$player.ancient_kills <- data$players$ancient_kills[id]
  temp$player.lane_efficiency <- data$players$lane_efficiency[id]
  temp$player.lane <- data$players$lane[id]
  temp$player.lane_role <- data$players$lane_role[id]
  return(temp)
}
getChat <- function(data,id) {
  if(!(is.null(data$players$chat[id]))) {
    temp$player.match_id <- data$players$match_id[id]
    temp$player.chat <- data$players$chat[id]
    chat <- rbind(chat,temp)
  }
}