setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4");
lista <- c(3393069424,3393100973,3393099678);
 debug(getByIDS);
 getByIDS(lista);

pegar index de onde esta o player com hero_id: 4,
data$players$match_id[1]
, pegar variaveis especificas do player

getByIDS <- function(lista ){
setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4")
library(jsonlite)
link <- "https://api.opendota.com/api/matches/"
temp <- list()
retorno <- list()
chats <- list()
for (i in 1:length(list)){	
	data <- fromJSON(paste( lista[i],".json", sep=""), flatten=TRUE)
	temp = getVariables(data)
	retorno <- rbind(retorno,temp)
	temp = list()
	temp = list()
	i= i+1
}
write.csv(retorno, file="teste.csv")
}


getVariables <- function(data) {
temp <- list()
	temp$match_id <- data$match_id
	temp$game_mode <- data$game_mode
	temp$dire_score <- data$dire_score
	temp$human_players <- data$human_players
	temp$lobby_type <- data$lobby_type
	temp$radiant_win <- data$radiant_win
	temp$radiant_score <- data$radiant_score
	temp$purchase_log <- data$purchase_log
	temp$lane_pos <- data$lane_pos
	temp$player_slot <- data$player_slot
	temp$ability_upgrades_arr <- data$ability_upgrades_arr
	temp$ability_uses <- data$ability_uses
	temp$actions <- data$actions
	temp$assists <- data$assists
	temp$backpack_0 <- data$backpack_0
	temp$backpack_1 <- data$backpack_1
	temp$backpack_2 <- data$backpack_2
	temp$damage <- data$damage
	temp$damage_inflictor <- data$damage_inflictor
	temp$damage_inflictor_received <- data$damage_inflictor_received
	temp$damage_taken <- data$damage_taken
	temp$deaths <- data$deaths
	temp$denies <- data$denies
	temp$hero_damage <- data$hero_damage
	temp$hero_healing <- data$hero_healing
	temp$hero_hits <- data$hero_hits
	temp$hero_id <- data$hero_id
	temp$item_0 <- data$item_0
	temp$item_1 <- data$item_1
	temp$item_2 <- data$item_2
	temp$item_3 <- data$item_3
	temp$item_4 <- data$item_4
	temp$item_5 <- data$item_5
	temp$item_uses <- data$item_uses
	temp$kill_streaks <- data$kill_streaks
	temp$killed <- data$killed
	temp$killed_by <- data$killed_by
	temp$kills <- data$kills
	temp$kills_log <- data$kills_log
	temp$leaver_status <- data$leaver_status
	temp$level <- data$level
	temp$purchase <- data$purchase
	temp$purchase_log <- data$purchase_log
	temp$rune_pickups <- data$rune_pickups
	temp$runes <- data$runes
	temp$runes_log <- data$runes_log
	temp$tower_damage <- data$tower_damage
	temp$towers_killed <- data$towers_killed
	temp$isRadiant <- data$isRadiant
	temp$win <- data$win
	temp$lose <- data$lose
	temp$benchmarks.kills_per_min <- data$benchmarks.kills_per_min
	temp$kda <- data$kda
	temp$abandons <- data$abandons
	temp$neutral_kills <- data$neutral_kills
	temp$tower_kills <- data$tower_kills
	temp$courier_kills <- data$courier_kills
	temp$lane_kills <- data$lane_kills
	temp$hero_kills <- data$hero_kills
	temp$observer_kills <- data$observer_kills
	temp$sentry_kills <- data$sentry_kills
	temp$roshan_kills <- data$roshan_kills
	temp$necronomicon_kills <- data$necronomicon_kills
	temp$ancient_kills <- data$ancient_kills
	temp$lane_efficiency <- data$lane_efficiency
	temp$lane <- data$lane
	temp$lane_role <- data$lane_role
	return(temp)
}
getChat <- function(data) {
if(!(is.null(data$chat))) {
		temp$match_id <- data$match_id
		temp$chat <- data$chat
		chat <- rbind(chat,temp)
	}
}