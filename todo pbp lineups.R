library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)
library(future)

seconds_to_minutes <- function(x){
  paste0(floor(x / 60), ":", str_pad(x %% 60, side = "left", width = 2, pad = 0))
}

game_logs <- game_logs(seasons = 2021)

games <- game_logs %>%
  select(idGame, slugTeam, slugOpponent, locationGame) %>%
  mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
         slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
  select(-c(slugTeam, slugOpponent, locationGame)) %>%
  distinct(idGame, .keep_all = TRUE)

plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = unique(games$idGame))

replay_reviews <- play_logs_all %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
  filter(numberEventActionType == 18)

new_pbp <- play_logs_all %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
  filter(numberEventMessageType != 18) %>%
  group_by(idGame) %>%
  mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
  ungroup() %>%
  select(idGame, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
         slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, minuteRemainingQuarter,          
         secondsRemainingQuarter, descriptionPlayHome, numberEvent, descriptionPlayVisitor, scoreHome, scoreAway) %>%
  mutate(shotPtsHome = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,                               # Note 1
    numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,                                 # Note 2
    numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(shotPtsAway = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
    numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
    numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  group_by(idGame) %>%
  mutate(ptsHome = cumsum(shotPtsHome),
         ptsAway = cumsum(shotPtsAway)) %>%
  ungroup() %>%
  mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%                       # Note 3 
  mutate(secsStartQuarter = case_when(                                                                        # Note 4
    numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
    TRUE ~ 2880 + (numberPeriod - 5) * 300
  )) %>%
  mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),  # Note 5
         secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  select(idGame, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1, slugTeamPlayer2, 
         slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, namePlayer3, 
         descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway) %>%
  mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
         marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
         timeQuarter = str_pad(timeQuarter, width = 5, pad = 0))

subs_made <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%        # Note 6
  mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1,
         slugTeamLocation, playerOut = namePlayer1, playerIn = namePlayer2) %>%
  pivot_longer(cols = starts_with("player"),
               names_to = "inOut",
               names_prefix = "player",
               values_to = "namePlayer") %>%
  group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
  filter(row_number() == 1) %>%
  ungroup()

others_qtr <- new_pbp %>%
  filter(numberEventMessageType != 8) %>%                             
  filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>%     # Note 7
  pivot_longer(cols = starts_with("namePlayer"),
               names_to = "playerNumber",
               names_prefix = "namePlayer",
               values_to = "namePlayer") %>%
  mutate(slugTeamPlayer = case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                    playerNumber == 2 ~ slugTeamPlayer2,
                                    playerNumber == 3 ~ slugTeamPlayer3,
                                    TRUE ~ "None")) %>%
  mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, "Home", "Away")) %>%
  filter(!is.na(namePlayer),
         !is.na(slugTeamPlayer)) %>%
  anti_join(subs_made %>%
              select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%    # remove players that were subbed in the quarter
  distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

lineups_quarters <- subs_made %>%
  filter(inOut == "Out") %>%
  select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
  bind_rows(others_qtr) %>%
  arrange(idGame, numberPeriod, slugTeamPlayer)

# lineups_quarters %>%
#   count(idGame, numberPeriod, slugTeamPlayer) %>%
#   filter(n != 5)

missing_players_ot <- tribble(
  ~idGame,   ~slugTeamPlayer,          ~namePlayer,     ~numberPeriod,
  22000023,        "DET",           "Delon Wright",           5,
  22000100,        "IND",           "Justin Holiday",         5,
  22000120,        "DEN",           "Gary Harris",            5
) %>%
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  mutate(slugTeamLocation = ifelse(slugTeamHome == slugTeamPlayer, "Home", "Away")) %>%
  select(-c(slugTeamHome, slugTeamAway))

lineups_quarters <- lineups_quarters %>%
  bind_rows(missing_players_ot) %>%
  arrange(idGame, numberPeriod, slugTeamPlayer)

lineup_subs <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1, playerOut = namePlayer1, 
         playerIn = namePlayer2, numberEvent) %>%
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamPlayer) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              mutate(row1 = 1)) %>%
  select(-row1) %>%
  mutate(lineupBefore = str_split(lineupBefore, ", ")) %>% 
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(lineupAfter = accumulate2(playerIn, playerOut, ~setdiff(c(..1, ..2), ..3), .init = lineupBefore[[1]])[-1],
         lineupBefore = coalesce(lineupBefore, lag(lineupAfter))) %>%
  ungroup() %>% 
  mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
  mutate_at(vars("numberEvent", "numberPeriod", "idGame"), ~ as.integer(.)) %>%
  mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
  arrange(idGame, numberEvent) %>%
  left_join(lineups_quarters %>%
              distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
  filter(!is.na(slugTeamLocation))

lineup_game <- new_pbp %>%
  group_by(idGame, numberPeriod) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamLocation) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              pivot_wider(names_from = slugTeamLocation,
                          names_prefix = "lineupInitial",
                          values_from = lineupBefore) %>%
              mutate(row1 = 1)) %>%
  select(-row1) %>%
  left_join(lineup_subs %>%
              mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                     lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                     lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                     lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
              select(idGame, numberPeriod, timeQuarter, secsPassedGame, numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                     contains("Home"), contains("Away"))) %>%
  mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), ~ ifelse(!is.na(lineupInitialHome), lineupInitialHome, .)) %>%
  mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), ~ ifelse(!is.na(lineupInitialAway), lineupInitialAway, .)) %>%
  select(-starts_with("lineupInitial")) %>%
  group_by(idGame, numberPeriod) %>%
  mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
         lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
         lineupHome = ifelse(is.na(lineupHome), na.locf(lineupBeforeHome, fromLast = TRUE, na.rm = FALSE), lineupHome),
         lineupAway = ifelse(is.na(lineupAway), na.locf(lineupBeforeAway, fromLast = TRUE, na.rm = FALSE), lineupAway),
         lineupHome = str_split(lineupHome, ", "),
         lineupAway = str_split(lineupAway, ", "),
         lineupHome = map_chr(lineupHome, ~ paste(sort(.), collapse = ", ")),
         lineupAway = map_chr(lineupAway, ~ paste(sort(.), collapse = ", "))) %>%
  ungroup() %>%
  select(-c(starts_with("lineupBefore"), starts_with("lineupAfter")))


lineup_game_stats <- lineup_game %>%
  mutate(canSub = case_when(numberEventMessageType == 5 & !numberEventActionType %in% c(1, 2) ~ 1,    # dead ball turnovers
                            numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                            numberEventMessageType == 11 & numberEventActionType != 4 ~ 1,
                            numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                            TRUE ~ 0)) %>%
  mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                    (str_detect(str_to_lower(descriptionPlayHome), "technical") |
                                       str_detect(str_to_lower(descriptionPlayVisitor), "technical")),
                                  secsPassedGame + 0.5, secsPassedGame)) %>%    # Note 4
  group_by(idGame, numberPeriod, secsPassedGame) %>%
  mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                            as.character(numberEvent)),
         numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                            numberNew)) %>%
  mutate(numberNew = str_split(numberNew, ", "),
         numberNew = map(numberNew, ~as.numeric(.)),
         numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%
  ungroup() %>%
  arrange(idGame, numberNew, numberEvent) %>%
  group_by(idGame) %>%
  mutate(newptsHome = cumsum(shotPtsHome),
         newptsAway = cumsum(shotPtsAway)) %>%
  group_by(idGame, numberPeriod, secsPassedGame2) %>%
  mutate(subOpp = cumsum(canSub)) %>%
  group_by(idGame = as.character(idGame), numberPeriod = as.character(numberPeriod), subOpp, secsPassedGame2 = as.character(secsPassedGame2)) %>%
  mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
  mutate(newptsHome = ifelse(hasFouls > 0,
                             newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsHome),
         newptsAway = ifelse(hasFouls > 0,
                             newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsAway)) %>%
  ungroup() %>%
  select(-hasFouls) %>%
  select(-c(numberNew, secsPassedGame2)) %>%
  mutate_all(~ as.character(.)) %>%
  mutate(secsPassedGame = as.numeric(secsPassedGame),
         numberEvent = as.numeric(numberEvent))

lineup_stats <- lineup_game_stats %>%
  select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
         newptsHome, newptsAway, lineupHome, lineupAway) %>%
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineupLocation",
               names_prefix = "lineup",
               values_to = "lineup") %>%
  mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
         ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
         slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
         slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
  distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp, lineup, 
           teamLocation = lineupLocation, numberEvent) %>%
  arrange(idGame, numberEvent) %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupChange = lineup != lag(lineup),
         lineupChange = coalesce(lineupChange, FALSE)) %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupStint = cumsum(lineupChange)) %>%
  ungroup() %>%
  arrange(idGame, lineupStint, numberEvent) %>%
  group_by(idGame, slugTeam, lineup, lineupStint) %>%
  summarise(initialScoreTeam = ptsTeam[row_number() == min(row_number())],
            initialScoreOpp = ptsOpp[row_number() == min(row_number())],
            finalScoreTeam = ptsTeam[row_number() == max(row_number())],
            finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
            initialTime = secsPassedGame[row_number() == min(row_number())],
            finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
  ungroup() %>%
  arrange(idGame, lineupStint) %>%
  group_by(idGame, slugTeam) %>%                              
  mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
  ungroup() %>%
  mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
  mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
         totalScoreOpp = finalScoreOpp - initialScoreOpp,
         netScoreTeam = totalScoreTeam - totalScoreOpp,
         totalTime = finalTime - initialTime) %>%
  arrange(idGame, lineupStint)

# indiv_stats <- lineup_stats %>%
#   separate_rows(lineup, sep = ", ") %>%
#   group_by(namePlayer = lineup, idGame, slugTeam) %>%
#   summarise(totalPlusMinus = sum(netScoreTeam),
#             totalSecs = sum(totalTime)) %>%
#   ungroup() %>%
#   arrange(-totalPlusMinus)
# 
# indiv_stats %>%
#   group_by(namePlayer) %>%
#   summarise(seasonPM = sum(totalPlusMinus),
#             seasonSecs = sum(totalSecs)) %>%
#   ungroup() %>%
#   arrange(-seasonPM) %>%
#   mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
#   select(-seasonSecs)
# 
# lineup_stats %>%
#   group_by(lineup) %>%
#   summarise(seasonPM = sum(netScoreTeam),
#             seasonSecs = sum(totalTime)) %>%
#   ungroup() %>%
#   arrange(-seasonPM, -seasonSecs) %>%
#   mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
#   select(-seasonSecs)

rm(lineup_game, games, df_nba_player_dict, lineup_subs, missing_players_ot, others_qtr, subs_made)

