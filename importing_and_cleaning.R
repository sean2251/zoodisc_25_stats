# Importing and cleaning

# Load packages
library(tidyverse)

# Set the filepath for raw statto files here:
statto_files <- "raw_data"

# Important reminders:
# x-coordinates: (0 -> 1 = left sideline -> right sideline)
# y-coordinates: (0 -> 1 = back of opponent endzone -> back of own endzone)

###################################################
## Helper functions ##
###################################################

# Generate list of teams, separated by | from files
team_list <- function(filepath) { 
  list_of_files <- list.files(path = filepath,
                              pattern = "(Stats)", 
                              full.names = T)
  names_extracted <- list_of_files |> 
    str_extract("(?<=vs. ).*") |> 
    str_sub(end = -8)
  str_flatten(names_extracted, collapse = "|")
}

read_plus <- function(flnm, teams) {
  read_csv(flnm) |> 
    mutate(filename = flnm,
           game = str_extract(flnm, teams))
}

###################################################
## Possessions ##
###################################################

possessions_raw <- 
  list.files(path = statto_files,
             pattern = "(Possessions)", 
             full.names = T) |> 
  map_df(~read_plus(., team_list(statto_files))) |> 
  mutate(`Scored?` = as.factor(`Scored?`),
         line = ifelse(`Started point on offense?`, 'oline', 'dline'),
         possessionID = paste0(game, "-", Point, "-", Possession),
         passes = cut(Passes, breaks = c(0, 5, 10, 15, 20, 25, 30))) |> 
  select(!c(Created, filename))

###################################################
## Passes ##
###################################################

passes_raw <- 
  list.files(path = statto_files,
             pattern = "(Passes)", 
             full.names = T) |> 
  map_df(~read_plus(., team_list(statto_files))) |> 
  mutate(possessionID = paste0(game, "-", Point, "-", Possession),
         `Throw to endzone?` = as.factor(`Throw to endzone?`)) |>
  left_join(possessions_raw, by = "possessionID", multiple = "all") |> 
  rename(pass.start.x = `Start X (0 -> 1 = left sideline -> right sideline).x`,
         pass.start.y = `Start Y (0 -> 1 = back of opponent endzone -> back of own endzone).x`,
         pass.end.x = `End X (0 -> 1 = left sideline -> right sideline)`,
         pass.end.y = `End Y (0 -> 1 = back of opponent endzone -> back of own endzone)`,
         possession.start.x = `Start X (0 -> 1 = left sideline -> right sideline).y`,
         possession.start.y = `Start Y (0 -> 1 = back of opponent endzone -> back of own endzone).y`,
         point = Point.x,
         possession = Possession.x,
         game = game.x) |> 
  select(!c(Created, filename, Point.y, Possession.y, game.y)) |> 
  relocate(game)


###################################################
## Points ##
###################################################
points_raw <- 
  list.files(path = statto_files,
             pattern = "(Points)", 
             full.names = T) |> 
  map_df(~read_plus(., team_list(statto_files))) |>
  select(!c(Created, filename)) |>
  relocate(game)


###################################################
## Thrower-Receiver Pairs ##
###################################################

thrower_receiver_pairs <- passes_raw |> 
  mutate(thrower_receiver = paste0(Thrower, "->", Receiver)) |> 
  group_by(thrower_receiver, `Turnover?`) |> 
  summarise(n = n(),
            "Total forward distance (m)" = sum(`Forward distance (m)`),
            "Mean forward distance (m)" = mean(`Forward distance (m)`)) |> 
  mutate('Completion %' = n/sum(n),
         `Turnover?` = ifelse(`Turnover?` == 1, 
                              "Incompleted",
                              "Completed"))  |> 
  pivot_wider(names_from = `Turnover?`,
              names_glue = "{`Turnover?`}_{.value}",
              values_from = n:`Completion %`) |> 
  mutate(across(where(is.numeric), \(x) round(x, digits = 2)),
         `Completed_n` = ifelse(is.na(`Completed_n`), 0, `Completed_n`),
         `Incompleted_n` = ifelse(is.na(Incompleted_n), 0, Incompleted_n)) |> 
  separate_wider_delim(cols = thrower_receiver, 
                       delim = "->",
                       names = c("thrower", "receiver"),
                       cols_remove = FALSE)



###################################################
## Export to csv (optional) ##
###################################################

# export all passes with possession
#write.csv(passes_raw, "final_csvs/all_passes.csv")
#save(passes_raw, file = "all_passes_rdataframe.RData")

# export all points
#write.csv(points_raw, "final_csvs/all_points.csv")
#save(points_raw, file = "all_passes_rdataframe.RData")

# export all thrower-receiver pairs
#write.csv(thrower_receiver_pairs, "final_csvs/thrower_receiver_pairs.csv")
#save(thrower_receiver_pairs, file = "thrower_receiver_pairs_rdataframe.RData")

