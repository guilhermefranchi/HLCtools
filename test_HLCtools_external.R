# Testing HLCtools in different df

remotes::install_github(
  "guilhermefranchi/HLCtools",
  auth_token = gitcreds::gitcreds_get()$password,
  upgrade = "never",
  force = TRUE
)

library(HLCtools)

"rank_hlc_methods" %in% getNamespaceExports("HLCtools")
"compare_hlc_methods" %in% getNamespaceExports("HLCtools")


d <- readxl::read_excel(
  "O:/Tech_Collab-MBJ-GAF/Bovaer studies/Ministerial_project/materials-methods/2026_Lactating_cows/cleaned_data/IceQube_15min_2026.xlsx",
  sheet = "Sheet 1"
) |>
  dplyr::mutate(
    week = factor(week, levels = gtools::mixedsort(unique(week)))
  )



hlc_weekly <- calculate_hlc(
  data = d,
  group = Treatment,
  animal = Cow,
  day = NULL,
  period = week,
  interval = NULL,
  lying = LyingTime,
  interval_min = 15,
  methods = c("sd", "mad", "iqr", "entropy"),
  sync_thresholds = c(0.6, 0.7, 0.8, 0.9),
  add_lying_weighted = TRUE
)

compare_hlc_methods(hlc_weekly)

rank_hlc_methods(hlc_weekly)



