install.packages("remotes")

remotes::install_github(
  "guilhermefranchi/HLCtools",
  auth_token = gitcreds::gitcreds_get()$password,
  upgrade = "never"
)

library(HLCtools)

d <- readxl::read_excel(
  "O:/Tech_Collab-MBJ-GAF/Bovaer studies/Ministerial_project/materials-methods/2026_Lactating_cows/cleaned_data/IceQube_15min_2026.xlsx",
  sheet = "Sheet 1"
) |>
  dplyr::mutate(
    week = factor(week, levels = gtools::mixedsort(unique(week)))
  )

data(example_lies)

hlc_new_intervals <- calculate_hlc(
  data = d,
  group = Treatment,
  animal = Cow,
  day = exp.day,
  interval = time,
  lying = LyingTime,
  interval_min = 15,
  methods = c("sd", "mad", "iqr", "entropy"),
  sync_thresholds = c(0.6, 0.7, 0.8, 0.9),
  add_lying_weighted = TRUE
)

hlc_new_intervals <- calculate_hlc(
  data = d,
  group = Treatment,
  animal = Cow,
  day = exp.day,
  period = week,
  interval = time,
  lying = LyingTime,
  interval_min = 15
)


hlc_new_weekly <- summarise_hlc_daily(
  data = hlc_new_intervals,
  group = Treatment,
  day = NULL,
  period = week,
  interval_min = 15
)

HLCtools::compare_hlc_methods(hlc_new_weekly)


