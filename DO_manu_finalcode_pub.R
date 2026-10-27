#### HJ Andrews WY2024-25: dissolved oxygen, thermal sensitivity, and controls
# Run from the repo root (HJ-Andrews-WY2324). All inputs and outputs live in Processed_data/.

library(tidyverse)   # ggplot2, dplyr, tidyr, readr, purrr, stringr, tibble, lubridate
library(lubridate)
library(slider)
library(patchwork)
library(ggrepel)
library(scales)
library(nlme)
library(MuMIn)
# ggtext is also needed (called as ggtext:: in Fig 6)

data_dir <- "Processed_data"
fig_dir  <- file.path(data_dir, "Figures")
out_dir  <- file.path(data_dir, "outputs")
dir.create(out_dir, showWarnings = FALSE)


#### setup ####
flow_colors <- c("Wet" = "#1b9e77", "Dry" = "#d95f02")
year_colors <- c("2024" = "#2a78d6", "2025" = "#e34948")

std_theme <- theme_classic(base_size=11) +
  theme(
    plot.title=element_text(face="bold", size=12),
    axis.title=element_text(size=10),
    axis.text=element_text(size=9),
    axis.ticks.x=element_blank(),
    legend.title=element_blank()
  )

# site order
site_positions <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8","S9","S10","S11","S12",
                    "S13.a", "S13.b", "S14", "S15", "S16","S17.a","S17.b")

# sites with sensors in both years / both wet and dry periods
multi_year_sites   <- c("S3", "S6", "S14", "S16", "S17.b")
multi_wetdry_sites <- c("S3", "S6", "S17.b")


#### load data ####
df <- read_csv(file.path(data_dir, "final_data.csv"))

nut <- read_csv(file.path(data_dir, "HJA_nutrient.csv")) %>%
  mutate(Date = as.Date(date_time)) %>%
  dplyr::select(-date_time) %>%
  dplyr::filter(
    dplyr::if_any(
      c(ph, alk, ssed, si, utp, tdp, po4p,
        utn, tdn, nh4n, no3n, doc),
      ~ !is.na(.x)
    )
  )

width_depth <- read_csv(file.path(data_dir, "width_depth_surveys.csv"))

gw_tracer <- read_csv(file.path(data_dir, "grab_samples.csv")) %>%
  mutate(Date = as.Date(Date),
         site_position = case_match(site_position,
                                    "SS11" ~ "S11.b",
                                    "SS15" ~ "SS15.b",
                                    "S11"  ~ "S11.b",
                                    .default = site_position))

# quick look at all nutrient data
nut %>%
  dplyr::select(Date, where(is.numeric)) %>%
  pivot_longer(-c(Date), names_to = "analyte", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(Date, value)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_line(alpha = 0.3) +
  facet_wrap(~analyte, scales = "free_y") +
  theme_classic(base_size = 10) +
  labs(x = NULL, y = NULL, title = "All nutrient variables over time")


#### shared daily summaries ####
# daily DO per site, only days with >= 72 15-min obs
daily_by_site <- df %>%
  filter(!is.na(DO_sat_percent), !is.na(site_position)) %>%
  dplyr::group_by(site_position, Day, wet_dry) %>%
  filter(n() >= 72) %>%
  dplyr::summarise(
    n_obs = n(),
    daily_mean = mean(DO_sat_percent, na.rm = TRUE),
    daily_min = min(DO_sat_percent, na.rm = TRUE),
    daily_max = max(DO_sat_percent, na.rm = TRUE),
    daily_amp = daily_max - daily_min,
    .groups = "drop"
  )

# daily site summaries of DO + drivers (all days, no obs filter)
all_day <- df %>%
  dplyr::mutate(Date = as.Date(PCT)) %>%
  dplyr::group_by(Date, site_position) %>%
  dplyr::summarise(
    daily_mean = mean(DO_sat_percent, na.rm = TRUE),
    daily_min = min(DO_sat_percent, na.rm = TRUE),
    daily_max = max(DO_sat_percent, na.rm = TRUE),
    daily_amp = daily_max - daily_min,
    mean_Q = mean(Discharge_L_s, na.rm = TRUE),
    benthic_light = mean(benthic_light, na.rm = TRUE),
    precip = mean(Gage_precip_total_mm, na.rm = TRUE),
    wet_dry = dplyr::first(wet_dry),
    .groups = "drop"
  ) %>%
  dplyr::mutate(Year = lubridate::year(Date))


#### Supp Table S1: site characteristics ####
site_meta <- tribble(
  ~site_position, ~lat,     ~lon,       ~dried, ~unit,
  "S1",    44.20730, -122.25800, "No",  "Pool-riffle",
  "S2",    44.20716, -122.25773, "No",  "Pool-riffle",
  "S3",    44.20684, -122.25739, "No",  "Bedrock",
  "S4",    44.20679, -122.25729, "No",  "Step-pool",
  "S5",    44.20659, -122.25696, "No",  "Step-pool",
  "S6",    44.20650, -122.25670, "No",  "Step-pool",
  "S7",    44.20639, -122.25649, "No",  "Step-pool",
  "S8",    44.20635, -122.25642, "No",  "Step-pool",
  "S9",    44.20632, -122.25637, "No",  "Step-pool",
  "S10",   44.20632, -122.25622, "Yes", "Pool-riffle",
  "S11",   44.20623, -122.25608, "No",  "Step-pool",
  "S12",   44.20619, -122.25593, "No",  "Step-pool",
  "S13.a", 44.20611, -122.25566, "Yes", "Pool-riffle",
  "S13.b", 44.20609, -122.25562, "No",  "Step-pool",
  "S14",   44.20603, -122.25537, "No",  "Step-pool",
  "S15",   44.20589, -122.25507, "No",  "Step-pool",
  "S16",   44.20593, -122.25486, "No",  "Step-pool",
  "S17.a", 44.20581, -122.25466, "Yes", "Bedrock",
  "S17.b", 44.20579, -122.25463, "No",  "Bedrock"
)

mean_sd <- function(x) {
  x <- na.omit(x)
  if (length(x) < 2) return(sprintf("%.2f", x)[1])
  sprintf("%.2f ± %.2f", mean(x), sd(x))
}

day_sum <- df %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(
    distance=dplyr::first(Distance_m[!is.na(Distance_m)],default=NA_real_),
    n_do_days=dplyr::n_distinct(Day[!is.na(DO_mg_L) & !is.na(Day)]),
    n_light_days=dplyr::n_distinct(Day[!is.na(benthic_light) & !is.na(Day)]),
    .groups="drop"
  )

wd_sum <- width_depth %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(
    depth=mean_sd(Water_depth_m),
    width=mean_sd(Wetted_width_m),
    .groups="drop"
  )

table1 <- site_meta %>%
  dplyr::left_join(day_sum,by="site_position") %>%
  dplyr::left_join(wd_sum,by="site_position") %>%
  dplyr::arrange(distance) %>%
  dplyr::transmute(
    Site=site_position,
    `Latitude, Longitude`=sprintf("(%.5f, %.5f)",lat,lon),
    `Distance (m)`=distance,
    Dried=dried,
    `# of DO days`=dplyr::coalesce(n_do_days,0L),
    `# of benthic light days`=dplyr::coalesce(n_light_days,0L),
    `Geomorphic unit`=unit,
    `Depth (m)`=depth,
    `Width (m)`=width
  )

print(table1,n=Inf)

## deployment coverage by year
sites_by_year <- daily_by_site %>%
  mutate(Year = year(Day)) %>%
  dplyr::group_by(Year, site_position) %>%
  dplyr::summarise(N_obs = sum(n_obs, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = N_obs, values_fill = 0)
sites_by_year

sites_deployed <- daily_by_site %>%
  mutate(Year = year(Day)) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    N_Sites = n_distinct(site_position),
    Total_Obs = sum(n_obs, na.rm = TRUE),
    Mean_Obs_Per_Site = round(Total_Obs / N_Sites, 0),
    .groups = "drop"
  )
sites_deployed

coverage_by_year_period <- daily_by_site %>%
  mutate(Year = year(Day)) %>%
  dplyr::group_by(Year, wet_dry) %>%
  dplyr::summarise(
    N_Sites = n_distinct(site_position),
    N_Obs = sum(n_obs, na.rm = TRUE),
    N_Days = n_distinct(Day),
    .groups = "drop"
  )
coverage_by_year_period

dry_deployment <- daily_by_site %>%
  filter(wet_dry == "Dry") %>%
  mutate(Year = year(Day)) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    N_Sites = n_distinct(site_position),
    N_Obs = sum(n_obs, na.rm = TRUE),
    N_Days = n_distinct(Day),
    Avg_Obs_Per_Day = round(N_Obs / N_Days, 0),
    .groups = "drop"
  )
dry_deployment

# sites lost/gained 2024 to 2025
sites_2024 <- daily_by_site %>% filter(year(Day) == 2024) %>% pull(site_position) %>% unique() %>% sort()
sites_2025 <- daily_by_site %>% filter(year(Day) == 2025) %>% pull(site_position) %>% unique() %>% sort()
setdiff(sites_2024, sites_2025)
setdiff(sites_2025, sites_2024)


#### storms ####
# one outlet observation per 15-min timestamp (outlet vars are repeated across sites)
outlet_15 <- df %>%
  distinct(PCT, .keep_all = TRUE) %>%
  transmute(
    PCT,
    wet_dry = str_to_title(wet_dry),
    Q = Discharge_L_s,
    precip = replace_na(Gage_precip_total_mm , 0)
  ) %>%
  arrange(PCT)

# rainfall events: new event after >= 24 h without rain
rain_events <- outlet_15 %>%
  filter(precip > 0) %>%
  mutate(
    new_event = is.na(lag(PCT)) |
      as.numeric(difftime(PCT, lag(PCT), units = "hours")) >= 24,
    event_id = cumsum(new_event)
  ) %>%
  dplyr::group_by(event_id) %>%
  dplyr::summarise(
    wet_dry = first(wet_dry),
    storm_start = min(PCT),
    rain_end = max(PCT),
    event_span_h = as.numeric(difftime(rain_end, storm_start, units = "hours")) + 0.25,
    total_precip = sum(precip),
    .groups = "drop"
  ) %>%
  arrange(storm_start) %>%
  mutate(
    next_storm = lead(storm_start),
    response_end = pmin(
      storm_start + hours(72),
      coalesce(next_storm, storm_start + hours(72))
    )
  )

# discharge response to each event
storm_summary <- rain_events %>%
  rowwise() %>%
  dplyr::mutate(
    pre_Q = median(
      outlet_15$Q[outlet_15$PCT >= storm_start - hours(24) & outlet_15$PCT < storm_start],
      na.rm = TRUE
    ),
    peak_Q = max(
      outlet_15$Q[outlet_15$PCT >= storm_start & outlet_15$PCT < response_end],
      na.rm = TRUE
    ),
    Q_increase = peak_Q - pre_Q,
    Q_ratio = peak_Q / pre_Q
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    hydro_response = Q_ratio >= 2,
    Year = year(storm_start)
  )

storm_summary %>%
  dplyr::select(event_id, Year, wet_dry, storm_start, rain_end, event_span_h,
                total_precip, pre_Q, peak_Q, Q_increase, Q_ratio, hydro_response)

# all sensor data from 72 h before to 144 h after each storm (or until the next storm)
storm_window_data <- rain_events %>%
  dplyr::select(event_id, storm_start, next_storm, wet_dry) %>%
  dplyr::rename(event_wet_dry = wet_dry) %>%
  dplyr::mutate(
    window_start = storm_start - lubridate::hours(72),
    window_end = pmin(
      storm_start + lubridate::hours(144),
      dplyr::coalesce(next_storm, storm_start + lubridate::hours(144))
    )
  ) %>%
  dplyr::inner_join(df, by = dplyr::join_by(window_start <= PCT, window_end > PCT)) %>%
  dplyr::mutate(
    hours_from_storm = as.numeric(difftime(PCT, storm_start, units = "hours")),
    days_from_storm = floor(hours_from_storm / 24)
  )

## checks: storm events and Q response
p_Q <- ggplot(outlet_15, aes(PCT, Q)) +
  geom_rect(
    data = storm_summary,
    aes(xmin = storm_start, xmax = rain_end, ymin = -Inf, ymax = Inf, fill = wet_dry),
    inherit.aes = FALSE, alpha = .25
  ) +
  geom_line(linewidth = .3) +
  labs(x = NULL, y = "Discharge (L/s)", fill = NULL) +
  theme_classic()

p_rain <- ggplot(outlet_15, aes(PCT, precip)) +
  geom_col(width = 15 * 60, fill = "#377eb8") +
  labs(x = NULL, y = "15-minute precipitation") +
  theme_classic()

p_Q / p_rain

# P vs Q
ggplot(storm_summary, aes(total_precip, Q_ratio, color = wet_dry)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_point(aes(shape = hydro_response), size = 2.5) +
  labs(x = "Total event precipitation", y = "Peak Q / pre-storm Q",
       color = NULL, shape = "Q doubled") +
  theme_classic()

# event-scale Q response
events_use <- storm_summary %>% filter(hydro_response)

event_Q <- map_dfr(seq_len(nrow(events_use)), \(i) {
  s <- events_use[i, ]
  outlet_15 %>%
    filter(PCT >= s$storm_start - hours(24), PCT < s$response_end) %>%
    transmute(
      event_id = s$event_id,
      wet_dry = s$wet_dry,
      hour = as.numeric(difftime(PCT, s$storm_start, units = "hours")),
      relative_Q = Q / s$pre_Q
    )
})

ggplot(event_Q, aes(hour, relative_Q, group = event_id, color = wet_dry)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_line(alpha = .5) +
  facet_wrap(~wet_dry, scales = "free_y") +
  labs(x = "Hours from rainfall onset", y = "Q / pre-storm Q", color = NULL) +
  theme_classic() +
  theme(legend.position = "none")

## days since last storm ended
storm_end_dates <- storm_summary %>%
  dplyr::mutate(Date = as.Date(rain_end, tz = "America/Los_Angeles")) %>%
  dplyr::distinct(Date) %>%
  dplyr::arrange(Date) %>%
  dplyr::pull(Date)

all_dates <- tibble::tibble(
  Date = seq(min(all_day$Date, na.rm = TRUE), max(all_day$Date, na.rm = TRUE), by = "day"))

idx <- findInterval(as.numeric(all_dates$Date), as.numeric(storm_end_dates))  # 0 = no prior storm
storm_days <- all_dates %>%
  dplyr::mutate(days_since_storm = ifelse(idx == 0, NA_real_,
                                          as.numeric(Date - storm_end_dates[pmax(idx, 1)])))

storm_days %>% dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(days_since_storm)),
                                max = max(days_since_storm, na.rm = TRUE))


#### thermal sensitivity ####
# rolling 7-day regression of stream T on air T (dry season)
calc_rolling_E <- function(df_site, window_days = 7, min_n = 5) {
  df_site <- df_site %>% arrange(Date)
  slide_index_dfr(
    .x = df_site, .i = df_site$Date,
    .before = window_days - 1, .complete = TRUE,
    .f = function(dat) {
      if (nrow(dat) < min_n || sd(dat$air_T, na.rm = TRUE) == 0) return(NULL)
      mod <- lm(stream_T ~ air_T, data = dat)
      tibble(
        site_position = unique(dat$site_position),
        Date = max(dat$Date),
        E = coef(mod)[2],
        r2 = summary(mod)$r.squared,
        n_obs = nrow(dat)
      )
    }
  )
}

temp_daily_Q <- df %>%
  filter(wet_dry == "Dry") %>%
  mutate(Date = as.Date(PCT)) %>%
  filter(!is.na(Water_temp_C), !is.na(air_temp_par), !is.na(Discharge_L_s)) %>%
  dplyr::group_by(site_position, Date) %>%
  dplyr::summarise(stream_T = mean(Water_temp_C, na.rm = TRUE),
                   air_T    = mean(air_temp_par, na.rm = TRUE),
                   Q        = mean(Discharge_L_s, na.rm = TRUE), .groups = "drop")

rolling_E_7day <- temp_daily_Q %>%
  dplyr::group_by(site_position) %>%
  group_split() %>%
  map_dfr(~ calc_rolling_E(.x, window_days = 7, min_n = 5))

## fit quality: how many windows are poor fits (r2 < 0.5)?
rolling_E_7day %>%
  dplyr::summarise(
    evaluated = sum(!is.na(r2)),
    poor_fit = sum(r2 < 0.5, na.rm = TRUE),
    pct_poor_fit = 100 * poor_fit / evaluated
  )

rolling_E_7day %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(
    n = sum(!is.na(r2)),
    median_r2 = median(r2, na.rm = TRUE),
    pct_below_05 = mean(r2 < 0.5, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_below_05))

ggplot(rolling_E_7day, aes(r2)) +
  geom_histogram(binwidth = 0.05, color = "white") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#e34948") +
  facet_wrap(~site_position) +
  labs(x = expression(R^2), y = "Number of estimates") +
  theme_classic()

rolling_E_7day <- rolling_E_7day %>%
  filter(is.finite(E), is.finite(r2))

## amplitude ratio (stream T range / air T range over the same window)
calc_rolling_amp_ratio <- function(df_site, window_days = 7, min_n = 5) {
  df_site <- df_site %>% arrange(Date)
  slide_index_dfr(
    .x = df_site, .i = df_site$Date,
    .before = window_days - 1, .complete = TRUE,
    .f = function(dat) {
      if (nrow(dat) < min_n) return(NULL)
      tibble(
        site_position = unique(dat$site_position),
        Date = max(dat$Date),
        amplitude_ratio = (max(dat$stream_T, na.rm=TRUE) - min(dat$stream_T, na.rm=TRUE)) /
          (max(dat$air_T, na.rm=TRUE) - min(dat$air_T, na.rm=TRUE))
      )
    }
  )
}

rolling_amp_ratio <- temp_daily_Q %>%
  dplyr::group_by(site_position) %>%
  group_split() %>%
  map_dfr(~ calc_rolling_amp_ratio(.x, window_days = 7, min_n = 5))

E_amp_comparison <- rolling_E_7day %>%
  dplyr::select(site_position, Date, E, r2) %>%
  inner_join(rolling_amp_ratio %>% dplyr::select(site_position, Date, amplitude_ratio),
             by = c("site_position", "Date"))

nrow(E_amp_comparison)
cor.test(E_amp_comparison$E, E_amp_comparison$amplitude_ratio, method = "spearman")

ggplot(E_amp_comparison, aes(amplitude_ratio, E, color = r2)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(name = "R\u00b2 (E fit)") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_classic(base_size = 12) +
  labs(x = "Amplitude ratio", y = "Thermal E (regression slope)")

# check that median E is robust to dropping poor fits
E_site <- E_amp_comparison %>%
  filter(is.finite(E)) %>%
  group_by(site_position) %>%
  summarise(E_all    = median(E),
            E_r2     = median(E[r2 >= 0.5], na.rm = TRUE),
            pct_kept = round(100 * mean(r2 >= 0.5, na.rm = TRUE)),
            .groups = "drop")
E_site


#### Supp Fig 1: stream vs air temperature ####
site_order_E <- temp_daily_Q %>%
  dplyr::distinct(site_position) %>%
  dplyr::mutate(n = as.numeric(stringr::str_extract(site_position, "\\d+"))) %>%
  dplyr::arrange(n, site_position) %>%
  dplyr::pull(site_position)

supp_fig1 <- temp_daily_Q %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_order_E)) %>%
  dplyr::filter(!is.na(site_position), !site_position %in% c("S10", "S13.a")) %>%
  ggplot(aes(air_T, stream_T, color = site_position)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_viridis_d(option = "C", begin = 0.15, end = 0.85) +
  labs(x = "Air temperature (°C)", y = "Stream temperature (°C)", color = "Site") +
  theme_classic(base_size = 11)

supp_fig1
#ggsave(plot = supp_fig1, file.path(fig_dir, "suppfig1.png"), width=6, height=5)


#### Supp Fig 2: sensitivity of E to method and time scale ####
# uses 15-min data rather than daily means
temp <- df %>%
  filter(wet_dry == "Dry", is.finite(Water_temp_C), is.finite(air_temp_par),
         site_position %in% site_order_E) %>%
  transmute(site_position = as.character(site_position), PCT,
            Day = as.Date(Day), Tw = Water_temp_C, Ta = air_temp_par)

slope_r2 <- function(x, y) {
  if (length(x) < 10 || var(x) == 0) return(c(E = NA_real_, r2 = NA_real_))
  c(E = cov(x, y) / var(x), r2 = cor(x, y)^2)
}

# E7: rolling 7-day regression, one value per site-day
E7 <- temp %>%
  group_by(site_position) %>%
  group_modify(function(d, key) {
    days <- sort(unique(d$Day))
    bind_rows(lapply(days, function(dd) {
      w <- d[d$Day > dd - 7 & d$Day <= dd, ]
      if (n_distinct(w$Day) < 5) return(NULL)        # require 5 of 7 days
      s <- slope_r2(w$Ta, w$Tw)
      tibble(Date = dd, E = s["E"], r2 = s["r2"])
    }))
  }) %>% ungroup()

# daily amplitude ratio
Amp <- temp %>%
  group_by(site_position, Date = Day) %>%
  filter(n() >= 72) %>%
  summarise(amplitude_ratio = diff(range(Tw)) / diff(range(Ta)), .groups = "drop") %>%
  filter(is.finite(amplitude_ratio))

# one slope per site at daily, weekly, monthly scales
E_sensitivity <- temp %>%
  group_by(site_position, Day) %>%
  summarise(Tw = mean(Tw), Ta = mean(Ta), .groups = "drop") %>%
  mutate(Daily = Day, Weekly = floor_date(Day, "week"), Monthly = floor_date(Day, "month")) %>%
  pivot_longer(c(Daily, Weekly, Monthly), names_to = "scale", values_to = "period") %>%
  group_by(site_position, scale, period) %>%
  summarise(Tw = mean(Tw), Ta = mean(Ta), .groups = "drop") %>%
  group_by(site_position, scale) %>%
  summarise(E = if (n() >= 3) unname(coef(lm(Tw ~ Ta))[2]) else NA_real_, .groups = "drop") %>%
  mutate(scale = factor(scale, levels = c("Daily", "Weekly", "Monthly")))

supp_site_order <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9",
                     "S11","S12","S13.b","S14","S15","S16","S17.b")

prep_sites <- function(d, value_col) {
  d %>%
    dplyr::mutate(site_position = factor(site_position, levels = supp_site_order)) %>%
    dplyr::filter(!is.na(site_position), is.finite(.data[[value_col]]))
}

E7            <- prep_sites(E7, "E")
Amp           <- prep_sites(Amp, "amplitude_ratio")
E_sensitivity <- prep_sites(E_sensitivity, "E")

# keep only sites present in all panels
sites_used <- Reduce(intersect, list(unique(as.character(E7$site_position)),
                                     unique(as.character(Amp$site_position)),
                                     unique(as.character(E_sensitivity$site_position))))
sites_used <- supp_site_order[supp_site_order %in% sites_used]

E7            <- E7            %>% dplyr::filter(site_position %in% sites_used) %>% droplevels()
Amp           <- Amp           %>% dplyr::filter(site_position %in% sites_used) %>% droplevels()
E_sensitivity <- E_sensitivity %>% dplyr::filter(site_position %in% sites_used) %>% droplevels()

x_sites <- scale_x_discrete(limits = sites_used)

# y-limits = whisker range across sites (ignores hidden outliers)
whisker_lim <- function(d, col) {
  s <- d %>% dplyr::group_by(site_position) %>%
    dplyr::summarise(lo = boxplot.stats(.data[[col]])$stats[1],
                     hi = boxplot.stats(.data[[col]])$stats[5], .groups = "drop")
  c(min(s$lo), max(s$hi))
}

small_titles <- theme(axis.title = element_text(size = 9),
                      plot.title = element_text(size = 10))
no_x_s2 <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

s2_E <- ggplot(E7, aes(site_position, E)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = whisker_lim(E7, "E")) + x_sites +
  labs(title = "A.", x = NULL, y = "Thermal sensitivity (E)") +
  theme_classic() + no_x_s2 + small_titles


s2_amp <- ggplot(Amp, aes(site_position, amplitude_ratio)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = whisker_lim(Amp, "amplitude_ratio")) + x_sites +
  labs(title = "B.", x = NULL, y = "Amplitude ratio") +
  theme_classic() + no_x_s2 + small_titles

s2_r2 <- ggplot(E7 %>% dplyr::filter(is.finite(r2)), aes(site_position, r2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0.5, linetype = "dashed") + x_sites +
  labs(title = "C.", x = NULL, y = expression(R^2)) +
  theme_classic() + no_x_s2

s2_scale <- ggplot(E_sensitivity, aes(site_position, E, color = scale, group = scale)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) + x_sites +
  scale_color_manual(values = c("#B8B8B8", "#777777", "#252525")) +
  labs(title = "D.", x = "Site", y = "Thermal sensitivity (E)", color = "Temporal scale") +
  theme_classic()

supp_fig2 <- s2_E / s2_amp / s2_r2 / s2_scale
supp_fig2
#ggsave(plot = supp_fig2, file.path(fig_dir, "supp_fig2.png"), width=7, height=5.5)


#### groundwater tracers / grab samples ####
# longitudinal radon profiles
radon_long_0827 <- gw_tracer %>%
  filter(Date == as.Date("2025-08-27")) %>%
  arrange(Distance_m)

ggplot(radon_long_0827, aes(x = Distance_m, y = Radon_pCi_L)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = site_position), nudge_y = 0.5, size = 4) +
  theme_minimal(base_size = 16) +
  labs(x = "Distance downstream (m)", y = expression(Radon~(pCi~L^-1)),
       title = "Longitudinal radon profile on 2025-08-27")

radon_long_1119 <- gw_tracer %>%
  filter(Date == as.Date("2025-11-19")) %>%
  arrange(Distance_m)

ggplot(radon_long_1119, aes(x = Distance_m, y = Radon_pCi_L)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = site_position), nudge_y = 0.5, size = 4) +
  theme_minimal(base_size = 16) +
  labs(x = "Distance downstream (m)", y = expression(Radon~(pCi~L^-1)),
       title = "Longitudinal radon profile on 2025-11-19")

# august synoptic (surface water) used for spatial comparisons
nut_0829 <- gw_tracer %>%
  filter(water_type == "Surface",
         Date >= as.Date("2025-08-27"), Date <= as.Date("2025-08-29")) %>%
  group_by(site_position) %>%
  summarise(Date = first(Date),
            across(c(DOC_mg_L, NH4_mg_L, NO3_mg_L, PO4_mg_L, TN_mg_L, TP_mg_L,
                     ALK_mg_L, DIC_mg_L),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
nut_0829


#### Supp Fig 3: surface vs groundwater chemistry ####
tracer_vars <- c("ALK_mg_L","DIC_mg_L","DOC_mg_L","NH4_mg_L","NO3_mg_L","PO4_mg_L",
                 "TN_mg_L","TP_mg_L","Radon_pCi_L","dD_avg","d18O_avg","Dexcess")

tracer_plot <- gw_tracer %>%
  dplyr::select(Date, water_type, dplyr::all_of(tracer_vars)) %>%
  tidyr::pivot_longer(-c(Date, water_type), names_to="variable", values_to="value") %>%
  dplyr::filter(is.finite(value)) %>%
  dplyr::mutate(
    Month=factor(format(Date, "%b"), levels=month.abb),
    water_type=dplyr::recode(water_type, "Surface"="SW", "Groundwater"="GW"),
    water_type=factor(water_type, levels=c("SW","GW")),
    variable=dplyr::recode(variable,
                           ALK_mg_L="'Alkalinity (mg/L)'",
                           DIC_mg_L="'DIC (mg/L)'",
                           DOC_mg_L="'DOC (mg/L)'",
                           NH4_mg_L="NH[4]~'(mg/L)'",
                           NO3_mg_L="NO[3]~'(mg/L)'",
                           PO4_mg_L="PO[4]~'(mg/L)'",
                           TN_mg_L="'TN (mg/L)'",
                           TP_mg_L="'TP (mg/L)'",
                           Radon_pCi_L="'Radon (pCi/L)'",
                           dD_avg="delta^2*H~('\u2030')",
                           d18O_avg="delta^18*O~('\u2030')",
                           Dexcess="'d-excess (\u2030)'"
    ),
    variable=factor(variable, levels=c(
      "'Alkalinity (mg/L)'", "'DIC (mg/L)'", "'DOC (mg/L)'",
      "NH[4]~'(mg/L)'", "NO[3]~'(mg/L)'", "PO[4]~'(mg/L)'",
      "'TN (mg/L)'", "'TP (mg/L)'", "'Radon (pCi/L)'",
      "delta^2*H~('\u2030')", "delta^18*O~('\u2030')", "'d-excess (\u2030)'"
    ))
  )

supp_fig3 <- ggplot(tracer_plot, aes(water_type, value)) +
  geom_boxplot(width=.6, outlier.shape=NA, fill="grey95") +
  geom_jitter(aes(color=Month), width=.12, size=1.7, alpha=.75) +
  facet_wrap(~variable, scales="free_y", ncol=4, labeller=label_parsed) +
  scale_color_viridis_d(option="C", end=.8) +
  labs(x=NULL, y=NULL, color="Month") +
  theme_classic() +
  theme(
    legend.position="bottom",
    strip.background=element_blank(),
    strip.text=element_text(size=9),
    axis.text.x=element_text(size=9)
  )

supp_fig3
#ggsave(plot = supp_fig3, file.path(fig_dir, "supp_fig3.png"), width=6, height=5)


#### Figure 2: watershed conditions ####
rects <- tibble::tribble(
  ~xmin, ~xmax, ~wet_dry,
  as.Date("2024-04-22"), as.Date("2024-06-17"), "Wet",
  as.Date("2024-06-17"), as.Date("2024-10-24"), "Dry",
  as.Date("2024-10-24"), as.Date("2025-06-22"), "Wet",
  as.Date("2025-06-22"), as.Date("2025-10-08"), "Dry",
  as.Date("2025-10-08"), as.Date("2025-11-30"), "Wet"
)

# daily, all sites pooled
daily <- df %>%
  dplyr::group_by(Day, wet_dry) %>%
  dplyr::summarise(
    PAR_day       = mean(PAR_understory, na.rm = TRUE),
    Tw_day        = mean(Water_temp_C, na.rm = TRUE),
    Tw_sd         = sd(Water_temp_C, na.rm = TRUE),
    Tw_min        = min(Water_temp_C, na.rm = TRUE),
    Tw_max        = max(Water_temp_C, na.rm = TRUE),
    DO_day        = mean(DO_mg_L, na.rm = TRUE),
    DO_sd         = sd(DO_mg_L, na.rm = TRUE),
    DO_min        = min(DO_mg_L, na.rm = TRUE),
    DO_max        = max(DO_mg_L, na.rm = TRUE),
    Discharge_day = mean(Discharge_L_s, na.rm = TRUE),
    Precip_day    = mean(Gage_precip_total_mm , na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Discharge_day = ifelse(Discharge_day > 0, Discharge_day, NA_real_)) %>%
  arrange(Day)

nut_daily <- nut %>%
  filter(Date >= min(daily$Day, na.rm = TRUE), Date <= max(daily$Day, na.rm = TRUE)) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(UTP_day = mean(utp, na.rm = TRUE),
                   UTN_day = mean(utn, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::rename(Day = Date)

# a. PAR
p_par <- ggplot(daily, aes(Day, PAR_day)) +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  annotate("text", x = min(daily$Day, na.rm = TRUE), y = Inf, label = "a",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = flow_colors) +
  geom_line(color = "black", linewidth = 1) +
  scale_x_date(expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  labs(x = NULL, y = expression("PAR ("*mu*"mol m"^{-2}*" s"^{-1}*")"), fill = NULL)

# b. discharge (log) + precip hanging from the top
daily2 <- daily %>%
  dplyr::mutate(
    Discharge_day = ifelse(is.finite(Discharge_day) & Discharge_day > 0, Discharge_day, NA_real_),
    Precip_day = ifelse(is.finite(Precip_day) & Precip_day >= 0, Precip_day, 0),
    logQ = log10(Discharge_day)
  ) %>%
  dplyr::filter(is.finite(logQ))

stopifnot(nrow(daily2) > 0)

logQ_min <- min(daily2$logQ, na.rm = TRUE)
logQ_max <- max(daily2$logQ, na.rm = TRUE)
logQ_top <- logQ_max + 0.30

# precip scaling onto the log Q axis
p_max <- max(daily2$Precip_day, na.rm = TRUE)
if (!is.finite(p_max) || p_max <= 0) p_max <- 1
precip_sf <- (logQ_top - logQ_min) / p_max * 0.9
if (!is.finite(precip_sf) || precip_sf <= 0) precip_sf <- 1

daily2 <- daily2 %>%
  dplyr::mutate(logQ_precip_end = logQ_top - Precip_day * precip_sf)

tick_Q <- 10^(floor(logQ_min):ceiling(logQ_top))
tick_logQ <- log10(tick_Q)

p_q <- ggplot() +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = logQ_min, ymax = logQ_top, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  scale_fill_manual(values = flow_colors) +
  geom_segment(
    data = daily2,
    aes(x = Day, xend = Day, y = logQ_top, yend = logQ_precip_end),
    inherit.aes = FALSE, color = "steelblue", linewidth = 1
  ) +
  geom_line(data = daily2, aes(x = Day, y = logQ), color = "black", linewidth = 1) +
  annotate("text", x = min(daily$Day, na.rm = TRUE), y = Inf, label = "b",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(
    name = expression(log[10]~Discharge~(L~s^{-1})),
    limits = c(logQ_min, logQ_top),
    expand = c(0, 0),
    breaks = tick_logQ,
    labels = scales::label_number()(10^tick_logQ),
    sec.axis = sec_axis(~ (logQ_top - .) / precip_sf, name = "Precipitation (mm)")
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.y.right = element_text(color = "steelblue"),
    axis.ticks.y.right = element_line(color = "steelblue")
  ) +
  labs(x = NULL, fill = NULL)

# c. nutrients
nut_long <- nut_daily %>%
  pivot_longer(c(UTP_day, UTN_day), names_to = "analyte", values_to = "value")

label_data <- nut_long %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(analyte) %>%
  dplyr::filter(Day == max(Day)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = ifelse(analyte == "UTN_day", "UTN", "UTP"))

p_nut <- nut_long %>%
  ggplot(aes(Day, value, color = analyte)) +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  scale_fill_manual(values = flow_colors) +
  geom_point(size = 1.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 0.8) +
  annotate("text", x = min(daily$Day, na.rm = TRUE), y = Inf, label = "c",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4) +
  geom_text(
    data = label_data, aes(label = label, x = Day, y = value),
    hjust = -0.15, size = 4.5, fontface = "bold"
  ) +
  scale_color_manual(values = c("UTP_day" = "black", "UTN_day" = "steelblue")) +
  coord_cartesian(xlim = range(daily$Day, na.rm = TRUE), expand = FALSE, clip = "off") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.margin = ggplot2::margin(5.5, 30, 5.5, 5.5)) +
  labs(x = NULL, y = "Concentration (mg/L)", fill = NULL)

# d. water temp + DO (DO rescaled onto temp axis)
tw_rng <- range(daily$Tw_day, na.rm = TRUE)
do_rng <- range(daily$DO_day, na.rm = TRUE)

do_to_tw <- function(do) (do - do_rng[1]) / diff(do_rng) * diff(tw_rng) + tw_rng[1]
tw_to_do <- function(tw) (tw - tw_rng[1]) / diff(tw_rng) * diff(do_rng) + do_rng[1]

p_twdo <- ggplot(daily, aes(Day)) +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  scale_fill_manual(values = flow_colors) +
  geom_ribbon(aes(ymin = do_to_tw(DO_min), ymax = do_to_tw(DO_max)),
              fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = Tw_min, ymax = Tw_max), fill = "gray40", alpha = 0.2) +
  annotate("text", x = min(daily$Day, na.rm = TRUE), y = Inf, label = "d",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4) +
  geom_line(aes(y = Tw_day), color = "black", linewidth = 1) +
  geom_line(aes(y = do_to_tw(DO_day)), color = "steelblue", linewidth = 1) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(
    name = "Water temperature (°C)",
    sec.axis = sec_axis(~ tw_to_do(.), name = expression("DO (mg L"^{-1}*")"))
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.y.right = element_text(color = "steelblue"),
    axis.ticks.y.right = element_line(color = "steelblue")
  ) +
  labs(x = NULL, fill = "Flow Period")

fig2_plot <- p_par / p_q / p_nut / p_twdo
fig2_plot
#ggsave(plot = fig2_plot, file.path(fig_dir, "fig2_plot.png"), width=8.5, height=8)


#### section 3.1 stats: wet vs dry ####
## sites with both wet and dry data
daily_multiyear <- df %>%
  filter(site_position %in% multi_wetdry_sites) %>%
  dplyr::group_by(Day, wet_dry, site_position) %>%
  dplyr::summarise(
    PAR_day       = mean(PAR_understory, na.rm = TRUE),
    Tw_day        = mean(Water_temp_C, na.rm = TRUE),
    DO_day        = mean(DO_mg_L, na.rm = TRUE),
    Discharge_day = mean(Discharge_L_s, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Discharge_day = ifelse(Discharge_day > 0, Discharge_day, NA_real_))

wet_dry_pooled <- daily_multiyear %>%
  filter(!is.na(wet_dry)) %>%
  dplyr::group_by(wet_dry) %>%
  dplyr::summarise(
    PAR_mean = round(mean(PAR_day, na.rm = TRUE), 1),
    PAR_sd   = round(sd(PAR_day, na.rm = TRUE), 1),
    Temp_mean = round(mean(Tw_day, na.rm = TRUE), 2),
    Temp_sd   = round(sd(Tw_day, na.rm = TRUE), 2),
    Discharge_mean = round(mean(Discharge_day, na.rm = TRUE), 3),
    Discharge_sd   = round(sd(Discharge_day, na.rm = TRUE), 3),
    DO_mean = round(mean(DO_day, na.rm = TRUE), 2),
    DO_sd   = round(sd(DO_day, na.rm = TRUE), 2),
    n_days = n(),
    .groups = "drop"
  )
print(wet_dry_pooled, n = Inf)

wilcox.test(Tw_day ~ wet_dry, data = daily_multiyear)
wilcox.test(Discharge_day ~ wet_dry, data = daily_multiyear)
wilcox.test(DO_day ~ wet_dry, data = daily_multiyear)
wilcox.test(PAR_day ~ wet_dry, data = daily_multiyear)

## watershed-scale daily conditions (DO and temp from multi-year sites only)
env_daily <- df %>%
  mutate(Day = as.Date(PCT), year = lubridate::year(Day)) %>%
  dplyr::group_by(Day, year, wet_dry) %>%
  dplyr::summarise(
    Q = mean(Discharge_L_s, na.rm = TRUE),
    PAR = mean(PAR_understory, na.rm = TRUE),
    precip = max(Gage_precip_total_mm, na.rm = TRUE),
    temp = mean(Water_temp_C[site_position %in% multi_year_sites], na.rm = TRUE),
    DO = mean(DO_sat_percent[site_position %in% multi_year_sites], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Day) %>%
  mutate(cumulative_precip = cumsum(replace_na(precip, 0))) %>%
  left_join(
    nut %>%
      dplyr::mutate(Day = as.Date(Date)) %>%
      dplyr::group_by(Day) %>%
      dplyr::summarise(
        UTN = mean(utn, na.rm = TRUE),
        UTP = mean(utp, na.rm = TRUE),
        TDN = mean(tdn, na.rm = TRUE),
        TDP = mean(tdp, na.rm = TRUE),
        NO3 = mean(no3n, na.rm = TRUE),
        NH4 = mean(nh4n, na.rm = TRUE),
        DOC = mean(doc, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Day"
  )

nut %>%
  dplyr::summarise(UTN_n = sum(!is.na(utn)), TDN_n = sum(!is.na(tdn)),
                   UTP_n = sum(!is.na(utp)), TDP_n = sum(!is.na(tdp)))

df %>%
  filter(site_position %in% multi_year_sites) %>%
  dplyr::group_by(wet_dry) %>%
  dplyr::summarise(
    DO_min = min(DO_sat_percent, na.rm = TRUE),
    DO_max = max(DO_sat_percent, na.rm = TRUE),
    DO_range = DO_max - DO_min,
    .groups = "drop"
  )

env_summary <- env_daily %>%
  dplyr::group_by(wet_dry) %>%
  dplyr::summarise(
    n_days = n(),
    Q_mean = mean(Q, na.rm = TRUE), Q_median = median(Q, na.rm = TRUE), Q_sd = sd(Q, na.rm = TRUE),
    temp_mean = mean(temp, na.rm = TRUE), temp_sd = sd(temp, na.rm = TRUE),
    PAR_mean = mean(PAR, na.rm = TRUE), PAR_sd = sd(PAR, na.rm = TRUE),
    DO_mean = mean(DO, na.rm = TRUE), DO_sd = sd(DO, na.rm = TRUE),
    precip_mean = mean(precip, na.rm = TRUE), precip_sd = sd(precip, na.rm = TRUE),
    precip_total = sum(precip, na.rm = TRUE),
    UTN_mean = mean(UTN, na.rm = TRUE), UTN_sd = sd(UTN, na.rm = TRUE),
    UTP_mean = mean(UTP, na.rm = TRUE), UTP_sd = sd(UTP, na.rm = TRUE),
    TDN_mean = mean(TDN, na.rm = TRUE), TDN_sd = sd(TDN, na.rm = TRUE),
    TDP_mean = mean(TDP, na.rm = TRUE), TDP_sd = sd(TDP, na.rm = TRUE),
    NO3_mean = mean(NO3, na.rm = TRUE), NO3_sd = sd(NO3, na.rm = TRUE),
    NH4_mean = mean(NH4, na.rm = TRUE), NH4_sd = sd(NH4, na.rm = TRUE),
    DOC_mean = mean(DOC, na.rm = TRUE), DOC_sd = sd(DOC, na.rm = TRUE),
    .groups = "drop"
  )
print(env_summary, width = Inf)

# wet vs dry comparisons
wilcox.test(Q ~ wet_dry, data = env_daily)
wilcox.test(temp ~ wet_dry, data = env_daily)
wilcox.test(PAR ~ wet_dry, data = env_daily)
wilcox.test(DO ~ wet_dry, data = env_daily)
wilcox.test(precip ~ wet_dry, data = env_daily)

wilcox.test(UTN ~ wet_dry, data = env_daily)
wilcox.test(UTP ~ wet_dry, data = env_daily)
wilcox.test(TDN ~ wet_dry, data = env_daily)
wilcox.test(TDP ~ wet_dry, data = env_daily)
wilcox.test(NO3 ~ wet_dry, data = env_daily)
wilcox.test(NH4 ~ wet_dry, data = env_daily)
wilcox.test(DOC ~ wet_dry, data = env_daily)

## DO by year and period (daily means first)
year_DO_stats <- df %>%
  filter(site_position %in% multi_year_sites) %>%
  mutate(Day = as.Date(PCT), year = lubridate::year(Day)) %>%
  dplyr::group_by(site_position, year, Day, wet_dry) %>%
  dplyr::summarise(DO_daily = mean(DO_sat_percent, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(year, wet_dry) %>%
  dplyr::summarise(
    mean_DO = mean(DO_daily, na.rm = TRUE),
    sd_DO = sd(DO_daily, na.rm = TRUE),
    var_DO = var(DO_daily, na.rm = TRUE),
    DO_min = min(DO_daily, na.rm = TRUE),
    DO_max = max(DO_daily, na.rm = TRUE),
    DO_range = DO_max - DO_min,
    n = n(),
    .groups = "drop"
  )
year_DO_stats

## hypoxia
thr      <- 50   # % saturation (Carter et al., 2021)
step_min <- 15   # sensor interval in minutes

hyp <- df %>%
  filter(!is.na(DO_sat_percent), !is.na(site_position)) %>%
  mutate(PCT = as.POSIXct(PCT), year = year(PCT),
         hypoxic = DO_sat_percent < thr) %>%
  arrange(site_position, PCT)

# each continuous run below the threshold = one event
events <- hyp %>%
  group_by(site_position) %>%
  mutate(gap      = as.numeric(difftime(PCT, lag(PCT), units = "mins")),
         new      = hypoxic & (!lag(hypoxic, default = FALSE) | coalesce(gap, Inf) > step_min),
         event_id = cumsum(new)) %>%
  filter(hypoxic) %>%
  group_by(site_position, event_id) %>%
  summarise(year = first(year), wet_dry = first(wet_dry),
            start = min(PCT), end = max(PCT),
            duration_h = n() * step_min / 60,
            min_DO = min(DO_sat_percent), .groups = "drop")

# frequency and duration by site and year
site_hyp <- hyp %>%
  group_by(site_position, year) %>%
  summarise(n_obs = n(), n_hyp = sum(hypoxic),
            pct_time = 100 * mean(hypoxic),
            n_days_hyp = n_distinct(as.Date(PCT)[hypoxic]),
            min_DO = min(DO_sat_percent), .groups = "drop") %>%
  left_join(events %>% group_by(site_position, year) %>%
              summarise(n_events = n(), median_dur_h = median(duration_h),
                        max_dur_h = max(duration_h), total_h = sum(duration_h),
                        first_event = min(start), last_event = max(end), .groups = "drop"),
            by = c("site_position", "year")) %>%
  filter(n_hyp > 0) %>%
  arrange(desc(pct_time))

print(site_hyp, n = Inf, width = Inf)
events %>% count(wet_dry)                                 # all in the dry period?
events %>% arrange(desc(duration_h)) %>% print(n = 10)    # longest events
hyp %>% filter(site_position == "S2") %>% summarise(min_DO = min(DO_sat_percent))   # outlet check
events %>% mutate(hour = lubridate::hour(start)) %>%
  count(night = hour >= 18 | hour < 6)


#### Figure 3: 2024 vs 2025 dry season ####
stars <- function(p) dplyr::case_when(p < 0.001 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
yr <- function(x) factor(lubridate::year(x))
upper_whisker <- function(v) boxplot.stats(v)$stats[5]
keep_years <- function(d) dplyr::filter(d, year %in% c("2024", "2025")) %>% droplevels()

year_box <- function(d, var, ylab, title, log = FALSE) {
  d <- dplyr::filter(d, is.finite(.data[[var]]), !log | .data[[var]] > 0)
  p <- wilcox.test(d[[var]] ~ d$year)$p.value
  top <- max(tapply(d[[var]], d$year, upper_whisker))
  y_lab <- if (log) top * 1.25 else top + 0.06 * diff(range(d[[var]]))
  g <- ggplot(d, aes(year, .data[[var]], fill = year)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    annotate("text", x = 1.5, y = y_lab, label = stars(p), size = 6, fontface = "bold") +
    scale_fill_manual(values = year_colors) +
    std_theme + theme(legend.position = "none") +
    labs(x = NULL, y = ylab, title = tolower(gsub("[^A-Za-z]", "", title)))
  if (log) g + scale_y_log10() else g
}

## data
# calendar days sampled in both years
overlapping_md <- all_day %>%
  dplyr::mutate(year = lubridate::year(Date), md = format(Date, "%m-%d")) %>%
  dplyr::group_by(md) %>%
  dplyr::filter(dplyr::n_distinct(year) == 2) %>%
  dplyr::pull(md) %>%
  unique()

overlapping_all_dates <- all_day %>%
  dplyr::filter(format(Date, "%m-%d") %in% overlapping_md) %>%
  dplyr::pull(Date) %>%
  unique()

# sites with dry-season DO in both years
common_do_sites <- daily_by_site %>%
  dplyr::filter(wet_dry == "Dry", !is.na(daily_mean)) %>%
  dplyr::distinct(site_position, y = lubridate::year(Day)) %>%
  dplyr::count(site_position) %>% dplyr::filter(n == 2) %>% dplyr::pull(site_position)

air_temp_data <- df %>%
  mutate(Year = year(Day),
         month_day = as.Date(paste0("2024-", format(Day, "%m-%d")))) %>%
  filter(!is.na(air_temp_par), month(Day) %in% 7:11) %>%
  dplyr::group_by(Day, Year, month_day) %>%
  dplyr::summarise(air_temp_par = mean(air_temp_par, na.rm = TRUE), .groups = "drop")

do_plot      <- daily_by_site %>% dplyr::filter(wet_dry == "Dry", site_position %in% common_do_sites) %>%
  dplyr::mutate(year = yr(Day))
q_plot       <- all_day %>% dplyr::filter(Date %in% overlapping_all_dates) %>% dplyr::mutate(year = yr(Date))
thermal_plot <- E_amp_comparison %>% dplyr::filter(site_position %in% common_do_sites) %>%
  dplyr::mutate(year = yr(Date))
light_plot   <- df %>% dplyr::filter(site_position %in% common_do_sites) %>% dplyr::mutate(year = yr(PCT))
air_plot     <- air_temp_data %>% dplyr::mutate(year = factor(Year))

precip_totals <- df %>%
  dplyr::filter(wet_dry == "Dry") %>%
  dplyr::distinct(PCT, Gage_precip_total_mm) %>%            # one gage value per timestamp
  dplyr::mutate(year = yr(PCT)) %>% keep_years() %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(total_mm = sum(Gage_precip_total_mm, na.rm = TRUE), .groups = "drop")
precip_totals

# stream surveys (entered manually)
surveys_meta <- tribble(
  ~Survey, ~Date, ~Year, ~PctDry, ~PctNotFlowing,
  7,  as.Date("2024-07-16"), 2024, 0, 0,
  8,  as.Date("2024-08-29"), 2024, 12.3, 2.564,
  9,  as.Date("2024-09-11"), 2024, 13.2, 2.930,
  10, as.Date("2024-10-24"), 2024, 10.4, 0,
  11, as.Date("2024-11-09"), 2024, 0, 0,
  15, as.Date("2025-07-01"), 2025, 0, 0,
  12, as.Date("2025-08-05"), 2025, 18.5, 8.974,
  13, as.Date("2025-08-26"), 2025, 25.6, 8.791,
  14, as.Date("2025-11-20"), 2025, 0, 0
) %>%
  dplyr::mutate(
    PctFlowing = 100 - PctDry - PctNotFlowing,
    SurveyLabel = format(Date, "%m-%d"),
    SurveyOrder = row_number()
  )

surveys_meta_long <- surveys_meta %>%
  dplyr::select(SurveyLabel, Date, Year, PctFlowing, PctNotFlowing, PctDry) %>%
  pivot_longer(cols = c(PctFlowing, PctNotFlowing, PctDry),
               names_to = "Condition", values_to = "Percentage") %>%
  mutate(
    Condition = dplyr::recode(Condition, PctFlowing = "Flowing",
                              PctNotFlowing = "Non-flowing", PctDry = "Dry"),
    Condition = factor(Condition, levels = c("Flowing", "Non-flowing", "Dry"))
  )
print(surveys_meta_long, n = Inf)

## panels a-e
p_do    <- year_box(keep_years(do_plot),      "daily_mean",    "Daily mean DO (%)",     "A.")
p_q     <- year_box(keep_years(q_plot),       "mean_Q",        "Discharge (L/s)",       "B.", log = TRUE)
p_E     <- year_box(keep_years(thermal_plot), "E",             "Thermal sensitivity",   "C.")
p_light <- year_box(keep_years(light_plot),   "benthic_light",
                    expression("Benthic light ("*mu*"mol m"^{-2}*" s"^{-1}*")"), "D.", log = TRUE)
p_air   <- year_box(keep_years(air_plot),     "air_temp_par",  "Air temperature (°C)",  "E.")

## f. dry-season precip total
p_precip <- ggplot(precip_totals, aes(year, total_mm, fill = year)) +
  geom_col(alpha = 0.7, width = 0.6, colour = "grey20") +
  geom_text(aes(label = round(total_mm, 1)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = year_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  std_theme + theme(legend.position = "none") +
  labs(x = NULL, y = "Cumulative precipitation (mm)", title = "f")

## g. nutrients
nut_plot <- nut %>%
  dplyr::filter(Date %in% overlapping_all_dates) %>%
  dplyr::mutate(year = yr(Date)) %>% keep_years() %>%
  tidyr::pivot_longer(c(utn, tdn, utp, tdp), names_to = "Nutrient", values_to = "Concentration") %>%
  dplyr::filter(is.finite(Concentration)) %>%
  dplyr::mutate(Nutrient = factor(toupper(Nutrient), levels = c("UTN", "TDN", "UTP", "TDP")))

nut_sig <- nut_plot %>%
  dplyr::group_by(Nutrient) %>%
  dplyr::summarise(p = wilcox.test(Concentration ~ year)$p.value,
                   label = stars(p),
                   y = max(tapply(Concentration, year, upper_whisker)) * 1.08, .groups = "drop")

p_nut <- ggplot(nut_plot, aes(Nutrient, Concentration, fill = year)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7, outlier.shape = NA) +
  geom_text(data = nut_sig, aes(Nutrient, y, label = label), inherit.aes = FALSE,
            size = 6, fontface = "bold") +
  scale_fill_manual(values = year_colors, name = "Year") +
  std_theme + theme(legend.position = "bottom") +
  labs(x = NULL, y = "Concentration (mg/L)", title = "g")

## h. reach connectivity
p_reach <- ggplot(surveys_meta_long, aes(reorder(SurveyLabel, Date), Percentage, fill = Condition)) +
  geom_col(alpha = 0.8, width = 0.7) +
  facet_wrap(~Year, scales = "free_x") +
  scale_fill_manual(values = c(Flowing = "#1b9e77", "Non-flowing" = "#fee5d9", Dry = "#d95f02")) +
  scale_y_continuous(limits = c(0, 100), oob = scales::squish, expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  std_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8)) +
  labs(x = NULL, y = "% of reach", title = "h")

# h gets extra width
design <- "
AABBCCDDD
EEFFGGHHH
"
fig3_plot <- p_do + p_q + p_E + p_light + p_air + p_precip + p_nut + p_reach +
  patchwork::plot_layout(design = design)
fig3_plot
#ggsave(plot = fig3_plot, file.path(fig_dir, "fig3_plot.png"), width = 10, height = 5.5)


#### section 3.2 stats: between years ####
fig_stats <- function(data, value, year) {
  data %>%
    dplyr::group_by({{ year }}) %>%
    dplyr::summarise(
      Mean = mean({{ value }}, na.rm = TRUE),
      SD = sd({{ value }}, na.rm = TRUE),
      Median = median({{ value }}, na.rm = TRUE),
      Min = min({{ value }}, na.rm = TRUE),
      Max = max({{ value }}, na.rm = TRUE),
      N = sum(is.finite({{ value }})),
      .groups = "drop"
    )
}

# same test as the stars in year_box()
year_wilcox <- function(d, var, log = FALSE) {
  d <- dplyr::filter(d, is.finite(.data[[var]]), !log | .data[[var]] > 0)
  wt <- wilcox.test(d[[var]] ~ d$year)
  tibble::tibble(W = unname(wt$statistic), p = wt$p.value)
}

do_stats      <- fig_stats(do_plot, daily_mean, year)
q_stats       <- fig_stats(q_plot, mean_Q, year)
thermal_stats <- fig_stats(thermal_plot, E, year)
light_stats   <- fig_stats(light_plot, benthic_light, year)
air_stats     <- fig_stats(air_plot, air_temp_par, Year)

do_sig      <- year_wilcox(keep_years(do_plot),      "daily_mean")
q_sig       <- year_wilcox(keep_years(q_plot),       "mean_Q", log = TRUE)
thermal_sig <- year_wilcox(keep_years(thermal_plot), "E")
light_sig   <- year_wilcox(keep_years(light_plot),   "benthic_light", log = TRUE)
air_sig     <- year_wilcox(keep_years(air_plot),     "air_temp_par")

nut_stats <- nut_plot %>%
  dplyr::group_by(Nutrient, year) %>%
  dplyr::summarise(
    Mean = mean(Concentration, na.rm = TRUE),
    SD = sd(Concentration, na.rm = TRUE),
    Median = median(Concentration, na.rm = TRUE),
    Min = min(Concentration, na.rm = TRUE),
    Max = max(Concentration, na.rm = TRUE),
    N = dplyr::n(),
    .groups = "drop"
  )

do_stats;      do_sig
q_stats;       q_sig
thermal_stats; thermal_sig
light_stats;   light_sig
air_stats;     air_sig
nut_stats;     nut_sig

## thermal sensitivity: size of the 2024 vs 2025 difference
thermal_plot %>%
  group_by(year) %>%
  summarise(n = n(), mean = mean(E), sd = sd(E),
            min = min(E), q25 = quantile(E, 0.25), median = median(E),
            q75 = quantile(E, 0.75), max = max(E))

e24 <- thermal_plot$E[thermal_plot$year == "2024"]
e25 <- thermal_plot$E[thermal_plot$year == "2025"]

wt <- wilcox.test(e25, e24, conf.int = TRUE, exact = FALSE)
tibble(median_diff    = median(e25) - median(e24),
       shift_estimate = unname(wt$estimate),            # typical 2025 - 2024 difference
       ci_low         = wt$conf.int[1],
       ci_high        = wt$conf.int[2],
       prob_2025_higher = unname(wt$statistic) / (length(e25) * length(e24)),
       diff_vs_spread = (median(e25) - median(e24)) / IQR(thermal_plot$E),
       p              = wt$p.value)

# by site: did each site shift, and by how much
thermal_plot %>%
  group_by(site_position, year) %>%
  summarise(median_E = median(E), n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = year, values_from = c(median_E, n)) %>%
  mutate(diff = median_E_2025 - median_E_2024) %>%
  arrange(desc(diff))

site_E <- thermal_plot %>%
  group_by(site_position, year) %>%
  summarise(E = median(E), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = year, values_from = E)

wilcox.test(site_E$`2025`, site_E$`2024`, paired = TRUE)

## site-specific dry-season DO change between years
site_DO_change <- df %>%
  dplyr::mutate(Day = as.Date(PCT), year = lubridate::year(Day)) %>%
  dplyr::filter(site_position %in% multi_year_sites, wet_dry == "Dry",
                year %in% c(2024, 2025), !is.na(DO_sat_percent)) %>%
  dplyr::group_by(site_position, year, Day) %>%
  dplyr::summarise(DO_daily = mean(DO_sat_percent, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(site_position, year) %>%
  dplyr::summarise(
    mean_DO  = mean(DO_daily, na.rm = TRUE),
    min_DO   = min(DO_daily, na.rm = TRUE),
    max_DO   = max(DO_daily, na.rm = TRUE),
    range_DO = max_DO - min_DO,
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = year, values_from = c(mean_DO, min_DO, max_DO, range_DO)) %>%
  dplyr::mutate(
    mean_change  = mean_DO_2025 - mean_DO_2024,
    min_change   = min_DO_2025 - min_DO_2024,
    range_change = range_DO_2025 - range_DO_2024,
    direction = dplyr::case_when(
      mean_change < 0 ~ "Decreased",
      mean_change > 0 ~ "Increased",
      TRUE ~ "No change"
    )
  ) %>%
  dplyr::arrange(mean_change)
site_DO_change


#### Figure 4: spatial vs temporal DO variability ####
window <- 28   # rolling window (days)

add_wd_segments <- function(data) {
  data %>%
    arrange(Day) %>%
    mutate(
      wet_dry = case_when(
        Day >= as.Date("2024-04-22") & Day <= as.Date("2024-06-17") ~ "Wet",
        Day >  as.Date("2024-06-17") & Day <  as.Date("2024-10-24") ~ "Dry",
        Day >= as.Date("2024-10-24") & Day <= as.Date("2025-04-01") ~ "Wet",
        Day >= as.Date("2025-04-02") & Day <= as.Date("2025-06-22") ~ "Wet",
        Day >  as.Date("2025-06-22") & Day <  as.Date("2025-10-08") ~ "Dry",
        Day >= as.Date("2025-10-08") & Day <= as.Date("2025-11-22") ~ "Wet",
        TRUE ~ NA_character_
      ),
      seg_id = cumsum(c(1, diff(as.integer(factor(wet_dry))) != 0))
    ) %>%
    filter(!is.na(wet_dry))
}

site_matrix_long_sat <- df %>%
  mutate(Day = as.Date(PCT)) %>%
  filter(!is.na(DO_sat_percent)) %>%
  dplyr::group_by(site_position, Day) %>%
  dplyr::summarise(
    DO_daily = mean(DO_sat_percent, na.rm = TRUE),
    n_obs    = n(),
    wet_dry  = first(wet_dry),
    .groups  = "drop"
  ) %>%
  filter(n_obs >= 72)

all_days_sat <- sort(unique(site_matrix_long_sat$Day))

# temporal CV = mean within-site CV; spatial CV = CV of site means
rolling_cv_sat <- map_dfr(seq(window, length(all_days_sat)), function(i) {
  window_days <- all_days_sat[(i - window + 1):i]
  sub <- site_matrix_long_sat %>% filter(Day %in% window_days)
  
  cv_temp <- sub %>%
    dplyr::group_by(site_position) %>%
    dplyr::summarise(cv = sd(DO_daily, na.rm = TRUE) / mean(DO_daily, na.rm = TRUE),
                     n  = sum(!is.na(DO_daily)), .groups = "drop") %>%
    filter(n >= 5) %>%
    dplyr::summarise(cv_temporal = mean(cv, na.rm = TRUE))
  
  cv_spat <- sub %>%
    dplyr::group_by(site_position) %>%
    dplyr::summarise(site_period_mean = mean(DO_daily, na.rm = TRUE),
                     n_days = sum(!is.na(DO_daily)), .groups = "drop") %>%
    filter(n_days >= 3) %>%
    dplyr::summarise(cv_spatial = sd(site_period_mean, na.rm = TRUE) / mean(site_period_mean, na.rm = TRUE))
  
  tibble(Day = all_days_sat[i], cv_temporal = cv_temp$cv_temporal,
         cv_spatial = cv_spat$cv_spatial, wet_dry = last(na.omit(sub$wet_dry)))
})

x_lims_sat <- range(rolling_cv_sat$Day, na.rm = TRUE)

cv_long_sat <- rolling_cv_sat %>%
  pivot_longer(cols = c(cv_temporal, cv_spatial), names_to = "type", values_to = "cv") %>%
  mutate(type = case_match(type, "cv_temporal" ~ "Temporal", "cv_spatial" ~ "Spatial")) %>%
  add_wd_segments()

## plot pieces
discharge_daily <- df %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(Q = mean(Discharge_L_s, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Q), Q > 0, Day >= x_lims_sat[1], Day <= x_lims_sat[2])

common_theme <- theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

common_x_axis <- scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                              limits = x_lims_sat, expand = c(0, 0))

# wet/dry shading from the data
rects_fig4 <- df %>%
  dplyr::filter(Day >= x_lims_sat[1], Day <= x_lims_sat[2]) %>%
  dplyr::distinct(Day, wet_dry) %>%
  dplyr::arrange(Day) %>%
  dplyr::filter(!is.na(wet_dry)) %>%
  dplyr::mutate(
    new_period = wet_dry != dplyr::lag(wet_dry, default = dplyr::first(wet_dry)),
    period = cumsum(new_period)
  ) %>%
  dplyr::group_by(period, wet_dry) %>%
  dplyr::summarise(xmin = min(Day), xmax = max(Day), .groups = "drop") %>%
  dplyr::mutate(
    xmin = dplyr::if_else(dplyr::row_number() == 1, x_lims_sat[1], xmin),
    xmax = dplyr::lead(xmin, default = x_lims_sat[2])
  )

# a. discharge
q_min <- min(discharge_daily$Q, na.rm = TRUE)
q_max <- max(discharge_daily$Q, na.rm = TRUE)

p_discharge <- ggplot(discharge_daily, aes(Day, Q)) +
  geom_rect(
    data = rects_fig4,
    aes(xmin = xmin, xmax = xmax, ymin = q_min, ymax = q_max, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  geom_line(color = "black", linewidth = 0.6) +
  scale_fill_manual(values = flow_colors, breaks = c("Wet", "Dry"), name = "Flow Period") +
  common_x_axis +
  scale_y_log10(expand = c(0, 0)) +
  common_theme +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  ) +
  labs(x = NULL, y = expression(log[10]~Q~(L~s^-1)), title = "a ")

# b. rolling CV
p_cv_sat_clean <- ggplot(cv_long_sat, aes(Day, cv, group = interaction(type, seg_id), linetype = type)) +
  geom_rect(
    data = rects_fig4,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = wet_dry),
    inherit.aes = FALSE, alpha = 0.30
  ) +
  geom_line(color = "black", linewidth = 0.9) +
  scale_fill_manual(values = flow_colors, guide = "none") +
  scale_linetype_manual(
    values = c("Temporal" = "solid", "Spatial" = "dashed"),
    labels = c("Temporal (solid)", "Spatial (dashed)"),
    name = "CV Component",
    guide = guide_legend(
      override.aes = list(linetype = c("solid", "dashed"), linewidth = 1),
      keywidth = unit(1, "cm")
    )
  ) +
  common_x_axis +
  scale_y_continuous(name = "CV (DO sat)", expand = c(0, 0)) +
  common_theme +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  ) +
  labs(x = NULL, title = "b")

# c. daily mean DO by site
wet_dry_transitions <- df %>%
  dplyr::distinct(Day, wet_dry) %>%
  dplyr::arrange(Day) %>%
  dplyr::filter(Day >= x_lims_sat[1], Day <= x_lims_sat[2]) %>%
  dplyr::mutate(prev_wd = dplyr::lag(wet_dry)) %>%
  dplyr::filter(!is.na(prev_wd), wet_dry != prev_wd) %>%
  dplyr::mutate(label = paste(prev_wd, "→", wet_dry))

p_tile <- daily_by_site %>%
  mutate(site_position = factor(site_position, levels = site_positions)) %>%
  ggplot(aes(Day, site_position, fill = daily_mean)) +
  geom_tile(height = 1) +
  common_x_axis +
  scale_fill_gradientn(colors = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4"),
                       name = "Daily mean\nDO (%)") +
  geom_vline(data = wet_dry_transitions, aes(xintercept = Day),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  common_theme +
  theme(
    panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.4),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "right") +
  labs(x = "Date", y = NULL, title ="c")

# bold y labels for the core (multi-year) sites
core_lab <- function(x) ifelse(x %in% multi_year_sites,
                               paste0("<b style='color:black'>", x, "</b>"),
                               paste0("<span style='color:grey35'>", x, "</span>"))

p_tile <- daily_by_site %>%
  mutate(site_position = factor(site_position, levels = site_positions)) %>%
  ggplot(aes(Day, site_position, fill = daily_mean)) +
  geom_tile(height = 1) +
  common_x_axis +
  scale_y_discrete(labels = core_lab) +
  scale_fill_gradientn(colors = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4"),
                       name = "Daily mean\nDO (%)") +
  geom_vline(data = wet_dry_transitions, aes(xintercept = Day),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  common_theme +
  theme(
    panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.4),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggtext::element_markdown(),
    legend.position = "right") +
  labs(x = "Date", y = NULL, title = "c")

fig4_plot <- p_discharge / p_cv_sat_clean / p_tile + plot_layout(heights = c(0.8, 0.8, 2))
fig4_plot
#ggsave(plot=fig4_plot, file.path(fig_dir, "fig4_plot.png"), width=8, height=6.5)

## CV stats
cv_comparison <- rolling_cv_sat %>%
  dplyr::filter(is.finite(cv_temporal), is.finite(cv_spatial)) %>%
  dplyr::summarise(
    n_days = dplyr::n(),
    pct_temporal_gt_spatial = 100 * mean(cv_temporal > cv_spatial),
    pct_spatial_gt_temporal = 100 * mean(cv_spatial > cv_temporal)
  )
cv_comparison

dry_cv_range <- rolling_cv_sat %>%
  dplyr::filter(wet_dry == "Dry", is.finite(cv_temporal), is.finite(cv_spatial)) %>%
  dplyr::summarise(
    temporal_min = min(cv_temporal), temporal_max = max(cv_temporal),
    spatial_min = min(cv_spatial),   spatial_max = max(cv_spatial)
  )
dry_cv_range

cv_by_period <- cv_long_sat %>%
  dplyr::group_by(wet_dry, type) %>%
  dplyr::summarise(
    Mean_CV = mean(cv, na.rm = TRUE),
    SD_CV = sd(cv, na.rm = TRUE),
    Min_CV = min(cv, na.rm = TRUE),
    Max_CV = max(cv, na.rm = TRUE),
    .groups = "drop"
  )
cv_by_period


#### Supp Fig 4: CV sensitivity to window length and number of sites ####
calc_rolling_cv <- function(data, window = 28, sites = NULL) {
  if (!is.null(sites)) data <- data %>% dplyr::filter(site_position %in% sites)
  all_days <- sort(unique(data$Day))
  
  purrr::map_dfr(seq(window, length(all_days)), function(i) {
    window_days <- all_days[(i - window + 1):i]
    sub <- data %>% dplyr::filter(Day %in% window_days)
    
    cv_temporal <- sub %>%
      dplyr::group_by(site_position) %>%
      dplyr::summarise(cv = sd(DO_daily, na.rm = TRUE) / mean(DO_daily, na.rm = TRUE),
                       n_days = sum(!is.na(DO_daily)), .groups = "drop") %>%
      dplyr::filter(n_days >= 5) %>%
      dplyr::summarise(cv = mean(cv, na.rm = TRUE)) %>%
      dplyr::pull(cv)
    
    cv_spatial <- sub %>%
      dplyr::group_by(site_position) %>%
      dplyr::summarise(DO_mean = mean(DO_daily, na.rm = TRUE),
                       n_days = sum(!is.na(DO_daily)), .groups = "drop") %>%
      dplyr::filter(n_days >= 3) %>%
      dplyr::summarise(cv = sd(DO_mean, na.rm = TRUE) / mean(DO_mean, na.rm = TRUE)) %>%
      dplyr::pull(cv)
    
    tibble(Day = all_days[i], cv_temporal = cv_temporal, cv_spatial = cv_spatial,
           cv_ratio = cv_spatial / cv_temporal)
  })
}

# window length
window_sensitivity <- purrr::map_dfr(c(14, 21, 28, 42), function(w) {
  calc_rolling_cv(site_matrix_long_sat, window = w) %>%
    dplyr::mutate(window = paste0(w, " days"))
}) %>%
  dplyr::mutate(window = factor(window, levels = c("14 days", "21 days", "28 days", "42 days")))

# number of sites: all combinations of the multi-year sites
cv_all <- calc_rolling_cv(site_matrix_long_sat, window = 28) %>%
  dplyr::select(Day, cv_ratio)

core_sites <- multi_year_sites

site_sensitivity <- purrr::map_dfr(2:length(core_sites), function(n_sensors) {
  site_combos <- combn(core_sites, n_sensors, simplify = FALSE)
  purrr::map_dfr(seq_along(site_combos), function(i) {
    calc_rolling_cv(site_matrix_long_sat, window = 28, sites = site_combos[[i]]) %>%
      dplyr::mutate(n_sensors = n_sensors, combination = i)
  })
})

site_sensitivity_summary <- site_sensitivity %>%
  dplyr::group_by(n_sensors, Day) %>%
  dplyr::summarise(
    n_valid = sum(is.finite(cv_ratio)),
    median_ratio = if (any(is.finite(cv_ratio))) median(cv_ratio[is.finite(cv_ratio)]) else NA_real_,
    .groups = "drop"
  )

# fill periods without enough core-site coverage with the all-site CV
site_sensitivity_plot <- tidyr::expand_grid(Day = cv_all$Day, n_sensors = 2:5) %>%
  dplyr::left_join(site_sensitivity_summary, by = c("Day", "n_sensors")) %>%
  dplyr::left_join(cv_all, by = "Day") %>%
  dplyr::mutate(
    ratio_plot = dplyr::if_else(is.finite(median_ratio), median_ratio, cv_ratio),
    n_sensors = factor(n_sensors, levels = 2:5,
                       labels = c("2 sensors", "3 sensors", "4 sensors", "5 sensors"))
  )

x_min <- min(window_sensitivity$Day, site_sensitivity_summary$Day, na.rm = TRUE)
x_max <- max(window_sensitivity$Day, site_sensitivity_summary$Day, na.rm = TRUE)
date_breaks <- seq(as.Date(format(x_min, "%Y-%m-01")), x_max, by = "3 months")

p_cv_window <- ggplot(window_sensitivity, aes(Day, cv_ratio, color = window)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  annotate("text", x = x_min + 10, y = 1, label = "Spatial = temporal",
           hjust = 0, vjust = 1.5, size = 3) +
  scale_color_manual(values = c("#B8B8B8", "#8A8A8A", "#5E5E5E", "#252525")) +
  scale_x_date(limits = c(x_min, x_max), breaks = date_breaks,
               date_labels = "%b\n%Y", expand = c(0, 0)) +
  labs(title="A.", x = NULL, y = "Spatial : temporal CV", color = "Window length") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

p_cv_sites <- ggplot(site_sensitivity_plot, aes(Day, ratio_plot, color = n_sensors)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = c("#B8B8B8", "#8A8A8A", "#5E5E5E", "#252525")) +
  scale_x_date(limits = c(x_min, x_max), date_breaks = "3 months",
               date_labels = "%b\n%Y", expand = c(0, 0)) +
  labs(title="B.", x = "Date", y = "Spatial : temporal CV", color = "Network size") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

supp_fig4 <- p_cv_window / p_cv_sites
supp_fig4
#ggsave(plot=supp_fig4, file.path(fig_dir, "supp_fig4.png"), width=6.5, height=5.5)


#### Figure 5: Kendall's tau, DO vs drivers (dry season) ####
dry_windows <- tibble::tibble(
  Year      = c(2024, 2025),
  dry_start = as.Date(c("2024-06-17", "2025-06-22")),
  dry_end   = as.Date(c("2024-10-24", "2025-10-08"))
)

nut_cols <- c("ph", "alk", "ssed", "si", "utp", "tdp", "po4p",
              "utn", "tdn", "nh4n", "no3n", "doc")

# daily DO + predictors, one row per site-day
dry_comparison_raw <- all_day %>%
  dplyr::mutate(Date = as.Date(Date), Year = lubridate::year(Date)) %>%
  dplyr::inner_join(dry_windows, by = "Year") %>%
  dplyr::filter(Date >= dry_start, Date <= dry_end) %>%
  dplyr::select(-dry_start, -dry_end) %>%
  dplyr::left_join(nut %>% dplyr::distinct(Date, .keep_all = TRUE) %>%
                     dplyr::select(Date, dplyr::all_of(nut_cols)),
                   by = "Date") %>%
  dplyr::left_join(E_amp_comparison %>%
                     dplyr::distinct(Date, site_position, .keep_all = TRUE) %>%
                     dplyr::select(Date, site_position, E, amplitude_ratio),
                   by = c("Date", "site_position")) %>%
  dplyr::left_join(radon_long_0827 %>%
                     dplyr::distinct(site_position, .keep_all = TRUE) %>%
                     dplyr::select(site_position, Radon_pCi_L),
                   by = "site_position") %>%
  dplyr::left_join(storm_days %>%
                     dplyr::distinct(Date, .keep_all = TRUE) %>%
                     dplyr::select(Date, days_since_storm),
                   by = "Date")

# checks
stopifnot(nrow(dry_comparison_raw) ==
            nrow(dplyr::distinct(dry_comparison_raw, site_position, Date)))
dry_comparison_raw %>% dplyr::count(Year)
range(dry_comparison_raw$Date)
summary(dry_comparison_raw$daily_mean)   # ~25-120 if % saturation

ts_predictors  <- c("mean_Q","days_since_storm", "benthic_light", "E")
nut_predictors <- nut_cols
years <- c(2024, 2025)

## nutrients are sparse, so pair each sample with mean DO over the prior 3 weeks
do_3week_composite <- df %>%
  dplyr::filter(wet_dry == "Dry", !is.na(DO_sat_percent)) %>%
  dplyr::mutate(obs_date = as.Date(Day)) %>%
  dplyr::select(obs_date, site_position, DO_sat_percent) %>%
  tidyr::crossing(nut %>% dplyr::distinct(nut_date = Date)) %>%
  dplyr::filter(obs_date > nut_date - 21, obs_date <= nut_date) %>%
  dplyr::group_by(site_position, nut_date) %>%
  dplyr::summarise(
    do_3week = mean(DO_sat_percent),
    n_days   = dplyr::n_distinct(obs_date),
    .groups  = "drop"
  ) %>%
  dplyr::filter(n_days >= 10)

nut_do_daily <- nut %>%
  dplyr::mutate(year = lubridate::year(Date)) %>%
  dplyr::left_join(dry_windows, by = c("year" = "Year")) %>%
  dplyr::filter(Date >= dry_start, Date <= dry_end) %>%
  dplyr::select(-dry_start, -dry_end) %>%
  dplyr::inner_join(do_3week_composite, by = c("Date" = "nut_date"))

nut_do_daily %>% dplyr::count(year, Date)   # which composites remain

site_order <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9","S11", "S12",
                "S14", "S15", "S16", "S17.b","S17.a")

## functions
min_n_ts  <- 14   # days of paired daily data
min_n_nut <- 3    # three-week composite samples

clean_pair <- function(pred_vals, do_vals, min_n) {
  idx <- is.finite(pred_vals) & is.finite(do_vals)
  x <- pred_vals[idx]
  y <- do_vals[idx]
  ok <- length(y) >= min_n && dplyr::n_distinct(x) >= 2 && dplyr::n_distinct(y) >= 2
  list(x = x, y = y, ok = ok)
}

run_kendall <- function(pred_vals, do_vals, min_n = 3) {
  d <- clean_pair(pred_vals, do_vals, min_n)
  if (!d$ok) return(tibble::tibble(tau = NA_real_, kendall_p = NA_real_, n = length(d$y)))
  test <- suppressWarnings(stats::cor.test(d$x, d$y, method = "kendall", exact = FALSE))
  tibble::tibble(tau = unname(test$estimate), kendall_p = test$p.value, n = length(d$y))
}

# Theil-Sen slope
run_sen <- function(pred_vals, do_vals, min_n = 3) {
  d <- clean_pair(pred_vals, do_vals, min_n)
  if (!d$ok) return(tibble::tibble(sen_slope = NA_real_))
  dx <- outer(d$x, d$x, "-")
  dy <- outer(d$y, d$y, "-")
  keep <- upper.tri(dx) & dx != 0          # skip tied x values
  tibble::tibble(sen_slope = stats::median(dy[keep] / dx[keep]))
}

add_directions <- function(d, type) {
  d %>%
    dplyr::filter(!is.na(tau), !is.na(sen_slope)) %>%
    dplyr::mutate(
      data_type = type,
      kendall_direction = dplyr::case_when(tau > 0 ~ "Positive", tau < 0 ~ "Negative", TRUE ~ "Zero"),
      sen_direction     = dplyr::case_when(sen_slope > 0 ~ "Positive", sen_slope < 0 ~ "Negative", TRUE ~ "Zero"),
      agree = kendall_direction == sen_direction
    )
}

## time series predictors vs daily DO
ts_results <- tidyr::expand_grid(
  predictor = ts_predictors,
  site = unique(dry_comparison_raw$site_position),
  year = years
) %>%
  dplyr::mutate(
    result = purrr::pmap(list(predictor, site, year), function(pred, s, yr) {
      dat <- dry_comparison_raw %>% dplyr::filter(site_position == s, Year == yr)
      dplyr::bind_cols(
        run_kendall(dat[[pred]], dat$daily_mean, min_n = min_n_ts),
        run_sen(dat[[pred]], dat$daily_mean, min_n = min_n_ts)
      )
    })
  ) %>%
  tidyr::unnest(result)

# tests dropped for too few days
ts_results %>% dplyr::filter(is.na(tau), n > 0) %>%
  dplyr::select(predictor, site, year, n) %>% print(n = Inf)

ts_results <- add_directions(ts_results, "Time Series")

## nutrients vs 3-week DO
nut_results <- tidyr::expand_grid(
  predictor = nut_predictors,
  site = unique(nut_do_daily$site_position),
  year = years
) %>%
  dplyr::mutate(
    result = purrr::pmap(list(predictor, site, year), function(pred, s, yr) {
      dat <- nut_do_daily %>% dplyr::filter(site_position == s, year == yr)
      dplyr::bind_cols(
        run_kendall(dat[[pred]], dat$do_3week, min_n = min_n_nut),
        run_sen(dat[[pred]], dat$do_3week, min_n = min_n_nut)
      )
    })
  ) %>%
  tidyr::unnest(result) %>%
  add_directions("Nutrients")

comparison_summary <- dplyr::bind_rows(ts_results, nut_results) %>%
  dplyr::group_by(data_type, predictor) %>%
  dplyr::summarise(
    n_tests = dplyr::n(),
    n_kendall_sig = sum(kendall_p < 0.05, na.rm = TRUE),
    pct_kendall_sig = 100 * mean(kendall_p < 0.05, na.rm = TRUE),
    n_direction_agree = sum(agree, na.rm = TRUE),
    pct_direction_agree = 100 * mean(agree, na.rm = TRUE),
    median_tau = median(tau, na.rm = TRUE),
    median_sen_slope = median(sen_slope, na.rm = TRUE),
    .groups = "drop"
  )
comparison_summary

## combine
mk_all <- dplyr::bind_rows(
  ts_results  %>% dplyr::mutate(data_type = "Time Series (Daily DO)"),
  nut_results %>% dplyr::mutate(data_type = "Nutrients (3-Week DO)")
) %>%
  dplyr::mutate(
    sig = kendall_p < 0.05,
    direction = dplyr::case_when(tau > 0 ~ "Increasing", tau < 0 ~ "Decreasing", TRUE ~ "No direction"),
    category3 = dplyr::case_when(
      direction == "Increasing" & sig ~ "Increasing - Sig",
      direction == "Decreasing" & sig ~ "Decreasing - Sig",
      TRUE ~ "Not significant"
    ),
    site = factor(site, levels = site_order)
  )

mk_all %>%
  dplyr::mutate(site = as.character(site)) %>%
  dplyr::arrange(data_type, predictor, year, site) %>%
  readr::write_csv(file.path(data_dir, "kendall_results_dry_season_DOsat.csv"))

# numbers for the methods
mk_all %>%
  dplyr::group_by(data_type) %>%
  dplyr::summarise(tests = dplyr::n(),
                   site_years = dplyr::n_distinct(paste(site, year)),
                   min_n = min(n), median_n = median(n), max_n = max(n))

## heatmaps
fill_scale <- scale_fill_gradient2(
  low = "#0072B2", mid = "grey95", high = "#D55E00", midpoint = 0,
  limits = c(-1, 1), breaks = seq(-1, 1, 0.5), name = expression("Kendall's" ~ tau),
  guide = guide_colorbar(direction = "horizontal", title.position = "top", title.hjust = 0.5,
                         barwidth = unit(6, "cm"), barheight = unit(0.4, "cm")))

pred_labels <- c(benthic_light = "'Benthic light'", days_since_storm = "'Days since storm'",
                 mean_Q = "'Discharge'", discharge = "'Discharge'",
                 E = "'Thermal sensitivity (E)'", thermal_sensitivity = "'Thermal sensitivity (E)'",
                 alk = "'Alkalinity'", doc = "'DOC'", nh4n = "NH[4]*'-N'", no3n = "NO[3]*'-N'",
                 ph = "'pH'", po4p = "PO[4]*'-P'", ssed = "'Susp. Sed.'",
                 tdn = "'TDN'", tdp = "'TDP'", utn = "'TN'", utp = "'TP'")

lab_fun <- function(x) parse(text = ifelse(x %in% names(pred_labels), pred_labels[x], paste0("'", x, "'")))

# sites with results in a given year, downstream to upstream
sites_in <- function(yr) {
  s <- mk_all %>% dplyr::filter(year == yr, !is.na(tau), !is.na(site)) %>% dplyr::pull(site)
  site_order[site_order %in% s]
}

make_panel <- function(yr, type, is_last) {
  ys  <- sites_in(yr)
  dat <- mk_all %>%
    dplyr::filter(year == yr, data_type == type, !is.na(tau), !is.na(site)) %>%
    dplyr::mutate(site = factor(site, levels = rev(ys)))
  
  ggplot(dat, aes(predictor, site, fill = tau)) +
    geom_tile(color = "grey50", linewidth = 0.5) +
    geom_text(data = dplyr::filter(dat, kendall_p < 0.05), label = "*", size = 3.5, fontface = "bold") +
    fill_scale +
    scale_x_discrete(labels = lab_fun) +
    scale_y_discrete(limits = rev(ys), drop = FALSE) +
    theme_classic(base_size = 11) +
    theme(plot.title   = element_text(size = 12, face = "bold"),
          axis.text.x  = if (is_last) element_text(angle = 45, hjust = 1) else element_blank(),
          axis.ticks.x = if (is_last) element_line() else element_blank()) +
    labs(x = NULL, y = "Site")
}

# one year = daily panel + nutrient panel
build_year_row <- function(yr, is_last) {
  p_ts  <- make_panel(yr, "Time Series (Daily DO)", is_last) + labs(title = yr)
  p_nut <- make_panel(yr, "Nutrients (3-Week DO)", is_last) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  p_ts + p_nut + patchwork::plot_layout(widths = c(4, 8))
}

fig5_plot <- patchwork::wrap_plots(
  purrr::map2(years, years == max(years), build_year_row),
  ncol = 1, heights = purrr::map_int(years, ~ length(sites_in(.x)))
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(legend.position   = "bottom",
        legend.box.spacing = unit(2, "pt"),
        legend.margin      = ggplot2::margin(-8, 0, 0, 0),
        plot.tag          = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.01, 0.96))

fig5_plot
#ggsave(plot=fig5_plot, file.path(fig_dir, "fig5_plot.png"), width=8, height=5)

## results tables
mk_table <- mk_all %>%
  dplyr::filter(!is.na(tau), !is.na(site)) %>%
  dplyr::mutate(tau = round(tau, 3), sen_slope = round(sen_slope, 4), kendall_p = round(kendall_p, 4)) %>%
  dplyr::select(
    Year = year, Data_Type = data_type, Predictor = predictor, Site = site, N = n,
    Tau = tau, Sen_Slope = sen_slope, Direction = direction, P_Value = kendall_p,
    Significant = sig, Kendall_Direction = kendall_direction,
    Sen_Direction = sen_direction, Direction_Agree = agree
  ) %>%
  dplyr::arrange(Year, Data_Type, P_Value)
print(mk_table)

# by year, split by direction
summary_table <- mk_table %>%
  dplyr::group_by(Year, Data_Type, Predictor) %>%
  dplyr::summarise(
    N_Sites = dplyr::n(),
    N_Inc_Sig = sum(Direction == "Increasing" & Significant),
    N_Inc_NS = sum(Direction == "Increasing" & !Significant),
    N_Dec_Sig = sum(Direction == "Decreasing" & Significant),
    N_Dec_NS = sum(Direction == "Decreasing" & !Significant),
    Pct_Inc_Sig = round(100 * N_Inc_Sig / N_Sites, 0),
    Pct_Inc_NS = round(100 * N_Inc_NS / N_Sites, 0),
    Pct_Dec_Sig = round(100 * N_Dec_Sig / N_Sites, 0),
    Pct_Dec_NS = round(100 * N_Dec_NS / N_Sites, 0),
    Pct_Overall_Sig = round(100 * (N_Inc_Sig + N_Dec_Sig) / N_Sites, 0),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year, Data_Type, dplyr::desc(Pct_Overall_Sig))
print(summary_table)

mk_rel <- mk_table %>%
  dplyr::mutate(Relationship = dplyr::case_when(
    Significant & Tau > 0 ~ "Increasing",
    Significant & Tau < 0 ~ "Decreasing",
    TRUE ~ "Not significant"
  ))

# by year
mk_summary <- mk_rel %>%
  dplyr::group_by(Year, Data_Type, Predictor) %>%
  dplyr::summarise(
    N = dplyr::n(),
    N_increasing = sum(Relationship == "Increasing"),
    N_decreasing = sum(Relationship == "Decreasing"),
    N_NS = sum(Relationship == "Not significant"),
    Pct_increasing = round(100 * N_increasing / N, 1),
    Pct_decreasing = round(100 * N_decreasing / N, 1),
    Pct_NS = round(100 * N_NS / N, 1),
    Median_tau = round(median(Tau[Significant], na.rm = TRUE), 3),
    .groups = "drop"
  )
mk_summary

# across years
mk_summary_overall <- mk_rel %>%
  dplyr::group_by(Data_Type, Predictor) %>%
  dplyr::summarise(
    N = dplyr::n(),
    N_sig = sum(Significant),
    Pct_sig = round(100 * N_sig / N, 1),
    N_increasing = sum(Relationship == "Increasing"),
    N_decreasing = sum(Relationship == "Decreasing"),
    N_NS = sum(Relationship == "Not significant"),
    Median_tau_sig = round(median(Tau[Significant], na.rm = TRUE), 3),
    Min_tau = round(min(Tau, na.rm = TRUE), 3),
    Max_tau = round(max(Tau, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Pct_sig))
mk_summary_overall

# Kendall vs Sen direction agreement
kendall_sen_comparison <- mk_table %>%
  dplyr::group_by(Data_Type, Predictor) %>%
  dplyr::summarise(
    N = dplyr::n(),
    N_direction_agree = sum(Direction_Agree, na.rm = TRUE),
    Pct_direction_agree = round(100 * mean(Direction_Agree, na.rm = TRUE), 1),
    N_direction_disagree = sum(!Direction_Agree, na.rm = TRUE),
    .groups = "drop"
  )
kendall_sen_comparison

# any disagreements
mk_table %>%
  dplyr::filter(!Direction_Agree) %>%
  dplyr::select(Year, Data_Type, Predictor, Site, N, Tau, Sen_Slope,
                Kendall_Direction, Sen_Direction, P_Value)

write_csv(mk_table,               file.path(out_dir, "mk_table_site_year.csv"))
write_csv(summary_table,          file.path(out_dir, "mk_summary_by_year_direction.csv"))
write_csv(mk_summary,             file.path(out_dir, "mk_summary_by_year.csv"))
write_csv(mk_summary_overall,     file.path(out_dir, "mk_summary_overall.csv"))
write_csv(kendall_sen_comparison, file.path(out_dir, "kendall_vs_sen.csv"))


#### Figure 6: longitudinal controls on DO ####
# sites ordered by distance (dried sites dropped)
site_distance_order <- df %>%
  dplyr::filter(is.finite(Distance_m), !site_position %in% c("S10","S13.a", "S17.a")) %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(Distance_m = median(Distance_m, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(Distance_m) %>%
  dplyr::pull(site_position)

shared_x <- scale_x_discrete(limits = site_distance_order, drop = FALSE)

small_y <- theme(axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 9))

no_x <- theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
              axis.ticks.x = element_blank())

## a. DO
DO_reach <- daily_by_site %>%
  dplyr::filter(site_position %in% site_distance_order, !is.na(daily_mean)) %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order))

p_do <- ggplot(DO_reach, aes(site_position, daily_mean)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  shared_x + std_theme +
  annotate("text", x = -Inf, y = Inf, label = "a",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  small_y + no_x +
  labs(y = "Daily mean DO (%)")

## b. radon + turnover points
radon_plot <- radon_long_0827 %>%
  dplyr::mutate(site_position = dplyr::if_else(site_position == "S11.b", "S11", site_position)) %>%
  dplyr::filter(is.finite(Radon_pCi_L), site_position %in% site_distance_order) %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(Radon_pCi_L = median(Radon_pCi_L, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order)) %>%
  dplyr::arrange(site_position)

turnover_lines <- data.frame(
  x = c(mean(match(c("S6", "S7"), site_distance_order)),
        mean(match(c("S8", "S9"), site_distance_order)))
)

turnover_rn <- ggplot(radon_plot, aes(site_position, Radon_pCi_L, group = 1)) +
  geom_vline(data = turnover_lines, aes(xintercept = x),
             colour = "#5B8DB8", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  annotate("text", x = turnover_lines$x[2] + 0.1, y = 27,
           label = "Turnover points", colour = "#5B8DB8",
           size = 2.5, angle = 90, hjust = 0, vjust = 1) +
  annotate("text", x = -Inf, y = Inf, label = "b",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  geom_line(linewidth = 0.8, colour = "black") +
  geom_point(size = 2.5, colour = "black") +
  scale_x_discrete(limits = site_distance_order, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  coord_cartesian(clip = "off") +
  std_theme + small_y + no_x +
  theme(legend.position = "none") +
  labs(y = "Radon (pCi/L)")

## c. thermal sensitivity
E_reach <- E_amp_comparison %>%
  dplyr::filter(is.finite(E), site_position %in% site_distance_order) %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order))

p_gw <- ggplot(E_reach, aes(site_position, E)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  annotate("text", x = -Inf, y = Inf, label = "c",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  labs(y = "Thermal sensitivity (E)") +
  shared_x + std_theme + no_x +
  theme(legend.position = "none")

## d. storm response (peak DO in 72 h after onset minus median DO 24 h before)
storm_response <- storm_window_data %>%
  dplyr::filter(site_position %in% site_distance_order, is.finite(DO_sat_percent)) %>%
  dplyr::group_by(event_id, site_position) %>%
  dplyr::summarise(
    pre_DO=median(DO_sat_percent[hours_from_storm >= -24 & hours_from_storm < 0], na.rm=TRUE),
    peak_DO=max(DO_sat_percent[hours_from_storm >= 0 & hours_from_storm <= 72], na.rm=TRUE),
    max_response=peak_DO-pre_DO,
    .groups="drop"
  ) %>%
  dplyr::filter(is.finite(max_response))

storm_reach <- storm_response %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order))

p_storm <- ggplot(storm_reach, aes(site_position, max_response)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  annotate("text", x = -Inf, y = Inf, label = "d",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  shared_x + std_theme + small_y + no_x +
  labs(y = expression(Delta*"DO after storm (%)"))

## e. benthic light (dry season)
light_reach <- df %>%
  dplyr::filter(wet_dry == "Dry", site_position %in% site_distance_order, is.finite(benthic_light)) %>%
  dplyr::mutate(Day = as.Date(PCT)) %>%
  dplyr::group_by(Day, site_position) %>%
  dplyr::summarise(benthic_light = mean(benthic_light, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order))

p_light <- ggplot(light_reach, aes(site_position, benthic_light)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  shared_x + std_theme +
  annotate("text", x = -Inf, y = Inf, label = "e",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  small_y + no_x +
  labs(y = expression("Benthic light ("*mu*"mol m"^{-2}*" s"^{-1}*")"))

## f. carbon (Aug synoptic)
carb_df <- nut_0829 %>%
  dplyr::mutate(ALK_C = ALK_mg_L / 50.04 * 12.011) %>%   # mg CaCO3/L -> mg C/L
  dplyr::select(site_position, DIC_mg_L, ALK_C)

# excess DIC ≈ CO2(aq)
co2_df <- carb_df %>%
  dplyr::mutate(exCO2_C = DIC_mg_L - ALK_C) %>%
  dplyr::select(site_position, exCO2_C)

carb_long <- carb_df %>%
  tidyr::pivot_longer(-site_position, names_to = "Var", values_to = "Value") %>%
  dplyr::mutate(
    Var = dplyr::recode(Var, "DIC_mg_L" = "DIC", "ALK_C" = "Alkalinity"),
    Var = factor(Var, levels = c("DIC", "Alkalinity")),
    site_position = factor(as.character(site_position), levels = site_distance_order)
  )

mid_x <- ceiling(length(site_distance_order) / 2)

carb_lab <- carb_long %>%
  dplyr::filter(is.finite(Value)) %>%
  dplyr::mutate(pos = match(as.character(site_position), site_distance_order)) %>%
  dplyr::group_by(Var) %>%
  dplyr::slice_min(abs(pos - mid_x), n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(x_lab = pos + ifelse(Var == "DIC", 0.5, 0))

p_carb <- ggplot(carb_long, aes(site_position, Value, color = Var, shape = Var, group = Var)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  annotate("text", x = -Inf, y = Inf, label = "f",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  geom_text(data = carb_lab, aes(x = x_lab, y = Value, label = Var),
            vjust = -1, size = 2.8, inherit.aes = FALSE,
            color = c("DIC" = "#E08214", "Alkalinity" = "#35978F")[carb_lab$Var]) +
  scale_color_manual(values = c("DIC" = "#E08214", "Alkalinity" = "#35978F")) +
  scale_shape_manual(values = c("DIC" = 16, "Alkalinity" = 15)) +
  coord_cartesian(clip = "off") +
  shared_x + std_theme + small_y + no_x +
  theme(legend.position = "none") +
  labs(y = expression(mg~C~L^{-1}))

## g. nutrients (Aug synoptic), N and P on secondary axis
nut_np <- nut_0829 %>%
  dplyr::select(site_position, NO3_mg_L, NH4_mg_L, PO4_mg_L) %>%
  tidyr::pivot_longer(-site_position, names_to = "Nutrient", values_to = "Concentration") %>%
  dplyr::mutate(Nutrient = dplyr::recode(Nutrient, "NO3_mg_L" = "NO3",
                                         "NH4_mg_L" = "NH4", "PO4_mg_L" = "PO4"))

scale_factor <- max(nut_0829$DOC_mg_L, na.rm = TRUE) / max(nut_np$Concentration, na.rm = TRUE)

p_nut <- ggplot() +
  geom_line(data = nut_0829, aes(site_position, DOC_mg_L, group = 1, color = "DOC"), linewidth = 1) +
  geom_point(data = nut_0829, aes(site_position, DOC_mg_L, color = "DOC"), size = 2.5) +
  geom_line(data = nut_np, aes(site_position, Concentration * scale_factor,
                               color = Nutrient, group = Nutrient), linewidth = 1) +
  geom_point(data = nut_np, aes(site_position, Concentration * scale_factor,
                                color = Nutrient, shape = Nutrient), size = 2.5) +
  scale_y_continuous(
    name = "DOC (mg/L)",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = paste0(
        "<span style='color:#2166AC'>NO<sub>3</sub></span>, ",
        "<span style='color:#D6604D'>NH<sub>4</sub></span>, ",
        "<span style='color:#762A83'>PO<sub>4</sub></span> (mg/L)"
      )
    )
  ) +
  annotate("text", x = -Inf, y = Inf, label = "g",
           hjust = -1, vjust = 1.5, fontface = "bold", size = 4) +
  scale_color_manual(
    name = NULL,
    values = c("DOC" = "#1B7837", "NO3" = "#2166AC", "NH4" = "#D6604D", "PO4" = "#762A83"),
    breaks = c("DOC", "NO3", "NH4", "PO4"),
    labels = c("DOC", expression(NO[3]), expression(NH[4]), expression(PO[4]))
  ) +
  scale_shape_manual(values = c("NO3" = 17, "NH4" = 15, "PO4" = 18), guide = "none") +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15, 18)))) +
  shared_x + std_theme +
  theme(
    axis.title.y.left  = element_text(size = 10, face = "bold", color = "#1B7837"),
    axis.title.y.right = ggtext::element_markdown(size = 10, face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    legend.position    = "none"
  ) +
  labs(x = "Site (downstream to upstream)")

fig6_plot <- p_do / turnover_rn / p_gw / p_storm / p_light / p_carb / p_nut +
  patchwork::plot_layout(heights = c(1.2, 1, 1, 1, 1, 1, 1.2))
fig6_plot
#ggsave(plot = fig6_plot, file.path(fig_dir, "fig6_plot.png"), width = 8.5, height = 12)


#### Supp Fig 5: excess DIC vs pH ####
# pH from July, paired with August DIC - alkalinity
ph_df <- tibble::tibble(
  site_position = c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10",
                    "S11","S12","S13.b","S14","S15","S16","S17.b"),
  pH = c(7.25, 7.80, 7.45, 6.79, 6.92, 7.21, 6.97, 6.85, 7.13, 6.91,
         7.27, 7.15, 7.11, 7.35, 7.49, 7.27, 7.61)
) %>%
  dplyr::filter(site_position %in% site_distance_order)

site_dist <- df %>%
  dplyr::distinct(site_position = as.character(site_position), Distance_m) %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(Distance_m = dplyr::first(Distance_m), .groups = "drop")

carbonph_df <- co2_df %>%
  dplyr::transmute(site_position = as.character(site_position), exCO2_C) %>%
  dplyr::inner_join(ph_df, by = "site_position") %>%
  dplyr::left_join(site_dist, by = "site_position") %>%
  dplyr::filter(is.finite(exCO2_C), is.finite(pH), is.finite(Distance_m))

fit <- lm(pH ~ exCO2_C, data = carbonph_df)
s   <- summary(fit)
lab <- sprintf("R\u00b2 = %.2f\np %s\nn = %d", s$r.squared,
               ifelse(coef(s)[2, 4] < 0.001, "< 0.001", sprintf("= %.3f", coef(s)[2, 4])),
               nrow(carbonph_df))

supp_fig5 <- ggplot(carbonph_df, aes(exCO2_C, pH)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.7) +
  geom_point(aes(colour = Distance_m), size = 3) +
  ggrepel::geom_text_repel(aes(label = site_position), size = 3,
                           colour = "grey25", seed = 1, min.segment.length = 0) +
  annotate("text", x = Inf, y = Inf, label = lab, hjust = 1.05, vjust = 1.3, size = 3.2) +
  scale_colour_viridis_c(name = "Distance upstream (m)", option = "D") +
  std_theme +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.3, "cm")) +
  labs(x = expression(DIC~-~alkalinity~(mg~C~L^{-1})), y = "pH")

supp_fig5
#ggsave(plot=supp_fig5, file.path(fig_dir, "supp_fig5.png"), width=6.5, height=5)


#### section 3.5 stats: spatial patterns ####
## site-level summaries
spatial_stats <- DO_reach %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(
    DO_median = median(daily_mean, na.rm = TRUE),
    DO_mean = mean(daily_mean, na.rm = TRUE),
    DO_min = min(daily_mean, na.rm = TRUE),
    DO_max = max(daily_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    E_reach %>% dplyr::group_by(site_position) %>%
      dplyr::summarise(E_median = median(E, na.rm = TRUE), .groups = "drop"),
    by = "site_position") %>%
  dplyr::left_join(
    storm_reach %>% dplyr::group_by(site_position) %>%
      dplyr::summarise(storm_median = median(max_response, na.rm = TRUE), .groups = "drop"),
    by = "site_position") %>%
  dplyr::left_join(
    light_reach %>% dplyr::group_by(site_position) %>%
      dplyr::summarise(light_median = median(benthic_light, na.rm = TRUE), .groups = "drop"),
    by = "site_position") %>%
  dplyr::left_join(radon_plot %>% dplyr::select(site_position, Radon_pCi_L), by = "site_position") %>%
  dplyr::left_join(nut_0829 %>% dplyr::select(site_position, DOC_mg_L, NO3_mg_L, NH4_mg_L, PO4_mg_L),
                   by = "site_position") %>%
  dplyr::left_join(
    gw_tracer %>%
      dplyr::filter(water_type=="Surface", site_position %in% site_distance_order) %>%
      dplyr::group_by(site_position) %>%
      dplyr::summarise(
        FI_median=median(flour_index, na.rm=TRUE),
        PeakA_median=median(peakA, na.rm=TRUE),
        PeakC_median=median(peakC, na.rm=TRUE),
        PeakT_median=median(peakT, na.rm=TRUE),
        SUVA_median=median(SUVA254, na.rm=TRUE),
        .groups="drop"
      ),
    by="site_position")

spatial_stats

## cross-panel relationships
spear <- function(x, y) cor.test(x, y, method = "spearman", exact = FALSE)

spear(spatial_stats$DO_median,   spatial_stats$Radon_pCi_L)
spear(spatial_stats$DO_median,   spatial_stats$E_median)
spear(spatial_stats$Radon_pCi_L, spatial_stats$E_median)
spear(spatial_stats$DO_median,   spatial_stats$storm_median)
spear(spatial_stats$DO_median,   spatial_stats$light_median)

## snapshot nutrients vs DO
nut_cor <- lapply(c("DOC_mg_L", "NO3_mg_L", "NH3_mg_L", "PO4_mg_L"), function(v) {
  test <- spear(spatial_stats$DO_median, spatial_stats[[v]])
  data.frame(variable = v, rho = unname(test$estimate), p = test$p.value,
             n = sum(is.finite(spatial_stats$DO_median) & is.finite(spatial_stats[[v]])))
}) %>%
  dplyr::bind_rows()
nut_cor

## ranges across the reach
spatial_ranges <- spatial_stats %>%
  dplyr::summarise(
    DO_min = min(DO_median, na.rm = TRUE),
    DO_min_site = as.character(site_position[which.min(DO_median)]),
    DO_max = max(DO_median, na.rm = TRUE),
    DO_max_site = as.character(site_position[which.max(DO_median)]),
    
    radon_min = min(Radon_pCi_L, na.rm = TRUE),
    radon_min_site = as.character(site_position[which.min(Radon_pCi_L)]),
    radon_max = max(Radon_pCi_L, na.rm = TRUE),
    radon_max_site = as.character(site_position[which.max(Radon_pCi_L)]),
    
    E_min = min(E_median, na.rm = TRUE),
    E_min_site = as.character(site_position[which.min(E_median)]),
    E_max = max(E_median, na.rm = TRUE),
    E_max_site = as.character(site_position[which.max(E_median)]),
    
    storm_min = min(storm_median, na.rm = TRUE),
    storm_min_site = as.character(site_position[which.min(storm_median)]),
    storm_max = max(storm_median, na.rm = TRUE),
    storm_max_site = as.character(site_position[which.max(storm_median)]),
    
    light_min = min(light_median, na.rm = TRUE),
    light_min_site = as.character(site_position[which.min(light_median)]),
    light_max = max(light_median, na.rm = TRUE),
    light_max_site = as.character(site_position[which.max(light_median)])
  )
spatial_ranges

## carbon vs DO (site median over full record)
do_site <- df %>%
  dplyr::group_by(site_position) %>%
  dplyr::summarise(DO_med = median(DO_sat_percent, na.rm = TRUE), .groups = "drop")

cor_df <- carb_df %>%
  dplyr::mutate(site_position = as.character(site_position),
                exCO2_C = DIC_mg_L - ALK_C) %>%
  dplyr::left_join(do_site %>% dplyr::mutate(site_position = as.character(site_position)),
                   by = "site_position") %>%
  dplyr::left_join(radon_plot %>% dplyr::mutate(site_position = as.character(site_position)),
                   by = "site_position") %>%
  dplyr::left_join(nut_0829 %>% dplyr::mutate(site_position = as.character(site_position)) %>%
                     dplyr::select(site_position, DOC_mg_L),
                   by = "site_position")

sapply(c("DIC_mg_L", "exCO2_C", "DOC_mg_L"), function(v) {
  ct <- spear(cor_df$DO_med, cor_df[[v]])
  c(rho = round(ct$estimate, 2), p = signif(ct$p.value, 2))
})

## do bigger storms homogenize DO across sites?
mag <- storm_window_data %>%
  distinct(event_id, PCT, hours_from_storm, event_wet_dry,
           Discharge_L_s, Gage_precip_total_mm) %>%
  group_by(event_id) %>%
  summarise(
    period = first(event_wet_dry),
    pre_Q  = median(Discharge_L_s[hours_from_storm < 0], na.rm = TRUE),
    peak_Q = max(Discharge_L_s[hours_from_storm >= 0], na.rm = TRUE),
    precip = sum(Gage_precip_total_mm[hours_from_storm >= 0], na.rm = TRUE)
  ) %>%
  mutate(dQ = peak_Q - pre_Q, Q_ratio = peak_Q / pre_Q)

spread <- storm_reach %>%
  group_by(event_id) %>%
  summarise(n_sites    = n(),
            pre_range  = diff(range(pre_DO)),
            peak_range = diff(range(peak_DO))) %>%
  mutate(narrowing = pre_range - peak_range)

# only events with a meaningful pre-storm spread
ev <- inner_join(spread, mag, by = "event_id") %>%
  filter(pre_range > 20)

sapply(c("peak_Q", "dQ", "Q_ratio", "precip"), function(v) {
  ct <- spear(ev$narrowing, ev[[v]])
  c(rho = unname(ct$estimate), p = ct$p.value)
})

ev %>% arrange(desc(narrowing)) %>%
  select(event_id, period, n_sites, pre_range, peak_range, narrowing,
         peak_Q, dQ, Q_ratio, precip)

summary(lm(peak_range ~ log(peak_Q) + pre_range + period + n_sites, data = ev))

ggplot(ev, aes(peak_Q, narrowing, colour = period, label = event_id)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 3) +
  geom_text(vjust = -1, show.legend = FALSE) +
  scale_x_log10() +
  labs(x = "Peak discharge (L/s)", y = "Narrowing of among-site DO range (%)")


#### linear mixed-effects models ####
# settings
out_dir    <- "output"
drop_sites <- c("S10", "S13.a", "S13.b", "S17.a")
nut_vars   <- c("doc", "tdn", "tdp", "alk")
dir.create(out_dir, showWarnings = FALSE)

# helpers
z <- function(x) as.numeric(scale(x))

# 3-week mean ending on `end`
window_mean <- function(data, site, end, col, min_days = 10) {
  x <- data %>%
    filter(site_position == site, Day >= end - 21, Day <= end) %>%
    pull(all_of(col))
  if (sum(is.finite(x)) < min_days) NA_real_ else mean(x, na.rm = TRUE)
}

coef_table <- function(m, label) {
  tt <- summary(m)$tTable
  ci <- intervals(m, which = "fixed")$fixed
  tibble(model = label, predictor = rownames(tt)[-1],
         est = tt[-1, "Value"], lo = ci[-1, "lower"], hi = ci[-1, "upper"],
         p = tt[-1, "p-value"])
}

n_summary <- function(d) {
  c(rows = nrow(d), sites = n_distinct(d$site),
    site_years = n_distinct(paste(d$site, d$yr)))
}

# daily data
src_daily <- dry_comparison_raw %>%
  mutate(Day = as.Date(Date), logQ = log(mean_Q))

daily <- src_daily %>%
  filter(!site_position %in% drop_sites) %>%
  mutate(site = factor(site_position),
         yr   = factor(Year),
         t    = as.numeric(Day))

daily <- daily %>% filter(wet_dry == "Dry")

# dry season only
stopifnot("dry_comparison_raw contains non-dry days; filter wet_dry == 'Dry'" =
            all(daily$wet_dry == "Dry", na.rm = TRUE))

# model a: daily model without light
d1 <- daily %>%
  filter(if_all(c(daily_mean, logQ, days_since_storm, E), is.finite)) %>%
  group_by(site, yr) %>%
  mutate(E_w = E - mean(E)) %>%
  ungroup() %>%
  mutate(across(c(logQ, days_since_storm, E_w), z))

d1s <- d1[, c("daily_mean","logQ","days_since_storm","E_w","site","yr","t")]

m_daily <- nlme::lme(daily_mean ~ logQ + days_since_storm + E_w,
                     random      = ~ 1 | site/yr,
                     correlation = nlme::corCAR1(form = ~ t | site/yr),
                     data = d1s, na.action = na.omit)

summary(m_daily)$tTable
intervals(m_daily, which = "fixed")$fixed
r.squaredGLMM(m_daily)
n_summary(d1)

# model b: daily model with benthic light
d2 <- daily %>%
  filter(if_all(c(daily_mean, logQ, days_since_storm, E, benthic_light), is.finite),
         benthic_light > 0) %>%
  mutate(logL = log(benthic_light)) %>%
  group_by(site, yr) %>%
  mutate(E_w = E - mean(E), logL_w = logL - mean(logL)) %>%
  ungroup() %>%
  mutate(across(c(logQ, days_since_storm, E_w, logL_w), z))

m_daily_light <- lme(daily_mean ~ logQ + days_since_storm + E_w + logL_w,
                     random      = ~ 1 | site/yr,
                     correlation = corCAR1(form = ~ t | site/yr),
                     data = d2, na.action = na.omit)

summary(m_daily_light)$tTable
intervals(m_daily_light, which = "fixed")$fixed
r.squaredGLMM(m_daily_light)
n_summary(d2)

# model c: three-week composite model
comp <- nut_do_daily %>%
  filter(!site_position %in% drop_sites) %>%
  mutate(Date = as.Date(Date), site = factor(site_position), yr = factor(year)) %>%
  mutate(
    do_3week  = map2_dbl(site_position, Date, ~ window_mean(daily_by_site, .x, .y, "daily_mean")),
    logQ      = map2_dbl(site_position, Date, ~ window_mean(src_daily,     .x, .y, "logQ")),
    storm_3wk = map2_dbl(site_position, Date, ~ window_mean(src_daily,     .x, .y, "days_since_storm")),
    E_3wk     = map2_dbl(site_position, Date, ~ window_mean(src_daily,     .x, .y, "E"))
  )

# outlet nutrients vs discharge
outlet_dates <- comp %>%
  distinct(Date, doc, tdn, tdp, alk) %>%
  left_join(
    src_daily %>% group_by(Day) %>%
      summarise(logQ_day = mean(logQ, na.rm = TRUE), .groups = "drop"),
    by = c("Date" = "Day"))

outlet_cor <- cor(
  outlet_dates %>% select(logQ = logQ_day, all_of(nut_vars)),
  method = "spearman", use = "pairwise.complete.obs")
outlet_cor["logQ", ]

comp <- comp %>%
  filter(if_all(c(do_3week, logQ, storm_3wk, E_3wk, all_of(nut_vars)), is.finite)) %>%
  group_by(site, yr) %>%
  mutate(E_w = E_3wk - mean(E_3wk)) %>%
  ungroup() %>%
  mutate(across(c(logQ, storm_3wk, E_w, all_of(nut_vars)), z))

count(comp, site, yr) %>% print(n = Inf)
n_summary(comp)

fit_comp <- function(f, data, method = "REML") {
  lme(f, random = ~ 1 | site/yr, data = data, method = method, na.action = na.omit)
}

base_f <- do_3week ~ logQ + storm_3wk + E_w
m_base <- fit_comp(base_f, comp, method = "ML")

# add each nutrient to the base model (dAIC < 0 favors the nutrient)
nut_tests <- map(nut_vars, function(v) {
  m  <- fit_comp(update(base_f, paste(". ~ . +", v)), comp, method = "ML")
  ci <- intervals(m, which = "fixed")$fixed
  tibble(nutrient = v,
         estimate = ci[v, "est."], lower = ci[v, "lower"], upper = ci[v, "upper"],
         dAIC  = AIC(m) - AIC(m_base),
         LRT_p = anova(m_base, m)$`p-value`[2],
         rho_with_logQ = outlet_cor["logQ", v])
}) %>% bind_rows()
nut_tests

m_comp <- fit_comp(base_f, comp)
summary(m_comp)
r.squaredGLMM(m_comp)

# output tables
S2 <- bind_rows(coef_table(m_daily,       "A"),
                coef_table(m_daily_light, "B"),
                coef_table(m_comp,        "C")) %>%
  mutate(across(c(est, lo, hi), ~ round(.x, 2)), p = signif(p, 2))
S2

#write_csv(S2, file.path(out_dir, "S2_coefficients.csv"))
#write_csv(nut_tests %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
          #file.path(out_dir, "S2_D_nutrient_tests.csv"))

#### End of script.