library(units)

stp_zoning <- read_sf("../PrincipalZoning_TEST_-8611068997044372997") %>%
  st_transform(26915) %>%
  st_make_valid()

st_paul_24 <- st_paul_24 %>%
  st_transform(26915) %>%     # critical: same CRS as zoning
  st_make_valid() %>%
  mutate(
    area_sqft = as.numeric(set_units(st_area(geometry), "ft^2")),
    area_acre = area_sqft / 43560
  )

names(stp_zoning)
ggplot(stp_zoning) +
  geom_sf(aes(fill = Zoning_Nam)) +
  theme_minimal() +
  theme(legend.position = "none")

print(unique(stp_zoning$Zoning))

st_paul_24_zoning <- st_paul_24 %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_set_geometry("centroid") %>%
  st_join(stp_zoning[, c("Zoning","Zoning_Nam")], join = st_within, left = TRUE) %>%
  st_set_geometry("geometry")  # restore polygon geometry


table(st_geometry_type(st_paul_24_zoning))
table(is.na(st_paul_24_zoning$Zoning))
# 
# unit_rules_rlh <- tibble::tribble(
#   ~Zoning, ~min_lot_area_per_unit_sqft, ~max_units_per_lot,
#   "RL",    9000,                        2,
#   "H1",    1500,                        4,
#   "H2",    1000,                        5
# )

# summary(st_paul_24_zoning$area_acre)
# vacant_capacity <- st_paul_24_zoning %>%
#   filter(vacant_strict == 1) %>%
#   left_join(unit_rules_rlh, by = "Zoning") %>%
#   mutate(
#     cap_units_rlh = ifelse(
#       !is.na(min_lot_area_per_unit_sqft),
#       pmin(floor(area_sqft / min_lot_area_per_unit_sqft), max_units_per_lot),
#       NA_real_
#     ))
#    

# =========================
# Helpers + Capacity Models
# =========================

library(dplyr)
library(purrr)
library(ggplot2)

# --- global assumption (only used for unit-based RM/T conversion) ---
AVG_NET_SQFT_PER_UNIT <- 1000  # keep fixed; do sensitivity later

# --- zoning ladder / residential set ---
zoning_ladder <- c("RL","H1","H2","RM1","RM2","RM3","T1","T2","T3","T4")
res_districts <- zoning_ladder

# -------------------------
# 1) Unit capacity functions
# -------------------------

cap_rlh <- function(z, area_sqft) {
  # allow scalar z recycled to length(area_sqft)
  if (length(z) == 1L) z <- rep(z, length(area_sqft))
  
  rules <- data.frame(
    z = c("RL","H1","H2"),
    min_per_unit = c(9000, 1500, 1000),
    max_units    = c(2, 4, 5),
    stringsAsFactors = FALSE
  )
  
  idx <- match(z, rules$z)
  out <- rep(NA_real_, length(z))
  ok <- !is.na(idx)
  
  min_per <- rules$min_per_unit[idx[ok]]
  max_u   <- rules$max_units[idx[ok]]
  
  out[ok] <- pmin(floor(area_sqft[ok] / min_per), max_u)
  out
}

cap_rm_units <- function(z, area_sqft, avg_unit = AVG_NET_SQFT_PER_UNIT) {
  # ignore parking: take the higher FAR listed in 66.231
  if (length(z) == 1L) z <- rep(z, length(area_sqft))
  
  far <- dplyr::case_when(
    z == "RM1" ~ 1.0,
    z == "RM2" ~ 2.25,
    z == "RM3" ~ 3.5,
    TRUE ~ NA_real_
  )
  floor((area_sqft * far) / avg_unit)
}

cap_t_units <- function(z, area_sqft, area_acre, avg_unit = AVG_NET_SQFT_PER_UNIT) {
  # Max across permissible residential "building types"
  if (length(z) == 1L) z <- rep(z, length(area_sqft))
  
  cap_1fam <- rep(NA_real_, length(z))
  cap_2fam <- rep(NA_real_, length(z))
  cap_mf   <- rep(NA_real_, length(z))
  
  is_T12T3 <- z %in% c("T1","T2","T3")
  
  # 1-family: 3500 sqft/unit or 12 u/ac max (T1/T2/T3)
  cap_1fam[is_T12T3 & area_acre < 1]   <- floor(area_sqft[is_T12T3 & area_acre < 1] / 3500)
  cap_1fam[is_T12T3 & area_acre >= 1]  <- floor(area_acre[is_T12T3 & area_acre >= 1] * 12)
  
  # 2-family: 2000 sqft/unit or 20 u/ac max (T1/T2/T3)
  cap_2fam[is_T12T3 & area_acre < 1]   <- floor(area_sqft[is_T12T3 & area_acre < 1] / 2000)
  cap_2fam[is_T12T3 & area_acre >= 1]  <- floor(area_acre[is_T12T3 & area_acre >= 1] * 20)
  
  # Multifamily:
  # T1: explicit 1700 sqft/unit or 25 u/ac
  is_T1 <- z == "T1"
  cap_mf[is_T1 & area_acre < 1]        <- floor(area_sqft[is_T1 & area_acre < 1] / 1700)
  cap_mf[is_T1 & area_acre >= 1]       <- floor(area_acre[is_T1 & area_acre >= 1] * 25)
  
  # T2/T3/T4: treat as FAR-based; use max FAR = 3.0 (your convention)
  is_T234 <- z %in% c("T2","T3","T4")
  cap_mf[is_T234] <- floor((area_sqft[is_T234] * 3.0) / avg_unit)
  
  pmax(cap_1fam, cap_2fam, cap_mf, na.rm = TRUE)
}

cap_units_under_zone <- function(zone, area_sqft, area_acre) {
  if (length(zone) == 1L) zone <- rep(zone, length(area_sqft))
  
  dplyr::case_when(
    zone %in% c("RL","H1","H2") ~ cap_rlh(zone, area_sqft),
    zone %in% c("RM1","RM2","RM3") ~ cap_rm_units(zone, area_sqft, avg_unit = AVG_NET_SQFT_PER_UNIT),
    zone %in% c("T1","T2","T3","T4") ~ cap_t_units(zone, area_sqft, area_acre, avg_unit = AVG_NET_SQFT_PER_UNIT),
    TRUE ~ NA_real_
  )
}

# -------------------------
# 2) Sqft (FAR) capacity functions
# -------------------------

far_max_by_zone <- function(z) {
  # FAR maximums used for sqft-based capacity
  dplyr::case_when(
    z == "RM1" ~ 1.0,
    z == "RM2" ~ 2.25,
    z == "RM3" ~ 3.5,
    z %in% c("T2","T3","T4") ~ 3.0,
    TRUE ~ NA_real_  # RL/H1/H2/T1 not FAR-based in this setup
  )
}

cap_sqft_under_zone <- function(zone, area_sqft) {
  # allowed floor area = lot area * FARmax
  if (length(zone) == 1L) zone <- rep(zone, length(area_sqft))
  far <- far_max_by_zone(zone)
  ifelse(is.na(far), NA_real_, area_sqft * far)
}

# ==================================
# Apply capacities to parcel dataset
# ==================================

st_paul_24_zoning <- st_paul_24_zoning %>%
  filter(Zoning %in% res_districts) %>%
  mutate(
    cap_units_max = cap_units_under_zone(Zoning, area_sqft, area_acre),
    cap_units_max = pmax(cap_units_max, 1),
    
    far_max = far_max_by_zone(Zoning),
    cap_sqft_max = if_else(!is.na(far_max), area_sqft * far_max, NA_real_)
  )

# ==========================================
# Calibrate fiscal conversion factors (built)
# ==========================================

# (A) unit-based: tax capacity per realized unit (all zones where NUM_UNITS exists)
tc_per_unit <- st_paul_24_zoning %>%
  filter(
    vacant_strict == 0,
    Zoning %in% res_districts,
    !is.na(NUM_UNITS), NUM_UNITS > 0,
    !is.na(TAX_CAPAC), TAX_CAPAC > 0
  ) %>%
  mutate(tc_per_unit = TAX_CAPAC / NUM_UNITS) %>%
  group_by(Zoning) %>%
  summarise(
    n = n(),
    tc_per_unit_p50 = median(tc_per_unit, na.rm = TRUE),
    .groups = "drop"
  )

# (B) sqft-based: tax capacity per sqft of *allowed* floor area (RM/T FAR zones only)
# Optional winsorization to reduce crazy leverage points
winsorize <- function(x, p = 0.01) {
  lo <- quantile(x, p, na.rm = TRUE)
  hi <- quantile(x, 1 - p, na.rm = TRUE)
  pmin(pmax(x, lo), hi)
}

tc_per_sqft <- st_paul_24_zoning %>%
  filter(
    vacant_strict == 0,
    Zoning %in% c("RM1","RM2","RM3","T2","T3","T4"),
    !is.na(TAX_CAPAC), TAX_CAPAC > 0,
    !is.na(cap_sqft_max), cap_sqft_max > 0
  ) %>%
  mutate(
    tc_per_sqft_raw = TAX_CAPAC / cap_sqft_max,
    tc_per_sqft = winsorize(tc_per_sqft_raw, p = 0.01)
  ) %>%
  group_by(Zoning) %>%
  summarise(
    n = n(),
    tc_per_sqft_p50 = median(tc_per_sqft, na.rm = TRUE),
    .groups = "drop"
  )

# =====================================
# Incremental curve 1: units-based (old)
# =====================================

incremental_tc_units <- map_dfr(seq_len(length(zoning_ladder) - 1), function(i) {
  
  z_from <- zoning_ladder[i]
  z_to   <- zoning_ladder[i + 1]
  
  tc_to <- tc_per_unit %>%
    filter(Zoning == z_to) %>%
    pull(tc_per_unit_p50)
  
  if (length(tc_to) == 0 || is.na(tc_to)) return(NULL)
  
  st_paul_24_zoning %>%
    filter(Zoning == z_from) %>%
    mutate(
      cap_from = cap_units_under_zone(z_from, area_sqft, area_acre),
      cap_to   = cap_units_under_zone(z_to,   area_sqft, area_acre),
      delta_units = pmax(cap_to - cap_from, 0),
      delta_tax_capacity = delta_units * tc_to
    ) %>%
    summarise(
      zoning_from = z_from,
      zoning_to   = z_to,
      parcels     = n(),
      total_delta_units = sum(delta_units, na.rm = TRUE),
      total_delta_tax_capacity = sum(delta_tax_capacity, na.rm = TRUE),
      mean_delta_tc_per_parcel = mean(delta_tax_capacity, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(
    model = "units",
    ladder_step = paste(zoning_from, "→", zoning_to),
    step = row_number()
  ) %>%
  arrange(step) %>%
  mutate(cumulative_tax_capacity = cumsum(total_delta_tax_capacity))

# ==========================================
# Incremental curve 2: sqft-based (corrected)
# ==========================================

incremental_tc_sqft <- map_dfr(seq_len(length(zoning_ladder) - 1), function(i) {
  
  z_from <- zoning_ladder[i]
  z_to   <- zoning_ladder[i + 1]
  
  far_from <- far_max_by_zone(z_from)
  far_to   <- far_max_by_zone(z_to)
  
  # only steps where BOTH zones are FAR-based
  if (is.na(far_from) || is.na(far_to)) return(NULL)
  
  tc_sqft_to <- tc_per_sqft %>%
    filter(Zoning == z_to) %>%
    pull(tc_per_sqft_p50)
  
  if (length(tc_sqft_to) == 0 || is.na(tc_sqft_to)) return(NULL)
  
  st_paul_24_zoning %>%
    filter(Zoning == z_from) %>%
    mutate(
      cap_sqft_from = area_sqft * far_from,
      cap_sqft_to   = area_sqft * far_to,
      delta_sqft = pmax(cap_sqft_to - cap_sqft_from, 0),
      delta_tax_capacity = delta_sqft * tc_sqft_to
    ) %>%
    summarise(
      zoning_from = z_from,
      zoning_to   = z_to,
      parcels     = n(),
      total_delta_sqft = sum(delta_sqft, na.rm = TRUE),
      total_delta_tax_capacity = sum(delta_tax_capacity, na.rm = TRUE),
      mean_delta_tc_per_parcel = mean(delta_tax_capacity, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(
    model = "sqft",
    ladder_step = paste(zoning_from, "→", zoning_to),
    step = row_number()
  ) %>%
  arrange(step) %>%
  mutate(cumulative_tax_capacity = cumsum(total_delta_tax_capacity))

# ======================
# Plot: separate graphs
# ======================

# Units-based marginal
ggplot(incremental_tc_units, aes(x = ladder_step, y = total_delta_tax_capacity)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Zoning upgrade",
    y = "Incremental tax capacity (units-based)",
    title = "Marginal tax capacity gains (units-based model)"
  )

# Units-based cumulative
ggplot(incremental_tc_units, aes(x = step, y = cumulative_tax_capacity)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = incremental_tc_units$step,
                     labels = incremental_tc_units$ladder_step) +
  theme_minimal() +
  labs(
    x = "Zoning ladder step",
    y = "Cumulative tax capacity (units-based)",
    title = "Cumulative fiscal gains (units-based model)"
  )

# Sqft-based marginal
ggplot(incremental_tc_sqft, aes(x = ladder_step, y = total_delta_tax_capacity)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Zoning upgrade",
    y = "Incremental tax capacity (sqft-based)",
    title = "Marginal tax capacity gains (sqft/FAR-based model)"
  )

# Sqft-based cumulative
ggplot(incremental_tc_sqft, aes(x = step, y = cumulative_tax_capacity)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = incremental_tc_sqft$step,
                     labels = incremental_tc_sqft$ladder_step) +
  theme_minimal() +
  labs(
    x = "Zoning ladder step",
    y = "Cumulative tax capacity (sqft-based)",
    title = "Cumulative fiscal gains (sqft/FAR-based model)"
  )

print(unique(incremental_tc_sqft$ladder_step))
print(unique(incremental_tc_units$ladder_step))

# ============================================
# Plot: combined comparison (where both exist)
# ============================================

combined <- bind_rows(
  incremental_tc_units %>% select(model, ladder_step, total_delta_tax_capacity, cumulative_tax_capacity),
  incremental_tc_sqft  %>% select(model, ladder_step, total_delta_tax_capacity, cumulative_tax_capacity)
)

# # Combined marginal comparison (faceted)
# ggplot(combined, aes(x = ladder_step, y = total_delta_tax_capacity)) +
#   geom_col() +
#   facet_wrap(~model, scales = "free_y") +
#   theme_minimal() +
#   labs(
#     x = "Zoning upgrade",
#     y = "Incremental tax capacity",
#     title = "Marginal tax capacity gains: units-based vs sqft-based"
#   )
# 
# # Combined cumulative comparison (overlay)
# ggplot(combined, aes(x = ladder_step, y = cumulative_tax_capacity, group = model)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   theme_minimal() +
#   labs(
#     x = "Zoning ladder step",
#     y = "Cumulative tax capacity",
#     title = "Cumulative fiscal gains: units-based vs sqft-based"
#   )