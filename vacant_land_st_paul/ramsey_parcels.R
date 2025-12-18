
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(beepr)

ramsey_data_24 <- st_read("../vacant_land_st_paul/shp_plan_regonal_parcels_2024", layer = "Parcels2024Ramsey")
names(ramsey_data_24)
st_paul_24 <- subset(ramsey_data_24, CTU_NAME %in% c("Saint Paul"))

#identify vacant parcels in St. Paul ----
st_paul_24 <- st_paul_24 %>%
  mutate(
    vacant_strict = ifelse(
      grepl("VACANT|UNIMPROVED", DWELL_TYPE, ignore.case = TRUE) &
        EMV_BLDG == 0 &
        NUM_UNITS == 0 &
        !grepl("FORD LOT", ABB_LEGAL, ignore.case = TRUE)&
        !grepl(c("HIGHLAND BRIDGE ROWHOMES"), PLAT_NAME, ignore.case = TRUE),
      1, 0
    ),
    parcel_type = ifelse(
      vacant_strict == 1,
      DWELL_TYPE,
      "Other Parcels"
    )
  )

# plot them
sum(st_paul_24$vacant_strict,na.rm=T)
st_paul_24_v <- subset(st_paul_24,vacant_strict==1)
ggplot(st_paul_24) +
  geom_sf(aes(fill = parcel_type), color = NA) +
  scale_fill_manual(
    values = c(
      "Other Parcels" = "grey80",
      "RESIDENTIAL, VACANT LAND, LOT" = "red",
      "APARTMENT VACANT LAND" = "blue",
      "TOWNHOME - VACANT LOT" = "green",
      "CONDO VACANT LAND" = "purple"
    ),
    breaks = c("RESIDENTIAL, VACANT LAND, LOT", "APARTMENT VACANT LAND", "TOWNHOME - VACANT LOT", "CONDO VACANT LAND", "Other Parcels"),
    name = "Parcel Type"
  ) +
  theme_minimal()

ggplot(st_paul_24) +
  geom_sf(aes(fill = factor(vacant_strict))) +
  scale_fill_manual(values = c("0" = "grey80", "1" = "red"))

vac <- st_paul_24 %>% filter(vacant_strict == 1 & !is.na(USECLASS1))
vac_ll <- sf::st_transform(vac, 4326)
bb <- sf::st_bbox(vac_ll)
pal <- colorFactor(palette = "Set3", domain = vac_ll$USECLASS1, na.color = "#BDBDBD")

leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  fitBounds(unname(bb["xmin"]), unname(bb["ymin"]), unname(bb["xmax"]), unname(bb["ymax"])) %>%
  addPolygons(
    data = vac_ll,
    fillColor = ~pal(USECLASS1),
    color = ~pal(USECLASS1),
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>USECLASS1: </b>", USECLASS1,
      "<br><b>DWELL_TYPE: </b>", DWELL_TYPE,
      "<br><b>EMV_BLDG: </b>", EMV_BLDG
    ),
    highlightOptions = highlightOptions(weight = 1.5, opacity = 1, fillOpacity = 0.9)
  ) %>%
  addLegend(pal = pal, values = vac_ll$USECLASS1, title = "USECLASS1", opacity = 0.7)

# Crude Calculatation of taxes ----
stpaul_summary <- st_paul_24 %>%
  st_drop_geometry()%>%
  summarise(
    total_tax_capacity = sum(TAX_CAPAC, na.rm = TRUE),
    levy = sum(TOTAL_TAX, na.rm = TRUE)
  )

stpaul_summary <- stpaul_summary %>%
  mutate(tax_rate = total_tax_capacity  / levy)
# Didn't match observed rate - could be do to many things

# New tax calculation ----
st_paul_24$city_rate <- .497
st_paul_24$city_tax_est <- st_paul_24$TAX_CAPAC*st_paul_24$city_rate # what does each parcel pay to the city?
sum(st_paul_24$city_tax_est,na.rm = T)
unique(st_paul_24$DWELL_TYPE[st_paul_24$vacant == 1])
st_paul_24$tax_acre <- st_paul_24$city_tax_est/st_paul_24$ACRES_POLY
hist(st_paul_24$tax_acre)
# st_paul_24 <- st_paul_24 %>%
#   mutate(
#     parcel_type = ifelse(vacant == 1, DWELL_TYPE, "Other Parcels")
#   )
st_paul_24 %>%
  st_drop_geometry()%>%
  filter(tax_acre > 0) %>%
  group_by(vacant_strict)%>%
  summarize(avg_tax_acre = mean(tax_acre,na.rm=T))

st_paul_24 <- st_paul_24 %>%
  mutate(super_class = case_when(
    # --- Vacant (priority) ---
    grepl("VACANT|UNIMPROVED", USECLASS1, ignore.case = TRUE) ~ "Vacant",
    
    # --- Single-Family Residential ---
    grepl("SINGLE UNIT", USECLASS1, ignore.case = TRUE) ~ "Single-Family Residential",
    
    # --- Multi-Family Residential ---
    grepl("1-3 UNITS|4 OR MORE UNITS|APARTMENT|HOUSING|4D", USECLASS1, ignore.case = TRUE) ~ "Multi-Family Residential",
    
    # --- Commercial ---
    grepl("COMMERCIAL", USECLASS1, ignore.case = TRUE) ~ "Commercial",
    
    # --- Manufacturing / Industrial ---
    grepl("INDUSTRIAL|MANUFACTURING", USECLASS1, ignore.case = TRUE) ~ "Manufacturing / Industrial",
    
    # --- Agricultural ---
    grepl("AGRICULTURAL", USECLASS1, ignore.case = TRUE) ~ "Agricultural",
    
    # --- Exempt / Institutional / Government ---
    grepl("5E|EXEMPT|CHURCH|SCHOOL|COLLEGE|HOSPITAL|MUNICIPAL|STATE|COUNTY|PUBLIC|FEDERAL|CHARITABLE|CEMETERY|PILT",
          USECLASS1, ignore.case = TRUE) ~ "Exempt / Institutional / Government",
    
    # --- Catch-all ---
    TRUE ~ "Other / Unknown"
  ))
st_paul_24 %>%
  st_drop_geometry()%>%
  filter(tax_acre > 0) %>%
  group_by(super_class)%>%
  summarize(avg_tax_acre = mean(tax_acre,na.rm=T),
            avg_tax = mean(city_tax_est,na.rm = T))%>%
  arrange(desc(avg_tax_acre))

ggplot(st_paul_24) +
  geom_sf(aes(fill = parcel_type), color = NA) +
  scale_fill_manual(
    values = c(
      "Other Parcels" = NA,
      "RESIDENTIAL, VACANT LAND, LOT" = "red",
      "APARTMENT VACANT LAND" = "blue",
      "TOWNHOME - VACANT LOT" = "green",
      "CONDO VACANT LAND" = "purple"
    ),
    name = "Parcel Type"
  ) +
  theme_minimal()

st_paul_vacant <- st_paul_24 %>%
  filter(vacant_strict == 1)

ggplot() +
  geom_sf(data = st_paul_24) +
  geom_sf(data = st_paul_vacant, fill = "red", color = NA) +
  labs(
    title = "Map of Vacant Parcels in St. Paul",
    subtitle = "Vacant Parcels in Red"
  ) +
  theme_minimal()

# Verify that data has a vacant flag 
flag_col <- if ("vacant_strict" %in% names(st_paul_24)) "vacant_strict" else stop("No vacant flag found")

# make sure tax rate and dollars are in data
st_paul_24 <- st_paul_24 %>%
  mutate(city_rate = if (!"city_rate" %in% names(st_paul_24)) 0.497 else city_rate,
         city_tax_est = TAX_CAPAC * city_rate)

# find the total revenue from property taxes
city_levy_est <- st_paul_24 %>% st_drop_geometry() %>% summarise(v = sum(city_tax_est, na.rm = TRUE)) %>% pull(v)

#simulate 2024 shortfall
shortfall <- 0.053 * city_levy_est
shortfall

n_vacant <- sum(st_paul_24$vacant_strict, na.rm = TRUE)
n_vacant

current_fee <- 2705
required_fee_full <- if (n_vacant > 0) shortfall / n_vacant else NA_real_
required_fee_full

multiplier_full <- required_fee_full / current_fee
multiplier_full

sensitivity <- tibble(
  compliance = seq(0.5, 1.0, by = 0.05)
) %>%
  mutate(
    effective_payers = n_vacant * compliance,
    required_fee = ifelse(effective_payers > 0, shortfall / effective_payers, NA_real_),
    multiplier = required_fee / current_fee
  )

summary_table <- tibble(
  city_levy_est = city_levy_est,
  shortfall_5_9pct = shortfall,
  n_vacant_flagged = n_vacant,
  current_fee = current_fee,
  required_fee_full_compliance = required_fee_full,
  multiplier_full_compliance = multiplier_full
)

list(summary = summary_table, sensitivity = sensitivity)


plot_data <- sensitivity %>%
  mutate(compliance = compliance * 100)  # convert to %

ggplot(plot_data, aes(x = compliance, y = required_fee)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(aes(label = scales::dollar(round(required_fee, -1))), 
            vjust = -1, size = 3.5, color = "black") +
  geom_hline(yintercept = current_fee, linetype = "dashed", color = "red") +
  annotate("text", x = 62, y = current_fee + 300, 
           label = "Current Minimum Fee ($2,705)", color = "red", hjust = 0) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(50, 100, by = 5), 
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Required Vacant Building Fee to Cover St. Paul’s 5.9% Shortfall",
    x = "Vacany Miscount Rate",
    y = "Required Fee per Vacant Parcel"
  ) +
  theme_minimal(base_size = 14)


csv <- subset(st_paul_24, ANUMBER == "499" & ST_NAME == "Snelling")
