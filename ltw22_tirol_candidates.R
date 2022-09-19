library(tidyverse)
library(pdftools)
library(readxl)
library(janitor)
library(rgdal)
library(broom)
library(maptools)
library(ggiraph)
library(reactable)
library(htmltools)


###################
# candidates list #
###################

# data source: https://www.tirol.gv.at/buergerservice/landtagswahl-2022/barrierefreies-waehlen/barrierefreie-bekanntmachungen/
## last accessed: 2022-09-09 

ltw_wahlvorschlag_raw = pdf_text("data/Kundmachung_Landeswahlvorschlag.pdf")

ltw_wahlvorschlag_tibble = as_tibble(ltw_wahlvorschlag_raw)
ltw_wahlvorschlag_tibble <- as_tibble(unlist(str_split(ltw_wahlvorschlag_tibble$value, "\n")))

titles = c("BA,|BSc,|BEd,|MEd,|MA,|MBA MSc|M\\.A\\.|MBA,|MMag\\.|Mag\\. iur\\.|Mag\\.a phil\\.|Mag\\.a|Mag\\.|Ing.in|PhD,|Dr\\.in|Dr\\.[[:blank:]]med\\.|Dr\\.|Dipl\\.\\-Ing\\.|Ing\\.in|Ing\\.|Dipl\\.[[:blank:]]BW|[[:blank:]]DI[[:blank:]]|^DI[[:blank:]]|\\(FH\\)|Univ\\. Prof\\.")


ltw_wahlvorschlag = ltw_wahlvorschlag_tibble %>%
  filter(str_detect(value, "^\\d")) %>%
  mutate(listenplatz = str_extract(value, "\\d+\\."),
         title = str_extract_all(value, titles)) %>%
  mutate(value = str_trim(str_replace(value, "\\d+\\.", "")),
         value = str_trim(str_replace_all(value, titles, "")),
         value = str_replace(value, ", ,", ","),
         listenplatz = str_replace(listenplatz, "\\.", ""),
         listenplatz = as.numeric(listenplatz)) %>%
  mutate(value = str_replace(value, "^,", "")) %>%
# separate name from rest
  separate(value, into=c("name", "value"), sep = ",", extra = "merge") %>%
  mutate(value = str_trim(value),
         name = str_trim(name)) %>%
# separate birthdate from rest and clean
  separate(value, into=c("birthdate", "value"), sep = ",", extra = "merge") %>%
  mutate(value = str_trim(value),
         birthdate = as.numeric(str_replace_all(birthdate, "\\D", ""))) %>%
# separate place of living from rest (i.e. occupations)
  mutate(city = str_extract(value, "\\d.+"),
         value = str_replace(value, ",[[:blank:]]\\d.+", "")) %>%
  mutate(plz = str_extract(city, "\\d+"),
         city = str_replace(city, "\\d+", "")) %>%
# add missing city and plz
  mutate(city = ifelse(name == "UNTERRAINER Maximilian", "Absam", city),
         plz = ifelse(name == "UNTERRAINER Maximilian", 6067, plz)) %>%
  mutate(plz = as.factor(str_trim(plz)),
         city = str_trim(city)) %>%
# add missing party affiliation
  mutate(party = factor(rep(c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "Liste FRITZ", "NEOS", "MFG", "KPÖ", "MACH MIT"), times = c(72, 72, 72, 72, 72, 72, 47, 16, 11)), levels = c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Liste FRITZ", "MFG", "KPÖ", "MACH MIT"))) %>%
# calculate age
  mutate(age = 2022-birthdate) %>%
  mutate(age_bins = ifelse(age >= 16 & age <= 25, 20, 
                      ifelse(age >= 26 & age <= 35, 30,
                        ifelse(age >= 36 & age <= 45, 40,
                         ifelse(age >= 46 & age <= 55, 50,
                           ifelse(age >= 56 & age <= 65, 60,
                            ifelse(age >= 66 & age <= 75, 70, 80)))))))



rm(ltw_wahlvorschlag_raw, ltw_wahlvorschlag_tibble, titles)


###################
# metadata cities #
###################

# data source: https://www.tirol.gv.at/statistik-budget/statistik/gemeindedaten/
## last accessed: 2022-09-09

meta_cities_header <- read_excel('data/gem_plv_HTML_2021_layout.xlsx', n_max = 2, col_names = F)
meta_cities <- read_excel('data/gem_plv_HTML_2021_layout.xlsx', skip = 3)

meta_cities_header = mapply(paste, sep = " ", meta_cities_header[c(T,F),], meta_cities_header[c(F,T),])
meta_cities_header = str_replace_all(meta_cities_header, "NA", "")

colnames(meta_cities) = meta_cities_header

names(meta_cities)[names(meta_cities) == 'Bezirk '] <- 'City'

meta_cities = meta_cities %>%
  clean_names() %>%
  remove_empty() %>%
  mutate(gemnr = as.numeric(ifelse(plv == 0, 70101, gemnr))) %>% # add gemnr for Innsbruck 
  filter(!is.na(wohnbevolkerung_01_01_2021) & gemnr != ".")

rm(meta_cities_header)



# candidates per city
city_candidates = ltw_wahlvorschlag  %>%
  mutate(city_clean = city,
         city = str_replace_all(city, "[[:blank:]]in[[:blank:]]|[[:blank:]]im[[:blank:]]|[[:blank:]]am[[:blank:]]|[[:blank:]]bei[[:blank:]]", "/"),
         city = str_replace_all(city, "ß", "ss"),
         city = str_replace(city, "St\\.[[:blank:]]", "St\\."),
         city = ifelse(city == "Erpfendorf", "Kirchdorf/Tirol", 
                       ifelse(city == "Iselsberg", "Iselsberg-Stronach",
                              ifelse(city == "Hall/Tirol", "Hall in Tirol",
                                     ifelse(city == "Kematen", "Kematen/Tirol",
                                            ifelse(city == "Mayrhofen/Zillertal", "Mayrhofen",
                                                   ifelse(city == "St.Jakob/Haus", "St.Jakob in Haus", 
                                                          ifelse(city == "Igls" | city == "Innsbruck/Igls", "Innsbruck",
                                                                 ifelse(city == "Mils", "Mils/Hall",
                                                                        ifelse(city == "Polling", "Polling/Tirol", city)))))))))) %>%
  group_by(city) %>%
  summarize(no_candidates = n(),
            mean = mean(age))

meta_candidates_cities = meta_cities %>%
  left_join(city_candidates, by = "city")



##############
# tyrol maps #
##############

# data source: https://www.data.gv.at/katalog/dataset/verwaltungsgrenzen-vgd-stichtagsdaten-tirol/resource/fc75bbf0-9b33-4ada-a684-b420ab7cd104
## last accessed: 2022-09-09

tyrol_spdf <- readOGR( 
  dsn= "data/shapefiles/", 
  layer="Tirol_BEV_VGD_50_LAM00",
  verbose=FALSE
)

gpclibPermit()

spdf_fortified <- tidy(tyrol_spdf, region = "PG")

spdf_fortified = tyrol_spdf@data %>%
  select(GKZ, PG) %>%
  unique() %>%
  right_join(spdf_fortified, by = c("PG"="id")) %>%
  mutate(GKZ = as.numeric(GKZ))

spdf_enriched = spdf_fortified %>%
  left_join(meta_candidates_cities, by = c("GKZ"="gemnr")) %>%
  mutate(no_candidates = ifelse(is.na(no_candidates), 0, no_candidates),
         candidates_percent = no_candidates / wohnbevolkerung_01_01_2021, # pecentage of candidates relative to number of residents
         candidates_perthousand = 1000 / wohnbevolkerung_01_01_2021 * no_candidates) %>% # number of candidates per thousand residents
  mutate(label_fullcounting = paste0(PG, ": ", no_candidates),
         label_percent = paste0(PG, ": ", round(candidates_percent,4)*100, "%"),
         label_perthousand = paste0(PG, ": ", round(candidates_perthousand,2)))


spdf_enriched$oc = "alert(this.getAttribute(\"data-id\"))"

rm(spdf_fortified)

#-----------
# map: candidates per city
#-----------

tyrol_full = ggplot(spdf_enriched, aes(x = long, y = lat)) +
  geom_polygon_interactive(aes(group = group, fill= sqrt(no_candidates), tooltip = label_fullcounting, data_id = oc), color="grey80") +
  theme_void() +
  theme(legend.position = "none", )  +
  scale_fill_gradient(low = "white", high = "red")


tyrol_full_rendered = girafe(ggobj = tyrol_full, width_svg = 8.5, height_svg = 5)
htmlwidgets::saveWidget(tyrol_full_rendered, "graphs/interactive/tyrol_full_interactive.html")

png("graphs/png/tyrol_full.png", width = 1500, height = 900, res = 150)
ggplot(spdf_enriched, aes(x = long, y = lat)) +
  geom_polygon_interactive(aes(group = group, fill= sqrt(no_candidates), tooltip = label_fullcounting, data_id = oc), color="grey80") +
  theme_void() +
  theme(legend.position = "none", )  +
  scale_fill_gradient(low = "white", high = "red")
dev.off()



#-----------
# map: candidates per 1,000 residents
#-----------
tyrol_perthousand = ggplot(spdf_enriched, aes(x = long, y = lat)) +
  geom_polygon_interactive(aes(group = group, fill= candidates_perthousand, tooltip = label_perthousand, data_id = oc), color="grey80") +
  theme_void() +
  theme(legend.position = "none")  +
  scale_fill_gradient(low = "white", high = "red")

tyrol_perthousand_rendered = girafe(ggobj = tyrol_perthousand, width_svg = 8.5, height_svg = 5)
htmlwidgets::saveWidget(tyrol_perthousand_rendered, "graphs/interactive/tyrol_perthousand_interactive.html")

png("graphs/png/tyrol_perthousand.png", width = 1500, height = 900, res = 150)
tyrol_perthousand
dev.off()



#-----------
# map: candidates per party and district
#-----------

bt_districts = tyrol_spdf@data %>%
  select(GKZ, PG, PB) %>%
  mutate(GKZ = as.numeric(GKZ))


city_party_candidates = ltw_wahlvorschlag  %>%
  mutate(city_clean = city,
         city = str_replace_all(city, "[[:blank:]]in[[:blank:]]|[[:blank:]]im[[:blank:]]|[[:blank:]]am[[:blank:]]|[[:blank:]]bei[[:blank:]]", "/"),
         city = str_replace_all(city, "ß", "ss"),
         city = str_replace(city, "St\\.[[:blank:]]", "St\\."),
         city = ifelse(city == "Erpfendorf", "Kirchdorf/Tirol", 
                       ifelse(city == "Iselsberg", "Iselsberg-Stronach",
                              ifelse(city == "Hall/Tirol", "Hall in Tirol",
                                     ifelse(city == "Kematen", "Kematen/Tirol",
                                            ifelse(city == "Mayrhofen/Zillertal", "Mayrhofen",
                                                   ifelse(city == "St.Jakob/Haus", "St.Jakob in Haus", 
                                                          ifelse(city == "Igls" | city == "Innsbruck/Igls", "Innsbruck",
                                                                 ifelse(city == "Mils", "Mils/Hall",
                                                                        ifelse(city == "Polling", "Polling/Tirol", city)))))))))) %>%
  group_by(city, party) %>%
  summarize(no_candidates = n())


meta_candidates_party_cities = meta_cities %>%
  left_join(city_party_candidates, by = "city") %>%
  left_join(bt_districts, by = c("gemnr" = "GKZ")) %>%
  unique()

meta_candidates_party_districts = meta_candidates_party_cities %>%
  group_by(PB, party) %>%
  summarize(no_candidates = sum(no_candidates)) %>%
  filter(!is.na(no_candidates))


spdf_fortified_district <- tidy(tyrol_spdf, region = "PB")

spdf_fortified_district = tyrol_spdf@data %>%
  select(PB) %>%
  unique() %>%
  right_join(spdf_fortified_district, by = c("PB"="id"))


spdf_district_enriched_party = spdf_fortified_district %>%
  left_join(meta_candidates_party_districts, by = "PB") %>%
  mutate(no_candidates = ifelse(is.na(no_candidates), 0, no_candidates)) %>%
  mutate(label_fullcounting = paste0(PB, ": ", no_candidates),
         party = factor(party, levels = c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Liste FRITZ", "MFG", "KPÖ", "MACH MIT")))

rm(spdf_fortified_district)

spdf_district_enriched_party$oc = "alert(this.getAttribute(\"data-id\"))"


tyrol_party_district = ggplot(spdf_district_enriched_party %>% filter(!is.na(party)), aes(x = long, y = lat)) +
  geom_polygon_interactive(aes(group = group, fill= no_candidates, tooltip = label_fullcounting, data_id = oc), color="grey80") +
  facet_wrap(~party) +
  theme_void() +
  theme(legend.position = "none")  +
  scale_fill_gradient(low = "#faf0f0", high = "red")


tyrol_party_district_rendered = girafe(ggobj = tyrol_party_district, width_svg = 8.5, height_svg = 5)
htmlwidgets::saveWidget(tyrol_party_district_rendered, "graphs/interactive/tyrol_party_district_interactive.html")

png("graphs/png/tyrol_party_district.png", width = 1500, height = 900, res = 150)
tyrol_party_district
dev.off()




#-----------
# table: candidates per city
#-----------

ltw_wahlvorschlag_city_clean = ltw_wahlvorschlag  %>%
  mutate(city_clean = city,
         city = str_replace_all(city, "[[:blank:]]in[[:blank:]]|[[:blank:]]im[[:blank:]]|[[:blank:]]am[[:blank:]]|[[:blank:]]bei[[:blank:]]", "/"),
         city = str_replace_all(city, "ß", "ss"),
         city = str_replace(city, "St\\.[[:blank:]]", "St\\."),
         city = ifelse(city == "Erpfendorf", "Kirchdorf/Tirol", 
                       ifelse(city == "Iselsberg", "Iselsberg-Stronach",
                              ifelse(city == "Hall/Tirol", "Hall in Tirol",
                                     ifelse(city == "Kematen", "Kematen/Tirol",
                                            ifelse(city == "Mayrhofen/Zillertal", "Mayrhofen",
                                                   ifelse(city == "St.Jakob/Haus", "St.Jakob in Haus", 
                                                          ifelse(city == "Igls" | city == "Innsbruck/Igls", "Innsbruck",
                                                                 ifelse(city == "Mils", "Mils/Hall",
                                                                        ifelse(city == "Polling", "Polling/Tirol", city))))))))))



city_plz <- unique(ltw_wahlvorschlag_city_clean[, c("city", "plz")])

city_plz_wide = city_plz %>%
  group_by(city) %>%
  mutate(no_plz = paste0("plz_", row_number())) %>%
  pivot_wider(id_cols = "city", names_from = no_plz, values_from = "plz") %>%
  mutate(plz_comb = ifelse(is.na(plz_2), paste0(plz_1), ifelse(!is.na(plz_2) & is.na(plz_3), paste0(plz_1, ", ", plz_2), paste0(plz_1, ", ", plz_2, ", ", plz_3)))) %>%
  select(-c(plz_1, plz_2, plz_3))


wahlvorschlag_react = reactable(
  city_plz_wide,
  searchable = TRUE,
  columns = list(
    city = colDef(name = "Gemeinde"),
    plz_comb = colDef(name = "Postleitzahl")),
  highlight = TRUE,
  details = function(index) {
    candidates <- ltw_wahlvorschlag_city_clean[ltw_wahlvorschlag_city_clean$city == city_plz_wide$city[index],]
    htmltools::div(style = "padding: 1rem",
                   reactable(candidates %>% select(name, birthdate, value, listenplatz, party),
                             columns = list(
                               name = colDef(name = "Kandidat_in"),
                               birthdate = colDef(name = "Geburtsjahr"),
                               value = colDef(name = "Beruf"), 
                               listenplatz = colDef(name = "Listenplatz"),
                               party = colDef(name = "Parteizugehörigkeit",
                                              filterable = TRUE,
                                              filterInput = function(values, name) {
                                                tags$select(
                                                  # Set to undefined to clear the filter
                                                  onchange = sprintf("Reactable.setFilter('party-select', '%s', event.target.value || undefined)", name),
                                                  # "All" has an empty value to clear the filter, and is the default option
                                                  tags$option(value = "", "Alle"),
                                                  lapply(unique(values), tags$option),
                                                  "aria-label" = sprintf("Filter %s", name),
                                                  style = "width: 100%; height: 28px;"
                                                )
                                              })),
                             elementId = "party-select",
                             outlined = TRUE,
                             highlight = TRUE))
  }
)



htmlwidgets::saveWidget(wahlvorschlag_react, "graphs/interactive/wahlvorschlag.html")

