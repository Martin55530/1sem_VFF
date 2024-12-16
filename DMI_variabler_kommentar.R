# Pakker vi loader med pacman
pacman::p_load(httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler, rvest)
# ------------------------------------------------------------------------------

# Henter data fra Superstats

# Laver en tom tibble
sl_tabs_samlede_13_23 <- list()

# Webscraper data fra superstats for sæsonerne 2013 til 2023
for (y in 2013:2023) {
  url <- paste0("https://superstats.dk/program?aar=", y, "%2F", y + 1)
  sl_tabs_alle_13_23 <- read_html(url, encoding = "UTF-8") |> 
    html_elements("table") |> 
    html_table(header = FALSE, convert = FALSE)
  
  # Filtrer tabeller for den valgte sæson
  sl_tabs_valgte_13_23 <- sl_tabs_alle_13_23[
    sapply(sl_tabs_alle_13_23, function(tibble) {
      # Tjek om tibble er tom
      if (nrow(tibble) == 0) return(FALSE)
      # Tjek om vigtige kolonner findes
      if (!all(c("X3", "X4", "X5") %in% names(tibble))) return(FALSE)
      # Tjek om relevante kolonner har gyldige værdier
      any(tibble$X3 != "" | tibble$X4 != "" | tibble$X5 != "")
    })
  ]
  
  # Konverter tabeller til tibble og tilføj sæsonkolonne
  for (i in seq_along(sl_tabs_valgte_13_23)) {
    temp <- as_tibble(sl_tabs_valgte_13_23[[i]]) |> 
      mutate(sæson = paste0(y, "/", y + 1))
    sl_tabs_samlede_13_23 <- bind_rows(sl_tabs_samlede_13_23, temp)
  }
}

# Tjekker hvordan tibblen ser ud
sl_tabs_samlede_13_23

# Fjern kolonnerne X1, X6, X7 og X8
sl_tabs_samlede_13_23 <- sl_tabs_samlede_13_23 |> 
  select(-X1, -X6, -X7, -X8)

# Fjern tomme rækker
sl_tabs_samlede_13_23 <- sl_tabs_samlede_13_23 |> 
  filter(!(X3 == "" & X4 == "" & X5 == ""))

# Filtrer kampene med VFF i kolonne X3
vff_kampe <- sl_tabs_samlede_13_23 |> 
  filter(grepl("VFF", X3))

# Omdøbning af kolonnenavne
vff_kampe <- vff_kampe |> 
  rename(
    dato = X2,
    kamp = X3,
    resultat = X4,
    antal_tilskuere = X5
  )

# Vis kun VFF-kampene i superligaen
vff_kampe

# Viser sæsoner hvor VFF var i superligaen og antal kampe pr. sæson
vff_kampe |> 
  count(sæson) # Har tjekket at det stemmer med antal kampe i sæsonen på superstats

#--------------------------------------------------------------------------------

# Henter data fra DMI

# https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06180&datetime=2020-07-01T12:00:00Z/2020-07-31T12:00:00Z&api-key=48de7afb-236e-430a-b814-130c6d2290f6

# Dette er basen i den fulde URL der skabes senere. Man kan altid bygge videre på den med en anden info URL
base_url <- "https://dmigw.govcloud.dk/v2/" 

# Vi ønsker at anvende MetObs og det er det som info_url linker til
info_url <- "metObs/collections/observation/items?"

# Her henviser vi til den station vi ønsker at trække meteoroligsk data fra, samt tidspunktet for observatioenn
req_url <- "stationId=06060&datetime=2013-01-01T12:00:00Z/2024-12-31T12:00:00Z&limit=100000"

# Her skal man skrive adgangskoden til sin API - husk at skrive din egen adgangskode
api_key <- "&api-key=8c762424-f683-4ad0-acea-197dc19ff258"

# Her sætter vi ovenstående bidder sammen til en full URL, som vi anvender senere
full_url <- base::paste0(base_url, info_url, req_url, api_key)

# Her viser vi den fulde URL
full_url

# Den efter spørger de data vi gerne vil have hentet (Angivet i req URL)
api_call <- httr::GET(full_url)

# Her tester vi hvad vi får tilbage. Vi vil gerne have det tilbage i JSON formatet
http_type(api_call)

# Her efterspørger vi om forespørgslen er succesfuld (200) ellers (400+) hvis det ikke lykkedes
api_call$status_code

# Her henter den rådata ned. Det ses som hexadecimal (talsystem med en base på 16, istedet for 10 som vi kender)
api_call$content

# Her laver vi rådataen om til en character
api_char <- base::rawToChar(api_call$content)

# Vi laver det om til en tabel, som R kan arbejde videre med (Flatten = TRUE gør at listerne bliver fladet ud og kolonnenavne laves)
api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)

# Vi laver api_JSON om til en liste og navngiver den list_dmi
list_dmi <- api_JSON

# Her laver vi listen om til en data frame og laver kolonner
coldmi <- as.data.frame(do.call(cbind, list_dmi))

# Her laver vi tre kolonner, hvor vi udvælger de data vi vil have med
col1 <- as.vector(coldmi[7]) #features.properties.observed
col2 <- as.vector(coldmi[8]) #features.properties.parameterID
col3 <- as.vector(coldmi[10]) #features.properties.value

# Her laver vi det om til en tibble, med de kolonner vi har udvalgt
add_cols <- c(col1, col2, col3)
dmi <- as_tibble(add_cols)

# Her ændrer vi navnene på de kolonner vi har udvalgt
dmi <- dmi |>   
  rename("Observationstidspunkt" =  "features.properties.observed", "Observationer" = "features.properties.parameterId", "Value" = "features.properties.value" )

# Her får vi et overblik over hvilke variabler vi har i vores tibble
variabler_datasæt <- dmi |> 
  distinct(Observationer)

view(variabler_datasæt)

# # Antag, at din dataframe hedder "data"
# ekstra_kolonner <- dmi  |> 
#   mutate(
#     dato = as.Date(Observationstidspunkt, format = "%Y-%m-%dT%H:%M:%OSZ"),
#     tidspunkt = format(as.POSIXc(Observationstidspunkt, format = "%Y-%m-%dT%H:%M:%OSZ"), "%H:%M:%S")
#   )
# 
# 
# # Viser observationer kl. 12:00:00
# filtered_data <- data |> 
#   filter(str_detect(Observationstidspunkt, "12:00:00"))

# Foulum 56.4931 9.5709: 06069
# FLYVESTATION KARUP 56.2934 9.1139: 06060 

# precip_past1h
# temp_dry
# wind_speed


# temp_dry, temp_mean_past1h, temp_max_past1h, temp_min_past1h, temp_max_past12h, temp_min_past12h, humidity_past1h, wind_dir_past1h, wind_speed, wind_speed_past1h, wind_gust_always_past1h, wind_max_per10min_past1h, precip_past1h, precip_past24h, precip_dur_past1h, sun_last1h_glob, radia_glob_past1h, visibility, cloud_height, cloud_cover, snow_depth_man, snow_cover_man, temp_soil_max_past1h, humidity, temp_dew, precip_past10min, sun_last10min_glob, wind_min_past1h, wind_min, temp_grass_max_past1h, visib_mean_last10min, temp_grass_min_past1h, leav_hum_dur_past1h, weather, temp_grass, pressure, temp_soil, radia_glob, wind_max, pressure_at_sea, wind_dir, temp_soil_mean_past1h, precip_past1min, temp_grass_mean_past1h, precip_dur_past10min, leav_hum_dur_past10min, temp_soil_min_past1h


# https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06180&parameterId=wind_speed&datetime=2000-01-01T00:00:00Z/2000-01-31T23:59:00Z&api-key=48de7afb-236e-430a-b814-130c6d2290f6&limit=10000


# test 

