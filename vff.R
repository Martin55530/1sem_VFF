#-------------------------------------------------------------------------------
#--------------------------------------INTRO------------------------------------
#-------------------------------------------------------------------------------

# Workflow:
# 1. Dataforberedelse: Indlæsning og rengøring af data (Excel, Superstats, DMI)
# 2. Joins: Kombinering af datasæt til én samlet tibble
# 3. Analyse: Tilføj nye variabler og undersøg korrelationer
# 4. Modeller: Test flere modeller (Baseline, Best Subset, Ridge, Lasso)
# 5. Evaluering: Sammenlign modeller med RMSE og residual plots



#-------------------------------------------------------------------------------
#-------------------------------------PACMAN------------------------------------
#-------------------------------------------------------------------------------

pacman::p_load(
  httr,          # Til API-kald (DMI-data)
  jsonlite,      # Til parsing af JSON-svar fra API-kald
  tidyverse,     # Samling af pakker til datahåndtering og analyse (dplyr, ggplot2, readr osv.)
  rlist,         # Til manipulation af lister
  rvest,         # Til webscraping (Superstats-data)
  readxl,        # Til læsning af Excel-filer
  tidyr,         # Til transformation af data (fx pivot_wider)
  glmnet,        # Til Ridge og Lasso regression (eksempelvis bedste lambda)
  boot,          # Til krydsvalidering (fx cv.glm)
  leaps,         # Til Best Subset Selection
  MASS,          # Muligvis til regression eller lignende
  dplyr,         # Til datamanipulation (filtering, selektion osv.)
  stringr,       # Til strengehåndtering (fx grepl)
  lubridate      # Til dato- og tidsmanipulation
)



# ------------------------------------------------------------------------------
# -------------------------------Dataforberedelse-------------------------------
# ------------------------------------------------------------------------------



# Excel
# ------------------------------------------------------------------------------

# Læser Excel-filen med VIP-data og fjerner uønskede kolonner
excel_guld <- read_excel("Guld.xlsx") |>
  dplyr::select(-Gule_poletter_stk, -Kamp) |>
  na.omit()  # Fjerner rækker med NA-værdier, så vi har 98 kampe tilbage

# Konverterer kolonnen 'Dato' til formatet "YYYY-MM-DD" og omdøber til lowercase 'dato'
excel_guld <- excel_guld |>
  mutate(dato = if_else(
    stringr::str_detect(Dato, "\\."),  # Hvis Dato indeholder et punktum
    as.Date(Dato, format = "%d.%m.%Y"),  # Konverterer "dd.mm.yyyy" til "YYYY-MM-DD"
    as.Date(as.numeric(Dato), origin = "1899-12-30")  # Konverterer Excel-serienumre til datoer
  )) |>
  dplyr::select(-Dato)  # Fjerner den originale 'Dato'-kolonne, så kun 'dato' forbliver

# I Excel-filen kan datoen enten stå i formatet "dd.mm.yyyy" eller som et serienummer (fx 44406).
# Serienummeret repræsenterer antal dage siden Excels startdato, "1899-12-30".

# Fjerner yderligere uønskede variabler
excel_guld <- excel_guld |>
  dplyr::select(-Antal_bestilte, -Antal_max)

# Funktion til at finde outliers
find_outliers <- function(data, column) {
  x <- data[[column]]
  iqr <- IQR(x, na.rm = TRUE)  # Interkvartil-range
  lower_bound <- quantile(x, 0.25, na.rm = TRUE) - 1.5 * iqr
  upper_bound <- quantile(x, 0.75, na.rm = TRUE) + 1.5 * iqr
  which(x < lower_bound | x > upper_bound)  # Returnerer indeks for outliers
}

# Håndter outliers i Excel_guld
numeric_columns_guld <- excel_guld |> 
  dplyr::select(where(is.numeric)) |> 
  names()

for (col in numeric_columns_guld) {
  excel_guld[[col]][find_outliers(excel_guld, col)] <- NA  # Sæt outliers til NA eller en anden værdi
}



# Superstats
# ------------------------------------------------------------------------------

# Funktion til at validere en tabel
# Denne funktion tjekker, om en tabel indeholder relevante data.
# Tabelen er gyldig, hvis:
# 1. Den ikke er tom.
# 2. Kolonnerne X3, X4 og X5 findes.
# 3. Mindst én af kolonnerne X3, X4 eller X5 indeholder data.
valider_tabel <- function(tabel) {
  nrow(tabel) > 0 &&  # Tjek om tabellen ikke er tom
    all(c("X3", "X4", "X5") %in% names(tabel)) &&  # Tjek om nødvendige kolonner findes
    any(tabel$X3 != "" | tabel$X4 != "" | tabel$X5 != "")  # Tjek om der er gyldige værdier
}

# Funktion til at hente og filtrere tabeller for en enkelt sæson
# Input: Årstal (fx 2013 for sæsonen 2013/2014)
# Output: En tibble med data for den pågældende sæson, inkl. sæsonoplysninger
hent_superstats_data <- function(år) {
  # Bygger URL'en for den specifikke sæson
  url <- paste0("https://superstats.dk/program?aar=", år, "%2F", år + 1)
  
  # Henter HTML-indholdet fra URL'en og udtrækker tabeller
  alle_tabeller <- read_html(url, encoding = "UTF-8") |> 
    html_elements("table") |>  # Finder alle tabeller på siden
    html_table(header = FALSE, convert = FALSE)  # Konverterer tabellerne til R-objekter
  
  # Filtrerer tabellerne, så kun de med data bliver tilbage
  gyldige_tabeller <- purrr::keep(alle_tabeller, valider_tabel)  # Beholder kun gyldige tabeller
  
  # Tilføjer sæsonoplysninger til hver tabel og returnerer samlet tibble
  purrr::map_dfr(gyldige_tabeller, ~ as_tibble(.x) |> mutate(sæson = paste0(år, "/", år + 1)))
}

# Hent data for alle sæsoner (2013 til 2023) og saml dem i én tibble
# Her bruges `purrr::map_dfr` til at kombinere data fra alle sæsoner
superstats <- purrr::map_dfr(2013:2023, hent_superstats_data)

# Fjerner unødvendige kolonner og rækker uden data
# Dette gør datasættet mere overskueligt og relevant
superstats <- superstats |> 
  dplyr::select(-X1, -X6, -X7, -X8) |>  # Fjerner kolonner, vi ikke skal bruge
  filter(!(X3 == "" & X4 == "" & X5 == ""))  # Fjerner rækker uden værdier i X3, X4, X5

# Filtrerer kampe med "VFF" (Viborg FF) i kolonne X3
# Omdøber kolonnerne for nemmere brug
superstats_vff <- superstats |> 
  filter(str_detect(X3, "VFF")) |>  # Beholder kun rækker, hvor "VFF" optræder i kolonne X3
  rename(
    dato = X2,               # Omdøber kolonnen X2 til "dato"
    kamp = X3,               # Omdøber kolonnen X3 til "kamp"
    resultat = X4,           # Omdøber kolonnen X4 til "resultat"
    antal_tilskuere = X5     # Omdøber kolonnen X5 til "antal_tilskuere"
  )

# Filtrerer kun hjemmekampe for Viborg FF og justerer datoformatet
# Tilføjer årstal baseret på sæson og justerer datoen til korrekt format
superstats_hjemmekampe <- superstats_vff |> 
  filter(str_starts(kamp, "VFF")) |>  # Beholder kun rækker, hvor kampen starter med "VFF"
  mutate(
    dato = as.Date(dato, format = "%d/%m"),  # Konverterer datoen fra tekst til datoformat
    måned = as.numeric(format(dato, "%m")),  # Uddrager månedsnummeret fra datoen
    årstal = ifelse(
      måned >= 7,  # Hvis måneden er juli eller senere
      as.numeric(substr(sæson, 1, 4)),  # Brug årstallet fra sæsonens start
      as.numeric(substr(sæson, 6, 9))  # Ellers brug årstallet fra sæsonens slutning
    ),
    dato = as.Date(paste(årstal, format(dato, "%m-%d"), sep = "-"))  # Samler årstal og måned-dag
  ) |> 
  dplyr::select(-måned, -årstal)  # Fjerner midlertidige kolonner

# Håndtering af outliers i superstats_hjemmekampe
numeric_columns_hjemmekampe <- superstats_hjemmekampe %>%
  dplyr::select(where(is.numeric)) %>%
  names()

for (col in numeric_columns_hjemmekampe) {
  superstats_hjemmekampe[[col]][find_outliers(superstats_hjemmekampe, col)] <- NA  # Sæt outliers til NA
}

# Valgfrit: Vis datasæt
# View(superstats)               # Hele datasættet fra webscrapingen
# View(superstats_vff)           # Kun VFF's ude og hjemmekampe
# View(superstats_hjemmekampe)   # Kun VFF's hjemmekampe



# DMI
# ------------------------------------------------------------------------------

# Vi laver en vektor med kampdatoer for hjemmekampe i formatet "YYYY-MM-DD"
# Dette format bruges, fordi det kræves af DMI's API
hjemmekampe_datoer <- format(superstats_hjemmekampe$dato, "%Y-%m-%d")

# Basisinformation til DMI API
# base_url: Basis-URL til API'et, som vi bygger videre på
# info_url: Endpoint til at hente observationer
# station_id: ID for den vejrstation, der skal bruges (i dette tilfælde station 06060)
# api_key: Nøgle, der giver adgang til DMI's API
base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
station_id <- "stationId=06060"
api_key <- "&api-key=8c762424-f683-4ad0-acea-197dc19ff258"

# VIGTIGT: Hvis du oplever problemer med API-kaldene (fx fejlstatuskoder eller langsom respons),
# kan du tjekke API'ens status på denne side:
# https://statuspage.freshping.io/25721-DMIOpenDatas

# Funktion til at konstruere den fulde forespørgsel URL
# Input: Dato i formatet "YYYY-MM-DD"
# Output: En fuldstændig URL, der kan bruges til API-kald
lav_dmi_url <- function(dato) {
  paste0(
    base_url, info_url, station_id,          # Basisinformation om API'et
    "&datetime=", dato, "T16:00:00Z/", dato, "T16:00:00Z",  # Start- og sluttidspunkt for observationer
    "&limit=100000", api_key                 # Begrænsning af antallet af rækker og API-nøglen
  )
}

# Funktion til at hente data for én dato via API
# Input: Dato i formatet "YYYY-MM-DD"
# Output: Data fra API'et som liste eller NULL, hvis der er fejl eller ingen data
hent_dmi_data <- function(dato) {
  # Generér den fulde URL for det pågældende dato-kald
  fuld_url <- lav_dmi_url(dato)
  
  # Udfør API-kald med timeout (10 sekunder)
  api_call <- httr::GET(fuld_url, httr::timeout(10))
  
  # Håndtering af API-svar baseret på statuskode
  if (httr::status_code(api_call) == 200) {
    # Hvis statuskoden er 200 (succes), konverter API-svaret til JSON-format
    api_json <- httr::content(api_call, as = "text", encoding = "UTF-8") |> 
      jsonlite::fromJSON(flatten = TRUE)
    
    # Tjek om svaret indeholder data (features)
    if (!is.null(api_json$features) && length(api_json$features) > 0) {
      message("Data hentet succesfuldt for dato: ", dato)  # Succesbesked
      return(api_json$features)  # Returnér data som liste
    } else {
      message("Ingen data for dato: ", dato)  # Besked, hvis der ikke er data
      return(NULL)
    }
  } else {
    # Hvis statuskoden ikke er 200, logges fejlen
    message("Fejl i API-kald for dato: ", dato, " - Statuskode: ", httr::status_code(api_call))
    return(NULL)
  }
}

# Hent data for alle datoer i hjemmekampe-datoer og gem resultatet som en liste
# Her bruges purrr::map til at iterere over alle datoer
dmi_data <- purrr::map(hjemmekampe_datoer, hent_dmi_data)
names(dmi_data) <- hjemmekampe_datoer  # Tilføj datoer som navne til listen for nemmere identifikation

# Kombinerer data fra listen dmi_data til én samlet tibble
# Input: Listen med data for hver dato
# Output: En tibble med alle observationer samlet
dmi_variabler <- purrr::map_dfr(names(dmi_data), function(dato) {
  if (!is.null(dmi_data[[dato]])) {  # Tjek om der er data for den pågældende dato
    dmi_data[[dato]] |> 
      as_tibble() |> 
      dplyr::select(
        Observationstidspunkt = properties.observed,  # Observationstidspunkt fra API'et
        Observationer = properties.parameterId,      # Observationstype (fx temperatur, vind)
        Værdi = properties.value                     # Værdi for observationen
      ) |> 
      mutate(
        Observationstidspunkt = lubridate::ymd_hms(Observationstidspunkt),  # Konvertering til datetime
        dato = as.Date(dato)  # Tilføj dato som separat kolonne
      )
  }
})

# Omstrukturerer data fra lange rækker (tidy format) til bredt format
# Observationstyper (fx temperatur, vind) bliver til kolonnenavne
# Værdierne fyldes i rækkerne for hver dato
dmi_variabler <- dmi_variabler |> 
  tidyr::pivot_wider(
    names_from = Observationer,  # Kolonnenavne baseret på observationstyper
    values_from = Værdi          # Værdier for observationerne
  ) |> 
  dplyr::select(-Observationstidspunkt)  # Fjerner observationstidspunktet, da vi har en dato kolonne og vi ved at data er hentet kl. 16:00 alle datoer

# Håndtering af outliers i dmi_variabler
numeric_columns_dmi <- dmi_variabler |> 
  dplyr::select(where(is.numeric)) |> 
  names()

for (col in numeric_columns_dmi) {
  dmi_variabler[[col]][find_outliers(dmi_variabler, col)] <- NA  # Sæt outliers til NA
}



# ------------------------------------------------------------------------------
# ------------------------------------JOINS--------------------------------------
# ------------------------------------------------------------------------------

# Sikrer at kolonnenavne er konsistente på tværs af datasæt
# Bemærk: Vi antager, at kolonnenavne allerede er standardiseret til "dato" i tidligere sektioner.
# Dette fjerner behovet for yderligere renaming her.

# Kontrollér formaterne af "dato"-kolonnerne i alle datasæt (valgfrit for debugging)
# str(excel_guld$dato)                   # Tjek datatype og format for dato i excel_guld
# str(superstats_13_23_hjemme$dato)      # Tjek datatype og format for dato i superstats
# str(dmi_variabler$dato)                # Tjek datatype og format for dato i dmi_variabler

# Joiner alle datasæt baseret på "dato"-kolonnen
# Datasættene kombineres i én samlet tibble, der indeholder alle relevante informationer:
# - VIP-data fra excel_guld
# - Kampdata fra superstats_13_23_hjemme
# - Vejrdata fra dmi_variabler
joins_samlet <- excel_guld |> 
  inner_join(superstats_hjemmekampe, by = "dato") |>   # Første join mellem VIP-data og kampdata
  inner_join(dmi_variabler, by = "dato")                # Andet join tilføjer vejrobservationer

# På dette trin har vi ét samlet datasæt kaldet "joins_samlet"
# Dette datasæt indeholder:
# - VIP-relateret data (fx antal billetter solgt)
# - Information om kampe (fx antal tilskuere, resultater)
# - Vejrdata for kampdage (fx temperatur, vindhastighed)





# ------------------------------------------------------------------------------
# ---------------------------------EGNE VARIABLER-------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tilføj ugedag og måned
# ------------------------------------------------------------------------------

# Tilføjer nye kolonner til datasættet 
# og placerer dem på specifikke positioner for bedre overblik.
joins_samlet <- joins_samlet |>
  mutate(
    ugedag = as.factor(weekdays(dato)),  # Tilføjer kolonnen 'ugedag'
    måned = factor(month(dato, label = TRUE, abbr = FALSE))  # Tilføjer kolonnen 'måned'
  ) |>
  relocate(ugedag, .after = dato) |>  # Flytter 'ugedag' til at være efter den første kolonne
  relocate(måned, .after = dato)  # Flytter 'måned' til at være efter 'ugedag'

# ------------------------------------------------------------------------------
# Tilføj point baseret på kampresultater og beregn sidste 3 kampe
# ------------------------------------------------------------------------------
joins_samlet <- joins_samlet |> 
  mutate(
    point_sidste_3_kampe = as.numeric(
      rowSums(
        cbind(
          lag(purrr::map_dbl(resultat, ~ {
            scores <- as.numeric(str_extract_all(.x, "\\d+")[[1]])
            case_when(
              scores[1] > scores[2] ~ 3,      # Sejr
              scores[1] == scores[2] ~ 1,     # Uafgjort
              scores[1] < scores[2] ~ 0,      # Nederlag
              TRUE ~ NA_real_                 # NA for fejl eller manglende data
            )
          }), 1, default = 0),  # Point fra seneste kamp
          lag(purrr::map_dbl(resultat, ~ {
            scores <- as.numeric(str_extract_all(.x, "\\d+")[[1]])
            case_when(
              scores[1] > scores[2] ~ 3,
              scores[1] == scores[2] ~ 1,
              scores[1] < scores[2] ~ 0,
              TRUE ~ NA_real_
            )
          }), 2, default = 0),  # Point fra to kampe tilbage
          lag(purrr::map_dbl(resultat, ~ {
            scores <- as.numeric(str_extract_all(.x, "\\d+")[[1]])
            case_when(
              scores[1] > scores[2] ~ 3,
              scores[1] == scores[2] ~ 1,
              scores[1] < scores[2] ~ 0,
              TRUE ~ NA_real_
            )
          }), 3, default = 0)   # Point fra tre kampe tilbage
        ),
        na.rm = TRUE  # Ignorer NA-værdier
      )
    )
  ) |> 
  relocate(point_sidste_3_kampe, .after = resultat)  # Flyt kolonnen til ønsket placering

# ------------------------------------------------------------------------------
# Tilføj modstander og tilskuere fra sidste møde
# ------------------------------------------------------------------------------
joins_samlet <- joins_samlet |> 
  mutate(
    Modstander = kamp |> 
      str_remove(".*VFF -") |> 
      str_trim() |> 
      as.factor()  # Konverterer Modstander til en faktor
  ) |> 
  arrange(Modstander, dato) |> 
  group_by(Modstander) |> 
  mutate(
    Tilskuere_sidste_møde = lag(antal_tilskuere) |> as.numeric()  # Laver lag og konverterer til numerisk
  ) |> 
  ungroup() |> 
  relocate(Tilskuere_sidste_møde, .after = antal_tilskuere)

# ------------------------------------------------------------------------------
# Tilføj guldmenuer fra sidste møde
# ------------------------------------------------------------------------------
joins_samlet <- joins_samlet |>
  group_by(Modstander) |>
  mutate(guld_menuer_sidste_møde = lag(Guld_menu_stk, default = 0)) |>
  ungroup() |>
  relocate(guld_menuer_sidste_møde, .after = Guld_menu_stk)

# ------------------------------------------------------------------------------
# Klassificér modstandere i kategorier
# ------------------------------------------------------------------------------
joins_samlet <- joins_samlet |> 
  mutate(
    kategori = case_when(
      Modstander %in% c("VFF-AGF", "VFF-OB", "VFF-BIF", "VFF-FCM") ~ "A",
      Modstander %in% c("VFF-RFC", "VFF-SIF", "VFF-FCK") ~ "B",
      TRUE ~ "C"
    ) |> factor(levels = c("A", "B", "C"))  # Angiver specifik rækkefølge af faktorniveauer
  ) |> 
  relocate(kategori, .before = antal_tilskuere)

# # ------------------------------------------------------------------------------
# # Interaktion mellem antal tilskuere og vejr
# # ------------------------------------------------------------------------------
# # Denne variabel undersøger, hvordan vejrforhold (temp_dry) påvirker antal tilskuere
# # (antal_tilskuere). Eksempel: Hvis dårligt vejr (lav temperatur) reducerer 
# # tilskuertallet mere for visse kampe, kan denne interaktion fange det.
# joins_samlet <- joins_samlet |>
#   mutate(tilskuere_vejr_interaktion = as.numeric(antal_tilskuere) * temp_dry)
# 
# 
# # ------------------------------------------------------------------------------
# # Interaktion mellem kampkategori og vejr
# # ------------------------------------------------------------------------------
# # Denne variabel repræsenterer samspillet mellem vejr (temp_dry) og kampkategorien 
# # (kategori). Eksempel: Kampkategorien kan have en forskellig effekt på kampens 
# # resultater afhængigt af vejret. For eksempel kan topkampe påvirkes mindre af 
# # dårligt vejr, da de har mere engagerede tilskuere.
# joins_samlet <- joins_samlet |>
#   mutate(vejr_kategori_interaktion = temp_dry * as.numeric(kategori))
# 
# 
# # ------------------------------------------------------------------------------
# # Interaktion mellem ugedag og vejr
# # ------------------------------------------------------------------------------
# # Denne variabel fanger, hvordan kombinationen af ugedag (ugedag) og kampkategori
# # (kategori) påvirker kampforholdene. Eksempel: Kampe i weekenden (højere ugedagsværdi)
# # kan have større tilskueropbakning, især hvis det er topkampe.
# joins_samlet <- joins_samlet |>
#   mutate(ugedag_kategori_interaktion = as.numeric(ugedag) * as.numeric(kategori))
# 
# 
# # ------------------------------------------------------------------------------
# # Interaktion mellem modstander og guldmenuer fra sidste møde
# # ------------------------------------------------------------------------------
# # Denne interaktion viser, hvordan tidligere salg af guldmenuer (guld_menuer_sidste_møde)
# # afhænger af modstanderens kategori (kategori). Eksempel: Hvis populære modstandere fører
# # til større salg, kan denne interaktion hjælpe med at modellere forventede salg.
# joins_samlet <- joins_samlet |>
#   mutate(modstander_guldmenu_interaktion = as.numeric(guld_menuer_sidste_møde) * as.numeric(kategori))
# 
# 
# # ------------------------------------------------------------------------------
# # Interaktion mellem point fra sidste kampe og antal tilskuere
# # ------------------------------------------------------------------------------
# # Denne variabel kombinerer holdets præstation i de seneste kampe (point_sidste_3_kampe)
# # med antallet af tilskuere (antal_tilskuere).
# # Eksempel: Hvis gode præstationer fører til højere tilskuertal, kan denne interaktion forklare det.
# joins_samlet <- joins_samlet |>
#   mutate(point_tilskuere_interaktion = point_sidste_3_kampe * as.numeric(antal_tilskuere))


#-------------------------------------------------------------------------------

# Undersøg datasættet "joins_samlet"
# Før vi går videre, får vi et overblik over datasættet:
# view(joins_samlet)  # Åbner datasættet i en visning for at inspicere indholdet
# str(joins_samlet)  # Viser datastrukturen, fx typer og dimensioner af kolonner
# colSums(is.na(joins_samlet))  # Tæller antallet af manglende værdier (NA) i hver kolonne

# Fjern variabler med mange manglende værdier eller lav relevans
# Dette reducerer risikoen for, at ubrugelige variabler påvirker modellen.
joins_samlet <- joins_samlet |> 
  dplyr::select(
    -dato,
    -kamp,
    -resultat,
    -antal_tilskuere,
    -sæson,
    -wind_dir_past1h,
    -precip_past1h,
    -wind_min_past1h,
    -temp_grass_max_past1h,
    -precip_past10min,
    -temp_mean_past1h,
    -precip_dur_past10min,
    -wind_min,
    -temp_grass_mean_past1h,
    -temp_max_past1h,
    -wind_max,
    -visib_mean_last10min,
    -temp_min_past1h,
    -humidity_past1h,
    -temp_grass_min_past1h,
    -wind_gust_always_past1h,
    -precip_dur_past1h,
    -wind_speed_past1h,
    -precip_past1min,
    -pressure_at_sea,
    -temp_grass
  )

# Imputation af manglende værdier (NA)
# Vi bruger medianen for numeriske værdier og "Unknown" for kategoriske værdier
# Medianen bruges fremfor gennemsnittet for at mindske påvirkning fra outliers.
joins_samlet <- joins_samlet |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
         across(where(is.character), ~ ifelse(is.na(.), "Unknown", .)))

# Undersøg korrelation mellem numeriske variabler
# Vi beregner korrelationen mellem alle numeriske kolonner i datasættet.
# Korrelationen hjælper med at identificere stærke lineære forhold mellem variablerne.
cor_matrix <- cor(
  joins_samlet %>% dplyr::select(where(is.numeric)),  # Udvælger kun numeriske kolonner
  use = "pairwise.complete.obs"  # Håndterer manglende værdier ved at bruge parvise observationer
)

# Visualisering af korrelationsmatrix
# Vi bruger `corrplot` til at vise en grafisk repræsentation af korrelationer:
# - Blå: Positiv korrelation (stærke sammenhænge)
# - Rød: Negativ korrelation (invers sammenhæng)
library(corrplot) # Skal vi gøre dette - tror ikke vi har haft den med Bjarne
corrplot(cor_matrix, method = "circle")  # Visualiseres som cirkler for nem fortolkning



# ------------------------------------------------------------------------------
# ----------------------------------MODELLER------------------------------------
# ------------------------------------------------------------------------------

# 0-Feature Model (Baseline)
# ------------------------------------------------------------------------------
# En baseline-model uden nogen prædiktorer.
# Denne model forudsiger altid middelværdien af målvariablen (Guld_menu_stk).
# Bruges som referencepunkt for mere avancerede modeller.

set.seed(123)
train <- sample(1:nrow(joins_samlet), nrow(joins_samlet) *2/3)  # 2/3 af data som træning
test <- (-train)  # Resten som testdata
y <- joins_samlet$Guld_menu_stk  # Definer målvariabel
y.test <- y[test]  # Definer målvariabel for testdata
glm.fit <- glm(Guld_menu_stk ~ 1, data = joins_samlet[train, ])  # Baseline-model
rmse_0_cv <- sqrt(cv.glm(joins_samlet[train, ], glm.fit, K = 5)$delta[1])  # RMSE fra krydsvalidering
rmse_0_test <- sqrt(mean((y.test - predict(glm.fit, joins_samlet[test, ]))^2))  # RMSE for testdata

rmse_0_test

# ------------------------------------------------------------------------------
# Ridge- og Lasso-regression
# ------------------------------------------------------------------------------
# Ridge og Lasso bruges til at reducere overfitting ved at tilføje en regulariseringsparameter.
# Ridge (alpha = 0) reducerer størrelsen af koefficienterne, mens Lasso (alpha = 1) kan sætte dem til 0.
set.seed(123)
x <- model.matrix(Guld_menu_stk ~ ., joins_samlet)[, -1]  # Skaber designmatrix
y <- joins_samlet$Guld_menu_stk  # Målvariabel (genbrugt fra før)
grid <- 10^seq(10, -2, length = 100)

# Ridge regression (alpha = 0)
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0, lambda = grid, nfolds = 5)
bestlam <- cv.out$lambda.min  # Optimalt lambda
rmse_ridge_cv <- sqrt(cv.out$cvm[cv.out$lambda == bestlam])  # RMSE fra krydsvalidering
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])  # Forudsigelser for testdata
rmse_ridge_test <- sqrt(mean((ridge.pred - y.test)^2))  # RMSE for testdata

rmse_ridge_test

# Udtræk koefficienter for Ridge-regression
ridge.coef <- coef(ridge.mod, s = bestlam)
print(ridge.coef)

# Lasso regression (alpha = 1)
set.seed(123)
x <- model.matrix(Guld_menu_stk ~ ., joins_samlet)[, -1]  # Skaber designmatrix
y <- joins_samlet$Guld_menu_stk  # Målvariabel (genbrugt fra før)
grid <- 10^seq(10, -2, length = 100)

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, lambda = grid, nfolds = 5)
bestlam <- cv.out$lambda.min  # Optimalt lambda
rmse_lasso_cv <- sqrt(cv.out$cvm[cv.out$lambda == bestlam])  # RMSE fra krydsvalidering
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])  # Forudsigelser for testdata
rmse_lasso_test <- sqrt(mean((lasso.pred - y.test)^2))  # RMSE for testdata

rmse_lasso_test

# Udtræk koefficienter og variabler med ikke-nul koefficienter
lasso.coef <- coef(lasso.mod, s = bestlam)
selected_vars <- lasso.coef[which(lasso.coef != 0), ]
print(selected_vars)

# ------------------------------------------------------------------------------
# Best Subset Selection
# ------------------------------------------------------------------------------

# # Best Subset Selection tester alle mulige kombinationer af prædiktorer og finder
# # den bedste model for hver modelstørrelse baseret på kriterier som MSE.
# 
# predict.regsubsets <- function(object, newdata, id, ...) {
#   form <- as.formula(object$call[[2]])
#   mat <- model.matrix(form, newdata)
#   coefi <- coef(object, id = id)
#   xvars <- names(coefi)
#   mat[, xvars] %*% coefi
# }
# 
# joins_samlet_train <- joins_samlet[train,]
# joins_samlet_test <- joins_samlet[test,]
# 
# k <- 10 # Antal folds
# n <- nrow(joins_samlet_train) # registrerer hvor mange observationer, vi har.
# set.seed(123) 
# folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
# dim(joins_samlet_train)[2]  # Der er 23 variabler og dermed 22 prædiktorer
# 
# cv.errors <- matrix(NA, k, dim(joins_samlet_train)[2]-1,
#                     dimnames = list(NULL, paste(1:(dim(joins_samlet_train)[2]-1))))
# cv.errors
# 
# for (j in 1:k) { # her gennemløbes alle folds
#   best.fit <- regsubsets(Guld_menu_stk ~ .,
#                          data = joins_samlet_train[folds != j, ],
#                          nvmax = (dim(joins_samlet_train)[2]-1))
#   for (i in 1:(dim(joins_samlet_train)[2]-1)) { # her gennemløbes alle kandidatmodeller
#     pred <- predict(best.fit, joins_samlet_train[folds == j, ], id = i)
#     # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
#     cv.errors[j, i] <-
#       mean((joins_samlet_train$Guld_menu_stk[folds == j] - pred)^2) # Her udregnes MSE for hver 
#     # fold for hver kandidatmodel 
#   }
# }
# 
# mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# # gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# # til hver kandidatmodel.
# mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
# par(mfrow = c(1, 1))
# plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
# which.min(mean.cv.errors)
# 
# 
# # Her fittes modellen til ALLE træningsdata
# reg.best <- regsubsets(Guld_menu_stk ~ ., data = joins_samlet_train,
#                        nvmax = (dim(joins_samlet_train)[2]-1))
# coef(reg.best, 1)
# 
# pred_best_subset <- predict(reg.best, joins_samlet_test, id = 1)
# 
# 
# mse_best_subset <- mean((joins_samlet[test,]$Guld_menu_stk - pred_best_subset)^2)
# rmse_bestsubset_test <- sqrt(mse_best_subset)
# rmse_bestsubset_cv <- sqrt(min(mean.cv.errors))


# RMSE PÅ ALLE MODELLERNE 
rmse_0_test
rmse_ridge_test
rmse_lasso_test


# ------------------------------------------------------------------------------
#---------------------------------Evaluering------------------------------------
# ------------------------------------------------------------------------------

# Denne sektion bruges til at implementere og evaluere forudsigelser baseret på de valgte modeller.
