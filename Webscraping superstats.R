# Pakker
pacman::p_load(rvest, tidyverse)

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
