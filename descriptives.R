## Descriptive stats for Sembrando Vida (SV) program (Mexico) ##
## Creation date: 10-Oct-2026
## Goals:
  # Basic descriptives of current status
  # Overview of elegible localities in states that do not have the SV program

## AI DISCLAIMER ##
# AI was utilised for debugging select parts of this code. The affected lines are:
  # Lines 121-122
  # Line 156
  # Line 199
# All other code was written without assistance of AI.

# Necessary libraries
library(tidyverse)
library(jsonlite)
library(httr)
library(purrr)
library(stargazer)

## Part 1: Descriptive data for most recent beneficiaries (June 2026) ##

# Data of sembrando vida beneficiaries in June 2026. Retrieved from
# https://pub.bienestar.gob.mx/v2/pub/programasIntegrales/16/868
sv_jun26 <- read.csv("data/sembrandovida-beneficiaries-jun26.csv")

## STEP 1: descriptives

# 416299 beneficiaries country-wide
sv_jun26 |>
  summarize(n = n())

# 24 states with program
sv_jun26 |>
  summarize(n = n_distinct(CVE_ENTIDAD))

# 1112 municipalities in total in the program
sv_jun26 |>
  summarize(n = n_distinct(CVE_MUNICIPIO))

## STEP 2: obtain states without SV beneficiaries

# count of beneficiaries per state
sv_state_count <- sv_jun26 |> 
  group_by(CVE_ENTIDAD) |>
  summarize(n = n())

# States without SV beneficiaries are (state codes crosschecked with INEGI data):
# 01 - Aguascalientes
# 02 - Baja California
# 03 - Baja California Sur
# 05 - Coahuila de Zaragoza
# 09 - Ciudad de México
# 11 - Guanajuato
# 19 - Nuevo León
# 22 - Querétaro


## PART 2: overview of elegible localities in states without SV ## 

# STEP 1: load data on localities and on IRS index

# The MX institute for statistics and geography (INEGI) establishes the 
# "locality" as the most granular, standardised population unit in the country.
# Information about localities can be consulted in:
# https://www.inegi.org.mx/servicios/catalogounico.html

# The INEGI supports obtaining JSON files for each locality through a link composed
# of a base URL and the unique location identifier. Some ways to obtain this
# information are:

# Method 1
example1 <- fromJSON("https://gaia.inegi.org.mx/wscatgeo/v2/localidades/190010042")
example1 <- as.data.frame(example1)

# Method 2 (used henceforth)
download.file("https://gaia.inegi.org.mx/wscatgeo/v2/localidades/190010042", "data/didthiswork") # Saves as txt file
example2 <- fromJSON("data/didthiswork")
example2 <- as.data.frame(example2)

# To measure socioeconomic standing, we use the data from the CONEVAL (National
# Council for the Evaluation of Social Development Policy), an autonomous gov org.

# Load "IRS" data (social "lag" index; higher values = lower development). Source:
# https://www.coneval.org.mx/Medicion/IRS/Paginas/Indice_de_Rezago_Social_2020_anexos.aspx .
# This index affects elegibility to SV program.
# Data came in two different files, so we need to merge to one common df
# (Some small cleanup of files was done manually, therefore the .csv files are already given in /data)
irs_p1 <- read.csv("data/irs-localidades-2020-ags-mich.csv")
irs_p2 <- read.csv("data/irs-localidades-2020-mor-zac.csv")
irs_df <- rbind(irs_p1, irs_p2)

# Further filter to only include data from municipalities with high or very
# high IRS (SV requirement) and from states without SV beneficiaries
irs_values <- c("Medio", "Alto", "Muy alto")
states <- c("Aguascalientes", "Baja California", "Baja California Sur", "Coahuila de Zaragoza",
            "Ciudad de México", "Guanajuato", "Nuevo León", "Querétaro")

# Create df with only elegible localities
svelegible_df <- irs_df |>
  filter(Grado.de.rezago.social %in% irs_values) |>
  filter(Entidad.federativa %in% states)

# Number of localities per state
svelegible_df |>
  group_by(Entidad.federativa) |>
  summarize(n = n())

## STEP 2: scrap data from web to obtain urban/rural distinction per locality

# Retrieving rural/urban data from the web (INEGI)
baseurl <- "https://gaia.inegi.org.mx/wscatgeo/v2/localidades/"

# Obtaining locality tag necessary to construct links
urls <- toString(svelegible_df$Clave.localidad)
urls <- unlist(str_split(urls, ", "))

# Add a "0" to tags that must start with it (0 got lost when parsing data
# into the df) and then for all cases add the base URL
for (i in seq_along(urls)) {
  if(nchar(urls[i]) == 8){
    urls[i] <- str_c("0", urls[i], sep = "")
  }
  urls[i] <- str_c(baseurl, urls[i], sep = "") 
}

## NOTE: from this point, and until the end of this step, the code is closely
# adapted from Prof. Simon Munzert's course "Introduction to Data Science": 
# https://github.com/intro-to-data-science-26

# Create directory to store files
store_directory <- "localities/"

# create filenames (percent-encoded to avoid trouble with special characters in file names)
filenames <- str_c(store_directory, map_chr(basename(urls), URLencode), ".txt")

dir.create(store_directory, showWarnings = FALSE)

# helper function that only downloads files that don't exist yet
download_files <- function(source, dest) {
  if (!file.exists(dest)) {
    download.file(source, dest, quiet = TRUE)
    Sys.sleep(0.5) # be polite
  }
}

# download all files
walk2(urls, filenames, download_files, .progress = TRUE)

## STEP 3: merge data based on rural variable values
# Load all localities files into a df
paths <- list.files("./localities", pattern = "[.]txt", full.names = TRUE)

localities_df <- 
  paths |> 
    map(\(x) fromJSON(x) |> as.data.frame()) |> # Have to convert into df immediately so that each observation is a row
    list_rbind()

# Create column with location ID as numeric, to merge with svelegible_df
localities_df <- localities_df |>
  mutate(Clave.localidad = as.numeric(datos.cvegeo))

# Add columns for state, municipality from svelegible_df to the localities df
localities_df <- svelegible_df |>
  select(c("Clave.localidad", "Entidad.federativa", "Municipio")) |>
  left_join(localities_df, svelegible_trim, by = "Clave.localidad")

## STEP 4: obtain final descriptives
sv_descriptives <- localities_df |>
                  group_by(Entidad.federativa) |>
                  filter(datos.ambito == "RURAL") |>
                  summarise(elegible_localities = n(),
                            total_population = sum(as.numeric(datos.pob_total)),
                            mean_population = mean(as.numeric(datos.pob_total)),
                            sd_population = sd(as.numeric(datos.pob_total)),
                            total_households = sum(as.numeric(datos.total_viviendas_habitadas)),
                            mean_households = mean(as.numeric(datos.total_viviendas_habitadas)),
                            sd_households = sd(as.numeric(datos.total_viviendas_habitadas)))

sv_descriptives <- sv_descriptives |>
  rename(State = Entidad.federativa,  # For easier reading in output
         Localities = elegible_localities,
         "Total population" = total_population,
         "Population mean" = mean_population,
         "SD population" = sd_population,
         "Total households" = total_households,
         "Households mean" = mean_households,
         "SD households" = sd_households)

# Save descriptives as output
dir.create("output/", showWarnings = FALSE)

# Edit decimal numbers
sv_descriptives <- sv_descriptives |>
  mutate_if(is.numeric, ~round(., 2))

# Make descriptives table
as.data.frame(sv_descriptives) |>
stargazer(type = "html", out = "output/descriptives.html", 
          summary = FALSE, rownames = FALSE, style = "qje",
          title = "Figure 1- Descriptive statistics of eligible localities in states with no SV coverage")