# LIBRARY
#### dataload
library(readxl)
library(dplyr)
library(openxlsx)
#### datawrangling
library(stringr)
library(lubridate)
#### visualization
library(ggplot2)
#### EDA
library(tidyr)
library(DataExplorer)
library(broom) # for OR
library(car) # for vif
#### Other
library(htmltools)
# Clustering
library(stats)
library(factoextra)
library(caret)
#### Modeling
library(glmnet)
library(randomForest)
#### Analysis
library(pROC) 


load("new_ds2023.RData")
# Column Selection
ds2023 <- new_ds2023 %>%
  select(
    `Aldur vid innskr. - heil ar (komur)`, `Kodi kyns`, Rikisfang, Postnumer, 
    Komudagur, `Heiti lotu`, `Komudagur`, `Lengd komu klst vidd`, `15 fyrstu sjukdomsgreiningar`, 
    Danardagur, `Yfirflokkur (nomesko)`, researchID, Fylgd, SLYSSTADUR, `Laegsta GCS`, Departments_visited
  )

# Month-Year Variable
year_month_str <- format(ds2023$Komudagur, "%Y-%m")
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2023-12-01")
date_seq <- seq(from = start_date, to = end_date, by = "month")
all_combinations <- format(date_seq, "%Y-%m")
ds2023$YearMonthOrdinal <- factor(year_month_str, levels = all_combinations, ordered = TRUE)
str(ds2023$YearMonthOrdinal)

# Quarter-Year Variable
ds2023$QuarterYear <- paste(year(ds2023$Komudagur), quarter(ds2023$Komudagur), sep="-Q")

# Response variable
ds2023 <- ds2023 %>%
  mutate(
    within_90_days = if_else(!is.na(Danardagur) & !is.na(Komudagur) &
                               difftime(Danardagur, Komudagur, units = "days") <= 90, 1, 0)
  )
# Postal Code
postcode_to_region <- list(
  "Hofudborgarsvaedid" = as.character(c(
    170, 172,200, 201, 202, 203, 206, 210, 212, 225, 220, 221, 222, 270, 271, 276)
  ),
  "Reykjavik" = as.character(
    c(
      101, 102, 103, 104, 105, 107, 108, 109, 110, 111, 112, 113, 116, 121, 123, 124, 125, 127, 128, 129, 130, 132, 150, 155, 161, 162
    )
  ),
  # "Seltjarnarnes" = as.character(c(170, 172)),
  # "Kopavogur" = as.character(c(200, 201, 202, 203, 206)),
  # "Gardabaer" = as.character(c(210, 212, 225)),
  # "Hafnarfjordur" = as.character(c(220, 221, 222)),
  # "Mosfellsbaer" = as.character(c(270, 271, 276))
  "Vesturland" = as.character(c(
    300, 301, 302, 310, 311, 320, 340, 341, 342, 345, 350, 351, 355, 356, 360, 370, 371, 380, 381
  )),
  "Sudurnes" = as.character(c(
    190, 191, 230, 232, 233, 235, 240, 241, 245, 246, 250, 251, 260, 262
  )),
  "Vestfirdir" = as.character(c(
    400, 401, 410, 415, 416, 420, 421, 425, 426, 430, 431, 450, 451, 460, 461, 465, 4466, 470, 471
  )),
  "Sudurland" = as.character(
    c(800, 801, 802, 803, 804, 805, 806, 810, 815, 816, 820, 825, 840, 845, 846, 850, 851, 860, 861, 870, 871, 880, 881
      
    )
  ),
  "Austurland" = as.character(c(
    700, 701, 710, 711, 715, 720, 721, 730, 731, 735, 736, 740, 741, 750, 751, 755, 756, 760, 761, 765, 766, 780, 781, 785
  )),
  "Nordurland eystra" = as.character(
    c(600, 601, 602, 603, 604, 605, 606, 607, 610, 611, 616, 620, 621, 625, 626, 630, 640, 641, 645, 650, 660, 670, 671, 675, 676, 680, 681, 685, 686, 690, 691
    )
  ),
  "Sudurland (vestmannaeyjar)" = as.character(c(900, 902)),
  #"Nordurland" = as.character(c(545, 540, 550)),
  "Nordurland vestra" = as.character(c(500, 510, 511, 512, 520, 524, 530, 531, 541, 546, 551, 560, 561, 565, 566, 570, 580, 581, 545, 540, 550)),
  "Erlendir" = c("999", "0"),
  "Missing" = c(NA, "N/A", "N/a", "NA")
)








#### ICD codes by head region 
Mjukpartauverkar_items_of_interest <- c(
  "S00.0", "S00.1", "S00.2", "S00.3", "S00.4", "S00.5", "S00.7", "S00.8", "S00.9",
  "S01.0", "S01.1", "S01.2", "S01.3", "S01.4", "S01.5", "S01.7", "S01.8", "S01.9",
  "S08.0", "S08.1", "S08.8", "S08.9", "S09.0", "S09.1", "S09.2", "S09.7", "S09.8", "S09.9",
  "T00.0", "T01.0", "T20.0", "T20.1", "T20.2", "T20.3", "T20.4", "T20.5", "T20.6",
  "T33.0", "T33.1", "T33.2", "T33.3", "T33.4", "T33.5", "T33.6", "T33.7", "T33.8", "T33.9",
  "T28.0", "T28.1", "T28.4", "T28.5", "T28.6", "T28.7"
)

augnaverkar_items_of_interest <- c(
  "S05.0", "S05.1", "S05.2", "S05.3", "S05.4", "S05.5", "S05.6", "S05.7", "S05.8", "S05.9",
  "T15.0", "T15.1", "T15.8", "T15.9", "T26.0", "T26.1", "T26.2", "T26.3", "T26.4", "T26.5",
  "T26.6", "T26.7", "T26.8", "T26.9"
)

hofudbeinaaverkar_items_of_interest <- c(
  "S02.0", "S02.1", "S02.3", "S02.4", "S02.7", "S02.8", "S02.9", "S02.2", "S02.5", "S02.6",
  "S03.0", "S03.1", "S03.2", "S03.4", "S03.5", "T02.0", "T03.0"
)

innankupu_og_heilataugaaverkar_items_of_interest <- c(
  "S04.0", "S04.1", "S04.2", "S04.3", "S04.4", "S04.5", "S04.6", "S04.7", "S04.8", "S04.9",
  "S06.0", "S06.1", "S06.2", "S06.3", "S06.4", "S06.5", "S06.6", "S06.8", "S06.9", "S07.0",
  "S07.1", "S07.8", "S07.9", "T04.0"
)

fjolaverka_med_averka_a_hofdi_items_of_interest <- c(
  "S09.7", "T07"
)

eftirstodvar_averka_a_hofdi_items_of_interest <- c(
  "T90.0", "T90.1", "T90.2", "T90.3", "T90.4", "T90.5", "T90.6", "T90.7", "T90.8", "T90.9"
)

all_codes_of_interest <- c(
  "T90.0", "T90.1", "T90.2", "T90.3", "T90.4", "T90.5", "T90.6", "T90.7", "T90.8", "T90.9", "S09.7", "T07", "S04.0", "S04.1", "S04.2", "S04.3", "S04.4", "S04.5", "S04.6", "S04.7", "S04.8", "S04.9",
  "S06.0", "S06.1", "S06.2", "S06.3", "S06.4", "S06.5", "S06.6", "S06.8", "S06.9", "S07.0",
  "S07.1", "S07.8", "S07.9", "T04.0",  "S02.0", "S02.1", "S02.3", "S02.4", "S02.7", "S02.8", "S02.9", "S02.2", "S02.5", "S02.6",
  "S03.0", "S03.1", "S03.2", "S03.4", "S03.5", "T02.0", "T03.0", "S05.0", "S05.1", "S05.2", "S05.3", "S05.4", "S05.5", "S05.6", "S05.7", "S05.8", "S05.9",
  "T15.0", "T15.1", "T15.8", "T15.9", "T26.0", "T26.1", "T26.2", "T26.3", "T26.4", "T26.5",
  "T26.6", "T26.7", "T26.8", "T26.9", "S00.0", "S00.1", "S00.2", "S00.3", "S00.4", "S00.5", "S00.7", "S00.8", "S00.9",
  "S01.0", "S01.1", "S01.2", "S01.3", "S01.4", "S01.5", "S01.7", "S01.8", "S01.9",
  "S08.0", "S08.1", "S08.8", "S08.9", "S09.0", "S09.1", "S09.2", "S09.7", "S09.8", "S09.9",
  "T00.0", "T01.0", "T20.0", "T20.1", "T20.2", "T20.3", "T20.4", "T20.5", "T20.6",
  "T33.0", "T33.1", "T33.2", "T33.3", "T33.4", "T33.5", "T33.6", "T33.7", "T33.8", "T33.9",
  "T28.0", "T28.1", "T28.4", "T28.5", "T28.6", "T28.7"
)

non_related_lotu <- c("Endurkoma G3", "Endurkoma averkar", "Endurkoma G2",
                      "Endurkoma", "Endurkoma veikindi", "COVID", 
                      "Endurkoma a bradamottoku")

### Inclusion Criteria Applied 
data_all <- ds2023 %>%
  # filter rows where the values in 15 fyrstu sjukdomsgreiningar contain any ICD codes in  innankupu_og_heilataugaaverkar_items_of_interest
  dplyr::filter(str_detect(
    `15 fyrstu sjukdomsgreiningar`,
    paste(innankupu_og_heilataugaaverkar_items_of_interest, collapse = "|")
  )) %>%
  dplyr::filter(
    Rikisfang == 'IS' &
      !(`Heiti lotu` %in% non_related_lotu) &
      as.character(Postnumer) %in% unlist(postcode_to_region[c("Hofudborgarsvaedid", "Reykjavik")])
  ) %>%
  group_by(researchID) %>%
  mutate(visit_num = row_number()) %>%
  ungroup()

dim(data_all)

postal_code_counts <- table(data_all$Postnumer)
less_than_50 <- names(postal_code_counts[postal_code_counts < 50])
data_all$Postnumer <- ifelse(data_all$Postnumer %in% less_than_50, ">50", data_all$Postnumer)

# Age
categorize_age <- function(age) {
  if (is.na(age)) {
    return("NA")
  } else if (age <= 17) {
    return("0-17")
  } else if (age <= 65) {
    return("18-65")
  } else {
    return("65+")
  }
}

data_all$age.a <- sapply(data_all$`Aldur vid innskr. - heil ar (komur)`, categorize_age)

# Sex
table(data_all$`Kodi kyns`) 
data_all <- data_all %>%
  mutate(`Kodi kyns` = case_when(
    `Kodi kyns` %in% c("1", "3") ~ "male",
    `Kodi kyns` %in% c("4", "2") ~ "female",
    TRUE ~ NA_character_ # Assign NA to those that don't match the above conditions
  )) %>%
  filter(!is.na(`Kodi kyns`)) 

# Arrival 
recode_ARRIVAL <- function(x) {
  x <- as.character(x)
  return(case_match(x,
                    "NA" ~ NA,
                    "Sjukrabill" ~ "Ambulance Service",
                    "thyrla" ~ "Ambulance Service",
                    "Flug" ~ "Ambulance Service",
                    "Flug / Sjukrabill" ~ "Ambulance Service",
                    "i fylgd starfsm heilbrigdisstofnunar" ~ "Ambulance Service",
                    "i fylgd starfsm dvalar-/hj.heimilis" ~ "Ambulance Service",
                    "i fylgd adstandenda" ~ "Family",
                    "i fylgd foreldra" ~ "Family",
                    "i fylgd foreldra/forradamanns" ~ "Family",
                    "i fylgd maka" ~ "Family",
                    "i fylgd aettingja" ~ "Family",
                    "i fylgd barna" ~ "Family",
                    "i fylgd systkina" ~ "Family",
                    "Logregla" ~ "Police",
                    "a annan hatt" ~ "Unknown",
                    "i fylgd starfsm skola" ~ "Unknown",
                    "i fylgd starfsmanna annarra" ~ "Unknown",
                    "i fylgd vinnufelaga" ~ "Unknown",
                    "a eigin vegum" ~ "Alone", 
                    "Annad" ~ "Unknown", 
                    "Ekki vitad" ~ "Unknown",
                    .default = "Unknown"
  ))
  return(result)
}

data_all <- data_all %>%
  mutate(arrival = recode_ARRIVAL(Fylgd))

table(data_all$arrival)

# Cause
data_all <- data_all %>%
  mutate(`Yfirflokkur (nomesko)` = case_when(
    `Yfirflokkur (nomesko)` == "Flutninga/samgonguohapp" ~ "Transportation/traffic",
    `Yfirflokkur (nomesko)` == "ithrottaohapp" ~ "Sports",
    `Yfirflokkur (nomesko)` == "Ofbeldisverk" ~ "Violence",
    `Yfirflokkur (nomesko)` %in% c("Sjoslys", "Vinnuslys") ~ "Work",  
    TRUE ~ "Unknown"  # Leave the rest of the categories unchanged
  ))

table(data_all$`Yfirflokkur (nomesko)`)

# Place
data_all$SLYSSTADUR <- tolower(data_all$SLYSSTADUR)

data_all <- data_all %>%
  mutate(
    SLYSSTADUR_new = case_when(
      SLYSSTADUR %in% c(
        "opinberar akbrautir innan baejarmarka",
        "opinberar akbrautir utan baejarmarka",
        "hradbraut",
        "stoður otilgreindar",
        "einkainnkeyrsla, bilastæði, bilskúr, bilskali, stigar og gongusvaedi",
        "umferðarsvaedi ekki nanar tilgreint",
        "gangstett, gangbraut",
        "hjolreidastigur", 
        "kappakstursbraut", "umferdarmidstod, vorugeymsla, voruafgreidsla, jarnbrautarstod",
        "umferdarsvaedi ekki nanar tilgreint",
        "vegur ekki tilgreindur", "umferdarsvaedi, annad tilgreint"
      ) ~ "traffic accidents",
      SLYSSTADUR %in% c(
        "íthrottahus, leikfimisalur",
        "ithrottahus, leikfimisalur",
        "ithrottasvaedi utanhus",
        "sundholl, sundlaug",
        "svaedi fyrir keppnis- og almenningsithrottir, tilgreint, ekki flokkad annars stadar",
        "ithrottasvaedi utanhuss",
        "svaedi fyrir alpagreinar",
        "svaedi fyrir keppnis- og almenningsithrottir, otilgreint",
        "svaedi fyrir keppnis- og almenningsithrottir, tilgreint, ekki flokkad annars staðar"
      ) ~ "competition/sports",
      SLYSSTADUR %in% c(
        "ibudarsvaedi innan dyra, annad",
        "ibudasvaedi, annad og otilgreint",
        "ibudarsvaedi utan dyra",
        "ibudasvaedi, annad og otilgreint",
        "leiksvaedi innan ibudasvaedis",
        "gistiheimili, gistihus, hotel, veghotel",
        "setustofa, svefnherbergi",
        "einkaatvinnusvaedi",
        "eldhus",
        "badherbergi, thvottahus",
        "gardur",
        "leiksvaedi innan ibudasvaedis",
        "ibudarsvaedi innan dyra, annad",
        "ibudarsvaedi utan dyra", 
        "vorugeymsla, birgdastod, thar med taldar bilageymslur og gatnakerfi"
      ) ~ "home",
      SLYSSTADUR %in% c(
        "sundholl, sundlaug",
        "framleidslusvaedi tilgreint ekki flokkad annars stadar",
        "byggingar og skrifstofur opnar almenningi",
        "diskotek, djassklubbur, danshus",
        "framleidslusvaedi ekki tilgreint",
        "framleidslusvaedi tilgreint ekki flokkad annars staðar",
        "mannvirki og vegir i byggingu",
        "verkstaedi, verkmiðjusalur, skipamidstod",
        "gangstett, gangbraut",
        "verslun, smasoluverslun, heildverslun, uppbodshus, solubasar",
        "skemmtisvaedi eda utivistarsvaedi, otilgreind",
        "skemmtisvaedi, utivistarsvaedi, tilgreint, ekki flokkad annars stadar",
        "skautaholl, skautasvaedi",
        "landbunadarsvaedi, gardyrkjusvaedi",
        "veitingastadur, matsolustadur",
        "verkstaedi, verksmidjusalur, skipamidastod",
        "verslanir, verslunarsvaedi og ymis einkathjonusta, tilgreind, ekki flokkud annars stadar",
        "leiksvaedi i skemmtigardi eda almenningsgardi", 
        "kvikmyndahus, leikhus, hljomleikasalur",
        "skemmtigardur",
        "almenningsgardur",
        "oraektad land",
        "haf, fjordur",
        "tjaldstaedi",
        "votn", 
        "hof og votn, annad tilgreint, ekki flokkad annars stadar",
        "bersvaedi, annad tilgreint, loftrymi",
        "bersvaedi, otilgreint",
        "skip, batar og onnur fljotandi for",
        "svid og ahorfendasvaedi inni/uti",
        "opinberar framkvaemdir",
        "nama, grjotnama, malargryfja",
        "verslanir, verslunarsvaedi og ymis einkathjonusta, otilgreint",
        "einkainnkeyrsla, bilastaedi, bilskur, bilskali, stigar og gongusvaedi", "is snjor a halendi, joklar"
      ) ~ "other public location",
      SLYSSTADUR %in% c(
        "leiksvaedi (vid skola og adrar stofnanir)",
        "stigi, innan dyra",
        "dagvistarstofnanir fyrir born og unglinga",
        "skolalod",
        "skrifstofur og motuneyti innan framleidslusvaedis",
        "stofnanir, skolar og opinber stjornsysla, otilgreind",
        "reidskoli",
        "dagvistarstofnanir fyrir born og unglinga",
        "skoli, framhaldsskoli, haskoli, onnur kennslustofnun",
        "sjukrahus, gongudeild, heilsugaeslustod",
        "stofnanir, skolar og opinber stjornsysla, tilgreind, ekki flokkud annars stadar",
        "hjukrunarheimili, dvalarheimili t.d. fyrir fatlada, onnur vistunarstofnun"
      ) ~ "educational and institutional areas",
      TRUE ~ "unknown"  # Assign other cases to this category
    )
  )

data_all <- data_all %>% 
  select(!SLYSSTADUR)

# GCS
categorize_GCS <- function(gcs_value) {
  if (is.na(gcs_value)) {
    return(NA)  # Return NA if the input is NA
  } else if (gcs_value >= 13 && gcs_value <= 15) {
    return("GCS 13-15")
  } else if (gcs_value >= 9 && gcs_value <= 12) {
    return("GCS 9-12")
  } else if (gcs_value >= 3 && gcs_value <= 8) {
    return("GCS 3-8")
  } else {
    return(NA)  # Return NA if the value is outside the expected range (e.g., 0-2)
  }
}

categorize_GCS_vectorized <- Vectorize(categorize_GCS)
data_all <- data_all %>%
  mutate(GCS_category = categorize_GCS_vectorized(`Laegsta GCS`))

table(data_all$GCS_category, useNA = "ifany")

data_all <- data_all %>%
  dplyr::select(
    researchID = researchID,
    monthyear = YearMonthOrdinal,
    quarteryear = QuarterYear,
    postalcode = Postnumer,
    response = within_90_days,
    age.a = age.a,
    sex.s = `Kodi kyns`,
    arrival.h = arrival,
    cause.r = `Yfirflokkur (nomesko)`,
    place.w = SLYSSTADUR_new,
    carepathway.c = Departments_visited,
    TBIcount.k = visit_num,
    GCS.g = GCS_category,
    staylength.t = `Lengd komu klst vidd`
  )

save(data_all, file = "data_all.RData")

missing_values <- data_all %>%
  summarise_all(~ sum(is.na(.)))

columns_with_NA <- missing_values %>%
  filter(if_any(everything(), ~ any(. > 0))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "num_NA")

columns_with_NA

data_all <- data_all %>%
  mutate(
    missing_GCS = ifelse(is.na(GCS.g), 1, 0),
    missing_cause = ifelse(cause.r == "Unknown", 1, 0),  
    missing_arrival = ifelse(arrival.h == "Unknown", 1, 0),  
    missing_carepathway = ifelse(is.na(carepathway.c), 1, 0)
  ) %>%
  select(!GCS.g)

# numeric vectors
str(data_all[c("missing_GCS", "missing_cause", "missing_arrival", "missing_carepathway")])

#### carepathways (tricky)
all_departments <- unlist(str_split(data_all$carepathway.c, ";"))
#unique_departments <- unique(all_departments)
dept <- read_excel("deparmtnets.xlsx")
# Clean the 'NAME' column to remove extra characters
dept$NAME <- gsub('^\\[\\d+\\]\\s\\"|\\"$', '', dept$NAME)
dept_mapping <- list()
categories <- c("ICU", "ER", "HOSPITAL ADMISSION", "Hospice", "Rehabilitation", "Death", "Other", "Missing")
for(cat in categories) {
  dept_names <- dept$NAME[!is.na(dept[[cat]]) & dept[[cat]] == 1]
  dept_mapping[dept_names] <- cat
}
dept_mapping_vector <- unlist(dept_mapping)
# Function: map departments to categories for each department in the carepathway column cells
replace_dept_with_category <- function(carepathway) {
  departments <- str_split(carepathway, ";\\s*")[[1]]  # Split on semicolon and optional spaces
  categories <- sapply(departments, function(dept) if(dept %in% names(dept_mapping_vector)) dept_mapping_vector[dept] else "Uncategorized")
  unique_categories <- unique(categories[!is.na(categories)])
  return(paste(unique_categories, collapse = "; "))
}
data_all$carepathway.c2 <- sapply(data_all$carepathway.c, replace_dept_with_category)

# extract the first department from the carepathway.c variable
data_all$carepathway.c <- sapply(str_split(data_all$carepathway.c2, ";\\s*"), function(x) x[1])
data_all$carepathway.c <- ifelse(data_all$carepathway.c == "Rehabilitation", "Uncategorized", data_all$carepathway.c)

data_all <- data_all %>% 
  select(!carepathway.c2)
dim(data_all)
save(data_all, file = "data_all.RData")

data_large <- data_all %>%
  dplyr::filter(TBIcount.k == 1)%>% 
  select(!TBIcount.k)
dim(data_large)
data_large$postalcode <- factor(data_large$postalcode, 
                                levels = unique(data_large$postalcode), 
                                labels = unique(data_large$postalcode))
data_large$response <- as.factor(data_large$response)
data_large$missing_GCS <- as.factor(data_large$missing_GCS)
data_large$age.a <- as.factor(data_large$age.a)
data_large$sex.s <- as.factor(data_large$sex.s)
data_large$arrival.h <- as.factor(data_large$arrival.h)
data_large$missing_cause <- as.factor(data_large$missing_cause)
data_large$place.w <- as.factor(data_large$place.w)
data_large$carepathway.c <- as.factor(data_large$carepathway.c)
data_large$cause.r <- as.factor(data_large$cause.r)
save(data_large, file = "data_large.RData")

data_small <- data_large %>%
  dplyr::filter(carepathway.c != "Uncategorized") # filter only the variables that have carepathways
data_small$carepathway.c <- droplevels(data_small$carepathway.c)
dim(data_small)
save(data_small, file = "data_small.RData")






