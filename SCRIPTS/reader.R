## Prelude
# Constants
bp_link = "https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review-2017/bp-statistical-review-of-world-energy-2017-underpinning-data.xlsx"

# Packages Used
lapply(c("magrittr", "rvest", "dplyr", "tidyr", "readr"), require, character.only = TRUE)

# Custom-defined Functions
`+` = function(x, y) {
  if (is.character(x) || is.character(y)) {
    return(paste(x , y, sep = ""))
  } else {
    .Primitive("+")(x, y)
  }
}

put.into = function(df, lst) {
  df %>% list() %>% append(x = lst)
}

# Set directory paths
script.path = normalizePath(sys.frame(1)$ofile)
script.dir = dirname(script.path)
data.dir = dirname(script.dir) + "/DATA/"
worldbank.dir = data.dir + "/worldbank/"

## WorldBank
# Read & clean WorldBank national data series
population <-
  read.csv(worldbank.dir + "population.csv", stringsAsFactors = F) %>%
  select(Country = Country.Name, Year = Time, Population = Value) %>%
  mutate(`Population (Millions)` = Population / 1e6,
         Country = recode(Country,
                          "Korea, Rep." = "South Korea",
                          "Iran, Islamic Rep." = "Iran",
                          "Venezuela, RB" = "Venezuela",
                          "Egypt, Arab Rep." = "Egypt",
                          "Hong Kong SAR, China" = "China Hong Kong SAR",
                          "Trinidad and Tobago" = "Trinidad & Tobago",
                          "Slovak Republic" = "Slovakia")) %>%
  select(Country, Year, Population, `Population (Millions)`)

gdp <-
  read.csv(worldbank.dir + "gdp.csv", stringsAsFactors = F) %>%
  select(Country = Country.Name, Year = Time, GDP = Value) %>%
  mutate(`GDP (Millions)` = GDP / 1e6,
         Country = recode(Country,
                          "Korea, Rep." = "South Korea",
                          "Iran, Islamic Rep." = "Iran",
                          "Venezuela, RB" = "Venezuela",
                          "Egypt, Arab Rep." = "Egypt",
                          "Hong Kong SAR, China" = "China Hong Kong SAR",
                          "Trinidad and Tobago" = "Trinidad & Tobago",
                          "Slovak Republic" = "Slovakia")) %>%
  select(Country, Year, GDP, `GDP (Millions)`)

gdp_per_capita <-
  read.csv(worldbank.dir + "gdpcapita.csv", stringsAsFactors = F) %>%
  select(Country = Country.Name, Year = Time, `GDP per Capita` = Value) %>%
  mutate(Country = recode(Country,
                          "Korea, Rep." = "South Korea",
                          "Iran, Islamic Rep." = "Iran",
                          "Venezuela, RB" = "Venezuela",
                          "Egypt, Arab Rep." = "Egypt",
                          "Hong Kong SAR, China" = "China Hong Kong SAR",
                          "Trinidad and Tobago" = "Trinidad & Tobago",
                          "Slovak Republic" = "Slovakia")) %>%
  select(Country, Year, `GDP per Capita`)

# Read and clean WorldBank global data series
global_population <-
  read.csv(worldbank.dir + "global-population.csv", stringsAsFactors = F) %>%
  select(Country = Country.Name, Date = Time, Population = Value) %>%
  mutate(Year = Date, `Population (Billions)` = Population / 1e9) %>%
  select(Year, Population, `Population (Billions)`)

global_gdp <-
  read.csv(worldbank.dir + "global-gdp.csv", stringsAsFactors = F) %>%
  select(Country = Country.Name, Date = Time, GDP = Value) %>%
  mutate(Year = Date, `GDP (Trillions)`= GDP / 1e12) %>%
  select(Year, GDP, `GDP (Trillions)`)

# Read and clean USSR population data series separately (as USSR is not found in WorldBank data)
ussr_population <- read_html("https://commons.wikimedia.org/wiki/File:Population_of_former_USSR.PNG") %>%
  html_nodes(xpath = '//*[@id="mw-imagepage-content"]/div/table[1]') %>% html_table() %>% `[[`(1) %>%
  rename(Year = X1, `Population (Millions)` = X2) %>%
  transmute(Country = "USSR",
            Year,
            `Population (Millions)` = `Population (Millions)` %>% gsub(pattern = ",", replacement = "") %>% as.numeric,
            Population = `Population (Millions)` * 1e6) %>%
  select(Country, Year, Population, `Population (Millions)`)

# Read and clean Taiwan population data series separately (as Taiwan is not found in WorldBank data)
taiwan_population <- read_csv(data.dir + "taiwan-population.csv") %>% transmute(Country = "Taiwan", Year, Population = `Population (Millions)` * 1e6, `Population (Millions)`)

# Compile data into national and global datasets
worldbank <- population %>% left_join(gdp) %>% left_join(gdp_per_capita) %>% bind_rows(ussr_population, taiwan_population)
global_worldbank <- global_population %>% left_join(global_gdp) %>%
  mutate(`GDP per Capita` = GDP / Population)

## BP
# Read nuclear electricity generation data for Ukraine, Russia, & Lithuinia (as they are not included in BP's dataset prior to 1985)
# Source: Power Reactor Information System (PRIS) from IAEA
lru_nuclear <- read_csv(data.dir + "lru-nuclear.csv")

# Read and clean BP Statistical Review of World Energy 2017 underpinning data
download.file(bp_link, "temp.xlsx")

readBPSheet <- function(file = "temp.xlsx", sheet, label, range = "A3:BA87", years = 1965:2016) {
  bp_sheet <- read_excel(path = file, sheet = sheet, range = range, na = "n/a")
  bp_sheet %>% .[rowSums(is.na(.)) != ncol(.),] %>%
    select(Country = 1, everything()) %>%
    filter(! grepl(Country, pattern = "Total"), ! grepl(Country, pattern = "Other")) %>%
    gather(key = "Year", value = !!label, as.character(years)) %>%
    mutate(Year = as.numeric(Year),
           Country = recode(Country,
                            "US" = "United States")) %>%
    return()
}
# Read each sheet into a list of dataframes, then join them. Change units if necessary.
bp_data_list = list()
readBPSheet(sheet = "Primary Energy Consumption", label = "Energy Consumption (Mtoe)") %>%
  mutate(`Energy Consumption (TWh)` = 11.63 * `Energy Consumption (Mtoe)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Electricity Generation ", label = "Electricity Generation (TWh)",
            range = "A3:AG86", years = 1985:2016) %>% # Note: BP only records total electricity generation after 1984
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Carbon Dioxide Emissions", label = "CO2 Emissions (Mt)") %>%
  mutate(`CO2 Emissions (kg)` = 1e9*`CO2 Emissions (Mt)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Oil Consumption - Tonnes", label = "Oil Energy Consumption (Mt)") %>%
  mutate(`Oil Energy Consumption (TWh)` = 11.63 * `Oil Energy Consumption (Mt)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Gas Consumption - Mtoe", label = "Gas Energy Consumption (Mtoe)") %>%
  mutate(`Gas Energy Consumption (TWh)` = 11.63 * `Gas Energy Consumption (Mtoe)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Coal Consumption -  Mtoe", label = "Coal Energy Consumption (Mtoe)") %>%
  mutate(`Coal Energy Consumption (TWh)` = 11.63 * `Coal Energy Consumption (Mtoe)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Nuclear Consumption - Mtoe", label = "Nuclear Energy Consumption (Mtoe)") %>%
  mutate(`Nuclear Energy Consumption (TWh)` = 11.63 * `Nuclear Energy Consumption (Mtoe)`) %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Nuclear Consumption - TWh", label = "Nuclear Electricity Generation (TWh)") %>%
  bind_rows(lru_nuclear) %>% # Note: BP only records nuclear electricity generation for Lithuania, Russia, and Ukraine after 1984
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Hydro Consumption - TWh", label = "Hydro Electricity Generation (TWh)") %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Wind Consumption - TWh ", label = "Wind Electricity Generation (TWh)") %>%
  put.into(bp_data_list) -> bp_data_list
readBPSheet(sheet = "Solar Consumption - TWh", label = "Solar Electricity Generation (TWh)") %>%
  put.into(bp_data_list) -> bp_data_list
unlink("temp.xlsx")

# Add WorldBank with Taiwan & USSR population data to list
worldbank %>% put.into(bp_data_list) -> bp_data_list

bp <- bp_data_list %>% reduce(left_join) # joins by Country, Year
bp %<>%
  mutate(`CO2 Emissions per Capita (kg)` = `CO2 Emissions (kg)` / Population) %>%
  mutate_at(vars(ends_with("(TWh)")), funs(pcmwh = . * 1e6 / Population)) %>%
  rename_at(vars(ends_with("_pcmwh")), funs(sub(pattern = "_pcmwh", replacement = "", x = .) %>%
                                            sub(pattern = "\\(TWh\\)", replacement = "per Capita \\(MWh\\)", x = .))) %>%
  mutate(Electrification = `Electricity Generation (TWh)` / `Energy Consumption (Mtoe)`,
         `Carbon Intensity of Economy (g/$)` = `CO2 Emissions (kg)` * 1000 / GDP,
         `Energy Intensity of Economy (kWh/$)` = `Energy Consumption (TWh)` * 1e9 / GDP,
         `Carbon Intensity of Energy (g/kWh)` = (`CO2 Emissions (kg)` * 1000) / (`Energy Consumption (TWh)` * 1e9),
         `Carbon Intensity of Electricity (g/kWh)` = (`CO2 Emissions (kg)` * 1000) / (`Electricity Generation (TWh)` * 1e9)) %>%
  select(sort(names(.))) %>% select(Country, Year, everything()) %>% arrange(Country, Year)

# Save BP national data as both CSV and RDS
bp %>% saveRDS(data.dir + "BP")
bp %>% write_csv(data.dir + "bp.csv")

## Global BP dataset made by summing national BP data
bp_global <- bp %>% select(Year, `CO2 Emissions (Mt)`, `Energy Consumption (TWh)`, `Electricity Generation (TWh)`) %>%
  group_by(Year) %>% summarise(`CO2 Emissions (Gt)` = sum(`CO2 Emissions (Mt)`, na.rm = T) / 1e3,
                               `Energy Consumption (PWh)` = sum(`Energy Consumption (TWh)`, na.rm = T) / 1e3,
                               `Electricity Generation (PWh)` = sum(`Electricity Generation (TWh)`, na.rm = T) / 1e3) %>%
  left_join(global_worldbank) %>% select(sort(names(.))) %>% select(Year, everything()) %>% arrange(Year)

## Save BP global data as both CSV and RDS
bp_global %>% saveRDS(data.dir + "BP_GLOBAL")
bp_global %>% write_csv(data.dir + "bp_global.csv")
