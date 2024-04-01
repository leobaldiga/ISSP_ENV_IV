#Attaching Packages-----
library(haven)
library(tidyr)
library(dplyr)
library(dm)
library(writexl)
library(kableExtra)
library(webshot2)
library(ggplot2)
library(likert)

#Reading in Data ----

Data = read_dta('ZA7650_v2-0-0.dta')

#take a look, by country
countries <- as.data.frame(unique(Data$c_alphan))
print(countries)

# Get data types of variables
data_types <- lapply(Data, class)
print(data_types)

#hmmm... all character class? i think not...
numeric_vars <- c()

# Loop over all variables in the dataframe
for (var_name in names(Data)) {
  # Try to convert the variable to numeric
  temp <- as.numeric(as.character(Data[[var_name]]))
  
  # Check if the conversion was successful (no NAs)
  if (!any(is.na(temp))) {
    # If successful, add the variable name to the numeric_vars vector
    numeric_vars <- c(numeric_vars, var_name)
  }
}
# Print the names of numeric variables
print(numeric_vars)

#Summary Stats for all variables
summary_stats <- as.data.frame(summary(Data))
print(summary_stats)

# Frequency tables for all variables
freq_tables <- lapply(Data, table)

print(freq_tables)

#A function to remove out of range values from likert variables
#for ISSP2020, this will remove all -9 and -8s from the likert vars, which 
#correspond to “No answer” and “Can’t choose”
remove_out_of_range <- function(data, likert_variables) {
  data %>% 
    mutate(across(all_of(likert_variables), ~ifelse(as.numeric(as.character(.)) > 5 | as.numeric(as.character(.)) < 1, NA, as.numeric(as.character(.)))))
}

#some postcoding
Postcoded <- Data %>%
  mutate(v1 = factor(v1,
                     levels = c(-9, -8, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                     labels = c('No answer',
                                'Can\'t choose',
                                'Health care',
                                'Education',
                                'Crime',
                                'The environment',
                                'Immigration',
                                'The economy',
                                'Terrorism',
                                'Poverty',
                                'None of these')))  %>%
  mutate(v54 = factor(v54,
                                 levels = c(-9, 1, 2),
                                 labels = c('No answer; AT, IN: Can\'t choose',
                                            'Yes',
                                            'No'))) %>%
  mutate(country = case_when(
    c_alphan == "AT" ~ "Austria",
    c_alphan == "AU" ~ "Australia",
    c_alphan == "CH" ~ "Switzerland",
    c_alphan == "CN" ~ "China",
    c_alphan == "DE" ~ "Germany",
    c_alphan == "DK" ~ "Denmark",
    c_alphan == "ES" ~ "Spain",
    c_alphan == "FI" ~ "Finland",
    c_alphan == "FR" ~ "France",
    c_alphan == "HU" ~ "Hungary",
    c_alphan == "HR" ~ "Croatia",
    c_alphan == "IN" ~ "India",
    c_alphan == "IS" ~ "Iceland",
    c_alphan == "IT" ~ "Italy",
    c_alphan == "JP" ~ "Japan",
    c_alphan == "KR" ~ "South Korea",
    c_alphan == "LT" ~ "Lithuania",
    c_alphan == "NO" ~ "Norway",
    c_alphan == "NZ" ~ "New Zealand",
    c_alphan == "PH" ~ "Philippines",
    c_alphan == "RU" ~ "Russia",
    c_alphan == "SE" ~ "Sweden",
    c_alphan == "SI" ~ "Slovenia",
    c_alphan == "SK" ~ "Slovakia",
    c_alphan == "TH" ~ "Thailand",
    c_alphan == "TW" ~ "Taiwan",
    c_alphan == "US" ~ "United States",
    c_alphan == "ZA" ~ "South Africa",
    TRUE ~ as.character(c_alphan) # Default case to handle any other codes
  ))%>%
  rename(
    prient20 = v3,
    govinq20 = v4,
    pm1 = v8,
    pm2 = v9,
    scisol20 = v20,
    worent20 = v21,
    hrment20 = v22,
    prghrm20 = v23,
    ecgrgd20 = v24,
    ecgrbd20 = v25,
    hiprce20 = v26,
    hitax20 = v27,
    stlvng20 = v28,
    toodif20 = v30,
    dorgt20 = v31,
    crpent20 = v37,
    nucent20 = v43,
    indpol20 = v38,
    pstent20 = v39,
    rlpent20 = v40,
    tmpent20 = v41,
    govres20 = v45,
    govrol20 = v44,
    recyc20 = v52,
    grpmem20 = v54,
    petit20 = v55,
    money20 = v56,
    prtst20 = v57,
    place20 = URBRURAL,
    morimp20 = v32,
    others20 = v33,
    threxg20 = v34,
    gmentl20 = v42,
    mostim20 = v1,
    nextim20 = v2,
    trsppl20 = v10
  )
Postcoded <- remove_out_of_range(
  data = Postcoded,
  likert_variables = c("prient20",
                        "govinq20",
                        "scisol20",
                        "worent20", 
                        "hrment20", 
                        "prghrm20", 
                        "ecgrgd20", 
                        "ecgrbd20", 
                        "hiprce20", 
                        "hitax20", 
                        "stlvng20", 
                        "toodif20", 
                        "dorgt20", 
                        "crpent20", 
                        "nucent20", 
                        "indpol20", 
                        "pstent20", 
                        "rlpent20", 
                        "tmpent20", 
                        "govres20", 
                        "govrol20", 
                        "recyc20", 
                        "grpmem20", 
                        "petit20", 
                        "money20", 
                        "prtst20", 
                        "morimp20", 
                        "others20", 
                        "threxg20", 
                        "gmentl20", 
                        "trsppl20"))


environmental_concern_desc_th <- Postcoded %>%
  select(worent20, prghrm20, morimp20, others20, threxg20, country) %>%
  filter(country == "Thailand") %>%
  select(-country) %>%
  describe(na.rm = TRUE) 

environmental_concern_desc_th_kable <- environmental_concern_desc_th %>%
  kable(caption = "Environmental Concerns in Thailand") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(environmental_concern_desc_th_kable)

#table of missing values for environmental concern variables
environmental_concern_desc_th_na <- Postcoded %>%
  select(worent20, prghrm20, morimp20, others20, threxg20, country) %>%
  filter(country == "Thailand") %>%
  summarise_all(~sum(is.na(.))) %>%
  select(-country) 

environmental_concern_desc_th_na_kable <- environmental_concern_desc_th_na %>%
  kable(caption = "Missing Values for Environmental Concerns in Thailand") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(environmental_concern_desc_th_na_kable)
  

#Some Basic Analyses ---- 

# Two Nominal Variables----
#one binary: v54 - Q20 Member of a group to preserve environment?
env_member<-as.data.frame(table(Postcoded$v54))
print(env_member)

#one not: v1 - Q1a Most important issue for [R's COUNTRY] today?
biggest_issue<-as.data.frame(table(Postcoded$v1))
print(biggest_issue)

#One ordinal variable - v11 - Q5a Trust in institutions: University research centres
filtered <- Postcoded %>%
  filter(v11 > 0)
summary(filtered$v11)

#One Interval - v49 - Q17b Typical week: number of hours spend in car/ another motor vehicle?
filtered <- Postcoded %>%
  filter(v49 >= 0)
summary(filtered$v49)

#One Ratio - AGE - Age of respondent
filtered <- Postcoded %>%
  filter(AGE > 0)
summary(filtered$AGE)

#Some more interesting analyses
most_pressing_issue_by_country <- Postcoded %>%
  group_by(c_alphan) %>%
  summarise(most_pressing_issue = names(which.max(table(v1))))

mean_age_by_country <- Postcoded %>%
  group_by(c_alphan) %>%
  summarise(mean_age = round(mean(AGE, na.rm = TRUE)))

#Getting Sample Sizes----

samples <- Postcoded %>%
  group_by(country) %>%
  summarise(n_observations = n()) %>%
  ungroup()

#getting country codes

country_codes <- Postcoded %>%
  select(country, c_sample)%>%
  distinct()

#Replicating a table from Pawel ----

# Calculate counts for each country and choice
counts <- Postcoded %>%
  group_by(country, v1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(count = round(count), # Round counts to the nearest whole number
         stat = 'n')

# Calculate percentages for each country and choice
percentages <- Postcoded %>%
  group_by(country, v1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(country) %>% # Regroup by country to calculate percentages within each country
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup() %>%
  select(country, v1, percentage) %>%
  rename(count = percentage) %>%
  mutate(count = round(count, 1), # Round percentages to one decimal place
         stat = '%')

# Combine counts and percentages into one dataframe
combined <- bind_rows(counts, percentages) %>%
  arrange(country, v1, stat)

# Spread the choices of v1 into separate columns
pivot_table <- combined %>%
  pivot_wider(
    id_cols = c(country, stat),
    names_from = v1,
    values_from = count,
    values_fill = list(count = 0) # Fill missing values with 0
  ) 

# Add a total row at the bottom
pivot_table <- pivot_table %>%
  bind_rows(pivot_table %>%
              filter(stat == '%') %>%
              summarise(across(-c(country, stat), ~mean(., na.rm = TRUE)),
                        country = 'Total', stat = '%')) %>%
  bind_rows(pivot_table %>%
              filter(stat == 'n') %>%
              summarise(across(-c(country, stat), sum, na.rm = TRUE),
                        country = 'Total', stat = 'n')) %>%
  mutate(across(where(is.numeric), ~ifelse(grepl("%", stat), scales::percent(.x / 100, accuracy = 0.1), .x))) 

#Looking Pretty good! Let's get it formatted for publishing
bolder_lines_css <- "
<style>
table {
  border-left: 2px solid;
}
thead tr th, tbody tr td {
  border-left: 2px solid;
  border-right: 2px solid;
}
</style>
"

# Format the table for display
kable_formatted <- pivot_table %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" " = 1, " " = 1, "Table 2: Opinions on most important issue today by country" = ncol(pivot_table) - 2))

print(kable_formatted)

#Nice! Let's Save it as a PDF.
save_kable(kable_formatted, "table.pdf")


#Assignment 2----

#defining a function to create a summary table for likert vars and save as a PDF----
summary_kable <- function(data, likert, grouping, filter_condition) {
  # Filter data
  filtered_data <- data %>%
    filter({{ filter_condition }})
  
  # Calculate summary statistics
  summary_table <- filtered_data %>%
    group_by({{ grouping }}) %>%
    summarise(
      mean = round(mean({{ likert }}, na.rm = TRUE), digits = 2),
      median = median({{ likert }}, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Format the table using kableExtra
  likert_name <- deparse(substitute(likert))
  kable_table <- summary_table %>%
    kable(
      caption = paste0(likert_name, " - Summary"),
      col.names = c("Group", "Mean", "Median", "N")
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
  
  # Save the table as a PDF with the name of the likert variable
  pdf_file_name <- paste0(likert_name, ".pdf")
  save_kable(kable_table, file = pdf_file_name)
  
  return(kable_table)
}

#All Countries----

thregx20_kable<- summary_kable(
  data = Postcoded,
  likert = threxg20,
  grouping = country,
  filter = threxg20 <= 5
)

prghrm20_kable <- summary_kable(
  data = Postcoded,
  likert = prghrm20,
  grouping = country,
  filter = prghrm20 <= 5
)

worent20_kable <- summary_kable(
  data = Postcoded,
  likert = worent20,
  grouping = country,
  filter = worent20 <= 5
)

morimp20_kable <- summary_kable(
  data = Postcoded,
  likert = morimp20,
  grouping = country,
  filter = morimp20 <= 5
)

others20_kable <- summary_kable(
  data = Postcoded,
  likert = others20,
  grouping = country,
  filter = others20 <= 5
)








#Thailand----
Thai_environmental_concerns <- summary_kable(
  data = Postcoded,
  likert = threxg20,
  grouping = country,
  filter = threxg20 <= 5 & country == "Thailand"
) %>%
  summary_kable(
    data = Postcoded,
    likert = prghrm20,
    grouping = country,
    filter = prghrm20 <= 5 & country == "Thailand"
  ) 
  

  








