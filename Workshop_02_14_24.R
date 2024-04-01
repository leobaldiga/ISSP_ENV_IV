#Attaching Packages-----
library(haven)
library(tidyr)
library(dplyr)
library(dm)
library(writexl)
library(kableExtra)
library(webshot2)
library(likert)

isspenvt2010 <- read.csv('2010/issp10_34_imp1.csv')

# Recodes
isspenvt2010_recode <- isspenvt2010 %>%
  mutate(unece = factor(isocntry,
                       labels = c("AR", "AU", "AT", "BE", "BG", "CA", "CL", "CN", "HR", "CZ", "DK", "FI", "FR", "DE", "HU", "IS", "IN", "IE", "IL", "IT", "JP", "KR", "LV", "LT", "MX", "NL", "NZ", "NO", "PH", "PL", "PT", "RU", "SK", "SI", "ZA", "ES", "SE", "CH", "TW", "TH", "TR", "US", "GB", "DE"),
                       levels = c(32, 36, 40, 56, 100, 124, 152, 156, 191, 203, 208, 246, 250, 276, 348, 352, 356, 372, 376, 380, 392, 410, 428, 440, 484, 528, 554, 578, 608, 616, 620, 643, 703, 705, 710, 724, 752, 756, 158, 764, 792, 840, 826, 277)))

#Environmental attitudes (Leo)

#ALL LIKERTS
#1 Agree strongly
#2 Agree
#3 Neither agree nor disagree
#4 Disagree
#5 Disagree strongly
#8 Can't choose
#9 No answer

summary(isspenvt2010_recode$threxg10)

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

#testing
threxg10_kable <- summary_kable(
  data = isspenvt2010_recode,
  likert = threxg10,
  grouping = unece,
  filter = threxg10 <= 5
)

print(threxg10_kable)

##(threxg10)	‘Threats to the environment are greatly exaggerated’.----
desc_threxg10 <- isspenvt2010_recode %>%
  filter(threxg10 <= 5) %>%
  group_by(unece) %>%
  summarise(mean = round(mean(threxg10, na.rm = TRUE), digits = 2),
            median = median(threxg10, na.rm = TRUE),
            n = n())

#format the table using kable package
thregx_kabel <- desc_threxg10 %>%
  kable(caption = "thregx10 - Threats to the environment are greatly exaggerated", 
        col.names = c("Country", "Mean", "Median", "N")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

save_kable(thregx_kabel, "table.pdf")


##(prghrm10)	‘People worry too much about human progress harming the environment’.  -----
desc_prghrm10 <- isspenvt2010_recode %>% 
  group_by(unece) %>%
  summarise(mean = round(mean(prghrm10, na.rm = T), digits = 2),
            median = median(prghrm10, na.rm = T),
            n = n())

#(worent10) ‘We worry too much about the future of the environment and not enough about prices and jobs today’.-----
desc_worent10 <- isspenvt2010_recode %>% 
  group_by(unece) %>%
  summarise(mean = round(mean(worent10, na.rm = T), digits = 2),
            n = n())

#(morimp10)	‘There are more important things to do in life than protect the environment’. ----- 
desc_morimp10 <- isspenvt2010_recode %>% 
  group_by(unece) %>%
  summarise(mean = round(mean(morimp10, na.rm = T), digits = 2),
            n = n())

#•	‘There is no point in doing what I can for the environment unless others do the same’. (others10) 
desc_others10 <- isspenvt2010_recode %>%
  group_by(unece) %>%
  summarise(mean = round(mean(others10, na.rm = T), digits = 2),
            n = n())




