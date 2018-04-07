# PREAMBLE ####

if(!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Get SIC codes ####

codes <- read.csv("data/SIC_codes.csv", stringsAsFactors = F)

# Source SIC codes from ONS
# url <- "http://resources.companieshouse.gov.uk/sic/"
# t <- htmltab(doc = url)
# cats <- ifelse(!grepl("[0-9]+", t$Code), t$Description, NA)
# t$cat <- zoo::na.locf(cats)
# codes <- t[grepl("[0-9]+", t$Code),]
# write.csv(t, "SIC_codes.csv")

# Get dap data ####

gap <- read.csv("data/gap_data.csv", stringsAsFactors = F)

# Assign sector data

gap$sector <- codes$cat[match(gap$SicCodes,codes$Code)]

# NOTE THAT ~ CODES WERE NOT MATCHED!

# Plot ####

sec_keep <- c("Manufacturing",
              "Administrative and support service activities",
              "Wholesale and retail trade; repair of motor vehicles and motorcycles",
              "Professional, scientific and technical activities",
              "Financial and insurance activities"
)

xlabs2 <- c("Manufacturing",
            "Admin & services",
            "Wholesale and retail", 
           "Professional & scientific",
           "Financial & insurance"
)

p <- gap %>%
  dplyr::mutate(
    # EmployerSize = factor(
    #   ifelse(EmployerSize == "Not Provided", NA, EmployerSize),
    #   levels = c("Less than 250","250 to 499","500 to 999","1000 to 4999",  "5000 to 19,999", "20,000 or more")
    # )
    sector = factor(sector, levels = sec_keep)
      ) %>%
  dplyr::filter(sector %in% sec_keep) %>%
  dplyr::rename(var = sector) %>%
      dplyr::group_by(var) %>%
      dplyr::summarise(
        `Hourly rate` = mean(DiffMeanHourlyPercent),
        # median = median(DiffMeanHourlyPercent),
        `Bonus payment` = mean(DiffMeanBonusPercent)
        # median_bonus = median(DiffMedianBonusPercent)
        ) %>%
  reshape2::melt(id="var") %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_col(position = "dodge") +
  scale_fill_discrete("Sector",labels = xlabs2) +
  scale_y_continuous("% by which women are paid less than men") +
  scale_x_discrete("") +
  facet_grid(~variable) +
  # theme_bw() +
  labs(title = "Gender pay gap in the UK (2017-18)",
       caption = "Source: @d_alburez with data from https://gender-pay-gap.service.gov.uk/") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=14)) 
  