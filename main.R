library("ggplot2")
library("dplyr")
library("png")
library("grid")
library("tidyr")
library("treemapify")
library("patchwork")
library("maps")
library("ggridges")
library("ggalluvial")
library("gganimate")

companies <- read.csv("data/companies.csv")
badges <- read.csv("data/badges.csv")
founders <- read.csv("data/founders.csv")
industries <- read.csv("data/industries.csv")
prior_companies <- read.csv("data/prior_companies.csv")
regions <- read.csv("data/regions.csv")
schools <- read.csv("data/schools.csv")
tags <- read.csv("data/tags.csv")

orange_palette <- colorRampPalette(c("#FEF5EF", "#FD5612"))

custom_theme <- theme(
  plot.background = element_rect(fill = "white"), # White background for the entire plot
  panel.background = element_rect(fill = "white"), # White background for the plotting area
  panel.grid.major = element_line(color = "gray90"), # Light gray major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  axis.text = element_text(color = "gray19"), # Black axis text
  axis.title = element_text(color = "gray19"), # Black axis title
  plot.title = element_text(color = "gray19", face = "bold", hjust = 0.5), # Black plot title
  legend.background = element_rect(fill = "white"), # White legend background
  legend.text = element_text(color = "gray19"), # Black legend text
  legend.title = element_text(color = "gray19"), # Black legend title
  axis.line = element_line(color = "lightgray"), # Light gray axis lines (tick lines)
  axis.ticks = element_line(color = "lightgray"), # Light gray axis ticks
)

# the batches represent Winter (W) and Summer (S) and year (number)
# Winter have 01-YYYY and Summer have 07-YYYY
convert_batch <- function(batch) {
  year <- paste0("20", substr(batch, 2, nchar(batch)))
  month <- ifelse(substr(batch, 1, 1) == "W", "01", "07") 
  return(paste0(month, "-", year))
}
companies$batch_date <- sapply(companies$batch, convert_batch)

##################################################################################
# bar plot for count of start-ups in each batch
##################################################################################

batch_counts <- companies %>%
  mutate(batch_date = as.Date(paste0(batch_date, "-01"), format = "%m-%Y-%d")) %>%
  group_by(batch_date) %>%
  summarise(count = n()) %>%
  arrange(batch_date)


airbnb_logo <- png::readPNG("images/airbnb.png") 
dropbox_logo <- png::readPNG("images/dropbox.png")
gitlab_logo <- png::readPNG("images/gitlab.png")
dordash_logo <- png::readPNG("images/dordash.png")
supabase_logo <- png::readPNG("images/supabase.png")
p <- ggplot(batch_counts, aes(x = batch_date, y = count)) +
  geom_bar(stat = "identity", fill = "#FD5612") +
  labs(
    title = "Count of Companies by Batch Date",
    x = "Batch Period"
  ) +
  custom_theme +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 year")

p <- p + annotation_custom(rasterGrob(airbnb_logo, width = unit(1, "cm"), height = unit(1, "cm")), 
                           xmin = as.Date("2008-01-07"), xmax = as.Date("2008-01-07"), 
                           ymin = 50, ymax = 100)
p <- p + annotation_custom(rasterGrob(dropbox_logo, width = unit(1, "cm"), height = unit(1, "cm")), 
                           xmin = as.Date("2007-07-01"), xmax = as.Date("2007-07-01"), 
                           ymin = 120, ymax = 170)
p <- p + annotation_custom(rasterGrob(gitlab_logo, width = unit(1, "cm"), height = unit(1, "cm")), 
                           xmin = as.Date("2015-01-01"), xmax = as.Date("2015-01-01"), 
                           ymin = 180, ymax = 230)
p <- p + annotation_custom(rasterGrob(dordash_logo, width = unit(1, "cm"), height = unit(1, "cm")), 
                           xmin = as.Date("2013-07-01"), xmax = as.Date("2013-07-01"), 
                           ymin = 100, ymax = 150)
p <- p + annotation_custom(rasterGrob(supabase_logo, width = unit(1, "cm"), height = unit(1, "cm")), 
                           xmin = as.Date("2020-07-01"), xmax = as.Date("2020-07-01"), 
                           ymin = 340, ymax = 390)

p <- p + geom_segment(
  aes(x = as.Date("2008-01-07"), xend = as.Date("2008-01-07"), y = 0, yend = 50),
  color = "black",
  linetype = "dotted",
  size = 0.5
)
p <- p + geom_segment(
  aes(x = as.Date("2007-07-01"), xend = as.Date("2007-07-01"), y = 0, yend = 120),
  color = "black",
  linetype = "dotted",
  size = 0.5
)
p <- p + geom_segment(
  aes(x = as.Date("2015-01-01"), xend = as.Date("2015-01-01"), y = 0, yend = 180),
  color = "black",
  linetype = "dotted",
  size = 0.5
)
p <- p + geom_segment(
  aes(x = as.Date("2013-07-01"), xend = as.Date("2013-07-01"), y = 0, yend = 100),
  color = "black",
  linetype = "dotted",
  size = 0.5
)
p <- p + geom_segment(
  aes(x = as.Date("2020-07-01"), xend = as.Date("2020-07-01"), y = 0, yend = 340),
  color = "black",
  linetype = "dotted",
  size = 0.5
)
p

##################################################################################
# treemap of industries
##################################################################################

industries_filtered <- industries %>%
  filter(!industry %in% c("B2B", "Consumer")) %>%
  group_by(industry) %>%
  summarise(value = n(), .groups = 'drop')

b2b_consumer <- industries %>%
  filter(industry %in% c("B2B", "Consumer")) %>%
  group_by(industry) %>%
  summarise(value = n(), .groups = 'drop')

orange_palette <- colorRampPalette(c("#FD5612", "#FEF5EF"))
colors <- orange_palette(nrow(industries_filtered))

#treemap
treemap_plot <- ggplot(industries_filtered, aes(area = value, fill = industry, label = industry)) +
  geom_treemap() +
  geom_treemap_text(colour = "gray19", place = "centre", grow = TRUE) +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none") +
  labs(title = "Industries and Target Group") +
  custom_theme

#bar chart
stacked_bar_plot <- ggplot(b2b_consumer, aes(x = 1, y = value, fill = industry)) +
  geom_bar(stat = "identity", position = "stack", width = 0.2) +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = industry),
            position = position_stack(vjust = 0.5),
            color = "gray19", size = 5) +
  scale_fill_manual(values = c("B2B" = "#FD5612", "Consumer" = "#FEF5EF")) +
  theme(legend.position = "none")

dotted_line_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray19", size = 0.5) +
  theme_void()

p <- treemap_plot / dotted_line_plot / stacked_bar_plot +
  plot_layout(heights = c(8, 0.1, 2))
p

##################################################################################
# waffle chart of company status
##################################################################################

df <- expand.grid(y = 1:10, x = 1:10)
categ_table <- round(table(companies$status) * ((10 * 10) / length(companies$status)))

if (sum(categ_table) != 100) {
  categ_table[which.max(categ_table)] <- categ_table[which.max(categ_table)] + (100 - sum(categ_table))
}

df$Categories <- factor(rep(names(categ_table), categ_table))

p <- ggplot(df, aes(x = x, y = y, fill = Categories)) +
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = "reverse") +
  scale_fill_manual(values = orange_palette(length(unique(df$Categories)))) +
  labs(
    title = "Proportion of Companies by Status | One Tile = 1%",
    fill = "Status"
  ) +
  custom_theme +
  theme(
    plot.margin = unit(c(0.2, 0.1, 0.2, 0.1), units = "in"),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
p

##################################################################################
# map of company origins
##################################################################################

company_counts <- regions %>%
  group_by(country) %>%
  summarise(n_companies = n(), .groups = 'drop')

map_data <- map_data("world") %>%
  left_join(company_counts, by = c("region" = "country"))

p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = n_companies)) +
  geom_polygon(color = "black") +
  scale_fill_gradientn(colors = orange_palette(10), na.value = "white") +
  labs(title = "Number of Companies by Country",
       fill = "Number of Companies") +
  guides(fill = guide_colorbar(
    direction = "horizontal",
    barwidth = unit(5, "cm"),
    barheight = unit(0.3, "cm"),
    title.position = "top",
    title.hjust = 0.5
  )) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.2),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "gray19"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
p

##################################################################################
# violin plot of comapny size
##################################################################################

filtered_data <- companies %>%
  filter(status != "Inactive", teamSize != 0)

median_data <- median_data %>%
  mutate(status_numeric = as.numeric(factor(status)))

p <- ggplot(filtered_data, aes(x = log(teamSize), y = status, fill = status)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = orange_palette(length(unique(filtered_data$status)))) +
  labs(title = "Log Team Size by Company Status",
       x = "Log Team Size") +
  custom_theme +
  theme(axis.title.y = element_blank()) +
  geom_text(data = median_data, 
            aes(x = max(log(filtered_data$teamSize)), y = status_numeric, 
                label = paste("Median:", round(median_teamSize, 2))), 
            hjust = 1.1, vjust = -1, color = "gray19", size = 5.5)
p

##################################################################################
# sankey plots of schools and previous companies
##################################################################################

data <- founders %>%
  left_join(companies, by = c("company_slug" = "slug")) %>%
  left_join(prior_companies, by = "hnid") %>%
  left_join(schools, by = "hnid")

data <- data %>%
  mutate(
    school = case_when(
      school == "Massachusetts Institute of Technology" ~ "MIT",
      school == "University of California, Berkeley" ~ "University of California",
      TRUE ~ school
    )
  )

top_schools <- data %>%
  group_by(school) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(5)

top_companies <- data %>%
  group_by(company) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(5)

sankey_data <- data %>%
  filter(school %in% top_schools$school & company %in% top_companies$company) %>%
  select(school, company, status)

aggregated_data <- sankey_data %>%
  group_by(school, company, status) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(aggregated_data,
       aes(axis1 = school, axis2 = company, axis3 = status, y = count)) +
  geom_alluvium(aes(fill = "grey"), color = "#FD5612", width = 1/12) +
  geom_stratum(width = 10/25, fill = "#FEF5EF", color = "black") +
  geom_text(
    stat = "stratum", 
    aes(label = after_stat(stratum)),
    size = 3
  ) +  
  theme_void() +
  theme(legend.position = "none")

##################################################################################
# trends plot of new technologies
##################################################################################

combined_data <- companies %>%
  left_join(industries, by = "id") %>%
  mutate(batch_date = as.Date(paste0(batch_date, "-01"), format = "%m-%Y-%d"))

filtered_data <- combined_data %>%
  filter(!is.na(industry) & !industry %in% c("B2B", "Consumer"))

cumulative_counts <- filtered_data %>%
  group_by(industry, batch_date) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(batch_date) %>%
  group_by(industry) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()

p <- cumulative_counts %>%
  ggplot(aes(x = cumulative_count, y = reorder(industry, cumulative_count), fill = industry)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = . %>% group_by(batch_date) %>% arrange(desc(cumulative_count)) %>% slice_head(n = 10),
    aes(label = sprintf("%d", round(cumulative_count))), hjust = -0.2, size = 3, color = "gray19"
  ) +
  labs(title = 'Number of companies from based on Industries | {frame_time}',
       x = 'Cumulative Count',
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = orange_palette(n = length(unique(cumulative_counts$industry)))) +
  transition_time(batch_date) +
  ease_aes('cubic-in-out')

animate(p, nframes = 100, fps = 10, width = 800, height = 600)
