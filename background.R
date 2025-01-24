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
library("ggimage")
library("igraph")
library("ggraph")
library("lubridate")

# Load datasets
companies <- read.csv("data/companies.csv")
badges <- read.csv("data/badges.csv")
founders <- read.csv("data/founders.csv")
industries <- read.csv("data/industries.csv")
prior_companies <- read.csv("data/prior_companies.csv")
regions <- read.csv("data/regions.csv")
schools <- read.csv("data/schools.csv")
tags <- read.csv("data/tags.csv")

companies


custom_theme <- theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray90"),
  panel.grid.minor = element_blank(),
  axis.text = element_text(color = "gray19"),
  axis.title = element_text(color = "gray19"),
  plot.title = element_text(color = "gray19", face = "bold", hjust = 0.5),
  legend.background = element_rect(fill = "white"),
  legend.text = element_text(color = "gray19"),
  legend.title = element_text(color = "gray19"),
  axis.line = element_line(color = "lightgray"),
  axis.ticks = element_line(color = "lightgray")
)


industries_filtered <- industries %>%
  filter(!industry %in% c("B2B", "Consumer")) %>%
  group_by(id, industry) %>%
  summarise(value = n(), .groups = 'drop')


convert_batch <- function(batch) {
  year <- paste0("20", substr(batch, 2, nchar(batch)))
  month <- ifelse(substr(batch, 1, 1) == "W", "01", "07") 
  return(paste0(month, "-", year))
}
companies$batch_date <- sapply(companies$batch, convert_batch)

latest_education <- schools %>%
  filter(!is.na(field_of_study) & field_of_study != "") %>%
  group_by(hnid) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  ungroup()




##################################################################################
# Top Schools and Companies Sankey Diagram
##################################################################################

sankey_data <- founders %>%
  left_join(companies, by = c("company_slug" = "slug")) %>%
  left_join(prior_companies, by = "hnid") %>%
  left_join(schools, by = "hnid") %>%
  mutate(
    school = case_when(
      school == "Massachusetts Institute of Technology" ~ "MIT",
      school == "University of California, Berkeley" ~ "University of California",
      TRUE ~ school
    )
  )

# Get top schools and companies
top_schools <- sankey_data %>%
  group_by(school) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(5)

top_companies <- sankey_data %>%
  group_by(company) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(5)

sankey_aggregated <- sankey_data %>%
  filter(school %in% top_schools$school & company %in% top_companies$company) %>%
  group_by(school, company, status) %>%
  summarise(count = n(), .groups = 'drop')


ggplot(sankey_aggregated,
       aes(axis1 = school, axis2 = company, axis3 = status, y = count)) +
  geom_alluvium(aes(fill = "grey"), color = "#FD5612", width = 1/12) +
  geom_stratum(width = 10/25, fill = "#FEF5EF", color = "black") +
  geom_text(
    stat = "stratum", 
    aes(label = after_stat(stratum)),
    size = 3
  ) +  
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size=20),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(title = "Most frequent background of Founders",
       subtitle = "From Schools to Previous Companies to Status of their Company")



##################################################################################
# Industry Rankings Plot
##################################################################################

industry_rankings <- companies %>%
  left_join(industries_filtered, by = "id") %>%
  mutate(year = year(as.Date(paste0(batch_date, "-01"), format = "%m-%Y-%d"))) %>%
  filter(!is.na(industry)) %>%
  group_by(industry, year) %>%
  summarise(count = n(), .groups = 'drop')

# Get top 10 industries
top_10_industries <- industry_rankings %>%
  group_by(industry) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(industry)

industry_rankings <- industry_rankings %>%
  filter(industry %in% top_10_industries) %>%
  group_by(year) %>%
  mutate(rank = rank(-count, ties.method = "first")) %>%
  ungroup()

# Create industry rankings plot
ggplot(industry_rankings, aes(x = year, y = rank, group = industry)) +
  geom_line(aes(color = industry), size = 1.2) +
  geom_point(aes(color = industry), size = 2.3, shape = 21, fill = '#FFFFFF', stroke = 1.2) +
  geom_text(data = industry_rankings %>% filter(year == min(year)),
            aes(label = industry, color = industry),
            hjust = 1.2, size = 3) +
  geom_text(data = industry_rankings %>% filter(year == max(year)),
            aes(label = industry, color = industry,
                hjust = -0.2 + (0.05 * (nchar(industry)/10))),
            size = 3) +
  scale_y_reverse(breaks = 1:10, limits = c(10, 1)) +
  scale_x_continuous(breaks = seq(min(industry_rankings$year), 
                                  max(industry_rankings$year), 
                                  by = 2),
                     expand = c(.2, .2)) +
  labs(title = "Top 10 Industries Rankings Over Time",
       x = "Year",
       y = "Rank") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values = colorRampPalette(c("#FD5612", "#000000", "#fda942"))(10))



##################################################################################
# Network Analysis and Visualization
##################################################################################

top_10_fields <- latest_education %>% 
  group_by(field_of_study) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(10) %>%
  pull(field_of_study)

top_10_fields

network_data <- founders %>%
  left_join(latest_education, by = "hnid") %>%
  left_join(companies, by = c("company_slug" = "slug")) %>%
  left_join(industries_filtered, by = "id") %>%
  filter(field_of_study %in% top_10_fields,
         industry %in% top_10_industries)

connections <- network_data %>%
  filter(!is.na(field_of_study), !is.na(industry)) %>%
  group_by(field_of_study, industry) %>%
  summarise(value = n(), .groups = 'drop') %>%
  filter(value > 0)


field_counts <- network_data %>%
  group_by(field_of_study) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(name = field_of_study) %>%
  distinct()

industry_counts <- network_data %>%
  group_by(industry) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(name = industry) %>%
  distinct()


nodes <- data.frame(
  name = c(paste0("field_", unique(connections$field_of_study)),
           paste0("ind_", unique(connections$industry))),
  label = c(unique(connections$field_of_study),
            unique(connections$industry)),
  type = c(rep("Field", length(unique(connections$field_of_study))),
           rep("Industry", length(unique(connections$industry))))
) %>%
  left_join(bind_rows(
    mutate(field_counts, type = "Field"),
    mutate(industry_counts, type = "Industry")
  ), by = c("label" = "name", "type" = "type")) %>%
  distinct()


edges <- connections %>%
  left_join(field_counts, by = c("field_of_study" = "name")) %>%
  mutate(
    from = paste0("field_", field_of_study),
    to = paste0("ind_", industry),
    edge_width = value/count
  ) %>%
  select(from, to, weight = edge_width) %>%
  distinct()

# Create and plot network
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
layout <- create_layout(graph, layout = "fr")

ggraph(layout) +   
  geom_edge_link(aes(width = weight, alpha = weight),
                 arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(3, 'mm'),
                 color = "#FD5612") +
  geom_node_point(aes(size = count, color = type),
                  stroke = 0.5,
                  color = "white") +
  geom_node_point(aes(size = count, color = type),
                  alpha = 0.9) +
  geom_node_text(aes(label = label), 
                 repel = TRUE,
                 size = 3.5,
                 bg.color = "white",
                 bg.r = 0.15,
                 segment.color = "gray50",
                 max.overlaps = 20) +
  scale_edge_width(range = c(0.2, 2)) +
  scale_size_continuous(range = c(5, 15)) +
  scale_color_manual(values = c("Field" = "#FD5612", "Industry" = "#fda942")) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold",
                              margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0.5, size = 11,
                                 margin = margin(b = 20)),
    legend.box = "vertical",
    legend.spacing = unit(10, "pt"),
    legend.margin = margin(10, 10, 10, 10),
    legend.title = element_text(size = 10),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  guides(
    edge_width = guide_legend(
      title = "Proportion of Field",
      override.aes = list(color = "#FD5612", alpha = 0.9),
      order = 2
    ),
    edge_alpha = guide_legend(
      title = "Proportion of Field",
      override.aes = list(color = "#FD5612", alpha = 0.9),
      order = 2
    ),
    size = guide_legend(
      title = "Total Count",
      override.aes = list(color = "gray50"),
      order = 3
    ),
    color = guide_legend(
      title = "Type",
      override.aes = list(size = 5),
      order = 1
    )
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Network of Fields of Study and Industries",
    subtitle = "Edge shows the proportion of field going to a given industry"
  )

##################################################################################
# Success Rate Heatmap
##################################################################################

success_rates <- founders %>%
  left_join(latest_education, by = "hnid") %>%
  left_join(companies, by = c("company_slug" = "slug")) %>%
  filter(!is.na(field_of_study), !is.na(status)) %>%

  group_by(field_of_study) %>%
  mutate(field_total = n()) %>%
  ungroup() %>%

  filter(field_total >= 50) %>%

  group_by(field_of_study, status) %>%
  summarise(
    count = n(),
    field_total = first(field_total),
    .groups = 'drop'
  ) %>%
  
  group_by(field_of_study) %>%
  mutate(
    percentage = count / sum(count),
    field_label = paste0(field_of_study, " (", field_total, ")")
  ) %>%
  ungroup() %>%
  mutate(field_label = reorder(field_label, field_total))


ggplot(success_rates, aes(x = status, y = field_label, fill = percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "#FEF5EF", high = "#FD5612",
                      labels = scales::percent) + 
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            color = ifelse(success_rates$percentage > 0.5, "white", "black")) +
  theme_minimal() +
  labs(title = "Company Status by Educational Background",
       x = "Company Status",
       y = "Field of Study",
       fill = "Percentage") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

