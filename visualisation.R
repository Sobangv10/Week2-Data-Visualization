
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(ggrepel)
  library(patchwork)
})
options(width = 100, digits = 5, scipen = 999)
dir.create("figures", showWarnings = FALSE)


blue <- "#2a78d6"; orange <- "#eb6834"; aqua <- "#1baf7a"; yellow <- "#eda100"
grey_line <- "#9b9a93"; ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_story <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title          = element_text(face = "bold", colour = ink, size = 13),
      plot.title.position = "plot",
      plot.subtitle       = element_text(colour = ink2, size = 10),
      axis.text           = element_text(colour = ink2),
      axis.title          = element_text(colour = ink2),
      strip.text          = element_text(colour = ink, face = "bold", size = 9),
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = grid_col, linewidth = 0.3),
      legend.position     = "top",
      legend.justification = "left",
      plot.background     = element_rect(fill = "white", colour = NA)
    )
}
theme_set(theme_story())


save_fig <- function(name, plot, w = 7, h = 4.2) {
  ggsave(file.path("figures", name), plot, width = w, height = h, dpi = 300,
         bg = "white")
}
usd_k <- label_dollar(scale = 1/1000, suffix = "K", accuracy = 1)


tx <- read.csv("data/txhousing.csv", stringsAsFactors = FALSE)
tx$month_date <- as.Date(sprintf("%d-%02d-01", tx$year, tx$month))
tx$date <- NULL                      # decimal-year column; month_date replaces it

cat("Rows:", nrow(tx), "  Cities:", n_distinct(tx$city), "\n")
cat("Coverage:", format(min(tx$month_date), "%b %Y"), "to",
    format(max(tx$month_date), "%b %Y"), "\n\n")
head(tx[, c("city", "month_date", "sales", "volume", "median", "listings",
            "inventory")])


str(tx)

miss <- data.frame(
  variable    = names(tx),
  n_missing   = colSums(is.na(tx)),
  pct_missing = round(100 * colMeans(is.na(tx)), 1),
  row.names   = NULL
)
miss[miss$n_missing > 0, ]

summary(tx[, c("sales", "volume", "median", "listings", "inventory")])


city_totals <- tx %>%
  group_by(city) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_sales))
top12 <- head(city_totals$city, 12)
big4  <- c("Houston", "Dallas", "Austin", "San Antonio")


state_month <- tx %>%
  mutate(inv_x_sales    = inventory * sales,
         sales_with_inv = ifelse(is.na(inventory), NA, sales)) %>%
  group_by(month_date, year, month) %>%
  summarise(
    sales  = sum(sales, na.rm = TRUE),
    volume = sum(volume, na.rm = TRUE),
    inv    = sum(inv_x_sales, na.rm = TRUE) / sum(sales_with_inv, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(month_date) %>%
  mutate(avg_price = volume / sales,
         sales_12m = as.numeric(stats::filter(sales, rep(1/12, 12), sides = 1)))

cat("Months in statewide series:", nrow(state_month), "\n")
cat("Months of data in 2015:", sum(state_month$year == 2015),
    "(a partial year, so 2015 is left out of yearly totals)\n")
head(as.data.frame(state_month[, c("month_date", "sales", "avg_price", "inv")]), 3)

top10 <- city_totals %>%
  head(10) %>%
  mutate(city = reorder(city, total_sales))

cat("Share of all sales made in the top 10 areas:",
    percent(sum(top10$total_sales) / sum(city_totals$total_sales), 0.1), "\n")
cat("Share made in Houston and Dallas alone:",
    percent(sum(head(city_totals$total_sales, 2)) / sum(city_totals$total_sales), 0.1), "\n\n")
as.data.frame(head(city_totals, 10))

p_bar <- ggplot(top10, aes(total_sales, city)) +
  geom_col(fill = blue, width = 0.65) +
  geom_text(aes(label = comma(total_sales)), hjust = -0.12, size = 3.3, colour = ink) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     expand = expansion(mult = c(0, 0.16))) +
  labs(title = "Houston and Dallas dominate the Texas housing market",
       subtitle = "Total homes sold in each area, January 2000 to July 2015",
       x = "Homes sold", y = NULL) +
  theme(panel.grid.major.y = element_blank())
save_fig("fig01_sales_by_city.png", p_bar, 7, 4.2)


yearly <- state_month %>%
  filter(year <= 2014) %>%
  group_by(year) %>%
  summarise(sales = sum(sales), .groups = "drop")
yearly$change_vs_2006 <- round(100 * (yearly$sales / yearly$sales[yearly$year == 2006] - 1), 1)
as.data.frame(yearly)

peak   <- state_month %>% filter(year %in% 2005:2008) %>% slice_max(sales_12m, n = 1)
trough <- state_month %>% filter(year %in% 2009:2012) %>% slice_min(sales_12m, n = 1)
cat("\n12-month average at the 2006-07 peak :", round(peak$sales_12m), "sales per month\n")
cat("12-month average at the 2009-11 low  :", round(trough$sales_12m), "sales per month\n")
cat("Drop from peak to low                :",
    percent(1 - trough$sales_12m / peak$sales_12m, 0.1), "\n")

p_line_sales <- ggplot(state_month, aes(month_date)) +
  geom_line(aes(y = sales), colour = "#b7d3f6", linewidth = 0.5) +
  geom_line(aes(y = sales_12m), colour = blue, linewidth = 1.2, na.rm = TRUE) +
  geom_point(data = rbind(peak, trough), aes(y = sales_12m), colour = orange, size = 2.8) +
  annotate("text", x = peak$month_date, y = peak$sales_12m + 3300,
           label = "Peak", colour = ink, size = 3.3, fontface = "bold") +
  annotate("text", x = trough$month_date, y = trough$sales_12m - 3300,
           label = "Low point", colour = ink, size = 3.3, fontface = "bold") +
  annotate("text", x = as.Date("2000-03-01"), y = 41500, hjust = 0, size = 3.2,
           colour = ink2, label = "Pale line: each month.  Bold line: 12-month average.") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma, limits = c(0, 43000)) +
  labs(title = "Texas home sales rose, fell by about a third, then recovered",
       subtitle = "Homes sold per month across all 46 areas",
       x = NULL, y = "Homes sold per month")
save_fig("fig02_statewide_sales.png", p_line_sales, 7, 4.2)


heat <- state_month %>%
  mutate(month_lab = factor(month, levels = 12:1, labels = rev(month.abb)))

season <- state_month %>%
  filter(year %in% 2001:2014) %>%
  group_by(year) %>% mutate(share = sales / sum(sales)) %>%
  group_by(month) %>% summarise(avg_share = 100 * mean(share), .groups = "drop")
cat("Average share of a year's sales made in each month (2001-2014, full years):\n")
print(transform(season, month = month.abb[month], avg_share = round(avg_share, 1)),
      row.names = FALSE)
cat("\nBusiest month is", round(max(season$avg_share) / min(season$avg_share), 1),
    "times as busy as the quietest month\n")

p_heat <- ggplot(heat, aes(factor(year), month_lab, fill = sales)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_gradientn(colours = c("#cde2fb", "#6da7ec", "#2a78d6", "#184f95", "#0d366b"),
                       labels = comma, name = "Homes sold",
                       na.value = "#efeeea") +
  labs(title = "Sales peak every summer and dip every winter",
       subtitle = "Statewide homes sold per month (the data end in July 2015)",
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank(), legend.key.width = unit(0.45, "in"),
        axis.text.x = element_text(size = 8.5))
save_fig("fig03_seasonality_heatmap.png", p_heat, 7, 4.6)


price_big4 <- tx %>%
  filter(city %in% big4) %>%
  arrange(city, month_date) %>%
  group_by(city) %>%
  mutate(median_12m = as.numeric(stats::filter(median, rep(1/12, 12), sides = 1))) %>%
  ungroup() %>%
  mutate(city = factor(city, levels = c("Austin", "Dallas", "Houston", "San Antonio")))

ends <- price_big4 %>% group_by(city) %>% filter(month_date == max(month_date)) %>% ungroup()
yr <- price_big4 %>%
  filter(year %in% c(2000, 2008, 2014)) %>%
  group_by(city, year) %>% summarise(price = round(mean(median, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = price)
as.data.frame(yr)

p_price <- ggplot(price_big4, aes(month_date, median_12m, colour = city)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_text(data = ends, aes(label = city, y = median_12m), hjust = 0, nudge_x = 70,
            colour = ink, size = 3.4, fontface = "bold") +
  scale_colour_manual(values = c(Austin = blue, Dallas = orange,
                                 Houston = aqua, `San Antonio` = yellow)) +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), as.Date("2015-01-01"), by = "2 years"),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.16))) +
  scale_y_continuous(labels = usd_k, limits = c(80000, NA)) +
  labs(title = "Prices rose in all four big cities and then took off after 2011",
       subtitle = "Typical (median) sale price, 12-month average",
       x = NULL, y = "Median sale price") +
  theme(legend.position = "none")
save_fig("fig04_big_city_prices.png", p_price, 7, 4.2)


growth <- tx %>%
  filter(city %in% top12, year %in% c(2000, 2014)) %>%
  group_by(city, year) %>%
  summarise(price = mean(median, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = price, names_prefix = "y") %>%
  mutate(pct = 100 * (y2014 / y2000 - 1)) %>%
  arrange(pct)
growth$city <- factor(growth$city, levels = growth$city)
as.data.frame(transform(growth, y2000 = round(y2000), y2014 = round(y2014), pct = round(pct, 1)))

growth_long <- growth %>%
  pivot_longer(c(y2000, y2014), names_to = "year", values_to = "price") %>%
  mutate(year = sub("y", "", year))

p_dumb <- ggplot(growth) +
  geom_segment(aes(x = y2000, xend = y2014, y = city, yend = city),
               colour = "#cfcdc6", linewidth = 1.6) +
  geom_point(data = growth_long, aes(price, city, colour = year), size = 3.6) +
  geom_text(aes(x = y2014, y = city, label = paste0("+", round(pct), "%")),
            hjust = -0.35, size = 3.3, colour = ink, fontface = "bold") +
  scale_colour_manual(values = c(`2000` = orange, `2014` = blue), name = NULL) +
  scale_x_continuous(labels = usd_k, expand = expansion(mult = c(0.03, 0.12))) +
  labs(title = "Prices nearly doubled in Fort Bend and San Antonio; Denton County rose least",
       subtitle = "The 12 largest areas: typical sale price in 2000 and 2014, with the percentage rise",
       x = "Median sale price (average of the year's months)", y = NULL) +
  theme(panel.grid.major.y = element_blank())
save_fig("fig05_price_growth.png", p_dumb, 7, 4.6)


hist_data <- tx %>%
  filter(!is.na(median), year %in% c(2000:2003, 2012:2015)) %>%
  mutate(period = ifelse(year <= 2003, "2000-2003", "2012-2015"))
med_by_period <- hist_data %>%
  group_by(period) %>%
  summarise(typical = median(median), q1 = quantile(median, 0.25),
            q3 = quantile(median, 0.75), n = n(), .groups = "drop")
as.data.frame(med_by_period)

p_hist <- ggplot(hist_data, aes(median, fill = period)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 10000,
                 boundary = 0, alpha = 0.7, colour = "white", linewidth = 0.2,
                 position = "identity") +
  geom_vline(data = med_by_period, aes(xintercept = typical, colour = period),
             linewidth = 0.9, linetype = "dashed", show.legend = FALSE) +
  scale_fill_manual(values = c(`2000-2003` = orange, `2012-2015` = blue), name = "Period") +
  scale_colour_manual(values = c(`2000-2003` = orange, `2012-2015` = blue)) +
  scale_x_continuous(labels = usd_k, limits = c(30000, 330000)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "The whole range of prices has shifted upward and spread out",
       subtitle = "Share of city-months at each price level; dashed lines mark each period's median",
       x = "Median sale price in a city for one month", y = "Share of city-months")
save_fig("fig06_price_distribution.png", p_hist, 7, 4.2)


facet_data <- tx %>%
  filter(city %in% top12) %>%
  arrange(city, month_date) %>%
  group_by(city) %>%
  mutate(median_12m = as.numeric(stats::filter(median, rep(1/12, 12), sides = 1))) %>%
  ungroup() %>%
  mutate(city = factor(city, levels = top12))

state_ref <- state_month %>% select(month_date, avg_price) %>%
  mutate(state_12m = as.numeric(stats::filter(avg_price, rep(1/12, 12), sides = 1)))

p_facets <- ggplot(facet_data, aes(month_date)) +
  geom_line(data = state_ref, aes(y = state_12m), colour = grey_line, linewidth = 0.6,
            na.rm = TRUE) +
  geom_line(aes(y = median_12m), colour = blue, linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~city, ncol = 4) +
  scale_x_date(date_breaks = "5 years", date_labels = "'%y") +
  scale_y_continuous(labels = usd_k) +
  labs(title = "Each major market has its own price story",
       subtitle = "Blue: the area's median sale price.  Grey: average Texas sale price, for comparison",
       x = NULL, y = "Sale price (12-month average)") +
  theme(axis.text = element_text(size = 8))
save_fig("fig07_city_small_multiples.png", p_facets, 7.4, 5.2)


city14 <- tx %>%
  filter(year == 2014) %>%
  group_by(city) %>%
  summarise(price = mean(median, na.rm = TRUE),
            inv   = mean(inventory, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(price), !is.na(inv))

cat("Cities in the 2014 comparison:", nrow(city14), "\n")
cat("Correlation, supply vs price (log scale for supply):",
    round(cor(log10(city14$inv), city14$price), 2), "\n")
cat("Correlation, supply vs price (plain scale)         :",
    round(cor(city14$inv, city14$price), 2), "\n")
as.data.frame(transform(city14[order(-city14$sales), ][1:6, ],
                        price = round(price), inv = round(inv, 1)))

label_cities <- c("Houston", "Dallas", "Austin", "San Antonio", "Collin County",
                  "Fort Bend", "El Paso", "Harlingen", "South Padre Island",
                  "Kerrville", "McAllen", "Galveston", "Midland", "Paris")

p_scatter <- ggplot(city14, aes(inv, price)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = grey_line,
              linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(size = sales), colour = blue, alpha = 0.65) +
  geom_text_repel(data = filter(city14, city %in% label_cities), aes(label = city),
                  size = 3, colour = ink, min.segment.length = 0.2,
                  segment.colour = grey_line, max.overlaps = 30, seed = 7) +
  scale_x_log10(breaks = c(2, 3, 5, 10, 20)) +
  scale_y_continuous(labels = usd_k) +
  scale_size_area(max_size = 12, labels = comma, name = "Homes sold in 2014",
                  breaks = c(5000, 20000, 80000)) +
  labs(title = "Areas with more homes waiting to be sold tend to have lower prices",
       subtitle = "One bubble per area, 2014. Dashed line shows the overall tendency",
       x = "Months it would take to sell all current listings (log scale)",
       y = "Median sale price")
save_fig("fig08_supply_vs_price.png", p_scatter, 7, 4.6)


inv_year <- state_month %>% filter(year <= 2014) %>%
  group_by(year) %>% summarise(inv = round(mean(inv), 1), .groups = "drop")
as.data.frame(t(inv_year))

hi <- state_month %>% filter(year %in% 2008:2011) %>% slice_max(inv, n = 1)
lo <- state_month %>% slice_min(inv, n = 1)
cat("\nHighest monthly supply:", round(hi$inv, 1), "months in", format(hi$month_date, "%b %Y"), "\n")
cat("Lowest monthly supply :", round(lo$inv, 1), "months in", format(lo$month_date, "%b %Y"), "\n")

p_inv <- ggplot(state_month, aes(month_date, inv)) +
  geom_hline(yintercept = 6, colour = grey_line, linetype = "dashed") +
  annotate("text", x = as.Date("2000-02-01"), y = 7.3, hjust = 0, size = 3.2, colour = ink2,
           label = "Dashed line: about 6 months is often\ncalled a balanced market", lineheight = 0.95) +
  geom_line(colour = blue, linewidth = 1.1) +
  geom_point(data = rbind(hi, lo), colour = orange, size = 2.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Homes became much harder to find after 2011",
       subtitle = "Months needed to sell all current listings (sales-weighted across Texas areas)",
       x = NULL, y = "Months of supply")
save_fig("fig09_inventory.png", p_inv, 7, 4.2)


sep_aug <- tx %>%
  filter(city == "Houston", month %in% 8:9, year <= 2014) %>%
  select(year, month, sales) %>%
  pivot_wider(names_from = month, values_from = sales, names_prefix = "m") %>%
  mutate(ratio = m9 / m8,
         lowest = ratio == min(ratio))
as.data.frame(transform(sep_aug, ratio = round(ratio, 2)))
cat("\nTypical September/August ratio (median of all years):",
    round(median(sep_aug$ratio), 2), "\n")
cat("Lowest year:", sep_aug$year[sep_aug$lowest], "at", round(min(sep_aug$ratio), 2), "\n")

p_hurr <- ggplot(sep_aug, aes(year, ratio)) +
  geom_hline(yintercept = median(sep_aug$ratio), colour = grey_line, linetype = "dashed") +
  geom_segment(aes(xend = year, yend = 0, colour = lowest), linewidth = 1.2) +
  geom_point(aes(colour = lowest), size = 3.4) +
  annotate("segment", x = 2008, xend = 2008, y = 1.0, yend = min(sep_aug$ratio) + 0.04,
           colour = ink2, linewidth = 0.4) +
  annotate("text", x = 2008, y = 1.1, size = 3.1, colour = ink,
           lineheight = 0.95, label = "2008: the lowest of 15 years\nHurricane Ike hit Houston\nthat September") +
  annotate("text", x = 2000.1, y = median(sep_aug$ratio) + 0.05, hjust = 0, size = 3.1,
           colour = ink2, label = "Typical year") +
  scale_colour_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  scale_x_continuous(breaks = 2000:2014) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.2)) +
  labs(title = "Houston's September 2008 sales fell further than in any other year",
       subtitle = "Houston's September sales as a share of its August sales",
       x = NULL, y = "September as % of August") +
  theme(axis.text.x = element_text(size = 8.5))
save_fig("fig10_hurricane_anomaly.png", p_hurr, 7, 4.2)


swings <- tx %>%
  arrange(city, month_date) %>%
  group_by(city) %>%
  mutate(change = abs(median / lag(median) - 1)) %>%
  ungroup() %>%
  filter(!is.na(change), !is.na(sales), sales > 0)

by_size <- swings %>%
  mutate(size_group = ntile(sales, 10)) %>%
  group_by(size_group) %>%
  summarise(sales = median(sales), typical_swing = median(change), .groups = "drop")
cat("Typical month-to-month price swing by market size (10 equal groups):\n")
as.data.frame(transform(by_size, typical_swing = percent(typical_swing, 0.1)))

biggest <- swings %>% slice_max(change, n = 1)
cat("\nLargest single swing:", biggest$city, format(biggest$month_date, "%b %Y"),
    "- price changed", percent(biggest$change, 1), "on", biggest$sales, "sales\n")

p_funnel <- ggplot(swings, aes(sales, change)) +
  geom_point(colour = blue, alpha = 0.18, size = 0.9) +
  geom_line(data = by_size, aes(y = typical_swing), colour = orange, linewidth = 1.2) +
  geom_point(data = by_size, aes(y = typical_swing), colour = orange, size = 2.6) +
  annotate("text", x = 9000, y = 0.3, hjust = 1, size = 3.3, colour = ink,
           label = "Orange: typical swing\nfor markets of that size", lineheight = 0.95) +
  geom_text_repel(data = biggest, aes(label = paste0(city, ", ", format(month_date, "%b %Y"),
                                                     "\n", sales, " sales")),
                  size = 3, colour = ink, nudge_x = 0.4, nudge_y = 0.1, seed = 3) +
  scale_x_log10(labels = comma, breaks = c(10, 30, 100, 300, 1000, 3000, 10000)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Small markets produce wild price swings; big markets stay steady",
       subtitle = "Each dot is one city in one month: how much its median price moved vs the month before",
       x = "Homes sold that month (log scale)", y = "Month-to-month price change (either direction)")
save_fig("fig11_small_market_noise.png", p_funnel, 7, 4.4)

s1 <- ggplot(state_month, aes(month_date, sales_12m)) +
  geom_line(colour = blue, linewidth = 1, na.rm = TRUE) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "1. Sales: boom, dip, recovery", x = NULL, y = "Homes sold per month")

s2 <- ggplot(season, aes(factor(month, labels = month.abb), avg_share)) +
  geom_col(fill = blue, width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = "2. Summer is the busy season", x = NULL, y = "Share of year's sales") +
  theme(axis.text.x = element_text(size = 6.5))

s3 <- ggplot(state_month, aes(month_date, avg_price)) +
  geom_line(colour = blue, linewidth = 1) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(labels = usd_k, limits = c(0, NA)) +
  labs(title = "3. Average price keeps climbing", x = NULL, y = "Average sale price")

s4 <- ggplot(state_month, aes(month_date, inv)) +
  geom_line(colour = blue, linewidth = 1) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "4. Fewer homes for sale", x = NULL, y = "Months of supply")

p_summary <- (s1 | s2) / (s3 | s4) +
  plot_annotation(title = "The Texas housing market at a glance",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))
save_fig("fig12_summary_panel.png", p_summary, 7.4, 5.4)

pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "ggrepel", "patchwork")
data.frame(
  component = c("R", pkgs),
  version   = c(as.character(getRversion()),
                sapply(pkgs, function(p) as.character(packageVersion(p)))),
  row.names = NULL
)
