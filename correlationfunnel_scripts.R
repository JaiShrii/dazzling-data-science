# Install required packages and call libraries
install.packages("correlationfunnel")
install.packages("tidyverse")
library(correlationfunnel)
library(tidyverse)

data("customer_churn_tbl")

head(customer_churn_tbl)

head(marketing_campaign_tbl)

marketing_campaign_tbl %>%select(-ID) %>%binarize() %>%correlate(TERM_DEPOSIT__yes) %>%plot_correlation_funnel()

marketing_campaign_tbl %>%select(-ID) %>%binarize() %>%correlate(TERM_DEPOSIT__yes) %>%plot_correlation_funnel(interactive = TRUE)

binary_1 <- binarize(mpg)
glimpse(binary_1)
correl_1 <- correlate(binary_1, hwy__27_Inf)

plot_correlation_funnel(correl_1)
plot_correlation_funnel(correl_1, interactive = TRUE)

head(iris)
iris <- iris[,1:5]

head(diamonds)
head(mpg)

correl_1 <- correlate(binary_1, hwy__27_Inf)
glimpse(correl_1)
plot_correlation_funnel(correl_1, interactive = TRUE)

# Example of piping correlate() directly into plot_correlation_funnel
binary_1 <- binarize(mpg)
glimpse(binary_1)
binary_1 %>%correlate(hwy__27_Inf) %>%plot_correlation_funnel()

# Example of combining correlate() and plot_correlation_funnel into one function
plot_cf <- function(data_1, target_1) {
  correl_1 <- correlate(data_1, target_1)
  plot_1 <- plot_correlation_funnel(correl_1)
  return(plot_1)
}

binary_1 <- binarize(mpg)
glimpse(binary_1)

correl_1 <- correlate(binary_1, hwy__27_Inf)
head(correl_1, 10)

plot_cf(binary_1, "hwy__27_Inf")

