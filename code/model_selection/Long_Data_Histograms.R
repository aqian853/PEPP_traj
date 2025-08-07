# check out distributions
library(reshape2)

# Check column names and plotting ----------------------------------------------
# LV1
colnames(PEPP_cdss_LV1_long)
ggplot(data = PEPP_cdss_LV1_long, mapping = aes(x = LV1_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# LV1_clean
ggplot(data = PEPP_cdss_LV1_long_clean, mapping = aes(x = LV1_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# LV1_filtered
ggplot(data = PEPP_cdss_LV1_long_filtered, mapping = aes(x = LV1_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# LV3
colnames(PEPP_cdss_LV3_long)
ggplot(data = PEPP_cdss_LV3_long, mapping = aes(x = LV3_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# LV3_clean
ggplot(data = PEPP_cdss_LV3_long_clean, mapping = aes(x = LV3_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# LV3_filtered
ggplot(data = PEPP_cdss_LV3_long_filtered, mapping = aes(x = LV3_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total CDSS
colnames(PEPP_cdss_tot_long)
ggplot(data = PEPP_cdss_tot_long, mapping = aes(x = cdss_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total CDSS_clean
ggplot(data = PEPP_cdss_tot_long_clean, mapping = aes(x = cdss_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total CDSS_filtered
ggplot(data = PEPP_cdss_tot_long_filtered, mapping = aes(x = cdss_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total SANS
colnames(PEPP_sans_long)
ggplot(data = PEPP_sans_long, mapping = aes(x = sans_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total SANS_clean
ggplot(data = PEPP_sans_long_clean, mapping = aes(x = sans_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')

# Total SANS_filtered
ggplot(data = PEPP_sans_long_filtered, mapping = aes(x = sans_total_score)) +
  geom_histogram(bins = 10) + facet_wrap(~time_point, scales = 'free_x')
