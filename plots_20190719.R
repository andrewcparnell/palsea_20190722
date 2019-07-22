# A script to create various chronology models for the Palsea presentation

# Start-up code
rm(list = ls())
auf::packages('tidyverse', 'Bchron', 'latex2exp', 'ggridges', 'viridis', 'mgcv', 'gganimate', 'gifski', 'png', 'transformr')
theme_set(theme_bw())

# Start with the Glendalough data
data(Glendalough)

# set plot widths and heights
my_width = 6
my_height = 4

# Plot 1 - uncalibrated dates -------------------------

p1 = Glendalough %>% ggplot(aes(x = ages, 
                           y = position)) + 
  geom_point() +
  geom_errorbarh(aes(xmax = ages + 4*ageSds, xmin = ages - 4*ageSds)) + 
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = TeX('$^{14}$C years before present'),
       y = 'Depth\n(cm)',
       title = 'Glendalough') + 
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0.5))
ggsave('p1.jpg', p1, width = my_width, height = my_height)

# p2 is with straight lines -----------------------------------------------

p2 = p1 + geom_line()
ggsave('p2.jpg', p2, width = my_width, height = my_height)
  
# p3 is now with calibrated dates -----------------------------------------

GlenOut = with(Glendalough,
               BchronCalibrate(ages=ages,
                               ageSds=ageSds,
                               calCurves=calCurves,
                               positions=position,
                               ids=id))

# Get 95% quantiles from ages
age_samples = sampleAges(GlenOut)
Glen_CIs = apply(age_samples, 2, quantile, prob=c(0.025,0.5,0.975)) %>% 
  t() %>% 
  as_tibble %>% 
  mutate(depth = Glendalough$position)

p3 = Glen_CIs %>% ggplot(aes(x = `50%`, 
                             y = depth)) + 
  geom_point() +
  geom_line() + 
  geom_errorbarh(aes(xmax = `97.5%`, xmin = `2.5%`)) + 
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = TeX('Calendar years before present'),
       y = 'Depth\n(cm)',
       title = 'Glendalough') + 
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0.5))
ggsave('p3.jpg', p3, width = my_width, height = my_height)


# Try it with boxplots ----------------------------------------------------

# Add more noise to the top to avoid it dominating
Glendalough2 = Glendalough
Glendalough2$ageSds[1] = 50
GlenOut2 = with(Glendalough2,
               BchronCalibrate(ages=ages,
                               ageSds=ageSds,
                               calCurves=calCurves,
                               positions=position,
                               ids=id))

# Get 95% quantiles from ages
age_samples2 = sampleAges(GlenOut2)

Glen_samples = age_samples2 %>% t() %>% as.data.frame() %>% 
  mutate(depth = Glendalough$position,
         id = Glendalough$id) %>% 
  gather(key = sample,
         value = age,
         -depth, -id) %>% 
  select(-sample) %>% 
  arrange(depth)
#depth = factor(Glendalough$position, levels = 1433:0),

p4 = Glen_samples %>% ggplot(aes(x = age, 
                            y = depth,
                            group = id)) + 
  geom_density_ridges(scale = 0.5, rel_min_height = 0.01, size = 0,
                      fill = "black") +
  #theme_ridges() +
  scale_y_reverse(expand = c(0.01, 0),
                  breaks = seq(1400,0,by= -200)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10)) + 
  labs(x = TeX('Calendar years before present'),
       y = 'Depth\n(cm)',
       title = 'Glendalough') + 
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0.5),
        legend.position = 'None')
ggsave('p4.jpg', p4, width = my_width, height = my_height)


# Next one as above but with a smooth --------------------------------------

x = Glen_CIs$`50%`
y = Glendalough$position
fit=gam(y~s(x, k=4))
#plot(fit)
newd = data.frame(x = seq(0, 14000, by = 10))
pred = predict(fit, newd, se = TRUE)
newdf = data.frame(age = newd$x,
                   mean = pred$fit,
                   low = pred$fit - 2*pred$se.fit,
                   high = pred$fit + 2*pred$se.fit,
                   id = 'Smooth')

p5 = p4 + geom_line(data = newdf, aes(x = age, y = mean))
ggsave('p5.jpg', p5, width = my_width, height = my_height)

# Next with confidence intervals ------------------------------------------

p6 = p5 + geom_line(data = newdf, aes(x = age, y = low), 
                    linetype=2) + 
  geom_line(data = newdf, aes(x = age, y = high),
            linetype=2)
ggsave('p6.jpg', p6, width = my_width, height = my_height)

# Finally proper Bchron version -------------------------------------------

# GlenChron = with(Glendalough2,
#                  Bchronology(ages=ages,
#                              ageSds=ageSds,
#                              calCurves=calCurves,
#                              positions=position,
#                              positionThicknesses = thickness,
#                              predictPositions=seq(0,1500,by=10),
#                              ids=id))
#saveRDS(GlenChron, file = 'GlenChron.rds')
GlenChron = readRDS(file = 'GlenChron.rds')

# Get summary of ages
GlenChronOut = summary(GlenChron, type = 'quantiles',
                       probs = c(0.1, 0.5, 0.9))

# Sort out for plotting
newdf = data.frame(age = GlenChronOut[,'50%'],
                   depth = GlenChron$predictPositions,
                   age_low = GlenChronOut[,'10%'],
                   age_high = GlenChronOut[,'90%'],
                   id = 'Bchron')

p7 = p4 + 
  geom_line(data = newdf, aes(x = age, y = depth)) +
  geom_line(data = newdf, aes(x = age_low, y = depth), linetype=2) + 
  geom_line(data = newdf, aes(x = age_high, y = depth),linetype=2)
ggsave('p7.jpg', p7, width = my_width, height = my_height)


# Try a version with animated chronologies --------------------------------

chrons = as.data.frame(GlenChron$thetaPredict)
chrons$sample = 1:nrow(chrons)
chrons2 = chrons %>% 
  gather(key = id,
         value = value,
         -sample) %>% 
  arrange(sample) %>% 
  mutate(depth = rep(GlenChron$predictPositions, nrow(chrons))) %>% 
  filter(sample %in% sample(1:nrow(chrons), 20))
stop()

p7a = p4 + geom_line(data = chrons2, aes(x = value, y = depth, 
                                         group = sample,
                                         colour = sample)) + 
  scale_colour_viridis() + 
  transition_states(sample,
                    transition_length = 2,
                    state_length = 1) + 
  enter_fade() +
  exit_fade()

options(gganimate.dpi = 300)
p7a_anim = animate(p7a, width = my_width*80, height = my_height*80, dpi = 300)

anim_save(filename = 'p7a.gif', animation = p7a_anim)

# Finally Sluggan ---------------------------------------------------------

data("Sluggan")

SlugganOut = with(Sluggan,
                BchronCalibrate(ages=ages,
                                ageSds=ageSds,
                                calCurves=calCurves,
                                positions=position,
                                ids=id))

# Get 95% quantiles from ages
age_samples = sampleAges(SlugganOut)

Slug_samples = age_samples %>% t() %>% as.data.frame() %>% 
  mutate(depth = Sluggan$position,
         id = Sluggan$id) %>% 
  gather(key = sample,
         value = age,
         -depth, -id) %>% 
  select(-sample) %>% 
  arrange(depth)


# SlugganChron = with(Sluggan,
#                  Bchronology(ages=ages,
#                              ageSds=ageSds,
#                              calCurves=calCurves,
#                              positions=position,
#                              positionThicknesses = thickness,
#                              jitterPositions = TRUE,
#                              predictPositions=seq(44,518,by=1),
#                              ids=id))
#saveRDS(SlugganChron, file = 'SlugganChron.rds')
SlugganChron = readRDS(file = 'SlugganChron.rds')

# Get summary of ages
SlugChronOut = summary(SlugganChron, type = 'quantiles')

# Sort out for plotting
newdf = data.frame(age = SlugChronOut[,'50%'],
                   depth = SlugganChron$predictPositions,
                   age_low = SlugChronOut[,'10%'],
                   age_high = SlugChronOut[,'90%'],
                   id = 'Bchron')

p8 = Slug_samples %>% ggplot(aes(x = age, 
                                 y = depth,
                                 group = id)) + 
  geom_density_ridges(scale = 0.8, rel_min_height = 0.01, size = 0,
                      fill = "black") +
  #theme_ridges() +
  scale_y_reverse(expand = c(0.01, 0),
                  breaks = seq(520,40,by= -40)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10)) + 
  labs(x = TeX('Calendar years before present'),
       y = 'Depth\n(cm)',
       title = 'Sluggan Moss') + 
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0.5),
        legend.position = 'None') +
  geom_line(data = newdf, aes(x = age, y = depth)) +
  geom_line(data = newdf, aes(x = age_low, y = depth), linetype=2) + 
  geom_line(data = newdf, aes(x = age_high, y = depth),linetype=2)
ggsave('p8.jpg', p8, width = my_width, height = my_height)
