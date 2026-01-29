#Code from Andy
# this part where I make N =  flow_age_my, P = 1 / flow_age_my needs
# to be changed to the correct calculation from the actual N and P columns 
np <- group_by(arth, site, tree) |>
  summarize(N = mean(flow_age_my), P = mean(1 / flow_age_my)) |>
  ungroup()

np$site <- factor(np$site, levels = c("VO", "LA", "KH",
                                      "MO", "KA"))

ggplot(np, aes(x = site, y = N)) + 
  geom_point()

ggplot(np, aes(x = site, y = P)) + 
  geom_point()

