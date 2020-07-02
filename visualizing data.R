shots <- read.csv(file = "shotscopy.csv", header = TRUE, stringsAsFactors = F, fill = T)





#ggplot(data = shots, mapping = aes(defender_velocity_angle, made)) + geom_point(mapping = aes(group = cut_number(defender_velocity_angle, 3)))





summaryangle <- shots %>% group_by(cut_number(shot_x, 3)) %>% summarise(tired = mean(made))
summaryangle


righthanders <- shots %>% filter(shot_x > 0)
lefthanders <- shots %>% filter(shot_x < 0)


rightsummaryangle <- righthanders %>% group_by(cut_number(defender_velocity_angle, 3)) %>% summarise(tired = mean(made))
rightsummaryangle


leftsummaryangle <- lefthanders %>% group_by(cut_number(defender_velocity_angle, 3)) %>% summarise(tired = mean(made))
leftsummaryangle

lefthanders$defender_velocity_angle <- abs(lefthanders$defender_velocity_angle)
righthanders$defender_velocity_angle <- abs(righthanders$defender_velocity_angle)

leftdefenderangle <- lefthanders %>% group_by(cut_number(defender_angle, 3)) %>% summarise(tired = mean(made))
leftdefenderangle


rightdefenderangle <- righthanders %>% group_by(cut_number(defender_angle, 3)) %>% summarise(tired = mean(made))
rightdefenderangle