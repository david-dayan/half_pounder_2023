

# pca

setEPS()
postscript(file = "manuscript/figures/pca.tiff", width = 3.307)
ggplot(data = pca_vals, aes(Axis1, Axis2, color = run))+geom_point(alpha = 0.6)+stat_ellipse()+theme_classic()+scale_color_viridis_d(name = "", labels = c("Late Summer", "Half Pounder", "Early Summer", "Winter"))+xlab("Principal Component 1\n1.4% of Variation")+ylab("Principal Component 2\n1.0% of Variation")+theme(axis.title = element_text(size = 8), legend.text = element_text(size = 8))
dev.off()


tiff("test.tiff", units="mm", width=84, height=50, res=600)

ggplot(data = pca_vals, aes(Axis1, Axis2, color = run))+geom_point(alpha = 0.6)+stat_ellipse()+theme_classic()+scale_color_viridis_d(name = "")+xlab("Principal Component 1\n1.4% of Variation")+ylab("Principal Component 2\n1.0% of Variation")+theme(axis.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8))
ggsave("manuscript/figures/pca.tiff", units="mm", width=120, height=80, device="tiff", dpi = 600)

ggsave("manuscript/figures/pca.eps", units="mm", width=120, height=80, device="cairo", fallback_resolution = 600)
ggsave("manuscript/figures/Fig1.pdf", units="mm", width=120, height=80, device="pdf")


cairo_ps(file = "test.eps", fallback_resolution = 600, width=5, height=4)
ggplot(data = pca_vals, aes(Axis1, Axis2, color = run))+geom_point(alpha = 0.6)+stat_ellipse()+theme_classic()+scale_color_viridis_d(name = "")+xlab("Principal Component 1\n1.4% of Variation")+ylab("Principal Component 2\n1.0% of Variation")+theme(axis.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8))
dev.off()

## fig3
sampled_ind_40 <- k2 %>%
  rownames_to_column(var="id") %>% 
  group_by(pop) %>%
  sample_n(40) %>% 
  pull(id)

plot_data <- k2 %>% 
  rownames_to_column(var="id") %>% 
  filter(id %in% sampled_ind_40) %>%# sample the half_pounder and fall fish to smaller size for plot
  gather('cluster', 'prob', clust1:clust2) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()

a <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_blank()) +
  scale_fill_manual(values = viridisLite::viridis(2))

plot_data <- k3 %>% 
  rownames_to_column(var="id") %>% 
  filter(id %in% sampled_ind_40) %>%
  gather('cluster', 'prob', clust1:clust3) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()

b <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_blank()) +
  scale_fill_manual(values = viridisLite::viridis(3))

plot_data <- k4 %>% 
  rownames_to_column(var="id") %>% 
  filter(id %in% sampled_ind_40) %>%
  gather('cluster', 'prob', clust1:clust4) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()

c <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x", labeller = labeller( pop =  c("fall" ="Late\nSummer",  "halfpounder"= "Half\nPounder", "summer" =  "Early\nSummer", "winter" =  "Winter"))) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_text(size = 10, angle = 90)) +
  scale_fill_manual(values = viridisLite::viridis(4))
cowplot::plot_grid(a,b,c, ncol=1, rel_heights  = c(1,1,1.6))
ggsave("manuscript/figures/Fig3.pdf", units="mm", width=84, height=100, device="pdf")


# fig4
ggplot(data=ld1)+geom_histogram(aes(x = LD1, y = ifelse(after_stat(count) > 0, after_stat(count), NA), fill=pop, color=pop), alpha = 0.9, bins = 50)+scale_color_viridis_d(labels = c("Late Summer", "Half Pounder", "Early Summer", "Winter"))+scale_fill_viridis_d(labels = c("Late Summer", "Half Pounder", "Early Summer", "Winter"))+theme_classic()+labs(color = "", fill = "")+ylab("Frequency")+xlab("LD1 Score")+theme(axis.title = element_text(size = 8), legend.text = element_text(size = 8))#+geom_vline(aes(xintercept = -30))+geom_vline(aes(xintercept = 30.5))  
ggsave("manuscript/figures/Fig4.pdf", units="mm", width=120, height=70, device="pdf")

# cline
ggplot(pred.mm) + 
  geom_line(aes(x = LD1, y = fit)) +          # slope
  geom_ribbon(aes(x = LD1, ymin = lower, ymax = upper), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_jitter(data = filter(cor_data, run =="fall"),                      # adding the raw data (scaled values)
              aes(x = LD1, y = jdate, fill = year.x), shape = 21, alpha = 0.7) + 
  labs(x = "LD1 Score\n(negative is more similar to late migrator)", y = "Julian Day of Sampling") + 
  theme_classic()+geom_rect(aes(xmin = 30.1, xmax=31.5, ymin = -Inf, ymax = Inf), fill = "#35B779FF", alpha = 0.01)+geom_rect(aes(xmin = -35, xmax=-29.5, ymin = -Inf, ymax = Inf), fill = "#FDE725FF", alpha = 0.01)+ scale_fill_manual(name = "",values = c("white", "black"))+scale_shape(solid = FALSE)+ theme(text = element_text(size = 8))+xlim(-35, 31.5) +
  theme(legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"))# note madethe boxes a little bigger to accomodate the width of the points in the figure at rendered scale, Allowing points to fall fully within the boxes will likely be easier to interpret
ggsave("manuscript/figures/Fig5.pdf", units="mm", width=120, height=80, device="pdf")

# fig 6
repol <- all_maf %>%
  mutate(fall = ifelse(marker == "Omy_GREB1_05", (1 - fall), fall)) %>%
  mutate(halfpounder = ifelse(marker == "Omy_GREB1_05", (1 - halfpounder), halfpounder)) %>%
  mutate(Summer = ifelse(marker == "Omy_GREB1_05", (1 - Summer), Summer)) %>%
  mutate(Winter = ifelse(marker == "Omy_GREB1_05", (1 - Winter), Winter)) #%>%
#  mutate(fall = ifelse(marker == "Omy_RAD47080-54", (1 - fall), fall)) %>%
#  mutate(halfpounder = ifelse(marker == "Omy_RAD47080-54", (1 - halfpounder), halfpounder)) %>%
#  mutate(Summer = ifelse(marker == "Omy_RAD47080-54", (1 - Summer), Summer)) %>%
#  mutate(Winter = ifelse(marker == "Omy_RAD47080-54", (1 - Winter), Winter))

repol <- repol %>%
  select(fall, halfpounder, Winter, Summer, marker) %>%
  rename("Late Summer" = fall, "Half Pounder" = halfpounder, "Early Summer" = Summer)

sunset <- colour("sunset")
col_pal <- sunset(128)
tmat <- t(as.matrix(repol[,1:4]))
colnames(tmat) <- repol$marker
pheatmap(tmat, col_pal, treeheight_row = 20, treeheight_col = 20, fontsize = 8)

#supp 1

plot_data <- k2 %>% 
  rownames_to_column(var="id") %>% 
  # sample the half_pounder and fall fish to smaller size for plot
  gather('cluster', 'prob', clust1:clust2) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()


a <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_blank()) +
  scale_fill_manual(values = viridisLite::viridis(2))

plot_data <- k3 %>% 
  rownames_to_column(var="id") %>% 
  gather('cluster', 'prob', clust1:clust3) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()

b <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_blank()) +
  scale_fill_manual(values = viridisLite::viridis(3))

plot_data <- k4 %>% 
  rownames_to_column(var="id") %>% 
  gather('cluster', 'prob', clust1:clust4) %>%
  group_by(id) %>% 
  mutate(likely_assignment = cluster[which.max(prob)],
         assingment_prob = max(prob)) %>% 
  arrange(likely_assignment, desc(assingment_prob)) %>% 
  ungroup()

c <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
  geom_col(width=1.0) +
  facet_grid(~pop, scales = 'free', space = 'free', switch = "x", labeller = labeller( pop =  c("fall" ="Late\nSummer",  "halfpounder"= "Half\nPounder", "summer" =  "Early\nSummer", "winter" =  "Winter"))) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = viridisLite::viridis(4))

# plot_data <- k5 %>% 
#   rownames_to_column(var="id") %>% 
#   gather('cluster', 'prob', clust1:clust5) %>%
#   group_by(id) %>% 
#   mutate(likely_assignment = cluster[which.max(prob)],
#          assingment_prob = max(prob)) %>% 
#   arrange(likely_assignment, desc(assingment_prob)) %>% 
#   ungroup()
# 
# d <- ggplot(plot_data, aes(id, prob, fill = cluster)) +
#   geom_col(width=1.0) +
#   facet_grid(~pop, scales = 'free', space = 'free', switch = "x", labeller = labeller( pop =  c("fall" ="Late-Summer",  "halfpounder"= "Half-Pounder", "summer" =  "Early-Summer", "winter" =  "Winter"))) +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_x_discrete(expand = expand_scale(add = 1)) +
#   theme(panel.spacing=unit(0.1, "lines"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = "none", axis.title.y=element_blank(), strip.background = element_rect(color = "white", fill = "white"), strip.text.x = element_text(angle = 90)) +
#   scale_fill_manual(values = viridisLite::viridis(5))


cowplot::plot_grid(a,b,c, rel_heights = c(1,1,1.6) ,ncol=1)
ggsave("manuscript/figures/SupFig1.pdf", units="mm", width=174, height=100, device="pdf")

# sup fig2

#plot
a <- ggplot(data = filter(ldreport_sw ))+geom_tile(aes(marker_1, marker_2, fill = R2))+scale_fill_viridis_c(option = "C")+theme_classic()+theme(axis.text.x = element_text(angle = 90))+ggtitle("Early-Summer\nand Winter Run")+coord_equal()+theme(text = element_text(size = 8))

b <- ggplot(data = filter(ldreport_hf ))+geom_tile(aes(marker_1, marker_2, fill = R2))+scale_fill_viridis_c(option = "C")+theme_classic()+theme(axis.text.x = element_text(angle = 90))+ggtitle("Late-Summer Run \nand Half Pounders")+coord_equal()+theme(text = element_text(size = 8))

plot_grid(a,b,ncol=2,labels="auto")
ggsave("manuscript/figures/SupFig2.pdf", units="mm", width=174, height=100, device="pdf")

