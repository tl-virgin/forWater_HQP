#forWater HQP Paper - Figure 2
#Author: "Taylor Virgin"
#Updated: "2025-08-20"

#Load required libraries and fonts
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(showtext)
library(ggpattern)

font_add("Arial", "/System/Library/Fonts/Supplemental/Arial.ttf")
font_add("Arial Bold", "/System/Library/Fonts/Supplemental/Arial Bold.ttf")
showtext_auto()

#### Figure 2A: Field of study bar chart####
#Load the required data
field <- read.csv("data/field.csv")

#Order the fields from highest to lowest 
field$Field <- factor(field$Field, levels = c("Economics",
                                              "Soil Science",
                                              "Earth Science",
                                              "Ecohydrology",
                                              "Forest Science and Management",
                                              "Biology",
                                              "Geography",
                                              "Environmental Engineering"))

#Group related fields (for fill colour)
field <- field %>%
  mutate(Group = case_when(
    Field %in% c("Environmental Engineering") ~ "Engineering",
    Field %in% c("Economics") ~ "Social Sciences",
    Field %in% c("Earth Science", "Environmental Science", "Soil Science",
                 "Forest Science and Management", "Geography", 
                 "Ecohydrology") ~ "Earth, Environmental, and Natural Resource Sciences",
    Field %in% c("Biology") ~ "Biological Sciences"
  ))

#Set the order of the group and multidisciplinary levels
field$Group <- factor(field$Group, levels = c("Earth, Environmental, and Natural Resource Sciences", "Engineering",
                                              "Biological Sciences", "Social Sciences"))

field$Multidisciplinary <- factor(field$Multidisciplinary, levels = c("Yes", "No"))

#Plot the data
fieldBar <- ggplot(field, aes(x = Field, y = Count, fill = Group,
                              pattern = Multidisciplinary)) +
  geom_bar_pattern(stat="identity", position = "stack",
                   aes(pattern_fill = I("white"), pattern_color = I("white")),
                   pattern_density = 0.5,
                   pattern_spacing = 0.02,
                   pattern_angle = 45) +
  scale_y_continuous(limits = c(0, 14.5), expand = c(0.00, 0.00),
                     breaks = seq(0,14,2)) +
  labs(x = "Field of study", y = "Number of students") +
  scale_fill_manual(values = c("Engineering" = "#0a5ea6",
                               "Biological Sciences" = "#62acc8",
                               "Earth, Environmental, and Natural Resource Sciences" = "#648326",
                               "Social Sciences" = "#535353"),
                    labels = c("Engineering" = "Engineering",
                               "Biological Sciences" = "Biological Sciences",
                               "Earth, Environmental, and Natural Resource Sciences" = "Earth, Environmental, and \nNatural Resource Sciences",
                               "Social Sciences" = "Social Sciences")) +
  scale_pattern_manual(values = c("No" = "none", "Yes" = "stripe")) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text = element_text(family = "Arial", colour = "black",
                                 size = 12),
        axis.title = element_text(family = "Arial Bold", colour = "black",
                                  size = 12, lineheight = 1.2),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", colour = "black",
                                   size = 10),
        legend.position = "none")

fieldBar

#### Figure 2B: Collaboration barchart ####
#Load the required data
collaborations <- read.csv("data/collaborations.csv")

#Aggregate the data by type of collaboration
collaborations_long <- collaborations[1:5]

collaborations_long <- collaborations_long %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(),
               names_to = "Collaborator",
               values_to = "Count") %>%
  filter(Count > 0)

#Order the collaboration types from highest to lowest
collaborations_long$Collaborator <- fct_reorder(collaborations_long$Collaborator, 
                                                collaborations_long$Count,
                                                .desc = TRUE)

#Format the labels for collaboration type
collaborations_long$Collaborator <- fct_recode(collaborations_long$Collaborator,
                                               "Water\nUtility" = "Water.Utility",
                                               "Forestry\nCompany" = "Forestry.Company")

#Plot the data
collabBar <- ggplot(collaborations_long, aes(x = Collaborator, y = Count, fill = Collaborator)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0,45), expand = c(0.00,0.00),
                     breaks = seq(0,45,5)) +
  labs(x = "Type of collaborator", y = "Number of collaborations") +
  scale_fill_manual(values = c("Academic" = "#535353",
                               "Water\nUtility" = "#62acc8",
                               "Government" = "#0a5ea6",
                               "Forestry\nCompany" = "#648326",
                               "Community" = "#000000")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text = element_text(family = "Arial", colour = "black",
                                 size = 12),
        axis.title = element_text(family = "Arial Bold", colour = "black",
                                  size = 12),
        legend.position = "none")

collabBar

#Compute collaboration stats for manuscript
collab_stats <- collaborations[1:5]
collab_stats$sum <- rowSums(collab_stats)

colSums(collab_stats !=0)
