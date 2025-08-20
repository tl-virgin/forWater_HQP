#forWater HQP Paper - Figure 1A
#Author: "Taylor Virgin"
#Updated: "2025-08-20"

#Load required libraries and fonts
library(ggplot2)
library(dplyr)
library(showtext)
library(canadianmaps)
library(sf)

font_add("Arial", "/System/Library/Fonts/Supplemental/Arial.ttf")
font_add("Arial Bold", "/System/Library/Fonts/Supplemental/Arial Bold.ttf")
showtext_auto()

#### Figure 1A: Map of Canada ####
#Load the required data
canada <- select(PROV, PT, PRENAME, rmapshaperid, X, Y, geometry)
universities_raw <- read.csv("data/university_coords.csv", header = TRUE)

#Transform the university coordinates to fit the map
universities <- coord_transform(universities_raw, "Longitude", "Latitude")
universities <- coord_transform(universities, "LabelLong", "LabelLat")

#Calculate and manually adjust centroids for province labels as needed
sf_use_s2(FALSE)
centroids_raw <- as.data.frame(st_coordinates(st_centroid(canada)))

#Adjust label: Northwest Territories
centroids_raw$X[10] = -120.85426
centroids_raw$Y[10] = 65.00000

#Adjust label: Nunavut
centroids_raw$X[3] = -97.00000
centroids_raw$Y[3] = 65.00000

#Adjust label: Newfoundland
centroids_raw$X[12] = -62.00000
centroids_raw$Y[12] = 54.00000

#Adjust label: Prince Edward Island
centroids_raw$X[4] = -62.82618
centroids_raw$Y[4] = 48.00000

#Adjust label: Nova Scotia
centroids_raw$X[13] = -65.83116
centroids_raw$Y[13] = 42.30000

#Transform centroids to fit on the map and add them to the main dataframe
centroids <- coord_transform(centroids_raw, "X", "Y")

canada$centroidX <- (centroids$coords.x1)
canada$centroidY <- (centroids$coords.x2)

#Plot the map
map_colour <- rep("#DADADA", 13)

map <- ggplot(data = canada) +
  geom_prov(colour = "black", size = 0.2) +
  crs_coord() +
  geom_point(data = universities, aes(x = coords.x1, y = coords.x2)) +
  geom_text(aes(label = PT, x = centroidX, y = centroidY), size = 2) +
  annotate("segment", x = 2077662.2, xend = 2095651.7, 
           y = 6900403, yend = 6773952, linewidth = 0.7) +
  annotate("segment", x = 2125984.2, xend = 2095651.7, 
           y = 6304638, yend = 6473952, linewidth = 0.7) +
  scale_fill_manual(values = map_colour) +
  scale_x_continuous(limits = c(-5000000, 5000000)) +
  scale_y_continuous(limits = c(5000000, 12000000)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Arial"))

#Add labels for the universities
#showtext_auto() required to display Arial font
#If you receive an error, ensure that Arial is installed using View(system_fonts())
labelled_map <- map + annotate("segment", x = -1408767.7, xend = -1508057.6,
                               y = 7330835, yend = 6518559) +
  annotate("text", x = -1508057.6, y = 6468559, label = "University of Alberta*",
           size = 2, family = "Arial") + 
  annotate("segment", x = 905871.0, xend = 502333.3,
           y = 6041457, yend = 6291457) +
  annotate("text", x = 502333.3, y = 6341457, label = "University of Waterloo*", 
           size = 2, hjust = 1, family = "Arial") + 
  annotate("segment", x = -2212521.8, xend = -2526379.2,
           y = 7177566, yend = 6852127) +
  annotate("text", x = -2226379.2, y = 6802127, label = "University of British Columbia", 
           size = 2, hjust = 1, family = "Arial") +
  annotate("segment", x = -2263907.7, xend = -2529407.1,
           y = 7192896, yend = 7230245) +
  annotate("text", x = -2529407.1, y = 7230245, label = "Vancouver Island University",
           size = 2, hjust = 1, family = "Arial") +
  annotate("segment", x = 1068774.9, xend = 2079103.4,
           y = 6175441, yend = 6037458) +
  annotate("text", x = 2119103.4, y = 6037458, label = "Trent University",
           size = 2, hjust = 0, family = "Arial") +
  annotate("segment", x = 961346.3, xend = 987570.0,
           y = 6027481, yend = 5515313) +
  annotate("text", x = 987570.0, y = 5435313, label = "McMaster University",
           size = 2, family = "Arial") +
  annotate("segment", x = 1019221.4, xend = 1107414.8,
           y = 6021497, yend = 5704585) +
  annotate("text", x = 1047414.8, y = 5644585, label = "Brock University",
           size = 2, hjust = 0, family = "Arial") +
  annotate("segment", x = 2186920.9, xend = 2645865.3, 
           y = 6583705, yend = 6714379) +
  annotate("text", x = 2695865.3, y = 6714379, label = "Dalhousie University",
           size = 2, hjust = 0, family = "Arial")

labelled_map

#Inkscape was used to paste the individual donut plots onto the map in Figure 1.
#Inkscape was also used to manually adjust the university labels for improved visibility,

#### Figure 1A: Donut plots ####
#Load the required data
school <- read.csv("data/school.csv")

#Count the number of students in each university/degree level pair
school_count <- school %>%
  group_by(School) %>%
  mutate(SchoolTotal = sum(Count, na.rm = TRUE)) %>%
  ungroup

#See https://r-graph-gallery.com/128-ring-or-donut-plot.html
#Compute the percentage of students in each degree level per school
school_count$Percentage <- school_count$Count/school_count$SchoolTotal

#Compute the cumulative percentages/top of rectangle
school_count <- school_count %>%
  group_by(School) %>%
  arrange(School, Degree.Level) %>%
  mutate(CumulativePercentage = cumsum(Percentage))

#Compute the bottom of the rectangle
school_count <- school_count %>%
  group_by(School) %>%
  arrange(School, CumulativePercentage) %>%
  mutate(ymin = c(0, head(CumulativePercentage, n=-1)))

#Plot the data
donuts <- ggplot(school_count, aes(ymax=CumulativePercentage, ymin= ymin,
                                   xmax = 10, xmin = 9, fill = Degree.Level)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(8, 10)) +
  scale_fill_manual(values = c("Master's Student" = "#0a5ea6",
                               "PhD Student" = "#62acc8",
                               "Research Staff" = "#648326",
                               "Undergraduate Student" = "#535353")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),
        strip.text = element_text(hjust = 0.5, vjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black")) +
  facet_wrap(~School) +
  geom_text(aes(x = 8, y = 0.75, label = SchoolTotal),
            size = 6, fontface = "bold", family = "Arial")

donuts

#Inkscape was used to paste the individual donut plots onto the map in Figure 1.
