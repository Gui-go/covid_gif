# rm(list = ls())
gc()

# Packages ----------------------------------------------------------------
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packagess("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(sp)) install.packages("sp")
if(!require(st)) install.packages("st")
if(!require(zoo)) install.packages("zoo")
if(!require(gganimate)) install.packages("gganimate")
if(!require(glue)) install.packages("glue")
if(!require(magick)) install.packages("magick")

# Data1 -------------------------------------------------------------------
# covid19data <- read_csv("data/covid_cases/covid19data.csv")
covid19datasc <- covid19data %>%
  filter(state=="SC"&city_ibge_code!=42) %>%
  mutate(date = as.Date(date), city_ibge_code = as.character(city_ibge_code))
sf_file_sc <- sf::read_sf("data/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(NM_ESTADO == "SANTA CATARINA")
# sf_file_meso <- sf::read_sf("data/sc_mesorregioes/42MEE250GC_SIR.shp")
# sf_file_micro <- sf::read_sf("data/sc_microrregioes/42MIE250GC_SIR.shp")
sf_file_muni <- sf::read_sf("data/sc_municipios/42MUE250GC_SIR.shp") %>%
  mutate(CD_GEOCMU=as.character(CD_GEOCMU))

# Data2 -------------------------------------------------------------------
dd <- seq.Date(from = as.Date(min(covid19datasc$date)), to = as.Date(max(covid19datasc$date)), by = "day")
gg <- data.frame(
  "date" = rep(dd, length(unique(covid19datasc$city_ibge_code))),
  "index" = rep(1:length(dd), length(unique(covid19datasc$city_ibge_code))),
  "city_ibge_code" = as.character(rep(unique(covid19datasc$city_ibge_code), each =length(dd))),
  stringsAsFactors = F
)
ccsj <- dplyr::right_join(covid19datasc, gg, by = c("date", "city_ibge_code"))
ccsjs <- split(ccsj, ccsj$city_ibge_code)

ll2 <- lapply(ccsjs, function(x){x %>%
    mutate(
      city = na.locf(city, fromLast = T),
      confirmed = replace_na(confirmed, 0),
      confirmed_per_100k_inhabitants = replace_na(confirmed_per_100k_inhabitants, 0),
      death_rate = replace_na(death_rate, 0),
      deaths = replace_na(deaths, 0),
      estimated_population_2019 = na.locf(estimated_population_2019, fromLast = T),
      is_last = replace_na(is_last, FALSE),
      order_for_place = replace_na(order_for_place, 0),
      place_type = na.locf(place_type, fromLast = T),
      state = na.locf(state, fromLast = T),
    )
})

ll2b <- do.call("rbind", ll2)
# ll2b[, c("city", "confirmed", "index")] %>% View()
ll2b <- ll2b %>% filter(index <= 30)
# ll2b %>% filter(index == 30) %>% summarise(sum_confirmed = sum(confirmed))

# correção ----------------------------------------------------------------
# ll2b[ll2b$index%in%c(13:18), c("city", "confirmed", "confirmed_per_100k_inhabitants", "index")] %>% View()

ll2b[ll2b$city=="Balneário Camboriú"&ll2b$index==14, "confirmed"] <- ll2b[ll2b$city=="Balneário Camboriú"&ll2b$index==13, "confirmed"]
ll2b[ll2b$city=="Balneário Camboriú"&ll2b$index==15, "confirmed"] <- ll2b[ll2b$city=="Balneário Camboriú"&ll2b$index==13, "confirmed"]
ll2b[ll2b$city=="Brusque"&ll2b$index==26, "confirmed"] <- ll2b[ll2b$city=="Brusque"&ll2b$index==25, "confirmed"]
ll2b[ll2b$city=="Chapecó"&ll2b$index==14, "confirmed"] <- ll2b[ll2b$city=="Chapecó"&ll2b$index==13, "confirmed"]
ll2b[ll2b$city=="Florianópolis"&ll2b$index==14, "confirmed"] <- ll2b[ll2b$city=="Florianópolis"&ll2b$index==13, "confirmed"]
ll2b[ll2b$city=="Laguna"&ll2b$index==15, "confirmed"] <- ll2b[ll2b$city=="Laguna"&ll2b$index==14, "confirmed"]
ll2b[ll2b$city=="Palhoça"&ll2b$index==30, "confirmed"] <- ll2b[ll2b$city=="Palhoça"&ll2b$index==29, "confirmed"]
ll2b[ll2b$city=="Paulo Lopes"&ll2b$index==c(29, 30), "confirmed"] <- ll2b[ll2b$city=="Paulo Lopes"&ll2b$index==28, "confirmed"]
ll2b[ll2b$city=="Tubarão"&ll2b$index%in%14:17, "confirmed"] <- ll2b[ll2b$city=="Tubarão"&ll2b$index==13, "confirmed"]

# Geom_sf() ---------------------------------------------------------------
covidgeodatasc <- left_join(ll2b, sf_file_muni, by = c("city_ibge_code"="CD_GEOCMU")) %>%
  sf::st_as_sf(.)

datascsf <- covidgeodatasc[covidgeodatasc$confirmed>0, ]

ganim <- ggplot() +
  geom_sf(data = sf_file_sc, size = .05, fill = "darkgray") +
  geom_sf(data = datascsf, aes(fill = confirmed), size = 0) +
  theme_void()+
  transition_manual(index)+
  labs(#title = "Disseminação do covid19 em Santa Catarina",
    #subtitle = "{frame}º dia após a primeira infecção no Estado",
    caption = "",
    fill = "Número absoluto\nde casos de covid19")+
  scale_fill_continuous(guide = guide_legend(direction = "horizontal", title.position = "top"))+
  theme(legend.position=c(.3, .2), legend.key.size = unit(0.5, "cm"))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 18),
        legend.title=element_text(size=18, face = "bold"),
        legend.text=element_text(size=18, face = "bold")
  )
# ganim

epause = 8
ganimate_conf <- animate(ganim, nframes = max(covidgeodatasc$index), fps = 1, duration = max(covidgeodatasc$index)+epause, end_pause = epause)
ganimate_conf
# magick::image_write(ganimate, path="covid19sc.gif")

# geom_histogram ----------------------------------------------------------
dataschist <- ll2b[ll2b$confirmed>0, ]

top10 <- dataschist[dataschist$index==max(dataschist$index), ] %>% arrange(., desc(.$confirmed)) %>% select(city) %>% slice(10:1) %>% pull()
datascselected <- dataschist[dataschist$city%in%top10, ] %>% as.data.frame()
datascselected$city <- factor(datascselected$city, levels = top10)

ganim_hist <- ggplot(datascselected, aes(x=city, y=confirmed, fill=city, alpha=0.8)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = 40, x = 0.6, y = 40), size=7)+
  geom_text(aes(label = 80, x = 0.6, y = 80), size=7)+
  geom_text(aes(label = 160, x = 0.6, y = 160), size=7)+
  coord_flip()+
  theme_void() +
  theme(legend.position = "none")+
  transition_manual(index) +
  # ease_aes('sine-in-out')+
  labs(# title = "Evolução da disseminação do covid19 em Santa Catarina",
    # subtitle = "{frame}º dia após a primeira infecção no Estado",
    # x = "sdsd",
    y = "Casos confirmados"#,
    #caption = "Guigo"
  )+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 21, hjust = 1, face = "bold"),
        plot.subtitle = element_text(size = 21, hjust = -1)
  )
# ganim_hist

epause = 8
ganimate_hist <- animate(ganim_hist, nframes = max(covidgeodatasc$index), fps = 1, duration = max(covidgeodatasc$index)+epause, end_pause = epause)
ganimate_hist

# geom_line ---------------------------------------------------------------

datascdf <- ll2b %>%
  group_by(index) %>%
  summarise(casos_sum = sum(confirmed))

ratio <- vector(mode = "numeric", length = max(datascdf$index))
for (i in seq_along(datascdf$casos_sum)) {
  ratio[i+1] <- (round(((datascdf$casos_sum[i+1]-datascdf$casos_sum[i])/datascdf$casos_sum[i])+1, digits = 2)*100)-100
}
datascdf$ratio <- ratio[1:length(ratio)-1]

taxasd <- datascdf$ratio[-1]/100
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
media_geo_d <- gm_mean(taxasd)
(1+media_geo_d)^4 # Praticamente dobra a cada 4 dias


ganim_line <- ggplot(datascdf, aes(x = index, y = casos_sum))+
  geom_point(size=2.3)+
  geom_line()+
  geom_segment(aes(x = 7, xend = 7, y = 0, yend = 200), color = "red", size = 1) +
  # geom_text(aes(x = 5, y = 150), label = ratio)+
  geom_text(aes(x = 10, y = 550, label = "Taxa de crescimento\nno número de casos de covid19\nem Santa Catarina\nem relação ao dia anterior"), size = 7)+
  geom_text(aes(x = 10, y = 390, label = paste0(round(ratio, 2), "%")), size = 11)+
  geom_text(aes(x = 27, y = 150, label = "Total"), size = 7)+
  geom_text(aes(x = 27, y = 90, label = casos_sum), size = 11)+
  transition_reveal(index)+
  theme_void()+
  labs(#title = "Evolução da disseminação do covid19 em Santa Catarina",
    #subtitle = "{frame_along}º dia após a primeira infecção no Estado",
    # x = "Dias",
    # y = "Casos confirmados em Santa Catarina",
    x="", y="",
    caption = "")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.text.y = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15, angle = 90),
        axis.title.x.bottom = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 15)
  )

epause = 8
ganimate_line <- animate(ganim_line, nframes = max(covidgeodatasc$index), fps = 1, duration = max(covidgeodatasc$index)+epause, end_pause = epause)
ganimate_line

# geom_info ---------------------------------------------------------------

df_info <- data.frame(
  date = unique(ll2b$date),
  index = unique(ll2b$index)
)
ganim_info <- ggplot(df_info) +
  geom_text(aes(x = 5.3, y = 9.5, label = "Disseminação do covid19\nno Estado de Santa Catarina"), size = 12)+
  geom_text(aes(x = 5.3, y = 6, label = paste0("Dia ", format(date, "%d/%m/%Y"))), size = 11)+
  geom_text(aes(x = 5.3, y = 2.3, label = paste0(index, "º dia\napós a primeira infecção no Estado")), size = 10)+
  ylim(0, 10)+
  xlim(0, 10)+
  theme_void() +
  transition_reveal(index) +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "Guigo")+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(size = 21, face = "bold")
  )

epause = 8
ganimate_info <- animate(ganim_info, nframes = max(df_info$index), fps = 1, duration = max(df_info$index)+epause, end_pause = epause)
ganimate_info

# joining gifs ------------------------------------------------------------

new_gif1 <- image_append(c(ganimate_line[1], ganimate_hist[1]), stack = T)
for(i in 2:max(covidgeodatasc$index)){
  combined <- image_append(c(ganimate_line[i], ganimate_hist[i]), stack = T)
  new_gif1 <- c(new_gif1, combined)
}
new_gif1

new_gif2 <- image_append(c(ganimate_conf[1], ganimate_info[1]), stack = T)
for(i in 2:max(covidgeodatasc$index)){
  combined <- image_append(c(ganimate_conf[i], ganimate_info[i]), stack = T)
  new_gif2 <- c(new_gif2, combined)
}
new_gif2

new_gif3 <- image_append(c(new_gif1[1], new_gif2[1]), stack = F)
for(i in 2:max(covidgeodatasc$index)){
  combined <- image_append(c(new_gif1[i], new_gif2[i]), stack = F)
  new_gif3 <- c(new_gif3, combined)
}
new_gif3

# magick::image_write(new_gif3, path="covid19sc.gif")
