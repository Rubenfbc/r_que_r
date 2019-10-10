library(tidyverse)
library(babynames)

a <- babynames %>%
  select(year, sex, name, n) %>%
  filter(year==1880 | year ==2017) %>%
  arrange (desc(n)) %>%
  group_by(year,sex) %>%
  top_n(15) 

b <- a %>%
  mutate(orden = fct_reorder(name, n))

a %>%
  ggplot(aes(name, n, fill= sex)) +
  geom_col()+
  facet_wrap(sex~year, scales = "free")



babynames %>%
  group_by(year) %>%
  summarise(suma = sum(n)) %>%
  ggplot(aes(x= year, y = suma, fill = "orange"))+
  geom_col() +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  guides(fill=F)+
  labs( x= "year", 
        y= " ",
        title = "Número de aplicaciones",
        source = "US SS")

babynames %>%
  filter(year==1880 | year ==2017)%>%
  group_by(year, sex) %>%
  mutate(name_2 =fct_reorder(name, n))%>%
  top_n(15, n) %>%
  ggplot(aes(fct_reorder(name_2,n), desc(n))) +
      geom_col(fill= "#fc6721") +
      facet_wrap(sex~year, scales= "free_y") +
      coord_flip() +
  theme_minimal()+
  labs( x= "", 
        y= "Nombre",
        title = "Nombres más populares en Estados Unidos en 1880 y en 2017",
        caption =  "US Social Security Administration")

    
ggplot(hombres, aes(reorder(Nombre, Frec, sum),Frec)) +
  geom_col(fill = "#fc6721", 
           alpha = 0.8)+
  coord_flip()+
  facet_wrap(~grupos_edad, scales= "free_y")+
  theme_lab()+
  labs(title = "Nombres masculinos m?s comunes", 
       subtitle = "Organizados en categor?as seg?n su edad promedio", 
       caption = "Source: INE  |  @Ruben46563154", 
       x = "", y = "", 
       fill = NULL) + 
  theme(panel.grid.major.x = element_blank())+
  scale_y_continuous(labels=comma)+
  geom_hline(yintercept=c(200000, 400000,600000), 
             color="lightgrey",
             linetype="dashed")+
  geom_text(aes(label=Frec, y=Frec + 0.05), 
            hjust=0,
            color="grey40",
            size= 3.2)     
