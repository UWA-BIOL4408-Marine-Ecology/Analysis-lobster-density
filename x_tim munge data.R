

url <- "https://docs.google.com/spreadsheets/d/1Wqn7m2jopx11n5fdl9MHZAujRVkjBusdmIHq_gMhf0A"

# Then we can read it in
options(httr_oob_default=TRUE) # to allow access to googlesheets

dat<-read_sheet(url, sheet = "lobster.density")%>%
  as_tibble()%>%
  select(-c('longitude','latitude','time','way.point','gps','group'))%>%
  glimpse()

glimpse(dat)


url2 <- "https://docs.google.com/spreadsheets/d/1FTCSZq32Tct1AsVX9F2tuJy_cni8SQSL2UyRNmunP5s"

dat.new<-read_sheet(url2, sheet = "Sheet1")%>%
  as_tibble()%>%
  # select(-c('longitude','latitude','time','way.point','gps','group'))%>%
  glimpse()

glimpse(dat.new)


dat.new2<-dat.new%>%
  mutate(size=as.numeric(as.character(unlist(size))))%>%
  mutate(Group=as.numeric(as.character(unlist(Group))))%>%
  mutate(size=ceiling(size/5)*5)%>%
  mutate(count=1)%>%
  # pivot_wider(names_from = size, values_from = WRL)%>%
  glimpse()

summarise





