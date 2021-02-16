
Need to check Little Salmon



library(usethis)


usethis::create_github_token()


library(tidyr)


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
  # select(-c('Group'))%>%
  glimpse()

glimpse(dat.new)


dat.new2<-dat.new%>%
  select(-c('algal.cover'))%>%
  mutate(size=as.numeric(as.character(unlist(size))))%>%
  mutate(size=ceiling(size/5)*5)%>%
  mutate(count=1)%>%
  group_by(Group,site,status,replicate,size)%>%
  dplyr::summarise(count=sum(count), complexity=max(complexity),depth=mean(depth))%>%
  pivot_wider(names_from = size, values_from = count)%>%
  mutate_at(vars("70":"110"),  replace_na, '0')%>%
rename_at(vars("70":"110"),funs(paste0("x", .)))%>%
  select(-c('x0'))%>%
  mutate_at(vars("x70":"x110"), as.numeric)%>%
  group_by(Group,site,status,replicate)%>%
  dplyr::summarise(complexity=max(complexity),depth=mean(depth),
                   x20=sum(x20),
                   x30=sum(x30),
                   x40=sum(x40),
                   x45=sum(x45),
                   x50=sum(x50),
                   x55=sum(x55),
                   x60=sum(x60),
                   x65=sum(x65),
                   x70=sum(x70),
                   x75=sum(x75),
                   x80=sum(x80),
                   x85=sum(x85),
                   x80=sum(x80),
                   x100=sum(x100),
                   x110=sum(x110),

                )%>%
  mutate(Group=as.character(unlist(Group)))%>%
  glimpse()

names(dat.new2)

setwd(data.dir)
dir()
write.csv(dat.new2,"dat.new2.csv")


