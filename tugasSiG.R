library(sf)
library(sp)
library(spdep)
library(CARBayes)
library(tmap)
library(dplyr)


#Baca Data Kemiskinan
raw_lines <- readLines("kasus.csv") # ganti dengan nama file Anda
data_clean <- raw_lines[3:length(raw_lines)]
data_split <- strsplit(data_clean, ",")
data_matrix <- do.call(rbind, data_split)
data_kemiskinan <- as.data.frame(data_matrix, stringsAsFactors = FALSE)

data_kemiskinan <- data_kemiskinan[, c("V1", "V3")]
names(data_kemiskinan) <- c("Kabupaten_Kota", "Jumlah_Miskin")

data_kemiskinan <- data_kemiskinan %>%
  mutate(
    Kabupaten_Kota = tolower(trimws(Kabupaten_Kota)),
    Jumlah_Miskin = round(as.numeric(gsub(",", ".", Jumlah_Miskin))),
    Populasi = 100
  ) %>%
  filter(
    Kabupaten_Kota != "",
    !grepl("provinsi", Kabupaten_Kota)
  ) %>%
  mutate(
    Kabupaten_Kota = gsub("mukomuko", "muko muko", Kabupaten_Kota)
  )
#Baca Shapefile Bengkulu
shapefile_path <- "RBI_50K_2023_Bengkulu.shp"
bengkulu_sf <- st_read(shapefile_path)
bengkulu_sf <- st_make_valid(bengkulu_sf)
bengkulu_sf <- st_buffer(bengkulu_sf, 0)


bengkulu_sf$NAMOBJ <- tolower(trimws(bengkulu_sf$NAMOBJ))

#Join Data CSV ke Shapefile
bengkulu_sf <- bengkulu_sf %>%
  left_join(data_kemiskinan, by = c("NAMOBJ" = "Kabupaten_Kota"))

if (any(is.na(bengkulu_sf$Jumlah_Miskin))) {
  warning("Beberapa kabupaten tidak cocok:")
  print(bengkulu_sf$NAMOBJ[is.na(bengkulu_sf$Jumlah_Miskin)])
}

#Model Spasial: CAR Leroux
bengkulu_sp <- as_Spatial(bengkulu_sf)

nb <- poly2nb(bengkulu_sp)
W <- nb2mat(nb, style = "B", zero.policy = TRUE)
bengkulu_sp@data$kasus <- bengkulu_sp@data$Jumlah_Miskin
bengkulu_sp@data$populasi <- bengkulu_sp@data$Populasi
bengkulu_sp@data$kasus <- as.integer(round(bengkulu_sp@data$Jumlah_Miskin))

summary(bengkulu_sp@data$kasus)
str(bengkulu_sp@data$kasus)
#Jalankan Model CAR Leroux
model <- S.CARleroux(
  formula = kasus ~ offset(log(populasi)),
  family = "poisson",
  W = W,
  burnin = 1000,
  n.sample = 5000,
  data = bengkulu_sp@data
)

summary(model)


#Visualisasi Peta Risiko Relatif
bengkulu_sp@data$RR <- model$fitted.values / bengkulu_sp@data$populasi
bengkulu_sf <- st_as_sf(bengkulu_sp)

tmap_mode("plot")
tm_shape(bengkulu_sf) +
  tm_fill("RR", palette = "YlOrRd", style = "quantile", title = "Relative Risk") +
  tm_borders() +
  tm_layout(
    main.title = "Risiko Relatif Kemiskinan di Provinsi Bengkulu (Model CAR Leroux)",
    main.title.size = 1.2
  )



