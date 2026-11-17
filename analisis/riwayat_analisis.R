# =====================================================================
# GALERI VISUALISASI LAHAN PERTANIAN KLATEN 2017-2024 (gaya A / B / C)
#   A = tren & perbandingan   B = sebaran & korelasi   C = peta
# Semua dihitung dari shapefile -> tidak bergantung nama kolom CSV
# =====================================================================
# ---- 0. PAKET --------------------------------------------------------
pkgs <- c("tidyverse", "sf", "terra", "scales", "janitor")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))
sf_use_s2(FALSE)
# ---- 1. PENGATURAN ---------------------------------------------------
setwd("C:/Users/NAMA_KAMU/Downloads/new 2-20261116T140128Z-1-001")  # GANTI
# ---- 1. PENGATURAN ---------------------------------------------------
setwd("C:/Users/Asus/Downloads/new 2-20261116T140128Z-1-001")  # GANTI
tahun   <- 2017:2024
crs_utm <- 32749                       # UTM 49S (Klaten)
dir.create("output_plot", showWarnings = FALSE)
tema <- theme_minimal(base_size = 12) +
theme(plot.title = element_text(face = "bold"),
panel.grid.minor = element_blank())
simpan <- function(p, nama, w = 10, h = 6)
ggsave(file.path("output_plot", nama), p, width = w, height = h, dpi = 300, bg = "white")
ha <- function(g) as.numeric(sum(st_area(g))) / 1e4
# ---- 2. BACA SHAPEFILE -----------------------------------------------
baca <- function(th) {
g <- st_read(sprintf("Klaten_Pertanian_%d.shp", th), quiet = TRUE) |>
st_make_valid() |> st_transform(crs_utm) |> st_geometry()
st_sf(geometry = g) |> mutate(ha = as.numeric(st_area(geometry)) / 1e4)
}
shp <- map(set_names(tahun), baca)                       # poligon asli
u   <- map(shp, \(x) st_union(x) |> st_make_valid())     # hasil dissolve
nm  <- as.character(tahun)
# ---- 3. SERI A : TREN & PERBANDINGAN ---------------------------------
luas_tahun <- tibble(tahun = tahun, luas_ha = map_dbl(u, ha))
print(luas_tahun)
# A1 tren luas
A1 <- ggplot(luas_tahun, aes(tahun, luas_ha)) +
geom_area(fill = "#4C9F70", alpha = .25) +
geom_line(color = "#1B4332", linewidth = 1.1) +
geom_point(color = "#1B4332", size = 3) +
geom_text(aes(label = comma(luas_ha, accuracy = 1)), vjust = -1, size = 3.2) +
scale_x_continuous(breaks = tahun) +
scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
labs(title = "A1. Tren Luas Lahan Pertanian Klaten 2017-2024",
x = "Tahun", y = "Luas (ha)") + tema
simpan(A1, "A1_tren_luas_pertanian.png")
# A2 laju perubahan tahunan (%)
laju <- luas_tahun |> mutate(laju = (luas_ha / lag(luas_ha) - 1) * 100) |> filter(!is.na(laju))
A2 <- ggplot(laju, aes(factor(tahun), laju, fill = laju >= 0)) +
geom_col(width = .7, show.legend = FALSE) + geom_hline(yintercept = 0) +
geom_text(aes(label = sprintf("%.2f%%", laju), vjust = ifelse(laju >= 0, -.5, 1.4)), size = 3.3) +
scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
scale_y_continuous(expand = expansion(mult = .2)) +
labs(title = "A2. Laju Perubahan Luas Pertanian Tahunan",
x = "Tahun", y = "Perubahan terhadap tahun sebelumnya (%)") + tema
simpan(A2, "A2_laju_perubahan_tahunan.png")
# A3 indeks (2017 = 100)
A3 <- luas_tahun |> mutate(indeks = luas_ha / first(luas_ha) * 100) |>
ggplot(aes(tahun, indeks)) +
geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
geom_line(color = "#264653", linewidth = 1.1) + geom_point(size = 3, color = "#264653") +
scale_x_continuous(breaks = tahun) +
labs(title = "A3. Indeks Luas Pertanian (2017 = 100)", x = "Tahun", y = "Indeks") + tema
simpan(A3, "A3_indeks_luas.png")
# A4 perubahan kumulatif terhadap 2017
A4 <- luas_tahun |> mutate(delta = luas_ha - first(luas_ha)) |>
ggplot(aes(factor(tahun), delta, fill = delta >= 0)) +
geom_col(width = .7, show.legend = FALSE) + geom_hline(yintercept = 0) +
scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
scale_y_continuous(labels = comma) +
labs(title = "A4. Perubahan Kumulatif Luas terhadap 2017",
x = "Tahun", y = "Selisih (ha)") + tema
simpan(A4, "A4_perubahan_kumulatif.png")
# A5 bertambah vs berkurang per periode (dari overlay vektor)
gl <- map_dfr(tahun[-length(tahun)], function(t) {
a <- u[[as.character(t)]]; b <- u[[as.character(t + 1)]]
tibble(periode = paste0(t, "-", t + 1),
Bertambah = ha(st_difference(b, a)),
Berkurang = -ha(st_difference(a, b)))
}) |> pivot_longer(-periode, names_to = "jenis", values_to = "ha")
A5 <- ggplot(gl, aes(periode, ha, fill = jenis)) +
geom_col(width = .7) + geom_hline(yintercept = 0) +
scale_fill_manual(values = c(Bertambah = "#2A9D8F", Berkurang = "#E76F51")) +
scale_y_continuous(labels = comma) +
labs(title = "A5. Lahan Pertanian Bertambah vs Berkurang per Periode",
x = "Periode", y = "Luas (ha)", fill = NULL) + tema +
theme(axis.text.x = element_text(angle = 30, hjust = 1))
simpan(A5, "A5_gain_loss_per_periode.png")
# A6 heatmap overlap antar tahun (% luas tahun baris yang masih ada di tahun kolom)
ov <- expand_grid(i = nm, j = nm) |>
mutate(pct = map2_dbl(i, j, \(i, j) ha(st_intersection(u[[i]], u[[j]])) / ha(u[[i]]) * 100))
A6 <- ggplot(ov, aes(j, fct_rev(i), fill = pct)) +
geom_tile(color = "white") +
geom_text(aes(label = sprintf("%.0f", pct)), size = 3.2) +
scale_fill_gradient(low = "#E9F5EC", high = "#1B4332", name = "% overlap") +
labs(title = "A6. Heatmap Overlap Lahan Pertanian Antar Tahun",
x = "Tahun pembanding", y = "Tahun acuan") + tema
simpan(A6, "A6_heatmap_overlap.png", 8, 6.5)
# ---- 4. SERI B : SEBARAN & KORELASI ----------------------------------
poly_all <- map_dfr(nm, \(n) shp[[n]] |> st_drop_geometry() |> mutate(tahun = n))
# B1 sebaran ukuran poligon
B1 <- ggplot(poly_all, aes(tahun, ha, fill = tahun)) +
geom_boxplot(outlier.alpha = .15, show.legend = FALSE) +
scale_y_log10(labels = comma) +
scale_fill_viridis_d(option = "D", end = .9) +
labs(title = "B1. Sebaran Ukuran Poligon Pertanian per Tahun",
x = "Tahun", y = "Luas poligon (ha, skala log)") + tema
simpan(B1, "B1_sebaran_ukuran_poligon.png")
# B2 jumlah poligon & rata-rata luas
B2 <- poly_all |> group_by(tahun) |>
summarise(jumlah = n(), rata_ha = mean(ha)) |>
pivot_longer(-tahun) |>
mutate(name = recode(name, jumlah = "Jumlah poligon", rata_ha = "Rata-rata luas (ha)")) |>
ggplot(aes(tahun, value, group = 1)) +
geom_col(fill = "#8AB17D", width = .7) +
facet_wrap(~name, scales = "free_y") +
scale_y_continuous(labels = comma) +
labs(title = "B2. Jumlah dan Rata-rata Luas Poligon", x = "Tahun", y = NULL) + tema
simpan(B2, "B2_jumlah_dan_rata_luas.png", 10, 5)
# ---- 5. RASTER KEPADATAN (untuk B3 & seri C) -------------------------
bb  <- st_bbox(map(u, st_bbox) |> map(\(b) st_as_sfc(b, crs = crs_utm)) |> do.call(c, args = _))
tpl <- rast(ext(unname(bb["xmin"]), unname(bb["xmax"]), unname(bb["ymin"]), unname(bb["ymax"])),
resolution = 100, crs = paste0("EPSG:", crs_utm))
bin <- map(nm, \(n) rasterize(vect(st_sf(geometry = u[[n]])), tpl, field = 1, background = 0))
names(bin) <- nm
stk100 <- rast(bin); names(stk100) <- paste0("y", nm)
stk1km <- aggregate(stk100, fact = 10, fun = "mean", na.rm = TRUE)   # fraksi per 1 km
# B3 korelasi kepadatan antar tahun
cm <- cor(as.data.frame(stk1km), use = "pairwise.complete.obs")
cm_l <- as_tibble(cm, rownames = "i") |> pivot_longer(-i, names_to = "j", values_to = "r") |>
mutate(across(c(i, j), \(x) str_remove(x, "y")))
B3 <- ggplot(cm_l, aes(j, fct_rev(i), fill = r)) +
geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
scale_fill_gradient2(low = "#E76F51", mid = "white", high = "#1B4332", midpoint = .5,
limits = c(min(cm_l$r), 1), name = "r") +
labs(title = "B3. Korelasi Kepadatan Pertanian Antar Tahun (grid 1 km)",
x = NULL, y = NULL) + tema
simpan(B3, "B3_korelasi_antar_tahun.png", 8, 6.5)
# ---- 6. SERI C : PETA ------------------------------------------------
# C1 peta sebaran per tahun (facet)
u_sf <- map_dfr(nm, \(n) st_sf(tahun = n, geometry = u[[n]]))
C1 <- ggplot(u_sf) + geom_sf(fill = "#4C9F70", color = NA) +
facet_wrap(~tahun, ncol = 4) +
labs(title = "C1. Peta Sebaran Lahan Pertanian Klaten 2017-2024") +
theme_void(base_size = 12) +
theme(plot.title = element_text(face = "bold", hjust = .5), strip.text = element_text(face = "bold"))
simpan(C1, "C1_peta_pertanian_per_tahun.png", 12, 7)
# C2 peta perubahan 2017 -> 2024 (tetap / bertambah / berkurang)
a <- u[["2017"]]; b <- u[["2024"]]
ekstrak <- \(g) st_collection_extract(g, "POLYGON")
chg <- bind_rows(
st_sf(kelas = "Tetap",     geometry = ekstrak(st_intersection(a, b))),
st_sf(kelas = "Bertambah", geometry = ekstrak(st_difference(b, a))),
st_sf(kelas = "Berkurang", geometry = ekstrak(st_difference(a, b))))
C2 <- ggplot(chg) + geom_sf(aes(fill = kelas), color = NA) +
scale_fill_manual(values = c(Tetap = "#A8DADC", Bertambah = "#2A9D8F", Berkurang = "#E76F51")) +
labs(title = "C2. Peta Perubahan Lahan Pertanian 2017-2024", fill = NULL) +
theme_void(base_size = 12) + theme(plot.title = element_text(face = "bold", hjust = .5))
simpan(C2, "C2_peta_perubahan_2017_2024.png", 8, 8)
# C3 komposisi kelas perubahan
komp <- chg |> mutate(ha = as.numeric(st_area(geometry)) / 1e4) |>
st_drop_geometry() |> group_by(kelas) |> summarise(ha = sum(ha))
C3 <- ggplot(komp, aes(reorder(kelas, ha), ha, fill = kelas)) +
geom_col(show.legend = FALSE, width = .65) + coord_flip() +
geom_text(aes(label = comma(ha, accuracy = 1)), hjust = -.1, size = 3.5) +
scale_fill_manual(values = c(Tetap = "#A8DADC", Bertambah = "#2A9D8F", Berkurang = "#E76F51")) +
scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
labs(title = "C3. Komposisi Kelas Perubahan 2017-2024", x = NULL, y = "Luas (ha)") + tema
simpan(C3, "C3_komposisi_perubahan.png", 8, 4)
# C4 peta kepadatan pertanian 2024 (grid 1 km)
df24 <- as.data.frame(stk1km[["y2024"]], xy = TRUE) |> setNames(c("x", "y", "frak"))
C4 <- ggplot(df24, aes(x, y, fill = frak)) + geom_raster() + coord_equal() +
scale_fill_viridis_c(option = "G", direction = -1, labels = percent, name = "Tutupan") +
labs(title = "C4. Kepadatan Lahan Pertanian 2024 (grid 1 km)") + theme_void(base_size = 12) +
theme(plot.title = element_text(face = "bold", hjust = .5))
simpan(C4, "C4_peta_kepadatan_2024.png", 8, 8)
# C5 peta selisih kepadatan 2024 - 2017
dd <- as.data.frame(stk1km[["y2024"]] - stk1km[["y2017"]], xy = TRUE) |> setNames(c("x", "y", "d"))
C5 <- ggplot(dd, aes(x, y, fill = d)) + geom_raster() + coord_equal() +
scale_fill_gradient2(low = "#E76F51", mid = "white", high = "#2A9D8F", midpoint = 0,
labels = percent, name = "Selisih") +
labs(title = "C5. Perubahan Kepadatan Pertanian 2017-2024") + theme_void(base_size = 12) +
theme(plot.title = element_text(face = "bold", hjust = .5))
simpan(C5, "C5_peta_selisih_kepadatan.png", 8, 8)
# C6 peta frekuensi: berapa tahun sebuah lokasi berstatus pertanian (0-8)
frek <- as.data.frame(sum(stk100), xy = TRUE) |> setNames(c("x", "y", "n"))
C6 <- ggplot(frek, aes(x, y, fill = factor(n))) + geom_raster() + coord_equal() +
scale_fill_viridis_d(option = "D", direction = -1, name = "Jumlah tahun") +
labs(title = "C6. Konsistensi Lahan Pertanian (jumlah tahun terdeteksi, 2017-2024)") +
theme_void(base_size = 12) + theme(plot.title = element_text(face = "bold", hjust = .5))
simpan(C6, "C6_peta_konsistensi.png", 9, 8)
# C7 citra Sentinel-2 RGB per tahun
png("output_plot/C7_sentinel2_rgb_per_tahun.png", width = 4000, height = 2200, res = 250)
par(mfrow = c(2, 4), mar = c(1, 1, 2, 1))
for (th in tahun) {
r <- rast(sprintf("Klaten_Sentinel2_RGB_10m_%d.tif", th))
plotRGB(r, r = 1, g = 2, b = 3, stretch = "lin", axes = FALSE, main = paste("Sentinel-2", th))
}
dev.off()
# C8 overlay poligon pertanian 2024 di atas citra 2024
r24 <- rast("Klaten_Sentinel2_RGB_10m_2024.tif")
png("output_plot/C8_overlay_citra_2024.png", width = 3000, height = 3000, res = 250)
plotRGB(r24, r = 1, g = 2, b = 3, stretch = "lin", axes = FALSE,
main = "C8. Poligon Pertanian 2024 di atas Citra Sentinel-2")
plot(vect(st_transform(st_sf(geometry = u[["2024"]]), crs(r24))), add = TRUE, border = "yellow", lwd = .6)
dev.off()
# ---- 7. (OPSIONAL) CEK CSV -------------------------------------------
luas_csv <- read_csv("Klaten_Luas_Pertanian_2017_2024_DARI_VEKTOR.csv", show_col_types = FALSE) |> clean_names()
glimpse(luas_csv)   # bandingkan dengan 'luas_tahun' hasil hitungan di atas
message("Selesai. Semua figure ada di folder 'output_plot'.")
# =====================================================================
# ANALISIS LAHAN PERTANIAN KLATEN 2017-2024
# Sumber data: shapefile (SHP), citra Sentinel-2 RGB (TIF), tabel luas (XLSX/CSV)
# =====================================================================
# ---- 0. PAKET --------------------------------------------------------
pkgs <- c("tidyverse", "sf", "terra", "scales", "janitor", "readxl")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))
sf_use_s2(FALSE)
# ---- 1. PENGATURAN ---------------------------------------------------
setwd("C:/Users/Asus/Downloads/new 2-20261116T140128Z-1-001")
tahun   <- 2017:2024
nm      <- as.character(tahun)
crs_utm <- 32749                                  # UTM 49S (Klaten)
dir.create("output_plot", showWarnings = FALSE)
tema <- theme_minimal(base_size = 12) +
theme(plot.title = element_text(face = "bold"),
panel.grid.minor = element_blank())
tema_peta <- theme_void(base_size = 12) +
theme(plot.title = element_text(face = "bold", hjust = .5),
strip.text = element_text(face = "bold"))
simpan <- function(p, nama, w = 10, h = 6)
ggsave(file.path("output_plot", nama), p, width = w, height = h, dpi = 300, bg = "white")
ha <- function(g) as.numeric(sum(st_area(g))) / 1e4
# ---- 2. BACA DATA ----------------------------------------------------
## 2a. Shapefile pertanian per tahun
baca <- function(th) {
g <- st_read(sprintf("Klaten_Pertanian_%d.shp", th), quiet = TRUE) |>
st_make_valid() |> st_transform(crs_utm) |> st_geometry()
st_sf(geometry = g) |> mutate(ha = as.numeric(st_area(geometry)) / 1e4)
}
shp <- map(set_names(tahun), baca)                      # poligon asli
u   <- map(shp, \(x) st_union(x) |> st_make_valid())    # hasil dissolve
## 2b. Shapefile perubahan 2017-2024 (kalau ada)
perub <- NULL
if (file.exists("Klaten_Perubahan_Pertanian_2017_2024.shp")) {
perub <- st_read("Klaten_Perubahan_Pertanian_2017_2024.shp", quiet = TRUE) |>
st_make_valid() |> st_transform(crs_utm)
cat("\n--- Kolom shapefile perubahan ---\n"); print(names(perub))
}
## 2c. Tabel luas resmi (XLSX, atau CSV jika ada)
tabel_ref <- NULL
f_xlsx <- "Tabel_Luas_Lahan_Pertanian_Klaten_2017_2024.xlsx"
f_csv  <- list.files(pattern = "Luas.*\\.csv$", ignore.case = TRUE)
if (file.exists(f_xlsx)) {
tabel_ref <- read_excel(f_xlsx) |> clean_names()
} else if (length(f_csv)) {
tabel_ref <- read_csv(f_csv[1], show_col_types = FALSE) |> clean_names()
}
if (!is.null(tabel_ref)) { cat("\n--- Tabel referensi ---\n"); glimpse(tabel_ref) }
# ---- 3. TREN & PERBANDINGAN LUAS -------------------------------------
luas_tahun <- tibble(tahun = tahun, luas_ha = map_dbl(u, ha))
print(luas_tahun)
# Tren luas
p <- ggplot(luas_tahun, aes(tahun, luas_ha)) +
geom_area(fill = "#4C9F70", alpha = .25) +
geom_line(color = "#1B4332", linewidth = 1.1) +
geom_point(color = "#1B4332", size = 3) +
geom_text(aes(label = comma(luas_ha, accuracy = 1)), vjust = -1, size = 3.2) +
scale_x_continuous(breaks = tahun) +
scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
labs(title = "Tren Luas Lahan Pertanian Klaten 2017-2024", x = "Tahun", y = "Luas (ha)") + tema
simpan(p, "tren_luas_pertanian.png")
# Laju perubahan tahunan
laju <- luas_tahun |> mutate(laju = (luas_ha / lag(luas_ha) - 1) * 100) |> filter(!is.na(laju))
p <- ggplot(laju, aes(factor(tahun), laju, fill = laju >= 0)) +
geom_col(width = .7, show.legend = FALSE) + geom_hline(yintercept = 0) +
geom_text(aes(label = sprintf("%.2f%%", laju), vjust = ifelse(laju >= 0, -.5, 1.4)), size = 3.3) +
scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
scale_y_continuous(expand = expansion(mult = .2)) +
labs(title = "Laju Perubahan Luas Pertanian Tahunan",
x = "Tahun", y = "Perubahan terhadap tahun sebelumnya (%)") + tema
simpan(p, "laju_perubahan_tahunan.png")
# Indeks (2017 = 100)
p <- luas_tahun |> mutate(indeks = luas_ha / first(luas_ha) * 100) |>
ggplot(aes(tahun, indeks)) +
geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
geom_line(color = "#264653", linewidth = 1.1) + geom_point(size = 3, color = "#264653") +
scale_x_continuous(breaks = tahun) +
labs(title = "Indeks Luas Pertanian (2017 = 100)", x = "Tahun", y = "Indeks") + tema
simpan(p, "indeks_luas.png")
# Perubahan kumulatif terhadap 2017
p <- luas_tahun |> mutate(delta = luas_ha - first(luas_ha)) |>
ggplot(aes(factor(tahun), delta, fill = delta >= 0)) +
geom_col(width = .7, show.legend = FALSE) + geom_hline(yintercept = 0) +
scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
scale_y_continuous(labels = comma) +
labs(title = "Perubahan Kumulatif Luas Pertanian terhadap 2017", x = "Tahun", y = "Selisih (ha)") + tema
simpan(p, "perubahan_kumulatif.png")
# Bertambah vs berkurang per periode
gl <- map_dfr(tahun[-length(tahun)], function(t) {
a <- u[[as.character(t)]]; b <- u[[as.character(t + 1)]]
tibble(periode = paste0(t, "-", t + 1),
Bertambah = ha(st_difference(b, a)),
Berkurang = -ha(st_difference(a, b)))
}) |> pivot_longer(-periode, names_to = "jenis", values_to = "ha")
p <- ggplot(gl, aes(periode, ha, fill = jenis)) +
geom_col(width = .7) + geom_hline(yintercept = 0) +
scale_fill_manual(values = c(Bertambah = "#2A9D8F", Berkurang = "#E76F51")) +
scale_y_continuous(labels = comma) +
labs(title = "Lahan Pertanian Bertambah dan Berkurang per Periode",
x = "Periode", y = "Luas (ha)", fill = NULL) + tema +
theme(axis.text.x = element_text(angle = 30, hjust = 1))
simpan(p, "gain_loss_per_periode.png")
# Heatmap overlap antar tahun
ov <- expand_grid(i = nm, j = nm) |>
mutate(pct = map2_dbl(i, j, \(i, j) ha(st_intersection(u[[i]], u[[j]])) / ha(u[[i]]) * 100))
p <- ggplot(ov, aes(j, fct_rev(i), fill = pct)) +
geom_tile(color = "white") + geom_text(aes(label = sprintf("%.0f", pct)), size = 3.2) +
scale_fill_gradient(low = "#E9F5EC", high = "#1B4332", name = "% overlap") +
labs(title = "Overlap Lahan Pertanian Antar Tahun",
x = "Tahun pembanding", y = "Tahun acuan") + tema
simpan(p, "heatmap_overlap.png", 8, 6.5)
# Validasi: hitungan vektor vs tabel referensi (xlsx/csv)
if (!is.null(tabel_ref)) {
kol_tahun <- names(tabel_ref)[str_detect(names(tabel_ref), "tahun|year")][1]
kol_luas  <- names(tabel_ref)[map_lgl(tabel_ref, is.numeric) & names(tabel_ref) != kol_tahun][1]
if (!is.na(kol_tahun) && !is.na(kol_luas)) {
cmp <- tabel_ref |> transmute(tahun = as.integer(.data[[kol_tahun]]), tabel = .data[[kol_luas]]) |>
inner_join(luas_tahun, by = "tahun") |> rename(vektor = luas_ha) |>
pivot_longer(c(tabel, vektor), names_to = "sumber", values_to = "ha")
p <- ggplot(cmp, aes(factor(tahun), ha, fill = sumber)) +
geom_col(position = "dodge", width = .7) +
scale_fill_manual(values = c(tabel = "#8AB17D", vektor = "#264653")) +
scale_y_continuous(labels = comma) +
labs(title = "Perbandingan Luas: Tabel Referensi vs Hitungan Shapefile",
x = "Tahun", y = "Luas (ha)", fill = "Sumber") + tema
simpan(p, "validasi_tabel_vs_shapefile.png")
} else message("Kolom tahun/luas di tabel referensi tidak terdeteksi otomatis; cek glimpse di atas.")
}
# ---- 4. SEBARAN & STRUKTUR POLIGON -----------------------------------
poly_all <- map_dfr(nm, \(n) shp[[n]] |> st_drop_geometry() |> mutate(tahun = n))
p <- ggplot(poly_all, aes(tahun, ha, fill = tahun)) +
geom_boxplot(outlier.alpha = .15, show.legend = FALSE) +
scale_y_log10(labels = comma) + scale_fill_viridis_d(option = "D", end = .9) +
labs(title = "Sebaran Ukuran Poligon Pertanian per Tahun",
x = "Tahun", y = "Luas poligon (ha, skala log)") + tema
simpan(p, "sebaran_ukuran_poligon.png")
p <- poly_all |> group_by(tahun) |> summarise(jumlah = n(), rata_ha = mean(ha)) |>
pivot_longer(-tahun) |>
mutate(name = recode(name, jumlah = "Jumlah poligon", rata_ha = "Rata-rata luas (ha)")) |>
ggplot(aes(tahun, value)) + geom_col(fill = "#8AB17D", width = .7) +
facet_wrap(~name, scales = "free_y") + scale_y_continuous(labels = comma) +
labs(title = "Jumlah dan Rata-rata Luas Poligon Pertanian", x = "Tahun", y = NULL) + tema
simpan(p, "jumlah_dan_rata_luas_poligon.png", 10, 5)
# ---- 5. RASTER KEPADATAN (dari SHP) ----------------------------------
bb  <- st_bbox(do.call(c, unname(lapply(u, st_geometry))))
tpl <- rast(ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]),
resolution = 100, crs = paste0("EPSG:", crs_utm))
bin <- map(nm, \(n) rasterize(vect(st_sf(geometry = u[[n]])), tpl, field = 1, background = 0))
stk100 <- rast(bin); names(stk100) <- paste0("y", nm)
stk1km <- aggregate(stk100, fact = 10, fun = "mean", na.rm = TRUE)
cm <- cor(as.data.frame(stk1km), use = "pairwise.complete.obs")
cm_l <- as_tibble(cm, rownames = "i") |> pivot_longer(-i, names_to = "j", values_to = "r") |>
mutate(across(c(i, j), \(x) str_remove(x, "y")))
p <- ggplot(cm_l, aes(j, fct_rev(i), fill = r)) +
geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
scale_fill_gradient2(low = "#E76F51", mid = "white", high = "#1B4332", midpoint = .5,
limits = c(min(cm_l$r), 1), name = "r") +
labs(title = "Korelasi Kepadatan Pertanian Antar Tahun (grid 1 km)", x = NULL, y = NULL) + tema
simpan(p, "korelasi_antar_tahun.png", 8, 6.5)
# ---- 6. ANALISIS CITRA SENTINEL-2 (TIF) ------------------------------
# Citra hanya 3 band (RGB) -> pakai Excess Green (ExG) sebagai proxy kehijauan.
# ExG dihitung dari koordinat kromatik sehingga tidak tergantung skala nilai piksel (0-255 / 0-1).
exg <- function(r) {
s <- r[[1]] + r[[2]] + r[[3]]
(2 * r[[2]] - r[[1]] - r[[3]]) / s
}
hasil_citra <- map_dfr(tahun, function(th) {
r <- rast(sprintf("Klaten_Sentinel2_RGB_10m_%d.tif", th))
v <- vect(st_transform(st_sf(geometry = u[[as.character(th)]]), crs(r)))
e <- exg(r)
tibble(tahun = th,
`Area pertanian`     = global(mask(e, v), "mean", na.rm = TRUE)[[1]],
`Area non-pertanian` = global(mask(e, v, inverse = TRUE), "mean", na.rm = TRUE)[[1]])
}) |> pivot_longer(-tahun, names_to = "area", values_to = "exg")
# ---- 6. ANALISIS CITRA SENTINEL-2 (TIF) ------------------------------
# Citra hanya 3 band (RGB) -> pakai Excess Green (ExG) sebagai proxy kehijauan.
# ExG dihitung dari koordinat kromatik sehingga tidak tergantung skala nilai piksel (0-255 / 0-1).
exg <- function(r) {
s <- r[[1]] + r[[2]] + r[[3]]
(2 * r[[2]] - r[[1]] - r[[3]]) / s
}
hasil_citra <- map_dfr(tahun, function(th) {
r <- rast(sprintf("Klaten_Sentinel2_RGB_10m_%d.tif", th))
v <- vect(st_transform(st_sf(geometry = u[[as.character(th)]]), crs(r)))
e <- exg(r)
tibble(tahun = th,
`Area pertanian`     = global(mask(e, v), "mean", na.rm = TRUE)[[1]],
`Area non-pertanian` = global(mask(e, v, inverse = TRUE), "mean", na.rm = TRUE)[[1]])
}) |> pivot_longer(-tahun, names_to = "area", values_to = "exg")
print(hasil_citra)
p <- ggplot(hasil_citra, aes(tahun, exg, color = area)) +
geom_line(linewidth = 1.1) + geom_point(size = 3) +
scale_x_continuous(breaks = tahun) +
scale_color_manual(values = c("Area pertanian" = "#2A9D8F", "Area non-pertanian" = "#E76F51")) +
labs(title = "Indeks Kehijauan Citra Sentinel-2 pada Area Pertanian vs Non-Pertanian",
x = "Tahun", y = "Rata-rata Excess Green", color = NULL) + tema
simpan(p, "indeks_kehijauan_citra.png")
# Citra RGB per tahun
png("output_plot/citra_sentinel2_per_tahun.png", width = 4000, height = 2200, res = 250)
par(mfrow = c(2, 4), mar = c(1, 1, 2, 1))
for (th in tahun) {
r <- rast(sprintf("Klaten_Sentinel2_RGB_10m_%d.tif", th))
plotRGB(r, r = 1, g = 2, b = 3, stretch = "lin", axes = FALSE, main = paste("Sentinel-2", th))
}
dev.off()
# Overlay poligon pertanian 2024 di atas citra 2024
r24 <- rast("Klaten_Sentinel2_RGB_10m_2024.tif")
png("output_plot/overlay_poligon_citra_2024.png", width = 3000, height = 3000, res = 250)
plotRGB(r24, r = 1, g = 2, b = 3, stretch = "lin", axes = FALSE,
main = "Poligon Pertanian 2024 di atas Citra Sentinel-2")
plot(vect(st_transform(st_sf(geometry = u[["2024"]]), crs(r24))), add = TRUE, border = "yellow", lwd = .6)
dev.off()
# Peta ExG 2024 (kehijauan) dengan batas pertanian
e24 <- exg(r24)
png("output_plot/peta_kehijauan_2024.png", width = 3000, height = 3000, res = 250)
plot(e24, col = hcl.colors(50, "Greens", rev = TRUE), axes = FALSE,
main = "Peta Indeks Kehijauan (Excess Green) 2024")
plot(vect(st_transform(st_sf(geometry = u[["2024"]]), crs(r24))), add = TRUE, border = "black", lwd = .4)
dev.off()
# ---- 7. PETA DARI SHAPEFILE ------------------------------------------
# Sebaran per tahun
u_sf <- map_dfr(nm, \(n) st_sf(tahun = n, geometry = u[[n]]))
p <- ggplot(u_sf) + geom_sf(fill = "#4C9F70", color = NA) + facet_wrap(~tahun, ncol = 4) +
labs(title = "Peta Sebaran Lahan Pertanian Klaten 2017-2024") + tema_peta
simpan(p, "peta_pertanian_per_tahun.png", 12, 7)
# Perubahan 2017 -> 2024 (dihitung dari overlay)
a <- u[["2017"]]; b <- u[["2024"]]
ekstrak <- \(g) st_collection_extract(g, "POLYGON")
chg <- bind_rows(
st_sf(kelas = "Tetap",     geometry = ekstrak(st_intersection(a, b))),
st_sf(kelas = "Bertambah", geometry = ekstrak(st_difference(b, a))),
st_sf(kelas = "Berkurang", geometry = ekstrak(st_difference(a, b))))
warna_chg <- c(Tetap = "#A8DADC", Bertambah = "#2A9D8F", Berkurang = "#E76F51")
p <- ggplot(chg) + geom_sf(aes(fill = kelas), color = NA) +
scale_fill_manual(values = warna_chg) +
labs(title = "Peta Perubahan Lahan Pertanian 2017-2024", fill = NULL) + tema_peta
simpan(p, "peta_perubahan_2017_2024.png", 8, 8)
komp <- chg |> mutate(ha = as.numeric(st_area(geometry)) / 1e4) |>
st_drop_geometry() |> group_by(kelas) |> summarise(ha = sum(ha))
p <- ggplot(komp, aes(reorder(kelas, ha), ha, fill = kelas)) +
geom_col(show.legend = FALSE, width = .65) + coord_flip() +
geom_text(aes(label = comma(ha, accuracy = 1)), hjust = -.1, size = 3.5) +
scale_fill_manual(values = warna_chg) +
scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
labs(title = "Komposisi Kelas Perubahan Lahan Pertanian 2017-2024", x = NULL, y = "Luas (ha)") + tema
simpan(p, "komposisi_perubahan.png", 8, 4)
# Shapefile perubahan bawaan (Klaten_Perubahan_Pertanian_2017_2024.shp)
if (!is.null(perub)) {
kol_kelas <- names(perub)[map_lgl(st_drop_geometry(perub), \(x) is.character(x) || is.factor(x))][1]
if (!is.na(kol_kelas)) {
p <- ggplot(perub) + geom_sf(aes(fill = .data[[kol_kelas]]), color = NA) +
scale_fill_viridis_d(option = "D", end = .9) +
labs(title = "Peta Perubahan Lahan Pertanian 2017-2024 (Shapefile Perubahan)", fill = NULL) + tema_peta
simpan(p, "peta_perubahan_shapefile_bawaan.png", 8, 8)
kp <- perub |> mutate(ha = as.numeric(st_area(geometry)) / 1e4) |>
st_drop_geometry() |> group_by(kelas = .data[[kol_kelas]]) |> summarise(ha = sum(ha))
p <- ggplot(kp, aes(reorder(kelas, ha), ha, fill = kelas)) +
geom_col(show.legend = FALSE, width = .65) + coord_flip() +
geom_text(aes(label = comma(ha, accuracy = 1)), hjust = -.1, size = 3.5) +
scale_fill_viridis_d(option = "D", end = .9) +
scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
labs(title = "Luas per Kelas pada Shapefile Perubahan", x = NULL, y = "Luas (ha)") + tema
simpan(p, "luas_kelas_shapefile_perubahan.png", 8, 4)
} else message("Shapefile perubahan tidak punya kolom teks untuk kelas; cek names(perub).")
}
# ---- 8. PETA KEPADATAN & KONSISTENSI (raster dari SHP) ---------------
df24 <- as.data.frame(stk1km[["y2024"]], xy = TRUE) |> setNames(c("x", "y", "frak"))
p <- ggplot(df24, aes(x, y, fill = frak)) + geom_raster() + coord_equal() +
scale_fill_viridis_c(option = "G", direction = -1, labels = percent, name = "Tutupan") +
labs(title = "Kepadatan Lahan Pertanian 2024 (grid 1 km)") + tema_peta
simpan(p, "peta_kepadatan_2024.png", 8, 8)
dd <- as.data.frame(stk1km[["y2024"]] - stk1km[["y2017"]], xy = TRUE) |> setNames(c("x", "y", "d"))
p <- ggplot(dd, aes(x, y, fill = d)) + geom_raster() + coord_equal() +
scale_fill_gradient2(low = "#E76F51", mid = "white", high = "#2A9D8F", midpoint = 0,
labels = percent, name = "Selisih") +
labs(title = "Perubahan Kepadatan Pertanian 2017-2024") + tema_peta
simpan(p, "peta_selisih_kepadatan.png", 8, 8)
frek <- as.data.frame(sum(stk100), xy = TRUE) |> setNames(c("x", "y", "n"))
p <- ggplot(frek, aes(x, y, fill = factor(n))) + geom_raster() + coord_equal() +
scale_fill_viridis_d(option = "D", direction = -1, name = "Jumlah tahun") +
labs(title = "Konsistensi Lahan Pertanian (jumlah tahun terdeteksi, 2017-2024)") + tema_peta
simpan(p, "peta_konsistensi.png", 9, 8)
message("Selesai. Semua gambar ada di folder 'output_plot'.")
