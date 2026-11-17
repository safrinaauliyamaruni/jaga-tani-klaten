JAGA TANI — Pemantauan Alih Fungsi Lahan Pertanian Klaten 2017–2024

CARA BUKA
  Klik dua kali serve.bat (atau: python serve.py), lalu buka http://localhost:8000
  (atau klik kanan index.html > Open with Live Server).

STRUKTUR FOLDER
  *.html            Halaman web: Beranda, Materi, Dashboard, Peta, Bandingkan, Spasiotemporal, Data
                    (cek.html = alat bantu memeriksa berkas yang hilang)
  css/  js/  lib/   Gaya, skrip situs, dan pustaka (Leaflet, Chart.js, PapaParse)
  js/klaten-data.js Pemuat data + nilai bawaan tabel CSV
  geo/              Poligon pertanian per tahun + perubahan (dipakai langsung oleh situs)
  img/              Gambar & animasi untuk halaman web
  data/             Tabel luas lahan (CSV, XLSX) + arsip shapefile asli (.zip, untuk QGIS/ArcGIS/R)
  analisis/         Skrip R (riwayat_analisis.R) dan grafik hasil analisis (grafik/)

CATATAN
  Situs hanya membutuhkan html, css, js, lib, geo, img. Folder data/ dan analisis/ adalah arsip.
  Total berkas: 63.
