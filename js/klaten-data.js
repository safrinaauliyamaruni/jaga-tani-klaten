/* Klaten Data: pemuat data hasil analisis (pola sama dengan geosim-data.js).
   Urutan membaca tiap berkas: (1) folder yang dipilih lewat tombol "Pilih folder data",
   (2) fetch dari folder data/ , (3) khusus CSV: nilai bawaan di bawah, supaya halaman
   tetap tampil walau dibuka lewat file:// atau data belum disalin. Shapefile tidak punya nilai bawaan. */
const CFG = {
  years: [2017,2018,2019,2020,2021,2022,2023,2024],
  csv: 'data/Tabel_Luas_Lahan_Pertanian_Klaten_2017_2024.csv',   // statistik resmi (DKPP/LPPD/BPS/RPJMD)
  shpPertanian: y => `data/Klaten_Pertanian_${y}`,            // tanpa ekstensi
  shpPerubahan: 'data/Klaten_Perubahan_Pertanian_2017_2024',
  center: [-7.70, 110.60], zoom: 11
};
const KD = { files: new Map(), fallback: {"Tabel_Luas_Lahan_Pertanian_Klaten_2017_2024.csv": "tahun,sawah_ha,bukan_sawah_ha,total_ha,status_keterbandingan,sumber_utama,url_sumber,bps_sawah_ha,bps_bukan_sawah_ha,bps_total_ha,rpjmd_total_pertanian_ha\n2017,33021,6581,39602,Seri DKPP: sawah + bukan sawah,\"Pertanian Dalam Angka Klaten 2020, Tabel 5.1; sumber DKPP\",https://ppid.klaten.go.id/assets/file/1775792278dokumen-statistik-pertanian-2020.pdf,,,,\n2018,31943,6579,38522,Seri DKPP yang diperbarui; ada angka berbeda pada dokumen lebih lama,LPPD Klaten 2023; DKPP update 22 Jan 2024; juga RPJMD 2021-2026,https://ppid.klaten.go.id/assets/file/1714459146LPPD_KABUPATEN_KLATEN_2023.pdf,,,,\n2019,31863,6578,38441,Seri DKPP: sawah + bukan sawah,Pertanian Dalam Angka Klaten 2020; LPPD Klaten 2023,https://ppid.klaten.go.id/assets/file/1775792278dokumen-statistik-pertanian-2020.pdf,31050.92,11333.57,42384.49,\n2020,31771.1,6574.5,38345.6,Seri DKPP: sawah + bukan sawah,Pertanian Dalam Angka Klaten 2020; LPPD Klaten 2023,https://ppid.klaten.go.id/assets/file/1775792278dokumen-statistik-pertanian-2020.pdf,30550.73,13292.62,43843.35,\n2021,31708.4,6573.2,38281.6,Seri DKPP: sawah + bukan sawah,\"LPPD Klaten 2023, tabel luas lahan pertanian\",https://ppid.klaten.go.id/assets/file/1714459146LPPD_KABUPATEN_KLATEN_2023.pdf,30878.56,13258.09,44136.65,\n2022,31708.4,6573.2,38281.6,Seri DKPP: sawah + bukan sawah,\"LPPD Klaten 2023, tabel luas lahan pertanian\",https://ppid.klaten.go.id/assets/file/1714459146LPPD_KABUPATEN_KLATEN_2023.pdf,,,,\n2023,31708.4,,,Sawah saja dilaporkan sama dengan 2021-2022; bukan sawah tidak tersedia pada tabel tersebut,\"LPPD Klaten 2023, uraian perubahan luas sawah 2018-2023\",https://ppid.klaten.go.id/assets/file/1714459146LPPD_KABUPATEN_KLATEN_2023.pdf,,,,\n2024,31667.4,6197.3,37864.7,Kategori 2024: sawah (irigasi + tadah hujan) dan tegal/kebun; bukan sawah lebih sempit dari seri lama,\"Buku Statistik Sektoral Klaten 2025, Tabel 4.2; sumber Dinas Ketahanan Pangan\",https://ppid.klaten.go.id/assets/file/1772562580BUKU_STATISTIK_SEKTORAL.pdf,,,,42480.94\n"}, usedFallback: false };
const _base = p => p.split('/').pop();
/* Berkas manual (opsional), kunci = nama berkas huruf kecil */
function kdSetFiles(list){ KD.files.clear(); [...list].forEach(f => KD.files.set(f.name.toLowerCase(), f)); window.dispatchEvent(new CustomEvent('klaten:files')); }
async function loadCSV(path){
  const k=_base(path), picked=KD.files.get(k.toLowerCase());
  const txt = picked ? new TextDecoder().decode(await picked.arrayBuffer()) : KD.fallback[k];
  if (!txt) throw new Error(k+' tidak tersedia');
  return Papa.parse(txt,{header:true,dynamicTyping:true,skipEmptyLines:true}).data;
}
/* Poligon pertanian/perubahan: GeoJSON WGS84 tertanam di geo/*.js (tanpa fetch) */
window.KGEO = window.KGEO || {};
async function loadShp(base){
  const k = /Perubahan/.test(base) ? 'chg' : 'p' + base.match(/(\d{4})$/)[1];
  if (KGEO[k]) return KGEO[k];
  await new Promise((ok,no)=>{ const t=document.createElement('script'); t.src="geo/"+k+".js"; t.onload=ok;
    t.onerror=()=>no(new Error('geo/'+k+'.js tidak ditemukan. Folder geo/ harus ada di samping index.html.')); document.head.appendChild(t); });
  return KGEO[k];
}
