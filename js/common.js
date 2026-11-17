const NAV = [['index.html','Beranda'],['materi.html','Materi'],['dashboard.html','Dashboard'],['peta.html','Peta'],['bandingkan.html','Bandingkan'],['spasiotemporal.html','Spasiotemporal'],['data.html','Data']];
function renderNav(){
  const cur = location.pathname.split('/').pop() || 'index.html';
  document.body.insertAdjacentHTML('afterbegin',
    '<nav class="top"><a class="brand" href="index.html"><i>🌾</i>Jaga Tani</a>' +
    '<div class="links">' + NAV.map(([h,t])=>`<a class="l ${h===cur?'active':''}" href="${h}">${t}</a>`).join('') + '</div>' +
    '<button class="burger" aria-label="Menu" aria-expanded="false">☰</button></nav>');
  const nav=document.querySelector('nav.top'), b=nav.querySelector('.burger');
  b.onclick=()=>{const o=nav.classList.toggle('open');b.setAttribute('aria-expanded',o)};
  if(!document.body.classList.contains('mappage'))
    document.body.insertAdjacentHTML('beforeend','<footer class="site">Jaga Tani · Pemantauan alih fungsi lahan pertanian Kabupaten Klaten 2017–2024</footer>');
}
function showErr(msg){
  const d=document.createElement('div');d.className='err';d.innerHTML=msg;
  (document.querySelector('main')||document.body).prepend(d);
}
const fmt = (n,d=0)=> n==null||isNaN(n)?'-':Number(n).toLocaleString('id-ID',{minimumFractionDigits:d,maximumFractionDigits:d});
function addBasemaps(map){
  const osm=L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',{maxZoom:19,attribution:'© OpenStreetMap'});
  const sat=L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',{maxZoom:19,attribution:'Esri'});
  osm.addTo(map); L.control.layers({'OpenStreetMap':osm,'Citra Esri':sat},null,{position:'topright'}).addTo(map);
}
document.addEventListener('DOMContentLoaded',renderNav);
