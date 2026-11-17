// Helper peta bersama (cache poligon + utilitas geometri)
const _shp={};
const getShp=y=>_shp[y]??=loadShp(CFG.shpPertanian(y));
// klasifikasi fitur perubahan: loss / gain / other (dari nilai atribut teks)
function chgClass(p){const s=Object.values(p||{}).filter(v=>typeof v==='string').join(' ').toUpperCase();
  return s.includes('HILANG')||s.includes('LOSS')?'loss':s.includes('TAMBAH')||s.includes('GAIN')?'gain':'other'}
// luas poligon (ha): aproksimasi planar lokal, lubang (ring ke-2 dst) dikurangkan
function ringA(r){const x0=r[0][0],y0=r[0][1],k=Math.cos(y0*Math.PI/180)*111320,m=110570;let a=0;
  for(let j=0;j<r.length-1;j++)a+=(r[j][0]-x0)*k*(r[j+1][1]-y0)*m-(r[j+1][0]-x0)*k*(r[j][1]-y0)*m;return Math.abs(a/2)}
function polyHa(g){const P=g.type==='Polygon'?[g.coordinates]:g.type==='MultiPolygon'?g.coordinates:[];let t=0;
  P.forEach(p=>p.forEach((r,i)=>t+=(i?-1:1)*ringA(r)));return t/1e4}

function geomCenter(g){let x0=1e9,x1=-1e9,y0=1e9,y1=-1e9;const w=c=>{if(typeof c[0]==='number'){if(c[0]<x0)x0=c[0];if(c[0]>x1)x1=c[0];if(c[1]<y0)y0=c[1];if(c[1]>y1)y1=c[1]}else c.forEach(w)};w(g.coordinates);return[(x0+x1)/2,(y0+y1)/2]}
