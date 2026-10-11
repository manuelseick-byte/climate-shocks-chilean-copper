# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
#!/usr/bin/env python3
"""Choropleth maps of Chile by region: water rights & non-use (PNU) fines, all-sector vs mining.
Sources: DGA national registry (DerechosConcedidos_national_2026-05.xlsx), PNU_2026_enriched.xlsx,
boundaries from caracena/chile-geojson (codregion key). Run with /opt/anaconda3/bin/python3."""
import warnings; warnings.filterwarnings("ignore")
import pandas as pd, numpy as np, geopandas as gpd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize, LogNorm
from matplotlib.cm import ScalarMappable
import os

PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE = PROJECT_ROOT
OUT = PROJECT_ROOT / "results" / "figures"; os.makedirs(OUT, exist_ok=True)
plt.rcParams.update({"font.family":"DejaVu Sans","figure.dpi":150})

# ---------- region name -> official codregion ----------
def code(s):
    s=str(s).lower()
    keys=[('arica',15),('tarapac',1),('antofagasta',2),('atacama',3),('coquimbo',4),
          ('valpara',5),('metropolit',13),('higgins',6),('maule',7),('ñuble',16),('nuble',16),
          ('araucan',9),('rios',14),('río',14),('lagos',10),('aysen',11),('aysén',11),('aisén',11),
          ('magallanes',12),('bío',8),('bio',8)]
    for k,c in keys:
        if k in s: return c
    return None

SHORT={15:'Arica y Parinacota',1:'Tarapacá',2:'Antofagasta',3:'Atacama',4:'Coquimbo',5:'Valparaíso',
       13:'Metropolitana',6:"O'Higgins",7:'Maule',16:'Ñuble',8:'Biobío',9:'La Araucanía',
       14:'Los Ríos',10:'Los Lagos',11:'Aysén',12:'Magallanes'}

# ---------- geometry ----------
gdf=gpd.read_file(str(PROJECT_ROOT / "data" / "raw" / "geo" / "chile_regiones.json"))[['codregion','geometry']]
gdf['codregion']=gdf['codregion'].astype(int)

# ---------- data ----------
reg=pd.read_excel(str(PROJECT_ROOT / "data" / "raw" / "dga" / "DerechosConcedidos_national_2026-05.xlsx"),
                  sheet_name=0,header=5,usecols=[3,15]); reg.columns=['region','uso']
reg=reg.dropna(subset=['region']); reg['cod']=reg['region'].map(code); reg=reg.dropna(subset=['cod'])
reg['cod']=reg['cod'].astype(int)
all_rights=reg.groupby('cod').size()
mining_rights=reg[reg['uso'].astype(str).str.contains('Minero',case=False,na=False)].groupby('cod').size()

pnu=pd.read_excel(str(PROJECT_ROOT / "data" / "processed" / "PNU_2026_enriched.xlsx"))
pnu['cod']=pnu['REGIÓN'].map(code); pnu['valor_utm']=pd.to_numeric(pnu['valor_utm'],errors='coerce')
pnu=pnu.dropna(subset=['cod']); pnu['cod']=pnu['cod'].astype(int)
pnu_all=pnu.groupby('cod')['valor_utm'].sum()
pnu_min=pnu[pnu['Water Use']=='Mining Use'].groupby('cod')['valor_utm'].sum()

PANELS=[
 dict(key='all_rights', series=all_rights, cmap='YlOrRd', log=False,
      title='Water rights granted, by region',
      sub='All uses · DGA national registry (May 2026) · n = {:,}'.format(int(all_rights.sum())),
      fmt=lambda v:f"{int(v):,}", cbar='Number of granted water rights'),
 dict(key='pnu_all', series=pnu_all, cmap='PuBuGn', log=True,
      title='Non-use fee (patente por no uso), by region',
      sub='All sectors · 2026 process · total = {:,.0f} UTM'.format(pnu_all.sum()),
      fmt=lambda v:f"{v/1000:,.0f}k", cbar='Annual non-use fee (UTM, log scale)'),
 dict(key='mining_rights', series=mining_rights, cmap='YlOrRd', log=False,
      title='Mining-use water rights, by region',
      sub='Mining use only · DGA national registry (May 2026) · n = {:,}'.format(int(mining_rights.sum())),
      fmt=lambda v:f"{int(v):,}", cbar='Number of mining-use rights'),
 dict(key='pnu_mining', series=pnu_min, cmap='OrRd', log=True,
      title='Non-use fee on mining rights, by region',
      sub='Mining sector only · 2026 process · total = {:,.0f} UTM'.format(pnu_min.sum()),
      fmt=lambda v:f"{v:,.0f}", cbar='Annual non-use fee, mining (UTM, log scale)'),
]

def draw(ax,p):
    g=gdf.copy(); g['val']=g['codregion'].map(p['series']);
    vals=g['val'].dropna()
    if p['log']:
        norm=LogNorm(vmin=max(vals.min(),1),vmax=vals.max())
    else:
        norm=Normalize(vmin=vals.min(),vmax=vals.max())
    cmap=plt.get_cmap(p['cmap'])
    # missing/zero regions in light grey (guard empty subsets to avoid aspect error)
    have=g[g['val'].notna()]; miss=g[g['val'].isna()]
    if len(have): have.plot(ax=ax,column='val',cmap=cmap,norm=norm,edgecolor='white',linewidth=0.5)
    if len(miss): miss.plot(ax=ax,color='#ECECEC',edgecolor='white',linewidth=0.5,hatch='///')
    ax.set_xlim(-76,-60.5); ax.set_ylim(-56,-17.3); ax.set_aspect(1.25); ax.axis('off')
    ax.text(0.0,1.045,p['title'],transform=ax.transAxes,fontsize=12,fontweight='bold',ha='left',va='bottom')
    ax.text(0.0,1.012,p['sub'],transform=ax.transAxes,fontsize=8.3,color='#555',ha='left',va='bottom')
    # labels with leader lines to a right-hand gutter
    LX=-64.5
    rows=g.dropna(subset=['val']).copy()
    rows['cy']=rows.geometry.apply(lambda gm: gm.representative_point().y)
    rows['cx']=rows.geometry.apply(lambda gm: gm.representative_point().x)
    for _,r in rows.sort_values('cy',ascending=False).iterrows():
        nm=SHORT[int(r['codregion'])]; lab=f"{nm}  ·  {p['fmt'](r['val'])}"
        ax.plot([r['cx'],LX-0.2],[r['cy'],r['cy']],color='#bbb',lw=0.4,zorder=1)
        ax.text(LX,r['cy'],lab,fontsize=7.2,va='center',ha='left',color='#222')
    sm=ScalarMappable(norm=norm,cmap=cmap); sm.set_array([])
    cax=ax.inset_axes([0.04,0.06,0.42,0.018])
    cb=plt.colorbar(sm,cax=cax,orientation='horizontal'); cb.ax.tick_params(labelsize=6)
    cb.set_label(p['cbar'],fontsize=6.8)

# individual maps
for p in PANELS:
    fig,ax=plt.subplots(figsize=(5.2,9))
    draw(ax,p)
    fig.text(0.5,0.012,"Source: DGA Catastro Público de Aguas; PNU enriched registry. Hatched = no data.",
             ha='center',fontsize=6.5,color='#888')
    fig.savefig(f"{OUT}/MAP_{p['key']}.png",bbox_inches='tight'); plt.close()
    print("saved MAP_"+p['key'])

# paired figures
for tag,pair in [('A_all_sectors',[PANELS[0],PANELS[1]]),('B_mining',[PANELS[2],PANELS[3]])]:
    fig,axes=plt.subplots(1,2,figsize=(10.4,9))
    for ax,p in zip(axes,pair): draw(ax,p)
    fig.text(0.5,0.02,"Source: DGA Catastro Público de Aguas (national registry, May 2026); "
             "non-use fee = patente por no uso, 2026 process. Hatched = no rights / no data.",
             ha='center',fontsize=7,color='#888')
    fig.savefig(f"{OUT}/MAP_PAIR_{tag}.png",bbox_inches='tight'); plt.close()
    print("saved MAP_PAIR_"+tag)

print("\nDONE. all_rights cods:",sorted(all_rights.index.tolist()))
print("mining cods:",sorted(mining_rights.index.tolist()))
print("pnu_all cods:",sorted(pnu_all.index.tolist()))
print("pnu_mining cods:",sorted(pnu_min.index.tolist()),"vals:",{SHORT[c]:round(v) for c,v in pnu_min.items()})
