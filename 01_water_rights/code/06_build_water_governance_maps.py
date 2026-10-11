# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
#!/usr/bin/env python3
"""
Water-governance maps for the institutional section.
Builds TWO deliverables (team chooses between them):
  (2) MAP_mines_in_closed_basins.png  — single focused map: mines over closed basins
  (4) MAP_PANEL_comprehensive.png     — 3-panel slide: limits | protected/reserved | mining response

Data:
  - DGA Mapoteca shapefiles (restriction/prohibition, agotamiento, protected vegas/bofedales) EPSG:5360
  - mine locations: (Gleb) registry/mines.csv  (lon/lat, n=19)
  - desal status: outputs/data/desal_status_by_mine.xlsx
  - basemap: source_inputs/geo/chile_regiones.json
Run: /opt/anaconda3/bin/python3 "Water Use Rights/scripts/build_water_governance_maps.py"
"""
import warnings; warnings.filterwarnings("ignore")
import geopandas as gpd, pandas as pd, numpy as np, glob, os
import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
from matplotlib.patches import Patch; from matplotlib.lines import Line2D
plt.rcParams.update({"font.family":"DejaVu Sans","figure.dpi":150})

PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE = PROJECT_ROOT
OUT = PROJECT_ROOT / "results" / "figures"
mp = PROJECT_ROOT / "data" / "raw" / "dga_mapoteca"
GLEB = PROJECT_ROOT / "data" / "raw" / "mine_registry"
os.makedirs(OUT,exist_ok=True)
def load(pat): return gpd.read_file(glob.glob(mp+"/**/"+pat,recursive=True)[0]).to_crs(4326)

# NOTE: DGA_Mapoteca was reorganised into Basins/ Inventories/ Publications/ Restrictions/ etc.
# load() uses a recursive filename glob, so it still resolves each layer regardless of subfolder.
restr=load("Areas_de_Restricci*n_Zonas_de_Prohibici*n.shp")  # Restrictions/...2025/  (FECHA_ACTU 2025-11-15)
agot =load("Declaraci*n_de_Agotamiento.shp")                  # Restrictions/...2017/
prot =load("Acuiferos_Protegidos_Vegas_y_Bofedales.shp")      # Restrictions/Acuiferos_y_Vegas_Protegidas_2010/
vegas=load("Vegas_Protegidas.shp")                            # Restrictions/Acuiferos_y_Vegas_Protegidas_2010/
reserva=load("Reserva_de_Caudales.shp")                       # Restrictions/Reserva_de_Caudales_2025/  (Art. 5 reserves)
glac =load("IPG_2022_v2.shp")                                 # Inventories/Inventario Publico de Glaciares 2022 v2/
regiones=gpd.read_file(str(PROJECT_ROOT / "data" / "raw" / "geo" / "chile_regiones.json"))[['codregion','geometry']]
rRestr=restr[restr['TIPO_LIMIT'].str.contains('Restric',case=False,na=False)]
rProh =restr[restr['TIPO_LIMIT'].str.contains('Prohib',case=False,na=False)]

# ---- mines + spatial joins ----
m=pd.read_csv(str(PROJECT_ROOT / "data" / "raw" / "mine_registry" / "mines.csv"))
gm=gpd.GeoDataFrame(m,geometry=gpd.points_from_xy(m.lon,m.lat),crs=4326)
def within(layer): return gpd.sjoin(gm,layer[['geometry']],how='left',predicate='within').groupby(level=0)['index_right'].first().notna().values
gm['in_r']=within(restr); gm['in_a']=within(agot); gm['closed']=gm['in_r']|gm['in_a']
# nearest closed-aquifer distance (km)
near=gpd.sjoin_nearest(gm.to_crs(32719),restr.to_crs(32719)[['geometry']],distance_col="d")
gm['near_km']=(near.groupby(level=0)['d'].first().values/1000)
N_CLOSED=int(gm['closed'].sum()); N_NEAR=int((gm['near_km']<=10).sum()); N=len(gm)

# ---- desal status per Gleb mine_operation ----
ds=pd.read_excel(str(PROJECT_ROOT / "data" / "processed" / "desal_status_by_mine.xlsx"))
def norm(s):
    s=str(s).lower()
    if '100' in s:return '100% seawater'
    if 'partial' in s:return 'Partial'
    if 'no transition' in s:return 'No plan'   # check before 'plan' (it contains "plans")
    if 'cease' in s or 'clos' in s:return 'Closed'
    if 'plan' in s:return 'Planned'
    return 'No plan'
ds['st']=ds['desal_status'].map(norm)
rank={'100% seawater':4,'Partial':3,'Planned':2,'No plan':1,'Closed':0}
# aggregate to most-advanced status per company-group (company col matches Gleb mine_operation)
agg=ds.groupby('company')['st'].apply(lambda s:max(s,key=lambda x:rank[x])).to_dict()
gm['desal']=gm['mine_operation'].map(agg).fillna('No plan')
gm.loc[gm.mine_operation=='Cerro Colorado','desal']='Closed'

YB,YT=-56.0,-17.3   # full Chile mainland (north to Cape Horn)
def base(ax,xr=-66.0):
    regiones.plot(ax=ax,color="#F7F7F7",edgecolor="#D9D9D9",linewidth=0.4)
    ax.set_xlim(-76,xr); ax.set_ylim(YB,YT); ax.set_aspect(1.25); ax.set_anchor('N'); ax.axis('off')

# ============================================================
# DELIVERABLE (2): single focused map
# ============================================================
SHORT={'Codelco - Gaby':'Gabriela Mistral','Codelco - Salvador':'Salvador','Codelco - Andina':'Andina',
 'Codelco - Teniente':'El Teniente','Codelco - Chuqui, RT y MH':'Chuquicamata / RT','Escondida':'Escondida',
 'Collahuasi':'Collahuasi','Los Pelambres':'Los Pelambres','Anglo American Sur':'Anglo American Sur',
 'Anglo American Norte':'Anglo American Norte','Zaldivar':'Zaldívar','El Abra':'El Abra','Candelaria':'Candelaria',
 'Cerro Colorado':'Cerro Colorado','El Tesoro':'Centinela / El Tesoro','Quebrada Blanca':'Quebrada Blanca',
 'Lomas Bayas':'Lomas Bayas','Esperanza':'Esperanza','Spence':'Spence'}
gm['lab']=gm['mine_operation'].map(SHORT)

fig,ax=plt.subplots(figsize=(7.0,11.0)); base(ax,xr=-63.5)
rRestr.plot(ax=ax,color="#F6C667"); rProh.plot(ax=ax,color="#C0654E")
agot.plot(ax=ax,facecolor='none',edgecolor="#1F6FB2",linewidth=1.0)
ins=gm[gm['closed']]; out=gm[~gm['closed']]
ax.scatter(ins.lon,ins.lat,marker='^',s=72,c="#7B1E1E",edgecolor='white',linewidth=0.7,zorder=5)
ax.scatter(out.lon,out.lat,marker='^',s=62,facecolor='white',edgecolor='#333',linewidth=0.9,zorder=5)
# labels confined to the northern band where the mines are (keeps leaders short)
LX=-68.6; g=gm.sort_values('lat',ascending=False).reset_index(drop=True); ys=np.linspace(-19.3,-34.8,len(g))
for i,r in g.iterrows():
    col="#7B1E1E" if r['closed'] else "#555"; mark="●" if r['closed'] else "○"
    ax.plot([r['lon'],LX-0.15],[r['lat'],ys[i]],color='#bbb',lw=0.4,zorder=2)
    ax.text(LX,ys[i],f"{mark} {r['lab']}",fontsize=7.0,va='center',ha='left',color=col)
ax.text(0.0,1.075,'Major copper mines sit inside Chile’s closed water basins',transform=ax.transAxes,fontsize=13,fontweight='bold',va='bottom')
ax.text(0.0,1.068,f"{N_CLOSED} of {N} major mines lie within a legally restricted/prohibited aquifer or an exhausted\nriver basin; {N_NEAR} of {N} are within 10 km of such water source. With restrictions on\ncontinental water, desalination becomes a viable alternative to expand activities.   ● inside a closed basin   ○ other",
        transform=ax.transAxes,fontsize=7.8,color='#333',va='top')
leg=[Patch(fc="#F6C667",label="Restriction area (controlled new rights)"),
     Patch(fc="#C0654E",label="Prohibition zone (no new rights)"),
     Patch(fc="none",ec="#1F6FB2",lw=1.0,label="Exhausted river basin"),
     Line2D([],[],marker='^',color='w',markerfacecolor="#7B1E1E",markeredgecolor='white',markersize=10,label=f"Mine in a closed basin ({N_CLOSED})"),
     Line2D([],[],marker='^',color='w',markerfacecolor='white',markeredgecolor='#333',markersize=10,label=f"Mine elsewhere ({N-N_CLOSED})")]
ax.legend(handles=leg,loc='lower left',fontsize=7.0,frameon=False,bbox_to_anchor=(0.0,0.01))
ax.text(0,-0.015,"Sources: DGA Mapoteca Digital (restriction/prohibition Nov 2025; agotamiento); mine locations from Gleb registry (n=19).",
        transform=ax.transAxes,fontsize=6.2,color='#999')
fig.savefig(f"{OUT}/MAP_mines_in_closed_basins.png",bbox_inches='tight'); plt.close()
print(f"(2) single map saved. closed {N_CLOSED}/{N}, within 10km {N_NEAR}/{N}")

# ============================================================
# DELIVERABLE (4): 3-panel comprehensive slide
# ============================================================
fig,axes=plt.subplots(1,3,figsize=(13.5,11.0))
# Panel 1 — limits on new rights
ax=axes[0]; base(ax)
rRestr.plot(ax=ax,color="#F6C667"); rProh.plot(ax=ax,color="#C0654E")
agot.plot(ax=ax,facecolor='none',edgecolor="#1F6FB2",linewidth=0.9)
ax.set_title("1 · Limits on new rights",fontsize=11,fontweight='bold',loc='left')
ax.legend(handles=[Patch(fc="#F6C667",label="Restriction area (121)"),Patch(fc="#C0654E",label="Prohibition zone (146)"),
    Patch(fc="none",ec="#1F6FB2",label="Exhausted river basin (15)")],loc='lower left',fontsize=6.6,frameon=False)
# Panel 2 — protected & reserved water (wetlands + glaciers + Art. 5 flow reserves)
ax=axes[1]; base(ax)
glac.plot(ax=ax,color="#5DADE2",linewidth=0,zorder=2)            # glaciers (Andean spine)
prot.plot(ax=ax,color="#27AE60",alpha=0.75,zorder=3)            # aquifers feeding wetlands
reserva.plot(ax=ax,color="#8E44AD",alpha=0.85,zorder=3)         # reserva de caudales (Art. 5 reserves)
ax.scatter(vegas.geometry.x,vegas.geometry.y,s=3,c="#0E6655",alpha=0.7,zorder=4)
ax.set_title("2 · Protected & reserved water",fontsize=11,fontweight='bold',loc='left')
ax.legend(handles=[
    Patch(fc="#5DADE2",label=f"Glaciers ({len(glac):,})"),
    Patch(fc="#27AE60",label="Aquifers feeding wetlands (216)"),
    Line2D([],[],marker='o',color='w',markerfacecolor="#0E6655",markersize=5,label="Protected wetlands (419)"),
    Patch(fc="#8E44AD",label=f"Reserved flows – Art. 5 ({len(reserva)})")],
    loc='lower left',fontsize=6.4,frameon=False)
# Panel 3 — mining response (desal status over faint constraints)
ax=axes[2]; base(ax)
for L,c in [(rRestr,"#F3D9A0"),(rProh,"#E0B3A6")]: L.plot(ax=ax,color=c,alpha=0.6)
agot.plot(ax=ax,facecolor='none',edgecolor="#ABC8DD",linewidth=0.7)
DCOL={'100% seawater':'#1E8449','Partial':'#7DCEA0','Planned':'#F39C12','No plan':'#909497','Closed':'#000000'}
for st,c in DCOL.items():
    sub=gm[gm['desal']==st]
    ax.scatter(sub.lon,sub.lat,marker='^',s=66,c=c,edgecolor='white',linewidth=0.6,zorder=5)
ax.set_title("3 · Mining response (desalination)",fontsize=11,fontweight='bold',loc='left')
order=['100% seawater','Partial','Planned','No plan','Closed']
ax.legend(handles=[Line2D([],[],marker='^',color='w',markerfacecolor=DCOL[s],markeredgecolor='white',markersize=9,
    label=f"{s} ({int((gm['desal']==s).sum())})") for s in order],loc='lower left',fontsize=6.6,frameon=False,title="Mine seawater status")
fig.suptitle("Chile's water-governance constraints and the mining response",fontsize=15,fontweight='bold',x=0.09,ha='left',y=0.98)
fig.text(0.09,0.945,f"Where the resource is legally closed (panel 1) and protected or reserved (panel 2), {N_CLOSED} of {N} major copper mines operate inside a restricted/prohibited aquifer or exhausted basin — and have moved, or are moving, to seawater (panel 3).",
         fontsize=8.5,color='#444',ha='left')
fig.text(0.5,0.04,"Sources: DGA Mapoteca Digital (Nov 2025); mine locations Gleb registry (n=19); desalination status from desal_status_by_mine. Status shown at company-group level.",
         ha='center',fontsize=7,color='#999')
fig.savefig(f"{OUT}/MAP_PANEL_comprehensive.png",bbox_inches='tight'); plt.close()
print("(4) comprehensive 3-panel saved. desal mix:",{s:int((gm['desal']==s).sum()) for s in order})
