"""
52b — Gleb-aligned drought adaptation test (audit 2026-06-01).
Changes vs 52:
  (i)  primary regressor = cumulative 3-yr drought (cum3_*), matching the main
       climate engine (52 used same-year shares).
  (ii) drops log_price_year (collinear with year FE; was dropping 2003-05).
  (iii) adds a TRUE switcher-only sample (early-adapted mines excluded, not
        recoded as zero controls).
Writes results/52b__drought_cum3_aligned.csv
"""
import pandas as pd, numpy as np, pyfixest as pf, warnings
from pathlib import Path
warnings.filterwarnings('ignore')
ROOT = Path(__file__).resolve().parents[1]
dr = pd.read_parquet(ROOT/'data'/'panels'/31__drought_mine_year.parquet')
ad = pd.read_parquet(ROOT/'data'/'panels'/50__adapt_mine_month.parquet')
ay = ad.groupby(['mine_operation','year']).agg(adapt=('adapt_main','max')).reset_index()
d  = dr.merge(ay, on=['mine_operation','year'], how='left').fillna({'adapt':0})
d  = d[d['in_sample']==True].copy()
EARLY = ['El Tesoro','Esperanza','Candelaria','Escondida']   # adapted at/near inception

def fit(df, x):
    df = df.dropna(subset=[x,'log_kt_year']).copy()
    df['xa'] = df[x]*df['adapt']
    m = pf.feols(f'log_kt_year ~ {x} + xa + adapt | mine_operation + year',
                 data=df, vcov={'CRV1':'codregion'})
    return m.coef()['xa'], m.pvalue()['xa'], int(m._N)

rows=[]
specs=[('cum3_str_meteo','ENSO meteo (PRIMARY, cum3)'),
       ('cum3_thr_meteo','all-cause meteo (cum3)'),
       ('cum3_str_hydro','ENSO hydro (cum3)'),
       ('cum3_thr_hydro','all-cause hydro (cum3)'),
       ('str_share_meteo','ENSO meteo (same-year — superseded spec)')]
for samp,lab in [('full',d),('switcher_only',d[~d.mine_operation.isin(EARLY)])]:
    for x,xl in specs:
        b,p,n = fit(lab, x)
        rows.append(dict(sample=samp, regressor=x, label=xl, b2=round(b,3), p=round(p,3), N=n))
res=pd.DataFrame(rows)
res.to_csv(ROOT/'results/52b__drought_cum3_aligned.csv', index=False)
print(res.to_string(index=False))
