import pandas as pd, numpy as np
from scipy.linalg import qr
np.set_printoptions(suppress=True)

def fullrank(X, keep0=True):
    # rank-revealing QR with pivoting; return independent column index set (orig indices)
    Q,R,piv = qr(X, mode='economic', pivoting=True)
    d = np.abs(np.diag(R))
    tol = d.max()*1e-9
    r = int((d>tol).sum())
    keep = sorted(piv[:r].tolist())
    return keep

def crv1_se_for(Xfr, y, beta, groups, target):
    # cluster-robust (CRV1) se of coefficient 'target' (index in Xfr)
    N,K = Xfr.shape
    XtX_inv = np.linalg.inv(Xfr.T@Xfr)
    u = y - Xfr@beta
    a = XtX_inv[:,target]                 # (X'X)^-1 e_target
    z = Xfr@a                             # N-vector
    G = len(np.unique(groups))
    s = 0.0
    for g in np.unique(groups):
        m = groups==g
        sg = np.sum(z[m]*u[m])
        s += sg*sg
    c = (G/(G-1.0))*((N-1.0)/(N-K))
    return np.sqrt(c*s), G

def wcb(df, ycol, tested_main, controls, fes, cluster, B=9999, seed=42):
    d = df.copy()
    d['xa'] = d[tested_main[0]]*d[tested_main[1]]
    # build design: continuous + intercept + FE dummies (drop first level)
    cont = ['xa'] + [tested_main[0]] + controls
    Xparts = [d[cont].astype(float).values, np.ones((len(d),1))]
    names = list(cont) + ['const']
    for fe in fes:
        dum = pd.get_dummies(d[fe].astype(str), prefix=fe, drop_first=True).astype(float)
        Xparts.append(dum.values); names += list(dum.columns)
    X = np.hstack(Xparts)
    y = d[ycol].astype(float).values
    groups = d[cluster].values
    xa_orig = 0
    keep = fullrank(X)
    if xa_orig not in keep: keep = [xa_orig]+keep
    Xfr = X[:,keep]; xa = keep.index(xa_orig)
    # full fit
    XtX_inv = np.linalg.inv(Xfr.T@Xfr)
    M = XtX_inv@Xfr.T
    beta = M@y
    b_xa = beta[xa]
    se_xa, G = crv1_se_for(Xfr, y, beta, groups, xa)
    t0 = b_xa/se_xa
    # restricted fit (drop xa column)
    rcols = [i for i in range(Xfr.shape[1]) if i!=xa]
    Xr = Xfr[:,rcols]
    br = np.linalg.lstsq(Xr, y, rcond=None)[0]
    fitted_r = Xr@br
    u_r = y - fitted_r
    # precompute for fast se*
    a = XtX_inv[:,xa]; zfix = Xfr@a; c = (G/(G-1.0))*((len(y)-1.0)/(len(y)-Xfr.shape[1]))
    uniq = np.unique(groups); gidx = {g:(groups==g) for g in uniq}
    def run(wtype):
        rng = np.random.default_rng(seed)
        tb = np.empty(B)
        for b in range(B):
            if wtype=='rademacher':
                wg = rng.choice([-1.0,1.0], size=len(uniq))
            else: # webb 6-point
                vals = np.array([-np.sqrt(1.5),-1,-np.sqrt(.5),np.sqrt(.5),1,np.sqrt(1.5)])
                wg = rng.choice(vals, size=len(uniq))
            wmap = dict(zip(uniq,wg)); w = np.array([wmap[g] for g in groups])
            ys = fitted_r + w*u_r
            bs = M@ys; us = ys - Xfr@bs
            s=0.0
            for g in uniq:
                mm=gidx[g]; sg=np.sum(zfix[mm]*us[mm]); s+=sg*sg
            se = np.sqrt(c*s)
            tb[b] = bs[xa]/se
        return np.mean(np.abs(tb) >= abs(t0))
    return b_xa, t0, G, run('webb'), run('rademacher')

# ---------- load ----------
ac = pd.read_parquet('data/panels/30__acute_mine_month.parquet'); ac['date']=pd.to_datetime(ac['date'])
ad = pd.read_parquet('data/panels/50__adapt_mine_month.parquet'); ad['date']=pd.to_datetime(ad['date'])
ac = ac.merge(ad[['mine_operation','date','adapt_main']], on=['mine_operation','date'], how='left')
ac = ac[ac['in_sample']==True].copy(); ac['adapt_main']=ac['adapt_main'].fillna(0)
ac['rm'] = ac['region_en'].astype(str)+'_'+ac['month'].astype(str)

print(f"{'channel':24s} {'beta_xa':>9s} {'CR1 t':>7s} {'G':>3s} {'WCB p (Webb)':>13s} {'WCB p (Rad)':>12s}")
for ch in ['stress_flood_regmean_M','stress_flood_mine','stress_heat_mine']:
    b,t0,G,pw,pr = wcb(ac, 'log_kt', (ch,'adapt_main'),
                       controls=['adapt_main','log_price'], fes=['mine_operation','rm','year'],
                       cluster='codregion', B=9999)
    print(f"{ch:24s} {b:+9.3f} {t0:7.2f} {G:3d} {pw:13.3f} {pr:12.3f}")

# drought
dr = pd.read_parquet('data/panels/31__drought_mine_year.parquet')
ay = ad.copy(); ay['year']=ay['date'].dt.year
ay = ay.groupby(['mine_operation','year'])['adapt_main'].max().reset_index().rename(columns={'adapt_main':'adapt'})
dd = dr.merge(ay, on=['mine_operation','year'], how='left')
dd = dd[dd['in_sample']==True].copy(); dd['adapt']=dd['adapt'].fillna(0)
dd = dd.dropna(subset=['cum3_str_meteo','log_kt_year'])
b,t0,G,pw,pr = wcb(dd, 'log_kt_year', ('cum3_str_meteo','adapt'),
                   controls=['adapt'], fes=['mine_operation','year'], cluster='codregion', B=9999)
print(f"{'drought cum3 ENSO meteo':24s} {b:+9.3f} {t0:7.2f} {G:3d} {pw:13.3f} {pr:12.3f}")
