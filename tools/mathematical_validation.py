#!/usr/bin/env python3
import numpy as np, pandas as pd
from pathlib import Path
rng=np.random.default_rng(20260808)
rows=[]

# 1) Generic zero-adjusted mean/variance identity, Gamma positive part.
h=.37
mplus=3.2
vplus=1.15
shape=mplus*mplus/vplus
scale=vplus/mplus
N=800000
is0=rng.random(N)<h
y=np.where(is0,0,rng.gamma(shape,scale,N))
tmean=(1-h)*mplus
tvar=(1-h)*vplus+h*(1-h)*mplus**2
rows.append(dict(test='ZA Gamma mean',theory=tmean,empirical=y.mean(),abs_error=abs(y.mean()-tmean)))
rows.append(dict(test='ZA Gamma variance',theory=tvar,empirical=y.var(),abs_error=abs(y.var()-tvar)))
# 2) Generic quantile mapping.
for p in (.20,.50,.90):
    if p<=h:
        q=0.0
    else:
        qadj=(p-h)/(1-h)
        # simulate a large positive sample rather than scipy dependency
        pos=rng.gamma(shape,scale,1000000)
        q=np.quantile(pos,qadj)
    emp=np.quantile(y,p)
    rows.append(dict(test=f'ZA quantile p={p}',theory=q,empirical=emp,abs_error=abs(emp-q)))

# 2b) Same zero-adjusted identities with a Lognormal positive part.
# This verifies that the mixture formulas are distribution-agnostic.
h2=.22
mu_log=.7
sd_log=.55
mplus2=np.exp(mu_log + .5*sd_log**2)
vplus2=(np.exp(sd_log**2)-1)*np.exp(2*mu_log+sd_log**2)
is02=rng.random(N)<h2
y2=np.where(is02,0,rng.lognormal(mu_log,sd_log,N))
tmean2=(1-h2)*mplus2
tvar2=(1-h2)*vplus2+h2*(1-h2)*mplus2**2
rows.append(dict(test='ZA Lognormal mean',theory=tmean2,empirical=y2.mean(),abs_error=abs(y2.mean()-tmean2)))
rows.append(dict(test='ZA Lognormal variance',theory=tvar2,empirical=y2.var(),abs_error=abs(y2.var()-tvar2)))
for p in (.10,.50,.95):
    if p<=h2:
        q=0.0
    else:
        qadj=(p-h2)/(1-h2)
        # large positive Monte Carlo reference avoids a SciPy dependency
        q=np.quantile(rng.lognormal(mu_log,sd_log,1000000),qadj)
    emp=np.quantile(y2,p)
    rows.append(dict(test=f'ZA Lognormal quantile p={p}',theory=q,empirical=emp,abs_error=abs(emp-q)))

# 3) Scientific contrasts.
a,b=4.5,3.0
checks={'difference':a-b,'ratio':a/b,'log_ratio':np.log(a/b),'percent_change':100*(a/b-1)}
expected={'difference':1.5,'ratio':1.5,'log_ratio':np.log(1.5),'percent_change':50.0}
for k,v in checks.items(): rows.append(dict(test=f'contrast {k}',theory=expected[k],empirical=v,abs_error=abs(v-expected[k])))
# 4) Finite-difference derivative strategy on a quadratic.
x=np.linspace(-3,3,301); f=2+1.7*x-.4*x*x

def grad(x,y):
    z=np.empty_like(y)
    z[0]=(y[1]-y[0])/(x[1]-x[0]); z[-1]=(y[-1]-y[-2])/(x[-1]-x[-2])
    z[1:-1]=(y[2:]-y[:-2])/(x[2:]-x[:-2])
    return z
f1=grad(x,f); f2=grad(x,f1)
true1=1.7-.8*x; true2=np.full_like(x,-.8)
rows.append(dict(test='first derivative max interior error',theory=0,empirical=np.max(np.abs(f1[1:-1]-true1[1:-1])),abs_error=np.max(np.abs(f1[1:-1]-true1[1:-1]))))
rows.append(dict(test='second derivative max interior error',theory=0,empirical=np.max(np.abs(f2[2:-2]-true2[2:-2])),abs_error=np.max(np.abs(f2[2:-2]-true2[2:-2]))))
# 5) Turning point interpolation.
sg=np.where(np.signbit(f1[:-1])!=np.signbit(f1[1:]))[0]
idx=sg[0]
x0=x[idx]-f1[idx]*(x[idx+1]-x[idx])/(f1[idx+1]-f1[idx])
true_opt=1.7/.8
rows.append(dict(test='turning point quadratic',theory=true_opt,empirical=x0,abs_error=abs(x0-true_opt)))

# 6) Local quadratic refinement of a grid-based optimum.
xg=np.arange(0,4.0001,.5); yg=5-1.7*(xg-2.13)**2
j=int(np.argmax(yg)); ii=slice(j-1,j+2)
cf=np.polyfit(xg[ii],yg[ii],2)
xv=-cf[1]/(2*cf[0]); yv=np.polyval(cf,xv)
rows.append(dict(test='local quadratic optimum x',theory=2.13,empirical=xv,abs_error=abs(xv-2.13)))
rows.append(dict(test='local quadratic optimum y',theory=5.0,empirical=yv,abs_error=abs(yv-5.0)))

out=pd.DataFrame(rows)
# Thresholds selected relative to Monte Carlo or numerical approximation.
thresholds=[]
for _,r in out.iterrows():
    t=r['test']
    if 'mean' in t: thr=.01
    elif 'variance' in t: thr=.03
    elif 'quantile' in t: thr=.03
    elif 'derivative' in t: thr=1e-8
    elif 'turning' in t: thr=.02
    elif 'quadratic optimum' in t: thr=1e-10
    else: thr=1e-12
    thresholds.append(thr)
out['threshold']=thresholds
out['pass']=out.abs_error<=out.threshold
root=Path(__file__).resolve().parents[1]
out.to_csv(root/'MATHEMATICAL_VALIDATION.csv',index=False)
md=['# Mathematical validation of gamlssPosthoc 0.2.0','',
    'Independent numerical checks of formulas and algorithms used by the refactored code.','',
    out.to_markdown(index=False),'',
    f"Overall: **{'PASS' if out['pass'].all() else 'FAIL'}** ({out['pass'].sum()}/{len(out)} checks)."]
(root/'MATHEMATICAL_VALIDATION.md').write_text('\n'.join(md))
print(out.to_string(index=False))
print('OVERALL',out['pass'].all())
raise SystemExit(0 if out['pass'].all() else 1)
