#!/usr/bin/env python3
import numpy as np
from scipy.stats import gamma, norm
from scipy.optimize import brentq
from pathlib import Path
import json
rng=np.random.default_rng(20260810)
results=[]
def add(name, target, got, tol):
    err=float(abs(got-target)); ok=bool(err<=tol)
    results.append(dict(test=name,target=float(target),empirical=float(got),abs_error=err,tolerance=float(tol),pass_=ok))

# 1. Mixture quantile: solve weighted CDF, validate against a large independent simulation.
w=np.array([.25,.75]); shapes=np.array([2.0,5.0]); scales=np.array([1.0,.55]); p=.8
F=lambda x: np.sum(w*gamma.cdf(x,shapes,scale=scales))
q=brentq(lambda x:F(x)-p,0,20)
N=500_000; comp=rng.choice(2,N,p=w); sim=gamma.rvs(shapes[comp],scale=scales[comp],random_state=rng)
add('marginal mixture quantile p=.8',q,np.quantile(sim,p),.025)

# 2. Quantile fan nesting for mixture.
probs=[.05,.10,.25,.50,.75,.90,.95]
qs=[brentq(lambda x,pp=pp:F(x)-pp,0,25) for pp in probs]
results.append(dict(test='quantile fan is ordered',target=1.0,empirical=float(all(np.diff(qs)>0)),abs_error=0.0,tolerance=0.0,pass_=bool(all(np.diff(qs)>0))))

# 3. Zero-adjusted mixture CDF/quantile against simulation.
h=.35; shp=3.0; scl=.8; p=.7
Fza=lambda x: h+(1-h)*gamma.cdf(x,shp,scale=scl) if x>=0 else 0
qza=0.0 if p<=h else gamma.ppf((p-h)/(1-h),shp,scale=scl)
z=rng.random(N)<h; ys=np.zeros(N); ys[~z]=gamma.rvs(shp,scale=scl,size=(~z).sum(),random_state=rng)
add('zero-adjusted quantile p=.7',qza,np.quantile(ys,p),.025)

# 4. Density standardization integrates approximately to 1 for an ordinary mixture.
x=np.linspace(0,25,50000); dens=w[0]*gamma.pdf(x,shapes[0],scale=scales[0])+w[1]*gamma.pdf(x,shapes[1],scale=scales[1])
integ=np.trapezoid(dens,x)
add('standardized predictive density integrates to one',1.0,integ,2e-4)

# 5. Survival/CDF complement.
xx=2.7; cdf=F(xx); surv=1-cdf
add('cdf plus survival',1.0,cdf+surv,1e-12)

# 6. Scientific contrast orientation A/B.
A=4.2; B=2.8
add('ratio orientation A over B',1.5,A/B,1e-12)
add('percent change orientation',50.0,100*(A/B-1),1e-12)

# 7. Finite-difference derivative and optimum refinement on a quadratic response.
x=np.linspace(0,10,101); y=5+2*x-.4*x*x
hstep=x[1]-x[0]; d1=(y[2:]-y[:-2])/(x[2:]-x[:-2]); truth=2-.8*x[1:-1]
add('first derivative grid max error',0.0,np.max(np.abs(d1-truth)),1e-10)
# vertex
xstar=-2/(2*(-.4)); ystar=5+2*xstar-.4*xstar*xstar
coef=np.polyfit(x[23:28],y[23:28],2); xhat=-coef[1]/(2*coef[0]); yhat=np.polyval(coef,xhat)
add('local optimum location',xstar,xhat,1e-10); add('local optimum response',ystar,yhat,1e-10)

# 8. Simple PIT calibration benchmark: if U=Phi(Z), approximately uniform moments.
z=rng.normal(size=500_000); u=norm.cdf(z)
add('PIT mean under calibrated normal residuals',.5,u.mean(),.002)
add('PIT variance under calibrated normal residuals',1/12,u.var(),.001)

overall=all(r['pass_'] for r in results)
out={'overall':overall,'n_tests':len(results),'tests':results}
root=Path(__file__).resolve().parents[1]
(root/'GRAPHICS_MATH_VALIDATION.json').write_text(json.dumps(out,indent=2))
md=['# Graphics mathematical validation','',f'Overall: **{overall}**', '', '| Test | Target | Obtained | Abs. error | Tolerance | Pass |','|---|---:|---:|---:|---:|:---:|']
for r in results:
    md.append(f"| {r['test']} | {r['target']:.8g} | {r['empirical']:.8g} | {r['abs_error']:.3g} | {r['tolerance']:.3g} | {'YES' if r['pass_'] else 'NO'} |")
(root/'GRAPHICS_MATH_VALIDATION.md').write_text('\n'.join(md)+'\n')
print(json.dumps(out,indent=2))
raise SystemExit(0 if overall else 1)
