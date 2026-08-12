#!/usr/bin/env python3
from pathlib import Path
import re, hashlib, json
ROOT=Path(__file__).resolve().parents[1]

exports=[]
for line in (ROOT/'NAMESPACE').read_text().splitlines():
    m=re.match(r'export\(([^)]+)\)',line.strip())
    if m: exports.append(m.group(1))

def strip_strings_comments(src):
    out=[]; i=0; state='code'; q=None
    while i<len(src):
        c=src[i]
        if state=='code':
            if c=='#': state='comment'; out.append(' ')
            elif c in ('"',"'",'`'): state='string';q=c;out.append(' ')
            else: out.append(c)
        elif state=='comment':
            if c=='\n': state='code';out.append('\n')
            else: out.append(' ')
        else:
            if c=='\\': out.append(' '); i+=1; out.append(' ')
            elif c==q: state='code';out.append(' ')
            else: out.append('\n' if c=='\n' else ' ')
        i+=1
    return ''.join(out),state

def balanced_body(src,start):
    # start points to opening (
    dep=0; state='code'; q=None; i=start
    while i<len(src):
        c=src[i]
        if state=='code':
            if c=='#': state='comment'
            elif c in ('"',"'",'`'): state='string';q=c
            elif c=='(': dep+=1
            elif c==')':
                dep-=1
                if dep==0: return src[start+1:i]
        elif state=='comment':
            if c=='\n': state='code'
        else:
            if c=='\\': i+=1
            elif c==q: state='code'
        i+=1
    raise ValueError('unclosed')

def split_top(s):
    items=[]; cur=[]; dep=0; state='code';q=None;i=0
    pairs={'(':')','[':']','{':'}'}
    opens=set(pairs); closes=set(pairs.values())
    while i<len(s):
        c=s[i]
        if state=='code':
            if c in ('"',"'",'`'): state='string';q=c;cur.append(c)
            elif c in opens: dep+=1;cur.append(c)
            elif c in closes: dep-=1;cur.append(c)
            elif c==',' and dep==0: items.append(''.join(cur).strip());cur=[]
            else:cur.append(c)
        else:
            cur.append(c)
            if c=='\\' and q!='`' and i+1<len(s): i+=1;cur.append(s[i])
            elif c==q: state='code'
        i+=1
    if ''.join(cur).strip(): items.append(''.join(cur).strip())
    return items

def argnames(body):
    out=[]
    for it in split_top(body):
        if not it: continue
        if it=='...': out.append('...'); continue
        name=it.split('=',1)[0].strip()
        out.append(name)
    return out

rtext='\n'.join(p.read_text() for p in (ROOT/'R').glob('*.R'))
results=[]; failures=[]
for fn in exports:
    m=re.search(r'(?m)^'+re.escape(fn)+r'\s*<-\s*function\s*\(',rtext)
    if not m:
        failures.append(f'exported function missing definition: {fn}');continue
    op=rtext.find('(',m.start())
    rargs=argnames(balanced_body(rtext,op))
    # Locate documentation by alias, because several related functions can
    # legitimately share one Rd page through @rdname.
    rd=None; rds=None
    for cand in (ROOT/'man').glob('*.Rd'):
        ct=cand.read_text()
        if re.search(r'\\alias\{'+re.escape(fn)+r'\}', ct):
            rd=cand; rds=ct; break
    if rd is None:
        failures.append(f'missing Rd alias: {fn}');continue
    um=re.search(re.escape(fn)+r'\s*\(',rds)
    if not um:
        # functions with compact usage should still match
        failures.append(f'usage missing in Rd: {fn}');continue
    uop=rds.find('(',um.start())
    rdargs=argnames(balanced_body(rds,uop))
    if rargs!=rdargs:
        failures.append(f'usage mismatch {fn}: R={rargs} Rd={rdargs}')
    # Rd argument items must cover all formal arguments, including `...`.
    rd_items=re.findall(r'\\item\{([^}]+)\}', rds)
    miss_items=[a for a in rargs if a not in rd_items]
    if miss_items:
        failures.append(f'Rd argument items missing {fn}: {miss_items}')
    # Roxygen @param tags immediately preceding the exported definition.
    src_file=None; src_text=None; def_pos=None
    for rp in (ROOT/'R').glob('*.R'):
        rt=rp.read_text()
        mm=re.search(r'(?m)^'+re.escape(fn)+r'\s*<-\s*function\s*\(',rt)
        if mm:
            src_file=rp; src_text=rt; def_pos=mm.start(); break
    if src_text is not None:
        prefix=src_text[:def_pos].splitlines()
        block=[]
        for ln in reversed(prefix):
            if ln.startswith("#'"):
                block.append(ln)
            elif not ln.strip() and not block:
                continue
            else:
                break
        block='\n'.join(reversed(block))
        roxy_params=[]
        for pm in re.finditer(r"(?m)^#' @param\s+([^\s]+)", block):
            token=pm.group(1)
            roxy_params.extend([z.strip('` ') for z in token.split(',') if z.strip()])
        miss_roxy=[a for a in rargs if a not in roxy_params]
        # @rdname blocks inherit argument documentation from the shared topic.
        if miss_roxy and "@rdname" not in block:
            failures.append(f'Roxygen @param missing {fn}: {miss_roxy}')
    results.append((fn,rargs))

# R delimiter checks
for p in list((ROOT/'R').glob('*.R'))+list((ROOT/'tests').rglob('*.R'))+list((ROOT/'inst/examples').glob('*.R')):
    t,state=strip_strings_comments(p.read_text())
    stack=[]; pair={')':'(',']':'[','}':'{'}
    for ln,line in enumerate(t.splitlines(),1):
        for col,c in enumerate(line,1):
            if c in '([{': stack.append((c,ln,col))
            elif c in ')]}':
                if not stack or stack[-1][0]!=pair[c]:
                    failures.append(f'delimiter mismatch {p.relative_to(ROOT)}:{ln}:{col}')
                    stack=[];break
                stack.pop()
    if stack: failures.append(f'unclosed delimiter {p.relative_to(ROOT)} {stack[-1]}')
    if state=='string': failures.append(f'unclosed string {p.relative_to(ROOT)}')

# duplicate function definitions
seen={}
for p in (ROOT/'R').glob('*.R'):
    for m in re.finditer(r'(?m)^([.A-Za-z][A-Za-z0-9._]*)\s*<-\s*function\s*\(',p.read_text()):
        seen.setdefault(m.group(1),[]).append(p.name)
for k,v in seen.items():
    if len(v)>1: failures.append(f'duplicate function {k}: {v}')

# NAMESPACE S3 targets exist
for line in (ROOT/'NAMESPACE').read_text().splitlines():
    m=re.match(r'S3method\(([^,]+),([^)]+)\)',line.strip())
    if m:
        generic=m.group(1).split('::')[-1]
        name=f'{generic}.{m.group(2)}'
        if not re.search(r'(?m)^'+re.escape(name)+r'\s*<-\s*function\s*\(',rtext):
            failures.append(f'S3 target missing: {name}')


# Critical architectural invariants introduced in 0.2.0
critical = {
    'dynamic parameters via object$parameters': ('R/adapters.R', r'object\$parameters'),
    'dynamic fallback parameters from formulas and links': ('R/adapters.R', r'union\(fpars, lpars\)'),
    'distribution constructor parameters discovered from formals': ('R/zero_adjusted.R', r'formals\(gamlss\.dist::GAMLSS\)'),
    'gamlssZadj formulas via documented S3 method': ('R/adapters.R', r'formula\(object, parameter = what\)'),
    'generic positive distribution constructor': ('R/zero_adjusted.R', r'gamlss\.dist::GAMLSS'),
    'zero mass distinguished from CDF at zero': ('R/zero_adjusted.R', r'is_discrete'),
    'emmeans restricted to explicit reference population': ('R/estimands.R', r'identical\(population, "reference"\)'),
    'marginaleffects avg_predictions integration': ('R/gamlss_posthoc.R', r'avg_predictions'),
    'marginaleffects avg_comparisons integration': ('R/gamlss_posthoc.R', r'avg_comparisons'),
    'marginaleffects pairwise direction harmonized': ('R/estimands.R', r'pairwise = "revpairwise"'),
    'marginaleffects multiplicity adjustment': ('R/gamlss_posthoc.R', r'p.value.adjusted'),
    'marginaleffects percent change from marginal-mean ratio': ('R/gamlss_posthoc.R', r'100 \* \(con\$estimate - 1\)'),
    'marginaleffects avg_slopes integration': ('R/gamlss_trend.R', r'avg_slopes'),
    'scientific ratio contrasts': ('R/utils.R', r'percent_change'),
    'formal estimand metadata': ('R/estimands.R', r'\.gph_estimand_info'),
    'layered uncertainty router': ('R/estimands.R', r'\.gph_choose_uncertainty'),
    'trend exposes covariance and bootstrap uncertainty layers': ('R/gamlss_trend.R', r'uncertainty = c\("auto", "delta", "simulation", "bootstrap", "none"\)'),
    'quantitative turning points': ('R/gamlss_trend.R', r'\.gph_turning_points'),
    'local quadratic optimum refinement': ('R/gamlss_trend.R', r'\.gph_refine_extremum'),
    'diagnostic plan': ('R/gamlss_posthoc_plan.R', r'gamlss_posthoc_plan'),
    'data-first graphics layer': ('R/plot_data.R', r'gamlss_plot_data'),
    'parameter link-scale plotting': ('R/plot_data.R', r'parameter_scale'),
    'marginal mixture quantile solver': ('R/plot_data.R', r'\.gph_mixture_quantile'),
    'distinct distribution group labels': ('R/plot_data.R', r'\.gph_label'),
    'ggdist optional uncertainty graphics': ('R/plots.R', r'ggdist::'),
    'bootstrap optimum location draws': ('R/gamlss_trend.R', r'optimum_draws'),
    'factor predictive fit layer': ('R/plots.R', r'geom_errorbar'),
    'native diagnostics': ('R/plots.R', r'plot_gamlss_diagnostics'),
    'tidy method': ('R/tidy_methods.R', r'tidy\.gamlss_posthoc'),
    'glance method': ('R/tidy_methods.R', r'glance\.gamlss_posthoc'),
    'augment method': ('R/tidy_methods.R', r'augment\.gamlss_posthoc'),
    'parameters integration': ('R/tidy_methods.R', r'model_parameters\.gamlss_posthoc'),
    'autoplot methods': ('R/plots.R', r'autoplot\.gamlss_posthoc'),
    'Word export': ('R/export_report.R', r'export_to_word'),
    'LaTeX export': ('R/export_report.R', r'export_to_latex'),
    'automatic report diagnostics': ('R/export_report.R', r'\.gph_report_model_info'),
    'visual regression specifications': ('tests/testthat/test-vdiffr-v03.R', r'expect_doppelganger'),
    'pkgdown configuration': ('_pkgdown.yml', r'reference:'),
    'cheatsheet included': ('inst/cheatsheet/CHEATSHEET_SOURCE.md', r'gamlssPosthoc'),
}
critical_results = {}
for label, (rel, pat) in critical.items():
    ok = bool(re.search(pat, (ROOT/rel).read_text()))
    critical_results[label] = ok
    if not ok: failures.append(f'critical invariant missing: {label}')

# files ASCII-ish path portability and line endings
for p in ROOT.rglob('*'):
    if p.is_file():
        rel=str(p.relative_to(ROOT))
        if len(rel)>100: failures.append(f'long path >100: {rel}')
        b=p.read_bytes()
        if b'\r\n' in b: failures.append(f'CRLF file: {rel}')

report={
    'exports': exports,
    'function_usage_checks': len(results),
    'r_files': len(list((ROOT/'R').glob('*.R'))),
    'test_files': len(list((ROOT/'tests').rglob('*.R'))),
    'example_files': len(list((ROOT/'inst/examples').glob('*.R'))),
    'critical_invariants': critical_results,
    'failures': failures,
}
print(json.dumps(report,indent=2,ensure_ascii=False))
(ROOT/'STATIC_VALIDATION.json').write_text(json.dumps(report,indent=2,ensure_ascii=False))
raise SystemExit(1 if failures else 0)
