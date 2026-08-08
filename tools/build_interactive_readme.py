from pathlib import Path
import re, html, mistune

root = Path(__file__).resolve().parents[1]
md_path = root / 'README.md'
text = md_path.read_text(encoding='utf-8')
renderer = mistune.HTMLRenderer(escape=False)
markdown = mistune.create_markdown(renderer=renderer, plugins=['table','strikethrough'])
body = markdown(text)

# Add stable ids to headings and build TOC.
used = {}
def slugify(s):
    plain = re.sub(r'<[^>]+>', '', s)
    plain = html.unescape(plain).lower()
    slug = re.sub(r'[^a-z0-9à-ÿ]+', '-', plain, flags=re.I).strip('-') or 'section'
    n = used.get(slug, 0) + 1; used[slug] = n
    return slug if n == 1 else f'{slug}-{n}'

toc=[]
def repl(m):
    level=int(m.group(1)); content=m.group(2); slug=slugify(content)
    if level <= 3:
        toc.append((level, re.sub(r'<[^>]+>','',content), slug))
    return f'<h{level} id="{slug}">{content}<a class="anchor" href="#{slug}">#</a></h{level}>'
body = re.sub(r'<h([1-6])>(.*?)</h\1>', repl, body, flags=re.S)

# Add copy buttons to code blocks.
body = re.sub(r'<pre><code([^>]*)>(.*?)</code></pre>',
              lambda m: '<div class="codewrap"><button class="copy" type="button">Copiar</button><pre><code'+m.group(1)+'>'+m.group(2)+'</code></pre></div>',
              body, flags=re.S)

nav=[]
for level,title,slug in toc:
    if level == 1: continue
    nav.append(f'<a class="toc l{level}" href="#{slug}">{html.escape(title)}</a>')
nav_html='\n'.join(nav)

page=f'''<!doctype html>
<html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>gamlssPosthoc 0.2.0 — README interativo</title>
<style>
:root{{--bg:#fff;--fg:#17202a;--muted:#657786;--panel:#f5f7f9;--line:#d9e0e6;--accent:#2457a6;--code:#f6f8fa}}
[data-theme="dark"]{{--bg:#11161c;--fg:#e9eef3;--muted:#9aabb9;--panel:#182028;--line:#34414c;--accent:#83b5ff;--code:#161b22}}
*{{box-sizing:border-box}} html{{scroll-behavior:smooth}} body{{margin:0;background:var(--bg);color:var(--fg);font:16px/1.62 system-ui,-apple-system,Segoe UI,Roboto,Arial,sans-serif}}
.top{{position:sticky;top:0;z-index:10;display:flex;gap:.7rem;align-items:center;padding:.7rem 1rem;background:color-mix(in srgb,var(--bg) 92%,transparent);border-bottom:1px solid var(--line);backdrop-filter:blur(8px)}}
.top strong{{margin-right:auto}} input{{min-width:18rem;max-width:42vw;padding:.55rem .7rem;border:1px solid var(--line);border-radius:8px;background:var(--bg);color:var(--fg)}} button{{border:1px solid var(--line);border-radius:8px;background:var(--panel);color:var(--fg);padding:.5rem .7rem;cursor:pointer}}
.layout{{display:grid;grid-template-columns:280px minmax(0,900px);gap:2rem;max-width:1240px;margin:auto;padding:1.5rem}}
aside{{position:sticky;top:64px;height:calc(100vh - 80px);overflow:auto;padding-right:1rem;border-right:1px solid var(--line)}} .toc{{display:block;color:var(--fg);text-decoration:none;padding:.25rem 0}} .toc.l3{{padding-left:1rem;font-size:.92em;color:var(--muted)}} .toc.active{{color:var(--accent);font-weight:650}}
main{{min-width:0}} h1,h2,h3{{line-height:1.25;scroll-margin-top:72px}} h1{{font-size:2.1rem}} h2{{margin-top:2.4rem;border-bottom:1px solid var(--line);padding-bottom:.35rem}} .anchor{{opacity:0;text-decoration:none;margin-left:.4rem;color:var(--accent)}} h1:hover .anchor,h2:hover .anchor,h3:hover .anchor{{opacity:.7}}
a{{color:var(--accent)}} code{{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}} :not(pre)>code{{background:var(--code);padding:.12rem .3rem;border-radius:4px}}
.codewrap{{position:relative;margin:1rem 0}} pre{{overflow:auto;padding:1rem;border:1px solid var(--line);border-radius:9px;background:var(--code)}} .copy{{position:absolute;right:.45rem;top:.45rem;font-size:.78rem;opacity:.8}}
blockquote{{margin-left:0;padding:.6rem 1rem;border-left:4px solid var(--accent);background:var(--panel)}} table{{border-collapse:collapse;width:100%;overflow:auto;display:block}} th,td{{border:1px solid var(--line);padding:.45rem .65rem}} th{{background:var(--panel)}}
.route{{border:1px solid var(--line);background:var(--panel);padding:1rem;border-radius:10px;margin:.8rem 0 1.5rem}} .route select{{padding:.4rem;margin:.25rem;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:6px}}
.hidden-search{{display:none!important}}
@media(max-width:900px){{.layout{{grid-template-columns:1fr}} aside{{display:none}} input{{min-width:0;max-width:none;flex:1}}}}
</style></head><body>
<div class="top"><strong>gamlssPosthoc 0.2.0</strong><input id="search" placeholder="Buscar no README…"><button id="theme">Claro/escuro</button></div>
<div class="layout"><aside><div class="route"><b>Assistente de engine</b><br><label>Estimando <select id="est"><option>parameter</option><option>mean</option><option>variance</option><option>quantile</option><option>prob_zero</option></select></label><br><label>População <select id="pop"><option>observed</option><option>balanced</option><option>reference</option></select></label><br><label>Smoother <select id="smooth"><option value="no">não</option><option value="yes">sim</option></select></label><p id="rec"></p></div>{nav_html}</aside><main>{body}</main></div>
<script>
const root=document.documentElement; document.getElementById('theme').onclick=()=>{{root.dataset.theme=root.dataset.theme==='dark'?'':'dark';localStorage.setItem('gph-theme',root.dataset.theme)}}; root.dataset.theme=localStorage.getItem('gph-theme')||'';
document.querySelectorAll('.copy').forEach(b=>b.onclick=async()=>{{await navigator.clipboard.writeText(b.parentElement.querySelector('code').innerText);b.textContent='Copiado';setTimeout(()=>b.textContent='Copiar',1200)}});
const search=document.getElementById('search'); search.oninput=()=>{{const q=search.value.toLowerCase().trim();document.querySelectorAll('main > h2, main > h3, main > p, main > ul, main > ol, main > .codewrap, main > table').forEach(el=>el.classList.toggle('hidden-search',q && !el.innerText.toLowerCase().includes(q)))}};
function route(){{let e=est.value,p=pop.value,s=smooth.value;let r;if(e!=='parameter')r='distribution';else if(p==='reference'&&s==='no')r='emmeans (diferença simples) ou marginaleffects';else if(s==='no')r='marginaleffects; fallback distribution';else r='distribution / predição';rec.textContent='Recomendação: '+r}}; ['est','pop','smooth'].forEach(id=>document.getElementById(id).onchange=route);route();
const links=[...document.querySelectorAll('.toc')], secs=links.map(a=>document.querySelector(a.getAttribute('href'))).filter(Boolean);addEventListener('scroll',()=>{{let cur=secs[0];for(const s of secs)if(s.getBoundingClientRect().top<120)cur=s;links.forEach(a=>a.classList.toggle('active',cur&&a.getAttribute('href')==='#'+cur.id))}});
</script></body></html>'''
for dest in [root/'README_interactive.html', root/'inst/html/README_interactive.html']:
    dest.parent.mkdir(parents=True, exist_ok=True); dest.write_text(page, encoding='utf-8')
print('built', len(page), 'bytes')
