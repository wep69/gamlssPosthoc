# Reference metadata audit

All bibliographic records in `vignettes/references.bib` were resolved against the
Crossref REST API (`https://api.crossref.org/works`) and transcribed from the
returned record rather than typed by hand.

Consultation date: 2026-08-12. API version reported by Crossref: 1.0.0.

| Key | DOI | Resolved | Notes |
|---|---|---|---|
| `rigby2005` | 10.1111/j.1467-9876.2005.00510.x | yes | Canonical container title is now *Journal of the Royal Statistical Society Series C: Applied Statistics*, publisher Oxford University Press. Earlier package text used the legacy Wiley form and was corrected. Volume 54, issue 3, pages 507-554. |
| `stasinopoulos2007` | 10.18637/jss.v023.i07 | yes | JSS 23(7), 2007. |
| `stasinopoulos2017` | 10.1201/b21973 | yes | Book record resolved by bibliographic query; Chapman and Hall/CRC, 2017-04-21. Five authors confirmed, including Voudouris and De Bastiani. |
| `rigby2019` | 10.1201/9780429298547 | yes | Book DOI confirmed through its registered chapter DOIs (`-1`, `-3`, `-9`); Chapman and Hall/CRC, 2019-10-08. |
| `arelbundock2024` | 10.18637/jss.v111.i09 | yes | JSS 111(9), 2024. Author order Arel-Bundock, Greifer, Heiss confirmed. |
| `dunn1996` | 10.1080/10618600.1996.10474708 | yes | JCGS 5(3), 1996-09. Added; it is the basis of the package diagnostics and was previously uncited. |
| `gneiting2007` | 10.1198/016214506000001437 | yes | JASA 102(477), 2007-03. Added for the proper scoring rules section. |
| `piepho2004` | 10.1198/1061860043515 | yes | JCGS 13(2), 2004-06. Added as the source of the letter-display algorithm. |
| `robins1986` | 10.1016/0270-0255(86)90088-6 | yes | *Mathematical Modelling* 7(9-12), 1986. Added as the g-formula source. |
| `vehtari2017` | 10.1007/s11222-016-9696-4 | yes | *Statistics and Computing* 27(5). Crossref `published` is the online-first date 2016-08-30; the issue year 2017 is retained, which is the conventional citation. |
| `viechtbauer2010` | 10.18637/jss.v036.i03 | yes | JSS 36(3), 2010. |
| `vanbuuren2011` | 10.18637/jss.v045.i03 | yes | JSS 45(3), 2011. Crossref records the first author's given name as "Stef van" and family as "Buuren"; the bib entry uses the correct form "van Buuren, Stef". |
| `rubin1987` | 10.1002/9780470316696 | yes | Wiley monograph, 1987-06-09. |
| `barnard1999` | 10.1093/biomet/86.4.948 | yes | *Biometrika* 86(4), 1999-12-01. Crossref lists only Barnard; the second author, Rubin, is restored from the printed article. |
| `kay2023` | 10.1109/tvcg.2023.3327195 | yes | *IEEE TVCG*, 2023. The DOI resolves only in lower case through the Crossref filter endpoint; the lower-case form is the one deposited and is stored in the bib file. |
| `lenth2016` | 10.18637/jss.v069.i01 | yes | JSS 69(1), 2016. This is the `lsmeans` paper, which remains the citable reference behind `emmeans`. |
| `wickham_ggplot2` | none | not applicable | The Springer book DOI 10.1007/978-3-319-24277-4 did not resolve through the API during the audit, so the package is cited through its CRAN canonical URL rather than through an unverified DOI. |
| `ich2019` | none | not applicable | Regulatory guideline, no DOI registered. |
| `lenth_emmeans` | none | not applicable | R package; cited through its CRAN canonical URL. |

Two records could not be retrieved through the plain `filter=doi:` route and were
resolved through `query.bibliographic` instead (`stasinopoulos2017`,
`rigby2019`). Both are monographs whose Crossref deposits register chapters
rather than a queryable top-level work.
