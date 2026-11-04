# Render 5_outputs/REPORT.md to a self-contained HTML page (figures embedded). Run from the project root.
md <- readLines("5_outputs/REPORT.md", encoding = "UTF-8")
body <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
# embed figures as data URIs
for (f in list.files("5_outputs", pattern = "^fig_.*[.]png$")) {
  uri <- paste0("data:image/png;base64,", base64enc::base64encode(file.path("5_outputs", f)))
  body <- gsub(sprintf('src="%s"', f), sprintf('src="%s"', uri), body, fixed = TRUE)
}
body <- gsub('<table>', '<div class="tw"><table>', body, fixed = TRUE)
body <- gsub('</table>', '</table></div>', body, fixed = TRUE)
css <- '
:root {
  /* VKM profile 2025: white paper, black ink, VKM green as the one accent; report layout, one column */
  --paper: #FFFFFF; --ink: #000000; --grey: #626250; --hairline: #CFCFCA; --mist: #E9EBE8; --accent: #86B29C;
  --body: Calibri, Carlito, "Segoe UI", system-ui, sans-serif;
  color-scheme: light;
}
* { box-sizing: border-box; }
html { background: var(--paper); }
body { margin: 0; background: var(--paper); color: var(--ink); font-family: var(--body); font-size: 17px; line-height: 1.55; }
main { max-width: 860px; margin: 0 auto; padding: 40px 24px 64px; }
h1 { font-size: 34px; line-height: 1.15; margin: 0 0 6px; text-wrap: balance; }
h1 + p { color: var(--grey); margin-top: 0; }
h1 + p::after { content: ""; display: block; width: 72px; height: 3px; background: var(--accent); margin-top: 18px; }
h2 { font-size: 24px; margin: 44px 0 10px; padding-top: 16px; border-top: 1px solid var(--hairline); text-wrap: balance; }
h3 { font-size: 19px; margin: 28px 0 8px; }
p, li { max-width: 70ch; }
li { margin: 4px 0; }
a { color: var(--ink); }
code { font-size: .9em; background: var(--mist); padding: 1px 5px; border-radius: 3px; }
strong { font-weight: 700; }
.tw { overflow-x: auto; margin: 14px 0 18px; }
table { border-collapse: collapse; font-size: 15px; font-variant-numeric: tabular-nums; min-width: 100%; }
th { text-align: left; border-bottom: 1px solid var(--ink); padding: 6px 10px; vertical-align: bottom; }
td { border-bottom: 1px solid var(--hairline); padding: 6px 10px; vertical-align: top; }
tbody tr:nth-child(odd) td { background: var(--mist); }
img { max-width: 100%; height: auto; display: block; margin: 16px 0; border: 1px solid var(--hairline); border-radius: 4px; }
footer { max-width: 860px; margin: 0 auto; padding: 16px 24px 32px; color: var(--grey); font-size: 14px; border-top: 1px solid var(--hairline); }
@media (max-width: 480px) { body { font-size: 16px; } h1 { font-size: 27px; } main { padding: 24px 16px 48px; } }
'
html <- paste0('<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<title>Host Map validation report</title>\n<style>', css, '</style>\n</head>\n<body>\n<main>\n', body,
  '</main>\n<footer>Host Map is part of the FinnPRIO project (VKM). Questions: Daniel Flø, Daniel.Flo@vkm.no. Source: github.com/floedaniel/host_map</footer>\n</body>\n</html>\n')
writeLines(html, "5_outputs/REPORT.html", useBytes = TRUE)
cat(nchar(html), "chars\n")
