# org-pdf — Pure Elisp Org → PDF Export

No dependency on LaTeX, Pandoc, a headless browser, or **any external program**. The entire PDF is written byte by byte by Emacs itself.

Works on macOS, Windows, and Linux with Emacs 30+.

## Why

Emacs's built-in `org-latex-export-to-pdf` essentially performs “Org → `.tex` → run `pdflatex`.” It cannot be used without a TeX environment.

org-pdf bypasses the entire LaTeX toolchain. It parses the syntax tree with `org-element-parse-buffer`, performs layout in Elisp (line wrapping, pagination, character-by-character CJK wrapping, and table layout), and writes the PDF file directly.

## Installation

Add the `org-pdf/` directory to `load-path`, then:

```elisp
(add-to-list 'load-path "/path/to/org-pdf")
(require 'org-pdf)
```

## Usage

```text
M-x org-pdf-export           ; Export to a PDF with the same name and open it automatically
M-x org-pdf-export-to-file   ; Specify the output path
```

**Integration with the Org export dispatcher**: After loading the package, a `p` entry appears in the `C-c C-e` menu:

```text
C-c C-e p p    ; Export to PDF (org-pdf)
C-c C-e p o    ; Export to PDF and open it
```

> The top-level key is lowercase **`p`**, because uppercase `P` is reserved for Org's built-in Publish menu. Taking over that key can cause the error
> `org-export-dispatch: Symbol's function definition is void: nil`.

**Exporting a subtree**: Use the scope prefix `C-s` to limit the export to the current subtree, or pass a prefix argument (`C-u`) before `M-x org-pdf-export`:

```text
C-c C-e C-s p p          ; Export the current subtree to PDF
C-u M-x org-pdf-export   ; Same as above
```

When exporting a subtree, the output filename follows Org's convention: **the heading's `:EXPORT_FILE_NAME:` property takes precedence** (resolved relative to the current file's directory). If the property is absent, the filename falls back to `<file-name>_<subtree-title>.pdf`:

```org
* Java Agent
:PROPERTIES:
:EXPORT_FILE_NAME: ./JavaAgentDynamicLog
:END:
```

This exports to `JavaAgentDynamicLog.pdf`.

The root heading of a subtree becomes the **title of the exported document** (rendered in the same style as `#+TITLE:`). Headings beneath it are numbered from the top level in the PDF: the first child heading is numbered `1`, rather than `0.1`, and the root heading itself does not consume number `1`.

(If the menu entry does not appear, call `(org-pdf-register-backend)` once in your init file, or add `org-pdf` to `org-export-backends` and restart Emacs.)

Batch processing from the command line:

```sh
emacs --batch -L /path/to/org-pdf -l org-pdf \
  --eval '(org-pdf-export-file "in.org" "out.pdf")'
```

## Supported Elements

Headings (multiple font sizes, hierarchical numbering, and PDF bookmarks); paragraphs; ordered, unordered, and nested lists; checkboxes; quote blocks; centered blocks and verse blocks; `src`/`example` code blocks (monospaced text, a light background, and **syntax highlighting**); tables (borders and wrapped cell contents); horizontal rules; **images** (PNG / JPEG / SVG); document metadata (`#+TITLE:`, `#+AUTHOR:`, and `#+DATE:`); inline `*bold*`, `/italic/`, `=verbatim=`, and `~code~`; and **`#+include:` file inclusion**.

Headings generate hierarchical **PDF bookmarks (the document outline)** and receive section numbers (1, 1.1, 1.1.1, …). The numbers appear both in the document headings and in the bookmarks. Numbering can be disabled with `org-pdf-number-headings`.

Code blocks are colored using **Emacs's own `font-lock`**. Any language is highlighted automatically as long as its major mode is installed. Common `font-lock` faces (keywords, strings, comments, function names, types, constants, and so on) are mapped to a fixed color palette; see `org-pdf--face-colors`. Syntax highlighting can be disabled with `org-pdf-syntax-highlight`.

Code lines wider than the page are **wrapped automatically** at character boundaries, preserving syntax highlighting instead of being truncated.

### Images

An image link that occupies **an entire paragraph on its own line** is embedded as an image rather than rendered as link text:

```org
[[file:./img/screenshot.png]]      ; Alone on a line → embed the image
[[file:./img/photo.jpg][Caption]]  ; Link with a description → still rendered as ordinary link text
[[file:./img/a.png]] Some text     ; Other text on the same line → rendered as an ordinary link
```

Text on the line after an image link is unaffected and continues to be laid out as a normal paragraph:

```org
[[file:./img/a.png]]
The runtime data areas are divided as follows:
```

Images can appear in body text or list items. In the body, they are centered and scaled proportionally to the text column width; images that are too tall are reduced to fit the page height and paginated automatically.

Relative paths are resolved against the directory containing the Org file, just as with `#+include:`. `.png`, `.jpg`, and `.jpeg` are supported. Links without the `file:` prefix, such as `[[./img/a.png]]`, are recognized as well.

The embedding strategy balances file size and fidelity:

- **JPEG** files are embedded directly from their original bytes (`/DCTDecode`), without re-encoding.
- **PNG files without transparency** (8/16-bit, non-interlaced) reuse their compressed data directly (`/FlateDecode` + `/Predictor 15`), without recompression.
- **PNG files with transparency** (grayscale + alpha or RGBA) must be split into a color image and a transparency mask (`SMask`), because PDF has no corresponding color space. This requires decoding the pixels. Since Emacs can decompress but cannot recompress them, these images are written in **uncompressed form**. To control file size, decoded pixels are **box-downsampled**; the maximum long-edge dimension is set by `org-pdf-image-max-pixels` (1600 by default; set it to `nil` to disable the limit). Pixel decoding uses a byte-by-byte Elisp loop, so **you should load this package in compiled form** (see “Performance” below); otherwise, a large image may take tens of seconds to process.

Interlaced (Adam7) PNG files are not yet supported. They are skipped with a warning.

### SVG

`.svg` images are rendered as **vector** PDF Form XObjects and embedded without loss of quality. Rendering is performed by a small helper program under `java/` (Batik parses the SVG, and PDFBox converts drawing instructions into PDF operators). The package then merges the helper's output into the document using its own object numbering.

Build the helper once before use:

```sh
cd java && mvn package        # Creates java/target/org-pdf-svg.jar
```

JDK 8+ and Maven are required. If `JAVA_HOME` points to a JRE, the build will fail with “No compiler is provided.” You can explicitly select a JDK:

```sh
JAVA_HOME=$(/usr/libexec/java_home -v 1.8) mvn package   # macOS
```

At runtime, `java` must be available on `PATH`. Override the JAR path with `org-pdf-svg-java-jar`, and the command with `org-pdf-svg-java-program`. If the JAR is missing, SVG images are skipped with a warning and the rest of the document is still exported.

Supported features include nested `<g>` elements; `transform` (matrix/translate/scale/rotate); `<use>` references to fragments in `<defs>`; `<path>` (including Bézier curves and arcs); `rect`, `circle`, `ellipse`, `line`, `polyline`, and `polygon`; `clip-path`; fill and stroke styles; `<text>`/`<tspan>` (including Chinese; if an SVG's `font-family` is set to a font-file path, that font is registered automatically); and embedded raster images.

`<filter>`, `<mask>`, and gradients are not rendered yet. Each SVG is rendered only once per export, with a cache keyed by path and modification time.

### `#+include:` File Inclusion

Org's include syntax is supported. Relative paths are resolved recursively against the directory of **the file being included**. By default, headings in an included file are nested one level below the section where the include occurs. For example, top-level headings in a file included under a level-1 heading become level-2 headings:

```org
#+include: "./part.org"                       # Include a file normally
#+include: "./code.py" :lines "10-25"         # Include only lines 10–25
#+include: "./code.py" src python             # Include as a Python source block (with syntax highlighting)
#+include: "./part.org" :minlevel 3           # Explicitly set the starting level (overrides parent level + 1)
```

Circular includes are detected and skipped without an error or infinite loop.

## Fonts

- **Latin text** uses PDF's built-in Base-14 fonts (Helvetica / Times / Courier), which are **not embedded**. A PDF containing only English text is therefore only a few kilobytes.
- **Chinese/CJK text** is taken from system fonts and embedded as a **subset** in Type0/CIDFontType2 form. Only glyphs actually used in the document are embedded; a Chinese document is typically around 50 KB.
- Embedded CJK fonts include a **ToUnicode CMap**, which maps each subset glyph ID back to its Unicode code point. This allows Chinese text in the PDF to be **copied, searched, and extracted correctly** (the `uni` column in `pdffonts` should say `yes`). Without this map, a viewer can display glyph outlines but cannot recover the underlying characters, so copied text may be blank or garbled.

See `org-pdf--cjk-font-candidates` for font lookup order; it lists common paths for macOS, Windows, and Linux. To specify a font manually, set `org-pdf-cjk-font-file`.

> Note: Only fonts with **TrueType outlines** (the `glyf`/`loca` tables) are supported. macOS Hiragino and PingFang fonts use CFF/OTTO outlines and are skipped automatically. The candidate list prioritizes Songti, STHeiti, Arial Unicode, and similar fonts.

## Customization

```elisp
(setq org-pdf-page-size 'a4)           ; a4 / letter / a3 / legal, or (width . height)
(setq org-pdf-base-font-size 11.0)     ; Body font size (pt)
(setq org-pdf-head-font 'helvetica)    ; helvetica or times
(setq org-pdf-cjk-font-file "/path/to/font.ttf")
(setq org-pdf-number-headings t)       ; Number heading levels (also reflected in PDF bookmarks)
(setq org-pdf-syntax-highlight t)      ; Syntax highlighting for code blocks (using font-lock)
(setq org-pdf-image-max-pixels 1600)   ; Maximum long-edge size when downsampling transparent PNGs; nil means unlimited
(setq org-pdf-svg-java-jar "/path/to/org-pdf-svg.jar")  ; SVG rendering helper JAR
(setq org-pdf-svg-java-program "java")  ; Command used to run the helper
(setq org-pdf-open-after-export nil)    ; Do not open the PDF automatically after export
```

## Performance

Decoding PNG transparency (removing row filters and box-downsampling) uses a byte-by-byte Elisp loop. **Interpreted execution is an order of magnitude slower**: a 2984×1552 image takes about 33 seconds when interpreted and about 2.7 seconds after byte-compilation. Load the package in compiled form:

```sh
emacs --batch -L /path/to/org-pdf -f batch-byte-compile /path/to/org-pdf/*.el
```

If `native-compile` is enabled, `.el` files are compiled to native code automatically and run faster. After modifying the source, remember to delete old `.elc` files (Emacs will warn that the “source file is newer than byte-compiled file”).

Each SVG requires starting a JVM (about 0.5–1.5 seconds per image), but each image is rendered only once during a single export.

## File Structure

| File | Responsibility |
|------|------|
| `org-pdf-pdf.el` | PDF object/stream/xref/trailer writer; binary-safe |
| `org-pdf-font.el` | Base-14 metrics; TrueType/TTC parsing, subsetting, Type0 embedding, and ToUnicode CMap |
| `org-pdf-image.el` | PNG/JPEG parsing and embedding (direct IDAT pass-through, alpha separation + downsampling); SVG dimension reading and helper invocation |
| `org-pdf-layout.el` | Org syntax tree → drawing commands (line wrapping, pagination, tables, images) |
| `org-pdf.el` | Commands, font discovery, content-stream generation, and assembly |
| `java/` | Batik + PDFBox SVG → PDF Form XObject renderer (`SvgRenderer.java`) |

## Known Limitations

- Font parsing supports only TrueType outlines; embedding CFF/OTTO fonts is not supported.
- Base-14 fonts cover Latin characters only (WinAnsi). All other text uses the embedded CJK font, which must contain every character used in the document in its `cmap`.
- List-item markers appear on the same line as the heading/term. Descriptions (or the `def` part of `term :: def`) wrap to subsequent lines with a hanging indent aligned to the start of the term.
- Interlaced (Adam7) PNGs and 16-bit PNGs are not yet supported. PNGs with transparency are embedded uncompressed (see “Images”), so many large images can significantly increase PDF size (some documents can reach 166 MB for this reason).
- SVG support requires a JDK and the JAR built once in advance. `<filter>`, `<mask>`, and gradients are not rendered.
- TOCs, footnotes, link annotations, and mathematical formulas are not supported.
- Tables do not support merged cells spanning multiple rows or columns.
