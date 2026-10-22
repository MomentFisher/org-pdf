# org-pdf — 纯 Elisp 的 Org → PDF 导出

不依赖 LaTeX、pandoc、无头浏览器或**任何**外部程序(不完全是，处理svg图片的时候用到了JDK)。整个 PDF 由 Emacs 自己逐字节写出。
在 macOS / Windows / Linux 上、Emacs 30+ 通用。

## 为什么

Emacs 自带的 `org-latex-export-to-pdf` 本质是「org → .tex → 调 `pdflatex`」。没有 TeX 环境就用不了。
org-pdf 绕开整条 LaTeX 链路，用 `org-element-parse-buffer` 解析语法树，自己在 Elisp 里做排版
（断行、分页、中文逐字换行、表格布局），最后直接生成 PDF 文件。

## 安装

把 `org-pdf/` 目录加入 `load-path`，然后：

```elisp
(add-to-list 'load-path "/path/to/org-pdf")
(require 'org-pdf)
```

## 使用

```
M-x org-pdf-export            ; 导出为同名 PDF，并自动打开
M-x org-pdf-export-to-file   ; 指定输出路径
```

**集成到 org 导出调度器**：加载后 `C-c C-e` 菜单里会出现 `p` 项：

```
C-c C-e p p    ; 导出为 PDF（org-pdf）
C-c C-e p o    ; 导出为 PDF 并打开
```

> 顶层键用**小写 `p`**，因为大写 `P` 是 org 内置的 Publish 菜单，占用会导致
> `org-export-dispatch: Symbol's function definition is void: nil`。

**导出子树**：用 scope 前缀 `C-s` 把导出范围限定为当前子树，或在 `M-x org-pdf-export`
前加前缀参数（`C-u`）：

```
C-c C-e C-s p p          ; 导出当前子树为 PDF
C-u M-x org-pdf-export   ; 同上
```

子树导出时的输出文件名规则，与 org 一致：**优先用该标题的 `:EXPORT_FILE_NAME:` 属性**
（相对当前文件目录解析），没有该属性时回退为 `<文件名>_<子树标题>.pdf`：

```org
* Java Agent
:PROPERTIES:
:EXPORT_FILE_NAME: ./JavaAgentDynamicLog
:END:
```
→ 导出为 `JavaAgentDynamicLog.pdf`。

子树的根标题会成为**导出文档的标题**（按 `#+TITLE:` 的样式渲染），其下各级标题在 PDF 中
从最顶级开始编号（也就是根标题下的第一个子标题编号为 `1`，不会出现 `0.1` 或让根标题占用
`1`）。

（若菜单里没出现，在 init 里调用一次 `(org-pdf-register-backend)`，或把 `org-pdf` 加入
`org-export-backends` 后重启。）

命令行批处理：

```sh
emacs --batch -L /path/to/org-pdf -l org-pdf \
  --eval '(org-pdf-export-file "in.org" "out.pdf")'
```

## 支持的元素

标题（多级字号 + 层级编号 + PDF 书签）、段落、有序/无序/嵌套列表、复选框、引用块、居中/诗行块、
src/example 代码块（等宽 + 浅色背景 + **语法高亮**）、表格（边框 + 单元格换行）、水平线、
**图片**（PNG / JPEG / SVG）、标题块（`#+TITLE:` / `#+AUTHOR:` / `#+DATE:`）、行内 `*bold*` `/italic/`
`=verbatim=` `~code~`，以及 **`#+include:` 子文件包含**。

标题会生成层级化的 **PDF 书签（大纲）**，并在标题前加上章节编号（1、1.1、1.1.1 …）。编号同时
出现在正文标题和书签里，可用 `org-pdf-number-headings` 关闭。

代码块用 **Emacs 自己的 font-lock** 着色——只要该语言的 major mode 已安装，任意语言都能自动
高亮。常见的 font-lock face（关键字、字符串、注释、函数名、类型、常量等）映射到一组固定
配色，见 `org-pdf--face-colors`。可用 `org-pdf-syntax-highlight` 关闭。

超过页面宽度的代码行会**自动折行**（在字符边界断开，颜色高亮保留），不会被截断。

### 图片

一个段落中**独占一行**的图片链接会被当作图片嵌入，而不是渲染成链接文字：

```org
[[file:./img/screenshot.png]]      ; 独占一行 → 嵌入图片
[[file:./img/photo.jpg][说明文字]]  ; 带描述的链接 → 仍按普通链接文字渲染
[[file:./img/a.png]] 说明文字       ; 同一行还有文字 → 按普通链接渲染
```

图片链接后另起一行的正文不受影响，会作为普通段落继续排版：

```org
[[file:./img/a.png]]
运行时数据区划分如下：
```

图片放在正文或列表项里都可以（正文居中、按文本列宽等比缩放，过高则缩到页高并自动分页）。
相对路径以 org 文件所在目录为基准解析，与 `#+include:` 一致；`.png` / `.jpg` / `.jpeg` 都支持。
没有 `file:` 前缀的写法（`[[./img/a.png]]`）同样识别。

嵌入方式对体积和保真度做了取舍：

- **JPEG** 以原始字节直接嵌入（`/DCTDecode`），不重新编码。
- **不带透明通道的 PNG**（8/16 位、非隔行）也直接复用其压缩数据（`/FlateDecode` +
  `/Predictor 15`），不重新压缩。
- **带透明通道的 PNG**（灰+alpha 或 RGBA）PDF 没有对应的色彩空间，必须拆成「颜色图 + 透明蒙版
  (SMask)」。这需要解码像素，而 Emacs 只能解压不能重新压缩，所以这类图**以未压缩形式**写入；
  为控制体积，解码后的像素会**盒式降采样**，长边限制在 `org-pdf-image-max-pixels`（默认 1600，
  设为 `nil` 关闭）。像素解码是逐字节的 Elisp 循环，**务必以编译形式加载本包**（见下方
  「性能」），否则一张大图可能要几十秒。

隔行（Adam7）PNG 暂不支持，会被跳过并给出告警。

### SVG

`.svg` 图片会被渲染成**矢量**的 PDF Form XObject 嵌入，不失真。渲染由 `java/` 下的一个小
辅助程序完成（Batik 负责解析 SVG，PDFBox 把绘制指令转成 PDF 算子），插件把它的输出按自己的
对象编号重新拼进文档。

使用前需要一次性构建：

```sh
cd java && mvn package        # 生成 java/target/org-pdf-svg.jar
```

要求 JDK 8+ 与 Maven。若 `JAVA_HOME` 指向 JRE，构建会报「No compiler is provided」，
可显式指定：

```sh
JAVA_HOME=$(/usr/libexec/java_home -v 1.8) mvn package   # macOS
```

运行时 `java` 必须在 `PATH` 中；jar 路径可用 `org-pdf-svg-java-jar` 覆盖，命令可用
`org-pdf-svg-java-program` 覆盖。jar 不存在时 SVG 会被跳过并给出告警，其余内容照常导出。

已覆盖的能力：`<g>` 嵌套、`transform`（matrix/translate/scale/rotate）、`<use>`
引用 `<defs>` 片段、`<path>`（含贝塞尔与圆弧）、`rect`/`circle`/`ellipse`/`line`/
`polyline`/`polygon`、`clip-path`、填充与描边样式、`<text>`/`<tspan>`（含中文；
SVG 的 `font-family` 写成字体文件路径时会自动注册该字体）、内嵌位图。
`<filter>`/`<mask>` 与渐变暂不渲染。同一张 SVG 在单次导出中只渲染一次（按路径与修改时间缓存）。


### `#+include:` 子文件包含

支持 org 的 include 语法，相对路径以**被包含文件所在目录**为基准递归解析。被包含文件的
标题默认**嵌套在包含位置所在章节的下一级**（例如 level-1 标题下 include 的文件，其顶层
标题会变成 level-2）：

```org
#+include: "./part.org"                       # 普通包含
#+include: "./code.py" :lines "10-25"         # 只取第 10–25 行
#+include: "./code.py" src python             # 作为 python 代码块（带语法高亮）
#+include: "./part.org" :minlevel 3           # 显式指定层级起点（覆盖默认的父级+1）
```

循环包含会被检测并跳过（不报错、不死循环）。

## 字体

- **拉丁文本**用 PDF 内置的 base-14 字体（Helvetica / Times / Courier），**不嵌入**，
  所以纯英文文档生成的 PDF 只有几 KB。
- **中文/CJK** 从系统字体里取，**子集化**后以 Type0/CIDFontType2 嵌入——只嵌入文档里
  真正用到的字形，一个中文文档通常 50KB 左右。
- 嵌入的 CJK 字体附带一张 **ToUnicode CMap**，把每个子集字形编号映射回对应的 Unicode
  码点，因此 PDF 里的中文可以**正常复制、搜索和提取**（`pdffonts` 的 `uni` 列为 `yes`）。
  没有这张表，阅读器只能看到字形轮廓而无法还原字符，复制出来就是空白或乱码。

字体查找顺序见 `org-pdf--cjk-font-candidates`（macOS / Windows / Linux 各列了常见路径）。
若需指定，设 `org-pdf-cjk-font-file`。

> 注意：只支持 **TrueType 轮廓**的字体（带 `glyf`/`loca` 表）。macOS 的 Hiragino、PingFang
> 是 CFF/OTTO 轮廓，会被自动跳过；候选列表里优先选 Songti / STHeiti / Arial Unicode 等。

## 自定义

```elisp
(setq org-pdf-page-size 'a4)          ; a4 / letter / a3 / legal 或 (宽 . 高)
(setq org-pdf-base-font-size 11.0)    ; 正文字号（pt）
(setq org-pdf-head-font 'helvetica)   ; helvetica 或 times
(setq org-pdf-cjk-font-file "/path/to/font.ttf")
(setq org-pdf-number-headings t)      ; 标题层级编号（同时也进 PDF 书签）
(setq org-pdf-syntax-highlight t)     ; 代码块语法高亮（用 font-lock）
(setq org-pdf-image-max-pixels 1600)  ; 带透明通道 PNG 降采样长边上限；nil 表示不限制
(setq org-pdf-svg-java-jar "/path/to/org-pdf-svg.jar")  ; SVG 渲染辅助程序
(setq org-pdf-svg-java-program "java") ; 运行该程序的命令
(setq org-pdf-open-after-export nil)  ; 导出后不自动打开
```

## 性能

PNG 的透明通道解码（去掉行过滤器、盒式降采样）是逐字节的 Elisp 循环，**解释执行会慢一个
数量级**：一张 2984×1552 的图解释执行约 33 秒，字节编译后约 2.7 秒。请以编译形式加载：

```sh
emacs --batch -L /path/to/org-pdf -f batch-byte-compile /path/to/org-pdf/*.el
```

若启用 `native-compile`，`.el` 会自动编译成原生代码，更快。修改源码后记得删掉旧的 `.elc`
（Emacs 会提示「source file is newer than byte-compiled file」）。

SVG 每张需要启动一次 JVM（约 0.5–1.5 秒/张），同一次导出中每张图只渲染一次。

## 文件结构

| 文件 | 职责 |
|------|------|
| `org-pdf-pdf.el` | PDF 对象/流/xref/trailer 写入器，二进制安全 |
| `org-pdf-font.el` | base-14 度量表；TrueType/TTC 解析、子集化、Type0 嵌入、ToUnicode CMap |
| `org-pdf-image.el` | PNG/JPEG 解析与嵌入（IDAT 直通、alpha 拆分 + 降采样）；SVG 尺寸读取与辅助程序调用 |
| `org-pdf-layout.el` | org 语法树 → 绘制指令（断行、分页、表格、图片） |
| `org-pdf.el` | 命令、字体发现、内容流生成、总装 |
| `java/` | Batik + PDFBox 的 SVG → PDF Form XObject 渲染器（`SvgRenderer.java`） |

## 已知限制

- 字体解析只支持 TrueType 轮廓（不支持 CFF/OTTO 字体嵌入）。
- base-14 字体只覆盖拉丁字母（WinAnsi），其他文字一律走嵌入的 CJK 字体，需要文档里
  用到的字符在该字体的 `cmap` 里存在。
- 列表项的标识与标题同行，描述（或 `term :: def` 中的 `def`）换行到下一行并悬挂缩进到标题起始处。
- 隔行（Adam7）PNG 和 16 位 PNG 暂不支持；带透明通道的 PNG 以未压缩形式嵌入（见「图片」），
  大量大图会使 PDF 体积显著增大（166 MB 级别的文档即由此产生）。
- SVG 需要 JDK 与一次性构建的 jar；`<filter>`、`<mask>` 与渐变不渲染。
- 不支持 TOC、脚注、链接注解、数学公式。
- 表格不支持跨行/跨列单元格合并。
