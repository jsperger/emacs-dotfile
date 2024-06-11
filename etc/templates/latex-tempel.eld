LaTeX-mode latex-mode org-mode markdown-mode


;; TODO: figure out how to make evil and tempel play nice
;; Issue: I'm inserting templates while in visual model
;; and can't edit a template after inserting until entering insert mode.
;; This is an issue because it seems to create multiple cursors.
;; also ugly text colors
;; Example: (section "\\section{" p "}")
;; Expected: \section{<type_here>}
;; Result: \section{normal-mode with second second cursor for typing}
(section "\\section{}")
(subsection "\\subsection{}")
(ssection "\\subsection{}")
(frame "\\begin{frame}{}" r> n> "\\end{frame}")
(vf "\\vfill \n")
(vp "\\vfill \\pause \n")
;; taken from https://github.com/minad/tempel/blob/5b09f612cfd805dba5e90bf06580583cab045499/README.org#template-file-format
;; part of tempel-collection but it doesn't want to recognize
;; Auctex's LaTeX-mode as latex-mode for some reason
(abstract "\\begin{abstract}\n" r> n> "\\end{abstract}")
(align "\\begin{align}\n" r> n> "\\end{align}")
(alignn "\\begin{align*}\n" r> n> "\\end{align*}")
(gather "\\begin{gather}\n" r> n> "\\end{gather}")
(gatherr "\\begin{gather*}\n" r> n> "\\end{gather*}")
(appendix "\\begin{appendix}\n" r> n> "\\end{appendix}")
(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(center "\\begin{center}\n" r> n> "\\end{center}")
(displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}")
(document "\\begin{document}\n" r> n> "\\end{document}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(equation "\\begin{equation}" r> n> "\\end{equation}")
(flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}")
(flushright "\\begin{flushright}" r> n> "\\end{flushright}")
(frac "\\frac{" p "}{" q "}")
(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")
(letter "\\begin{letter}\n" r> n> "\\end{letter}")
(math "\\begin{math}\n" r> n> "\\end{math}")
(minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")
(quotation "\\begin{quotation}\n" r> n> "\\end{quotation}")
(quote "\\begin{quote}\n" r> n> "\\end{quote}")
(sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")
(theindex "\\begin{theindex}\n" r> n> "\\end{theindex}")
(trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}")
(verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}")
(verbatimm "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

;; Beamer
(columns "\\begin{columns}
\\begin{column}{0.5\\textwidth}
\\end{column}
\\hfill
\\begin{column}{0.5\\textwidth}
\\end{column}
\\end{columns}")

(col-img-r "\\begin{columns}
\\begin{column}{0.5\\textwidth}
\\end{column}
\\begin{column}{0.5\\textwidth}
    \\begin{center}
     \\includegraphics[width=\\textwidth]{}
     \\end{center}
\\end{column}
\\end{columns}")

(col-img-l "\\begin{columns}
\\begin{column}{0.5\\textwidth}
    \\begin{center}
     \\includegraphics[width=\\textwidth]{}
     \\end{center}
\\end{column}
\\begin{column}{0.5\\textwidth}
\\end{column}
\\end{columns}")

