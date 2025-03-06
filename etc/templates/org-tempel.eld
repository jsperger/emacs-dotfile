org-mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " q n> r> n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: John Sperger" n "#+language: en")
(pikchr "#+begin_src pikchr :file " p".svg" n
n
"#+end_src" n)
(preamble-r
  "#+TITLE:" n
  "#+AUTHOR: John Sperger" n
  "#+email: jsperger@live.unc.edu" n
  "#+BABEL: :session *R* :cache yes :results output graphics :exports both :tangle yes" n
  "-----"
  )

;; Beamer - TODO: Figure out how to use latex inserts in org
;; to avoid copy pasting
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

