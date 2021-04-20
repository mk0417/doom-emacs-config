;;; p-abbrev.el -*- coding: utf-8; lexical-binding: t; -*-

;; define global abbrev
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ("afaik" "As far as i know" )
    ("atm" "at the moment" )
    ("btw" "By the way," )
    ("eq" "==" )
    ("hr" "--------------------------------------------------" )
    ("wrdspgcon" "psql postgresql://lubspl12@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require")
    ("wrdspgtablequery" "select table_name, table_schema, table_type from information_schema.tables where table_name='crsp';")
    ("wrdspgtable" "\\dtv crsp.*")
    ("wrdspgschema" "select schema_name from information_schema.schemata order by schema_name limit 10;")
    ))

;; define major mode abbrev
(when (boundp 'org-mode-abbrev-table)
  (clear-abbrev-table org-mode-abbrev-table))
(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("tit" "#+TITLE:
#+SUBTITLE:
#+AUTHOR: Peng Li
#+OPTIONS: toc:1
#+HTML_HEAD: <style>
#+HTML_HEAD: #table-of-contents {position: fixed; left: 0; top: 10; width: 200px; height: 100%;}
#+HTML_HEAD: body {max-width: 700px; margin: auto; padding: 10px; font-size: 16px; line-height: 1.5; -webkit-font-smoothing: antialiased; font-family: -apple-system, BlinkMacSystemFont;}
#+HTML_HEAD: .subtitle, p, ol, ul {font-size: 13px;}
#+HTML_HEAD: code {font-weight: bold; background-color: #fff8dc}
#+HTML_HEAD: </style>
")
    ("pysrc" "#+begin_src jupyter-python :session py :eval no-export
#+end_src
")
    ("shsrc" "#+begin_src sh
#+end_src
")
    ("stasrc"  "#+begin_src jupyter-stata :session stata :kernel stata :eval no-export
#+end_src")
    ("tab" "|   |   |
|---+---|
|   |   |
")
    ))

(setq save-abbrevs nil)
