library(DiagrammeR)

grViz("
      digraph mygraph {
        node [shape=box];
        //rankdir='LR';
        a [label='RStudio'];
        a -> b;
        b [label='R (Rmarkdown)'];
        b -> c;
        c [label='Pandoc'];
        c -> d [label='Язык-посредник (LaTeX)'];
        d [label='шаблон документа' style=dashed];
        d -> e;
        e [label='Целевой формат (HTML,DOCX,...)'];
      }
      ")
