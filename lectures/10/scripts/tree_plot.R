library(DiagrammeR)

grViz("
      graph mygraph {
        node [label=''];
        a -- b [xlabel='x1 < a'];
        c [label='x'];
        a -- c;
      }
      ")

grViz("
      graph mygraph {
        node [label=''];
        a -- b [xlabel='x1 < a'];
        c [label='x'];
        a -- c;
        b -- d [xlabel='x2 < b'];
        d [label='x'];
        b -- e;
        f [label='o'];
        e -- f;
        e -- g [label='x2 > c'];
        g -- h;
        i [label='o'];
        h [label='x'];
        g -- i [label='x1 > d'];
      }
      ")

grViz("
      graph mygraph {
      a [label='1'];
      b [label='2'];
      a -- b [xlabel='x1 < a'];
      c [label='3'];
      a -- c;
      b -- d [xlabel='x2 < b'];
      d [label='4'];
      e [label='5'];
      b -- e;
      f [label='10'];
      e -- f;
      g [label='11'];
      e -- g [label='x2 > c'];
      g -- h;
      i [label='23'];
      h [label='22'];
      g -- i [label='x1 > d'];
      }
      ")
