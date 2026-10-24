# row percentages and the Total row reach the Typst cells and the note

    Code
      cat(tinytable::save_tt(tt, output = "typst"), sep = "\n")
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Categorical table by group]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "6_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "6_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 2, "4_0": 2, "5_0": 2, "6_0": 2
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
          (align: left, indent: 1em,),
        )
      
        // Helper function to get cell style
        #let get-style(x, y) = {
          let key = str(y) + "_" + str(x)
          if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
        }
      
        #show table.cell: it => {
          if style-array.len() == 0 { return it }
          
          let style = get-style(it.x, it.y)
          if style == none { return it }
          
          let tmp = it
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
          if ("smallcaps" in style) { tmp = smallcaps(tmp) }
          tmp
        }
      
        // tinytable align-figure before
        #let tinytable-coldata = (
          ([], [level], [low], [mid], [high], [Total]),
          ([n], [  ], [10], [10], [16], [36]),
          ([%], [    ], [38.5], [37.0], [59.3], [45.0]),
          ([n], [  ], [16], [17], [11], [44]),
          ([%], [    ], [61.5], [63.0], [40.7], [55.0]),
          ([n], [  ], [26], [27], [27], [80]),
          ([%], [     ], [100.0], [100.0], [100.0], [100.0]),
          ([], [.186], [    ], [    ], [    ], [    ]),
          ([], [.20], [   ], [   ], [   ], [   ]),
        )
      
        #context layout(size => {
          // spicy: an indented cell is measured with its indent
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            let spicy_style = get-style(spicy_x, spicy_k + 1)
            if spicy_style != none and "indent" in spicy_style { pad(left: spicy_style.indent, spicy_cell) } else { spicy_cell }
          }))
          // spicy: a cell that spans columns is measured with none of them
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            if (spicy_x, spicy_k) in ((0, 1),) { [] } else { spicy_cell }
          }))
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]), (7, 7, [p]), (8, 8, [V]),) {
            let spicy_need = measure(grid(columns: 1, inset: table.inset, spicy_label)).width
            let spicy_have = tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()
            if spicy_need > spicy_have {
              for k in range(spicy_from, spicy_to + 1) {
                tinytable-naturals.at(k) += (spicy_need - spicy_have) / (spicy_to - spicy_from + 1)
              }
            }
          }
          let tinytable-total = calc.min(tinytable-naturals.sum(), size.width)
          block(width: tinytable-total)[
      
        #table( // tinytable table start
          columns: tinytable-naturals.map(w => w.pt() * 1fr),
          stroke: none,
          rows: auto,
          align: (x, y) => {
            let style = get-style(x, y)
            if style != none and "align" in style { style.align } else { left }
          },
          fill: (x, y) => {
            let style = get-style(x, y)
            if style != none and "background" in style { style.background }
          },
       table.hline(y: 1, start: 1, end: 7, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 7, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total], [p], [V],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[level], [.186], [.20],
      [low], [10], [38.5], [16], [61.5], [26], [100.0], [    ], [   ],
      [mid], [10], [37.0], [17], [63.0], [27], [100.0], [    ], [   ],
      [high], [16], [59.3], [11], [40.7], [27], [100.0], [    ], [   ],
      [Total], [36], [45.0], [44], [55.0], [80], [100.0], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] Percentages are row percentages. P\-value: Pearson chi\-squared test. V \= Cramer's V.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# p_adjust under holm is pinned, in English and in French

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Categorical table by am
      
       Variable   │  0 n    0 %     1 n    1 %     Total n    Total %      p      Effect size  
      ────────────┼────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       .018        .52      
         4        │   3     15.8     8     61.5      11        34.4                            
         6        │   4     21.1     3     23.1       7        21.9                            
         8        │  12     63.2     2     15.4      14        43.8                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <.001        .81      
         3        │  15     78.9     0      0.0      15        46.9                            
         4        │   4     21.1     8     61.5      12        37.5                            
         5        │   0      0.0     5     38.5       5        15.6                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       .341        .17      
         0        │  12     63.2     6     46.2      18        56.2                            
         1        │   7     36.8     7     53.8      14        43.8                            
      
      Note. P-values: Pearson chi-squared test (vs); Fisher exact test (cyl, gear). P-values adjusted for 3 tests (Holm method). Cramer's V: cyl, gear. Phi: vs.

---

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Tableau des variables catégorielles selon am
      
       Variable   │  0 n    0 %     1 n    1 %     Total n    Total %      p       Taille d'effet  
      ────────────┼────────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       0,018         0,52       
         4        │   3     15,8     8     61,5      11        34,4                                
         6        │   4     21,1     3     23,1       7        21,9                                
         8        │  12     63,2     2     15,4      14        43,8                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <0,001         0,81       
         3        │  15     78,9     0      0,0      15        46,9                                
         4        │   4     21,1     8     61,5      12        37,5                                
         5        │   0      0,0     5     38,5       5        15,6                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       0,341         0,17       
         0        │  12     63,2     6     46,2      18        56,2                                
         1        │   7     36,8     7     53,8      14        43,8                                
      
      Note. Valeurs p : test du khi-deux de Pearson (vs) ; test exact de Fisher (cyl, gear). Valeurs p ajustées pour 3 tests (méthode de Holm). V de Cramér : cyl, gear. Phi : vs.

# p_value = FALSE with assoc_measure = "none": a purely descriptive table

    Code
      print(tbl)
    Output
      Categorical table by sex
      
       Variable            │  Female n    Female %    Male n    Male %    Total n    Total %  
      ─────────────────────┼──────────────────────────────────────────────────────────────────
       smoking             │                                                                  
         No                │    475         76.6       451       77.8       926       77.2    
         Yes               │    131         21.1       118       20.3       249       20.8    
         (Missing)         │     14          2.3        11        1.9        25        2.1    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       education           │                                                                  
         Lower secondary   │    134         21.6       127       21.9       261       21.8    
         Upper secondary   │    290         46.8       249       42.9       539       44.9    
         Tertiary          │    196         31.6       204       35.2       400       33.3    

