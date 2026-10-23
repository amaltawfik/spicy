# the Typst source of a table with a long level label

    Code
      cat(.pt_typst(table_categorical(.pt_long_level(), parents, by = grp, output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Categorical table by grp]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 2, "4_0": 2
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
          ([], [parents], [At least one parent holds a university degree], [None]),
          ([n], [  ], [14], [ 6]),
          ([%], [    ], [70.0], [30.0]),
          ([n], [  ], [10], [10]),
          ([%], [    ], [50.0], [50.0]),
          ([n], [  ], [24], [16]),
          ([%], [    ], [60.0], [40.0]),
          ([], [.197], [    ], [    ]),
          ([], [.20], [   ], [   ]),
        )
      
        #context layout(size => {
          // spicy: an indented cell is measured with its indent
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            let spicy_style = get-style(spicy_x, spicy_k + 1)
            if spicy_style != none and "indent" in spicy_style { pad(left: spicy_style.indent, spicy_cell) } else { spicy_cell }
          }))
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]), (7, 7, [p]), (8, 8, [Phi]),) {
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
       table.hline(y: 5, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total], [p], [Phi],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [parents], [  ], [    ], [  ], [    ], [  ], [    ], [.197], [.20],
      [At least one parent holds a university degree], [14], [70.0], [10], [50.0], [24], [60.0], [    ], [   ],
      [None], [ 6], [30.0], [10], [50.0], [16], [40.0], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] Group comparison: Pearson chi\-squared test.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

