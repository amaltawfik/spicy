# the Typst source of a French note opened by the colon lead-in

    Code
      cat(.pt_nbsp(.pt_typst(table_categorical(d, x, by = g, style = spicy_style(
        note_label = "Note:"), output = "tinytable"))))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Tableau des variables catégorielles selon g]),
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
          ([], [x], [non], [oui]),
          ([n], [  ], [10], [10]),
          ([%], [    ], [50,0], [50,0]),
          ([n], [  ], [13], [ 7]),
          ([%], [    ], [65,0], [35,0]),
          ([n], [  ], [23], [17]),
          ([%], [    ], [57,5], [42,5]),
          ([], [0,337], [     ], [     ]),
          ([], [0,15], [    ], [    ]),
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
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]), (7, 7, [p]), (8, 8, [φ]),) {
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
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total], [p], [φ],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[x], [0,337], [0,15],
      [non], [10], [50,0], [13], [65,0], [23], [57,5], [     ], [    ],
      [oui], [10], [50,0], [ 7], [35,0], [17], [42,5], [     ], [    ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note<U+00A0>:] N \= 40<U+00A0>: A (n \= 20), B (n \= 20). Pourcentages en colonne. Valeur p<U+00A0>: test du khi\-deux de Pearson.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a note that defines every abbreviation

    Code
      cat(.pt_nbsp(.pt_typst(table_continuous_lm(d, y, by = g, effect_size = "omega2",
        style = spicy_style(abbreviations = "all"), output = "tinytable"))))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Variables dépendantes continues selon g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "0_0": 1, "1_0": 1, "2_0": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([], [y]),
          ([A], [53,46]),
          ([B], [48,80]),
          ([C], [44,73]),
          ([], [0,216]),
          ([], [0,11]),
          ([], [0,04]),
          ([], [30]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 3, [M]), (4, 4, [p]), (5, 5, [R²]), (6, 6, [ω²]), (7, 7, [n]),) {
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
       table.hline(y: 1, start: 1, end: 4, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 8, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 8, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 8, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 3, align: center)[M], [p], [R²], [ω²], [n],
      [], [A], [B], [C], [], [], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [53,46], [48,80], [44,73], [0,216], [0,11], [0,04], [30],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 8, text(size: 0.9em, [#emph[Note.] N \= 30<U+00A0>: A (n \= 10), B (n \= 10), C (n \= 10). Valeur p<U+00A0>: test F issu du modèle linéaire. M \= moyenne<U+00A0>; R² \= coefficient de détermination<U+00A0>; ω² \= oméga\-carré.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

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
          // spicy: a cell that spans columns is measured with none of them
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            if (spicy_x, spicy_k) in ((0, 1),) { [] } else { spicy_cell }
          }))
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]), (7, 7, [p]), (8, 8, [φ]),) {
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
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total], [p], [φ],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[parents], [.197], [.20],
      [At least one parent holds a university degree], [14], [70.0], [10], [50.0], [24], [60.0], [    ], [   ],
      [None], [ 6], [30.0], [10], [50.0], [16], [40.0], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 40: A (n \= 20), B (n \= 20). Percentages are column percentages. P\-value: Pearson chi\-squared test.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a table whose variable label spans its groups

    Code
      cat(.pt_typst(table_categorical(.pt_long_group(), parents, by = grp, labels = c(
        parents = .pt_group_label), output = "tinytable")))
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
          ([], [Parents holding a university degree], [At least one], [None]),
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
          // spicy: a cell that spans columns is measured with none of them
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            if (spicy_x, spicy_k) in ((0, 1),) { [] } else { spicy_cell }
          }))
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]), (7, 7, [p]), (8, 8, [φ]),) {
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
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total], [p], [φ],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[Parents holding a university degree], [.197], [.20],
      [At least one], [14], [70.0], [10], [50.0], [24], [60.0], [    ], [   ],
      [None], [ 6], [30.0], [10], [50.0], [16], [40.0], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 40: A (n \= 20), B (n \= 20). Percentages are column percentages. P\-value: Pearson chi\-squared test.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a difference column, its order in the note

    Code
      cat(.pt_typst(table_continuous_lm(d, y, by = g, ci = FALSE, r2 = "none",
        show_n = FALSE, output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Continuous outcomes by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_0": 1, "1_0": 1, "2_0": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([], [y]),
          ([A], [55.40]),
          ([B], [48.30]),
          ([], [−7.10]),
          ([], [.237]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [M]), (3, 3, [Δ]), (4, 4, [p]),) {
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
       table.hline(y: 1, start: 1, end: 3, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 5, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[M], [Δ], [p],
      [], [A], [B], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [55.40], [48.30], [−7.10], [.237],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 5, text(size: 0.9em, [#emph[Note.] N \= 20: A (n \= 10), B (n \= 10). P\-value: t\-test from the linear model. Δ \= B − A.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of decked heads, M over the groups and the difference alone

    Code
      cat(.pt_typst(.pt_decked(output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Continuous outcomes by smoking]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([], [Well\-being score (WHO\-5 index)], [Perceived stress, PSS\-10 score]),
          ([Never smoker], [58.05], [19.38]),
          ([Current smoker], [65.96], [16.13]),
          ([], [ 7.91], [−3.26]),
          ([LL], [ 2.38], [−5.45]),
          ([UL], [13.44], [−1.06]),
          ([], [.006], [.004]),
          ([], [0.09], [0.10]),
          ([], [80], [80]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [M]), (3, 3, [Δ]), (4, 5, [95% CI]), (6, 6, [p]), (7, 7, [R²]), (8, 8, [n]),) {
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
       table.hline(y: 1, start: 1, end: 3, stroke: 0.06em), table.hline(y: 1, start: 4, end: 6, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 4, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[M], [Δ], table.cell(colspan: 2, align: center)[95% CI], [p], [R²], [n],
      [], [Never smoker], [Current smoker], [], [LL], [UL], [], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [Well\-being score (WHO\-5 index)], [58.05], [65.96], [ 7.91], [ 2.38], [13.44], [.006], [0.09], [80],
      [Perceived stress, PSS\-10 score], [19.38], [16.13], [−3.26], [−5.45], [−1.06], [.004], [0.10], [80],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 80: Never smoker (n \= 40), Current smoker (n \= 40). P\-values: t\-test from the linear model. Δ \= Current smoker − Never smoker. CI \= confidence interval; LL \= lower limit; UL \= upper limit.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of an association measure headed by its symbol

    Code
      cat(.pt_typst(table_categorical(d, x, by = g, assoc_measure = "cramer_v",
        output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Categorical table by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 2, "4_0": 2, "5_0": 2
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
          ([], [x], [hi], [lo], [mid]),
          ([n], [ ], [6], [8], [6]),
          ([%], [    ], [30.0], [40.0], [30.0]),
          ([n], [ ], [8], [3], [9]),
          ([%], [    ], [40.0], [15.0], [45.0]),
          ([n], [  ], [14], [11], [15]),
          ([%], [    ], [35.0], [27.5], [37.5]),
          ([], [.206], [    ], [    ], [    ]),
          ([], [.28], [   ], [   ], [   ]),
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
       table.hline(y: 6, start: 0, end: 9, stroke: 0.06em),
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
      table.cell(colspan: 7)[x], [.206], [.28],
      [hi], [6], [30.0], [8], [40.0], [14], [35.0], [    ], [   ],
      [lo], [8], [40.0], [3], [15.0], [11], [27.5], [    ], [   ],
      [mid], [6], [30.0], [9], [45.0], [15], [37.5], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 40: A (n \= 20), B (n \= 20). Percentages are column percentages. P\-value: Pearson chi\-squared test. V \= Cramér's V.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of the sample size, the base and the Total rule

    Code
      cat(typ)
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Categorical table by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 2, "4_0": 2, "5_0": 2, "6_0": 2
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
          ([], [x], [no], [yes], [(Missing)], [Total]),
          ([n], [  ], [ 6], [ 8], [ 1], [15]),
          ([%], [     ], [ 42.9], [ 57.1], [     ], [100.0]),
          ([n], [  ], [ 5], [ 8], [ 2], [15]),
          ([%], [     ], [ 38.5], [ 61.5], [     ], [100.0]),
          ([n], [  ], [11], [16], [ 3], [30]),
          ([%], [     ], [ 40.7], [ 59.3], [     ], [100.0]),
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
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [A]), (3, 4, [B]), (5, 6, [Total]),) {
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
       table.hline(y: 6, start: 0, end: 7, stroke: 0.03em),
       table.hline(y: 1, start: 1, end: 7, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 7, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 7, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[A], table.cell(colspan: 2, align: center)[B], table.cell(colspan: 2, align: center)[Total],
      [], [n], [%], [n], [%], [n], [%],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[x],
      [no], [ 6], [ 42.9], [ 5], [ 38.5], [11], [ 40.7],
      [yes], [ 8], [ 57.1], [ 8], [ 61.5], [16], [ 59.3],
      [(Missing)], [ 1], [     ], [ 2], [     ], [ 3], [     ],
      [Total], [15], [100.0], [15], [100.0], [30], [100.0],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 7, text(size: 0.9em, [#emph[Note.] N \= 30: A (n \= 15), B (n \= 15). Percentages are column percentages.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a note naming the test of its one p-value

    Code
      cat(.pt_typst(table_continuous(d, y, by = g, show_columns = c("m", "sd", "n"),
      output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "0_0": 1, "1_0": 1, "2_0": 1, "0_1": 1, "1_1": 1, "2_1": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([Variable], [y], []),
          ([Group], [A], [B]),
          ([M], [55.00], [44.58]),
          ([SD], [8.73], [8.47]),
          ([n], [12], [12]),
          ([p], [.007], [    ]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
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
       table.hline(y: 1, start: 0, end: 6, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 6, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 6, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [Group], [M], [SD], [n], [p],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [A], [55.00], [8.73], [12], [.007],
      [], [B], [44.58], [8.47], [12], [    ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 6, text(size: 0.9em, [#emph[Note.] N \= 24: A (n \= 12), B (n \= 12). P\-value: Welch t\-test.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a note listing a missing `by` last

    Code
      cat(.pt_typst(table_continuous(d, y, by = g, show_columns = c("m", "sd", "n"),
      p_value = FALSE, output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "0_1": 1, "1_1": 1, "2_1": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([Variable], [y], []),
          ([Group], [A], [B]),
          ([M], [56.00], [48.22]),
          ([SD], [ 9.25], [17.30]),
          ([n], [6], [9]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
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
       table.hline(y: 1, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 5, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [Group], [M], [SD], [n],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [A], [56.00], [ 9.25], [6],
      [], [B], [48.22], [17.30], [9],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 5, text(size: 0.9em, [#emph[Note.] N \= 18: A (n \= 9), B (n \= 9). Missing values removed: y (3), g (2).])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of the sample size of a continuous table by group

    Code
      cat(typ)
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "0_1": 1, "1_1": 1, "2_1": 1, "3_1": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([Variable], [y], [], []),
          ([Group], [A], [B], [(Missing)]),
          ([M], [55.33], [48.22], [52.50]),
          ([SD], [ 8.92], [17.30], [ 4.95]),
          ([n], [9], [9], [2]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
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
       table.hline(y: 1, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 4, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 5, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [Group], [M], [SD], [n],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [A], [55.33], [ 8.92], [9],
      [], [B], [48.22], [17.30], [9],
      [], [(Missing)], [52.50], [ 4.95], [2],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 5, text(size: 0.9em, [#emph[Note.] N \= 20: A (n \= 9), B (n \= 9), (Missing) (n \= 2).])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a note with a sentence added before the stars legend

    Code
      cat(.pt_typst(table_regression(.pt_note_add_fit(), stars = TRUE, note_add = "Data: simulated for this example.",
      output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Linear regression: y]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "7_5": 0, "8_5": 0, "9_5": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "5_0": 2, "6_0": 2
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
          ([], [(Intercept)], [x], [g:], [a (ref.)], [b], [n], [R²], [Adj. R²]),
          ([], [−0.29   ], [ 0.71\*\*\*], [], [ –      ], [ 0.34   ], [50      ], [ 0.29   ], [ 0.26   ]),
          ([], [0.22], [0.17], [], [–   ], [0.30], [], [], []),
          ([LL], [−0.72], [ 0.38], [], [ –   ], [−0.25], [], [], []),
          ([UL], [0.15], [1.04], [], [–   ], [0.94], [], [], []),
          ([], [ .190], [\<.001], [], [–    ], [ .254], [], [], []),
        )
      
        #context layout(size => {
          // spicy: an indented cell is measured with its indent
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            let spicy_style = get-style(spicy_x, spicy_k + 1)
            if spicy_style != none and "indent" in spicy_style { pad(left: spicy_style.indent, spicy_cell) } else { spicy_cell }
          }))
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 1, [B]), (2, 2, [SE]), (3, 4, [95% CI]), (5, 5, [p]),) {
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
       table.hline(y: 7, start: 0, end: 6, stroke: 0.03em + rgb("#cccccc")),
       table.hline(y: 1, start: 3, end: 5, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 6, stroke: 0.06em),
       table.hline(y: 10, start: 0, end: 6, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 6, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [B], [SE], table.cell(colspan: 2, align: center)[95% CI], [p],
      [], [], [], [LL], [UL], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [(Intercept)], [−0.29   ], [0.22], [−0.72], [0.15], [ .190],
      [x], [ 0.71\*\*\*], [0.17], [ 0.38], [1.04], [\<.001],
      [g:], [], [], [], [], [],
      [a (ref.)], [ –      ], [–   ], [ –   ], [–   ], [–    ],
      [b], [ 0.34   ], [0.30], [−0.25], [0.94], [ .254],
      [n], [50      ], [], [], [], [],
      [R²], [ 0.29   ], [], [], [], [],
      [Adj. R²], [ 0.26   ], [], [], [], [],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 6, text(size: 0.9em, [#emph[Note.] Linear regression. Std. errors: classical (OLS). CI \= confidence interval. Data: simulated for this example. \
      \*\*\* p \< .001, \*\* p \< .01, \* p \< .05.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a note naming a variable by its note label

    Code
      cat(.pt_typst(table_continuous(d, bmi, by = sex, labels = c(bmi = "Body mass index computed from self-reported height and weight",
        sex = "Which sex were you assigned at birth?"), note_labels = c(bmi = "BMI",
        sex = "sex at birth"), show_columns = c("m", "sd", "n"), p_value = FALSE,
      output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by sex at birth]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "0_1": 1, "1_1": 1, "2_1": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([Variable], [Body mass index computed from self\-reported height and weight], []),
          ([Group], [Female], [Male]),
          ([M], [26.15], [25.19]),
          ([SD], [4.12], [2.60]),
          ([n], [8], [9]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
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
       table.hline(y: 1, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 5, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 5, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [Group], [M], [SD], [n],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [Body mass index computed from self\-reported height and weight], [Female], [26.15], [4.12], [8],
      [], [Male], [25.19], [2.60], [9],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 5, text(size: 0.9em, [#emph[Note.] N \= 19: Female (n \= 9), Male (n \= 10). Missing values removed: BMI (2), sex at birth (1).])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a table with decimal places per variable

    Code
      cat(.pt_typst(table_continuous(d, select = c(x, y), by = g, digits = c(x = 0,
        y = 2), output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "0_9": 0, "1_9": 0, "2_9": 0, "3_9": 0, "4_9": 0, "5_9": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "0_1": 1, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([], [x], [], [y], []),
          ([], [A], [B], [A], [B]),
          ([], [170   ], [174   ], [ 23.06], [ 22.28]),
          ([], [5   ], [8   ], [2.53], [2.48]),
          ([], [160   ], [162   ], [ 19.56], [ 18.97]),
          ([], [178   ], [184   ], [ 26.92], [ 26.73]),
          ([LL], [166   ], [169   ], [ 21.24], [ 20.51]),
          ([UL], [174   ], [180   ], [ 24.87], [ 24.05]),
          ([], [10], [10], [10], [10]),
          ([], [.116], [    ], [.496], [    ]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 1, [Group]), (2, 2, [M]), (3, 3, [SD]), (4, 4, [Min]), (5, 5, [Max]), (6, 7, [95% CI]), (8, 8, [n]), (9, 9, [p]),) {
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
       table.hline(y: 4, start: 0, end: 10, stroke: 0.03em),
       table.hline(y: 1, start: 6, end: 8, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 10, stroke: 0.06em),
       table.hline(y: 6, start: 0, end: 10, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 10, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [Group], [M], [SD], [Min], [Max], table.cell(colspan: 2, align: center)[95% CI], [n], [p],
      [], [], [], [], [], [], [LL], [UL], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [x], [A], [170   ], [5   ], [160   ], [178   ], [166   ], [174   ], [10], [.116],
      [], [B], [174   ], [8   ], [162   ], [184   ], [169   ], [180   ], [10], [    ],
      [y], [A], [ 23.06], [2.53], [ 19.56], [ 26.92], [ 21.24], [ 24.87], [10], [.496],
      [], [B], [ 22.28], [2.48], [ 18.97], [ 26.73], [ 20.51], [ 24.05], [10], [    ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 10, text(size: 0.9em, [#emph[Note.] N \= 20: A (n \= 10), B (n \= 10). P\-values: Welch t\-test. CI \= confidence interval; LL \= lower limit; UL \= upper limit.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a table with negative statistics

    Code
      cat(.pt_typst(.pt_negative(output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Continuous outcomes by g]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "0_0": 1, "1_0": 1, "2_0": 1
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center,),
          (align: left,),
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
          ([], [y]),
          ([A], [4.83]),
          ([B], [4.75]),
          ([], [−0.08]),
          ([LL], [−0.61]),
          ([UL], [0.46]),
          ([], [.766]),
          ([], [0.00]),
          ([], [40]),
        )
      
        #context layout(size => {
          let tinytable-naturals = tinytable-coldata.map(col => measure(grid(columns: 1, inset: table.inset, ..col)).width)
          // spicy: a column-group label wider than its columns widens them
          for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), (1, 2, [M]), (3, 3, [Δ]), (4, 5, [95% CI]), (6, 6, [p]), (7, 7, [R²]), (8, 8, [n]),) {
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
       table.hline(y: 1, start: 1, end: 3, stroke: 0.06em), table.hline(y: 1, start: 4, end: 6, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[M], [Δ], table.cell(colspan: 2, align: center)[95% CI], [p], [R²], [n],
      [], [A], [B], [], [LL], [UL], [], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [y], [4.83], [4.75], [−0.08], [−0.61], [0.46], [.766], [0.00], [40],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 40: A (n \= 20), B (n \= 20). P\-value: t\-test from the linear model. Δ \= B − A. CI \= confidence interval; LL \= lower limit; UL \= upper limit.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

