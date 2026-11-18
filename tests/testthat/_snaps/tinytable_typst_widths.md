# the Typst source of a battery, with and without a cell size

    Code
      cat(.tw_typst(.tw_battery()))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "0_2": 0, "0_3": 0, "0_4": 0, "0_5": 0, "0_6": 0, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1, "6_1": 1, "7_1": 1, "8_1": 1, "9_1": 1, "1_2": 1, "2_2": 1, "3_2": 1, "4_2": 1, "5_2": 1, "6_2": 1, "7_2": 1, "8_2": 1, "9_2": 1, "1_3": 1, "2_3": 1, "3_3": 1, "4_3": 1, "5_3": 1, "6_3": 1, "7_3": 1, "8_3": 1, "9_3": 1, "1_4": 1, "2_4": 1, "3_4": 1, "4_4": 1, "5_4": 1, "6_4": 1, "7_4": 1, "8_4": 1, "9_4": 1, "1_5": 1, "2_5": 1, "3_5": 1, "4_5": 1, "5_5": 1, "6_5": 1, "7_5": 1, "8_5": 1, "9_5": 1, "1_6": 1, "2_6": 1, "3_6": 1, "4_6": 1, "5_6": 1, "6_6": 1, "7_6": 1, "8_6": 1, "9_6": 1, "0_0": 2, "1_0": 3, "2_0": 3, "3_0": 3, "4_0": 3, "5_0": 3, "6_0": 3, "7_0": 3, "8_0": 3, "9_0": 3
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (align: center + bottom,),
          (align: center,),
          (align: left + bottom,),
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([Variable], [Organiser la conciliation entre travail, études et vie privée], [Acquérir des méthodes de travail et d'apprentissage], [Se préparer à endosser le rôle d'étudiant·e], [Connaître HESAV, son fonctionnement et ses ressources], [Développer un regard critique sur sa propre pratique], [Se familiariser avec les savoirs théoriques infirmiers], [Améliorer l'expression écrite et la prise de parole], [Chercher de la documentation et citer ses sources], [Clarifier son projet professionnel et sa motivation]),
          ([À renforcer], [57], [48], [43], [43], [43], [39], [35], [26], [17]),
          ([À maintenir tel quel], [35], [43], [43], [35], [52], [48], [52], [65], [70]),
          ([À réduire], [0], [4], [0], [9], [0], [4], [9], [4], [9]),
          ([À retirer], [0], [0], [0], [0], [0], [0], [0], [0], [0]),
          ([Pas d'avis], [ 9], [ 4], [13], [13], [ 4], [ 9], [ 4], [ 4], [ 4]),
          ([n], [23], [23], [23], [23], [23], [23], [23], [23], [23]),
        )
      
        #context layout(size => {
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 0, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 1, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 10, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 7, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [À renforcer], [À maintenir tel quel], [À réduire], [À retirer], [Pas d'avis], [n],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [Organiser la conciliation entre travail, études et vie privée], [57], [35], [0], [0], [ 9], [23],
      [Acquérir des méthodes de travail et d'apprentissage], [48], [43], [4], [0], [ 4], [23],
      [Se préparer à endosser le rôle d'étudiant·e], [43], [43], [0], [0], [13], [23],
      [Connaître HESAV, son fonctionnement et ses ressources], [43], [35], [9], [0], [13], [23],
      [Développer un regard critique sur sa propre pratique], [43], [52], [0], [0], [ 4], [23],
      [Se familiariser avec les savoirs théoriques infirmiers], [39], [48], [4], [0], [ 9], [23],
      [Améliorer l'expression écrite et la prise de parole], [35], [52], [9], [0], [ 4], [23],
      [Chercher de la documentation et citer ses sources], [26], [65], [4], [0], [ 4], [23],
      [Clarifier son projet professionnel et sa motivation], [17], [70], [9], [0], [ 4], [23],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 7, text(size: 0.9em, [#emph[Note.] Pourcentages en ligne : chaque ligne totalise 100 % aux arrondis près. n \= nombre de réponses.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

---

    Code
      cat(.tw_typst(.tw_battery(fontsize = 0.75)))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "0_2": 0, "0_3": 0, "0_4": 0, "0_5": 0, "0_6": 0, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1, "6_1": 1, "7_1": 1, "8_1": 1, "9_1": 1, "1_2": 1, "2_2": 1, "3_2": 1, "4_2": 1, "5_2": 1, "6_2": 1, "7_2": 1, "8_2": 1, "9_2": 1, "1_3": 1, "2_3": 1, "3_3": 1, "4_3": 1, "5_3": 1, "6_3": 1, "7_3": 1, "8_3": 1, "9_3": 1, "1_4": 1, "2_4": 1, "3_4": 1, "4_4": 1, "5_4": 1, "6_4": 1, "7_4": 1, "8_4": 1, "9_4": 1, "1_5": 1, "2_5": 1, "3_5": 1, "4_5": 1, "5_5": 1, "6_5": 1, "7_5": 1, "8_5": 1, "9_5": 1, "1_6": 1, "2_6": 1, "3_6": 1, "4_6": 1, "5_6": 1, "6_6": 1, "7_6": 1, "8_6": 1, "9_6": 1, "0_0": 2, "1_0": 3, "2_0": 3, "3_0": 3, "4_0": 3, "5_0": 3, "6_0": 3, "7_0": 3, "8_0": 3, "9_0": 3
        )
      
        #let style-array = ( 
          // tinytable cell style after
          (fontsize: 0.75em, align: center + bottom,),
          (fontsize: 0.75em, align: center,),
          (fontsize: 0.75em, align: left + bottom,),
          (fontsize: 0.75em, align: left,),
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([Variable], [Organiser la conciliation entre travail, études et vie privée], [Acquérir des méthodes de travail et d'apprentissage], [Se préparer à endosser le rôle d'étudiant·e], [Connaître HESAV, son fonctionnement et ses ressources], [Développer un regard critique sur sa propre pratique], [Se familiariser avec les savoirs théoriques infirmiers], [Améliorer l'expression écrite et la prise de parole], [Chercher de la documentation et citer ses sources], [Clarifier son projet professionnel et sa motivation]),
          ([À renforcer], [57], [48], [43], [43], [43], [39], [35], [26], [17]),
          ([À maintenir tel quel], [35], [43], [43], [35], [52], [48], [52], [65], [70]),
          ([À réduire], [0], [4], [0], [9], [0], [4], [9], [4], [9]),
          ([À retirer], [0], [0], [0], [0], [0], [0], [0], [0], [0]),
          ([Pas d'avis], [ 9], [ 4], [13], [13], [ 4], [ 9], [ 4], [ 4], [ 4]),
          ([n], [23], [23], [23], [23], [23], [23], [23], [23], [23]),
        )
      
        #context layout(size => {
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 0, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 1, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 10, start: 0, end: 7, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 7, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [À renforcer], [À maintenir tel quel], [À réduire], [À retirer], [Pas d'avis], [n],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [Organiser la conciliation entre travail, études et vie privée], [57], [35], [0], [0], [ 9], [23],
      [Acquérir des méthodes de travail et d'apprentissage], [48], [43], [4], [0], [ 4], [23],
      [Se préparer à endosser le rôle d'étudiant·e], [43], [43], [0], [0], [13], [23],
      [Connaître HESAV, son fonctionnement et ses ressources], [43], [35], [9], [0], [13], [23],
      [Développer un regard critique sur sa propre pratique], [43], [52], [0], [0], [ 4], [23],
      [Se familiariser avec les savoirs théoriques infirmiers], [39], [48], [4], [0], [ 9], [23],
      [Améliorer l'expression écrite et la prise de parole], [35], [52], [9], [0], [ 4], [23],
      [Chercher de la documentation et citer ses sources], [26], [65], [4], [0], [ 4], [23],
      [Clarifier son projet professionnel et sa motivation], [17], [70], [9], [0], [ 4], [23],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 7, text(size: 0.675em, [#emph[Note.] Pourcentages en ligne : chaque ligne totalise 100 % aux arrondis près. n \= nombre de réponses.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a categorical table by a group

    Code
      cat(.tw_typst(table_categorical(sochealth, select = c(smoking, education), by = sex,
      output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Categorical table by sex]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "7_5": 0, "8_5": 0, "9_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "7_6": 0, "8_6": 0, "9_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "6_7": 0, "7_7": 0, "8_7": 0, "9_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "6_8": 0, "7_8": 0, "8_8": 0, "9_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "6_0": 1, "3_0": 2, "4_0": 2, "5_0": 2, "7_0": 2, "8_0": 2, "9_0": 2
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([], [smoking], [No], [Yes], [(Missing)], [education], [Lower secondary], [Upper secondary], [Tertiary]),
          ([n], [   ], [475], [131], [ 14], [   ], [134], [290], [196]),
          ([%], [    ], [78.4], [21.6], [    ], [    ], [21.6], [46.8], [31.6]),
          ([n], [   ], [451], [118], [ 11], [   ], [127], [249], [204]),
          ([%], [    ], [79.3], [20.7], [    ], [    ], [21.9], [42.9], [35.2]),
          ([n], [   ], [926], [249], [ 25], [   ], [261], [539], [400]),
          ([%], [    ], [78.8], [21.2], [    ], [    ], [21.8], [44.9], [33.3]),
          ([], [.713], [    ], [    ], [    ], [.344], [    ], [    ], [    ]),
          ([], [.01], [   ], [   ], [   ], [.04], [   ], [   ], [   ]),
        )
      
        #context layout(size => {
          // spicy: a cell that spans columns is measured with none of them
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            if (spicy_x, spicy_k) in ((0, 1), (0, 5),) { [] } else { spicy_cell }
          }))
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 1, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a column-group label wider than its columns widens them
          let spicy_spans = ((0, 0, 0, [Variable]), (1, 2, 0, [Female]), (3, 4, 0, [Male]), (5, 6, 0, [Total]), (7, 7, 0, [p]), (8, 8, 0, [Effect size]),)
          let spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, spicy_least) = {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_t = table(columns: spicy_n, stroke: none, table.cell(x: spicy_from, y: spicy_row, colspan: spicy_span, spicy_label))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_span * spicy_inset
          }
          for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, false)
            let spicy_have = tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()
            if spicy_need > spicy_have {
              for k in range(spicy_from, spicy_to + 1) {
                tinytable-naturals.at(k) += (spicy_need - spicy_have) / spicy_span
              }
            }
          }
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
              let spicy_span = spicy_to - spicy_from + 1
              let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, true)
              let spicy_have = spicy_floor.slice(spicy_from, spicy_to + 1).sum()
              if spicy_need > spicy_have {
                for k in range(spicy_from, spicy_to + 1) {
                  spicy_floor.at(k) = calc.min(spicy_nat.at(k), spicy_floor.at(k) + (spicy_need - spicy_have) / spicy_span)
                }
              }
            }
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 6, start: 0, end: 9, stroke: 0.03em),
       table.hline(y: 1, start: 1, end: 7, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 10, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], table.cell(colspan: 2, align: center)[Female], table.cell(colspan: 2, align: center)[Male], table.cell(colspan: 2, align: center)[Total], [p], [Effect size],
      [], [n], [%], [n], [%], [n], [%], [], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      table.cell(colspan: 7)[smoking], [.713], [.01],
      [No], [475], [78.4], [451], [79.3], [926], [78.8], [    ], [   ],
      [Yes], [131], [21.6], [118], [20.7], [249], [21.2], [    ], [   ],
      [(Missing)], [ 14], [    ], [ 11], [    ], [ 25], [    ], [    ], [   ],
      table.cell(colspan: 7)[education], [.344], [.04],
      [Lower secondary], [134], [21.6], [127], [21.9], [261], [21.8], [    ], [   ],
      [Upper secondary], [290], [46.8], [249], [42.9], [539], [44.9], [    ], [   ],
      [Tertiary], [196], [31.6], [204], [35.2], [400], [33.3], [    ], [   ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] N \= 1200: Female (n \= 620), Male (n \= 580). Percentages are column percentages. P\-values: Pearson chi\-squared test. Phi: smoking. Cramér's V: education.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a multiple-response table

    Code
      cat(.tw_typst(table_multiresponse(sochealth, select = starts_with("info_"),
      output = "tinytable")))
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Multiple responses]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([Variable], [info\_doctor], [info\_pharmacist], [info\_internet], [info\_family], [info\_media], [(No item chosen)], [Base]),
          ([n], [ 721], [ 256], [ 523], [ 360], [ 141], [  67], [1164]),
          ([%], [61.9], [22.0], [44.9], [30.9], [12.1], [ 5.8], [    ]),
        )
      
        #context layout(size => {
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 0, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 7, start: 0, end: 3, stroke: 0.03em),
       table.hline(y: 1, start: 0, end: 3, stroke: 0.06em),
       table.hline(y: 8, start: 0, end: 3, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 3, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [Variable], [n], [%],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [info\_doctor], [ 721], [61.9],
      [info\_pharmacist], [ 256], [22.0],
      [info\_internet], [ 523], [44.9],
      [info\_family], [ 360], [30.9],
      [info\_media], [ 141], [12.1],
      [(No item chosen)], [  67], [ 5.8],
      [Base], [1164], [    ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 3, text(size: 0.9em, [#emph[Note.] Multiple answers possible: percentages can sum to more than 100%. Base: respondents who answered the question (excluded: 36 with no answer to the question).])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of two models with their CI

    Code
      cat(.tw_typst(table_regression(m, show_columns = c("b", "ci", "p"), output = "tinytable")))
    Message
      Ordered factor(s) detected. Polynomial contrasts (the R default for `ordered()`) decompose the factor into orthogonal trend components: `.L` = linear, `.Q` = quadratic, `.C` = cubic, `^k` = degree k. Coefficients are trends across the ordered levels, NOT per-level effects against a reference.
      i To display per-level (treatment) effects, refit with `factor(x, ordered = FALSE)` or set `options(contrasts = c("contr.treatment", "contr.treatment"))`.
      This message is displayed once per session.
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Linear regression comparison]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "14_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "14_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "14_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "14_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "7_5": 0, "8_5": 0, "9_5": 0, "10_5": 0, "11_5": 0, "12_5": 0, "13_5": 0, "14_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "7_6": 0, "8_6": 0, "9_6": 0, "10_6": 0, "11_6": 0, "12_6": 0, "13_6": 0, "14_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "6_7": 0, "7_7": 0, "8_7": 0, "9_7": 0, "10_7": 0, "11_7": 0, "12_7": 0, "13_7": 0, "14_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "6_8": 0, "7_8": 0, "8_8": 0, "9_8": 0, "10_8": 0, "11_8": 0, "12_8": 0, "13_8": 0, "14_8": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "9_0": 1, "12_0": 1, "13_0": 1, "14_0": 1, "7_0": 2, "8_0": 2, "10_0": 2, "11_0": 2
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([], [Outcome], [(Intercept)], [age], [sex:], [Female (ref.)], [Male], [education:], [.L], [.Q], [n], [R²], [Adj. R²]),
          ([], [wellbeing\_score], [  64.63], [   0.03], [], [   –   ], [   3.65], [], [  13.80], [  −1.71], [1200   ], [   0.22], [   0.22]),
          ([LL], [], [61.78], [−0.03], [], [ –   ], [ 2.09], [], [12.28], [−3.00], [], [], []),
          ([UL], [], [67.49], [ 0.08], [], [ –   ], [ 5.22], [], [15.32], [−0.41], [], [], []),
          ([], [], [\<.001], [ .343], [], [–    ], [\<.001], [], [\<.001], [ .010], [], [], []),
          ([], [bmi], [  23.96], [   0.04], [], [   –   ], [   0.58], [], [  −2.67], [   0.14], [1188   ], [   0.16], [   0.16]),
          ([LL], [], [23.25], [ 0.03], [], [ –   ], [ 0.19], [], [−3.05], [−0.19], [], [], []),
          ([UL], [], [24.67], [ 0.05], [], [ –   ], [ 0.97], [], [−2.29], [ 0.46], [], [], []),
          ([], [], [\<.001], [\<.001], [], [–    ], [ .004], [], [\<.001], [ .404], [], [], []),
        )
      
        #context layout(size => {
          // spicy: a cell that spans columns is measured with none of them
          let tinytable-coldata = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_col.enumerate().map(((spicy_k, spicy_cell)) => {
            if (spicy_x, spicy_k) in ((1, 1), (5, 1),) { [] } else { spicy_cell }
          }))
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 2, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a column-group label wider than its columns widens them
          let spicy_spans = ((1, 4, 0, [Model A]), (5, 8, 0, [Model B]), (0, 0, 1, [Variable]), (1, 1, 1, [B]), (2, 3, 1, [95% CI]), (4, 4, 1, [p]), (5, 5, 1, [B]), (6, 7, 1, [95% CI]), (8, 8, 1, [p]),)
          let spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, spicy_least) = {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_t = table(columns: spicy_n, stroke: none, table.cell(x: spicy_from, y: spicy_row, colspan: spicy_span, spicy_label))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_span * spicy_inset
          }
          for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, false)
            let spicy_have = tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()
            if spicy_need > spicy_have {
              for k in range(spicy_from, spicy_to + 1) {
                tinytable-naturals.at(k) += (spicy_need - spicy_have) / spicy_span
              }
            }
          }
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
              let spicy_span = spicy_to - spicy_from + 1
              let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, true)
              let spicy_have = spicy_floor.slice(spicy_from, spicy_to + 1).sum()
              if spicy_need > spicy_have {
                for k in range(spicy_from, spicy_to + 1) {
                  spicy_floor.at(k) = calc.min(spicy_nat.at(k), spicy_floor.at(k) + (spicy_need - spicy_have) / spicy_span)
                }
              }
            }
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 12, start: 0, end: 9, stroke: 0.03em + rgb("#cccccc")),
       table.hline(y: 2, start: 2, end: 4, stroke: 0.06em), table.hline(y: 2, start: 6, end: 8, stroke: 0.06em),
       table.hline(y: 3, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 15, start: 0, end: 9, stroke: 0.06em),
       table.hline(y: 0, start: 0, end: 9, stroke: 0.06em),
          // tinytable lines before
      
          // tinytable header start
          table.header(
            repeat: true,
      [ ], table.cell(colspan: 4, align: center)[Model A #place(bottom, dy: 0.4em, line(length: 100%, stroke: 0.06em))], table.cell(colspan: 4, align: center)[Model B #place(bottom, dy: 0.4em, line(length: 100%, stroke: 0.06em))],
      [Variable], [B], table.cell(colspan: 2, align: center)[95% CI], [p], [B], table.cell(colspan: 2, align: center)[95% CI], [p],
      [], [], [LL], [UL], [], [], [LL], [UL], [],
          ),
          // tinytable header end
      
          // tinytable cell content after
      [Outcome], table.cell(colspan: 4)[wellbeing\_score], table.cell(colspan: 4)[bmi],
      [(Intercept)], [  64.63], [61.78], [67.49], [\<.001], [  23.96], [23.25], [24.67], [\<.001],
      [age], [   0.03], [−0.03], [ 0.08], [ .343], [   0.04], [ 0.03], [ 0.05], [\<.001],
      [sex:], [], [], [], [], [], [], [], [],
      [Female (ref.)], [   –   ], [ –   ], [ –   ], [–    ], [   –   ], [ –   ], [ –   ], [–    ],
      [Male], [   3.65], [ 2.09], [ 5.22], [\<.001], [   0.58], [ 0.19], [ 0.97], [ .004],
      [education:], [], [], [], [], [], [], [], [],
      [.L], [  13.80], [12.28], [15.32], [\<.001], [  −2.67], [−3.05], [−2.29], [\<.001],
      [.Q], [  −1.71], [−3.00], [−0.41], [ .010], [   0.14], [−0.19], [ 0.46], [ .404],
      [n], [1200   ], [], [], [], [1188   ], [], [], [],
      [R²], [   0.22], [], [], [], [   0.16], [], [], [],
      [Adj. R²], [   0.22], [], [], [], [   0.16], [], [], [],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 9, text(size: 0.9em, [#emph[Note.] Linear regression models. Std. errors: classical (OLS). Ordered factor \`education\`: polynomial trends (.L \= linear, .Q \= quadratic). CI \= confidence interval; LL \= lower limit; UL \= upper limit.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

# the Typst source of a continuous table by a group (two label columns)

    Code
      cat(typ)
    Output
      #show figure: set block(breakable: false)
      #figure( // start preamble figure
        caption: text([Descriptive statistics by employment\_status]),
        kind: table, // end preamble figure
      
      block[ // start block
        #set par(justify: false)
      
        #let style-dict = (
          // tinytable style-dict after
          "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "0_5": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "7_5": 0, "8_5": 0, "9_5": 0, "0_6": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "7_6": 0, "8_6": 0, "9_6": 0, "0_7": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "6_7": 0, "7_7": 0, "8_7": 0, "9_7": 0, "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "6_8": 0, "7_8": 0, "8_8": 0, "9_8": 0, "0_9": 0, "1_9": 0, "2_9": 0, "3_9": 0, "4_9": 0, "5_9": 0, "6_9": 0, "7_9": 0, "8_9": 0, "9_9": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "0_1": 1, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1, "6_1": 1, "7_1": 1, "8_1": 1, "9_1": 1
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
          if ("indent" in style and it.body != []) { tmp = pad(left: style.indent, tmp) }
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
          ([], [age], [], [], [], [wellbeing\_score], [], [], []),
          ([], [Employed], [Student], [Unemployed], [Inactive], [Employed], [Student], [Unemployed], [Inactive]),
          ([], [49.62], [47.35], [48.68], [50.11], [69.42], [70.42], [65.78], [69.72]),
          ([], [14.74], [13.96], [14.90], [15.02], [15.07], [16.16], [17.17], [15.64]),
          ([], [25.00], [25.00], [25.00], [25.00], [19.60], [18.70], [23.20], [21.10]),
          ([], [ 75.00], [ 75.00], [ 75.00], [ 75.00], [100.00], [100.00], [100.00], [100.00]),
          ([LL], [48.57], [45.04], [46.45], [47.40], [68.35], [67.75], [63.21], [66.90]),
          ([UL], [50.67], [49.66], [50.91], [52.81], [70.49], [73.09], [68.35], [72.54]),
          ([], [762], [143], [174], [121], [762], [143], [174], [121]),
          ([], [.290], [    ], [    ], [    ], [.049], [    ], [    ], [    ]),
        )
      
        #context layout(size => {
          // spicy: each column measured at its place in a table
          let spicy_n = tinytable-coldata.len()
          let spicy_inset = measure(table(columns: 1, stroke: none, table.cell(x: 0, y: 0, []))).width
          let spicy_blank = measure(table(columns: spicy_n, stroke: none, ..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, [])))).width
          let spicy_width(spicy_x, spicy_col, spicy_least) = {
            let spicy_t = table(columns: spicy_n, stroke: none, ..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => table.cell(x: spicy_x, y: spicy_k + 1, spicy_cell)))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_inset
          }
          let tinytable-naturals = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))
          // spicy: a column-group label wider than its columns widens them
          let spicy_spans = ((0, 0, 0, [Variable]), (1, 1, 0, [Group]), (2, 2, 0, [M]), (3, 3, 0, [SD]), (4, 4, 0, [Min]), (5, 5, 0, [Max]), (6, 7, 0, [95% CI]), (8, 8, 0, [n]), (9, 9, 0, [p]),)
          let spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, spicy_least) = {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_t = table(columns: spicy_n, stroke: none, table.cell(x: spicy_from, y: spicy_row, colspan: spicy_span, spicy_label))
            measure(if spicy_least { show " ": linebreak(); spicy_t } else { spicy_t }).width - spicy_blank + spicy_span * spicy_inset
          }
          for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
            let spicy_span = spicy_to - spicy_from + 1
            let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, false)
            let spicy_have = tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()
            if spicy_need > spicy_have {
              for k in range(spicy_from, spicy_to + 1) {
                tinytable-naturals.at(k) += (spicy_need - spicy_have) / spicy_span
              }
            }
          }
          // spicy: a table wider than the space shrinks by the role of its columns
          if tinytable-naturals.sum() > size.width {
            let spicy_nat = tinytable-naturals
            let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>
              calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))
            for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {
              let spicy_span = spicy_to - spicy_from + 1
              let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, true)
              let spicy_have = spicy_floor.slice(spicy_from, spicy_to + 1).sum()
              if spicy_need > spicy_have {
                for k in range(spicy_from, spicy_to + 1) {
                  spicy_floor.at(k) = calc.min(spicy_nat.at(k), spicy_floor.at(k) + (spicy_need - spicy_have) / spicy_span)
                }
              }
            }
            let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)
            let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)
            let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)
            let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {
              if spicy_sum(spicy_cols, spicy_high) <= spicy_target {
                return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))
              }
              let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>
                calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))
              let spicy_lo = 0pt
              let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))
              for _ in range(30) {
                let spicy_mid = (spicy_lo + spicy_hi) / 2
                if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }
              }
              spicy_at(spicy_lo)
            }
            let spicy_stub = if 2 > 0 and 2 < spicy_n { 2 } else { 0 }
            let spicy_s = range(0, spicy_stub)
            let spicy_d = range(spicy_stub, spicy_n)
            let spicy_room = size.width
            let spicy_three_s = spicy_sum(spicy_s, spicy_three)
            let spicy_two_s = spicy_sum(spicy_s, spicy_two)
            let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)
            let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)
            let spicy_two_d = spicy_sum(spicy_d, spicy_two)
            let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)
            if spicy_room - spicy_nat_d >= spicy_three_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
            } else if spicy_room - spicy_two_d >= spicy_three_s {
              let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))
              let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>
                spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))
              for spicy_i in spicy_order {
                if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }
                spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))
              }
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_two_s {
              tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)
            } else if spicy_room - spicy_floor_d >= spicy_three_s {
              let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data
            } else if spicy_room - spicy_floor_d >= spicy_floor_s {
              tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))
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
       table.hline(y: 6, start: 0, end: 10, stroke: 0.03em),
       table.hline(y: 1, start: 6, end: 8, stroke: 0.06em),
       table.hline(y: 2, start: 0, end: 10, stroke: 0.06em),
       table.hline(y: 10, start: 0, end: 10, stroke: 0.06em),
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
      [age], [Employed], [49.62], [14.74], [25.00], [ 75.00], [48.57], [50.67], [762], [.290],
      [], [Student], [47.35], [13.96], [25.00], [ 75.00], [45.04], [49.66], [143], [    ],
      [], [Unemployed], [48.68], [14.90], [25.00], [ 75.00], [46.45], [50.91], [174], [    ],
      [], [Inactive], [50.11], [15.02], [25.00], [ 75.00], [47.40], [52.81], [121], [    ],
      [wellbeing\_score], [Employed], [69.42], [15.07], [19.60], [100.00], [68.35], [70.49], [762], [.049],
      [], [Student], [70.42], [16.16], [18.70], [100.00], [67.75], [73.09], [143], [    ],
      [], [Unemployed], [65.78], [17.17], [23.20], [100.00], [63.21], [68.35], [174], [    ],
      [], [Inactive], [69.72], [15.64], [21.10], [100.00], [66.90], [72.54], [121], [    ],
      
          // tinytable footer after
      
          table.footer(
            repeat: false,
            // tinytable notes after
          table.cell(align: left, colspan: 10, text(size: 0.9em, [#emph[Note.] N \= 1200: Employed (n \= 762), Student (n \= 143), Unemployed (n \= 174), Inactive (n \= 121). P\-values: Welch one\-way ANOVA. CI \= confidence interval; LL \= lower limit; UL \= upper limit.])),
          ),
          
      
        ) // end table
      
        ]
        })
      
        // tinytable align-figure after
      
      ] // end block
      ) // end figure

