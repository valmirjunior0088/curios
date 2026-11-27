//! A terminal as a drawing that redraws in place writes to one — the command line's live transcript —: a grid of rows under a cursor, moved by the four controls such a drawing writes — carriage return, newline, cursor up, erase to the end of the screen — and blind to paint, which no cell keeps, and to autowrap's switch, since its rows have no width to wrap at: what autowrap holds is the bytes' to show.

/// What a terminal shows, and where its cursor stands.
#[derive(Default)]
pub struct Screen {
    rows: Vec<Vec<char>>,
    row: usize,
    column: usize,
}

impl Screen {
    /// Write `bytes` as a terminal reads them.
    pub fn write(&mut self, bytes: &str) {
        let mut chars = bytes.chars().peekable();
        while let Some(char) = chars.next() {
            match char {
                '\r' => self.column = 0,
                '\n' => {
                    self.row += 1;
                    self.column = 0;
                }
                '\x1b' => {
                    assert_eq!(chars.next(), Some('['), "a control the drawing writes");
                    // A private mode set or reset, autowrap's `?7l` and `?7h`, moves nothing.
                    if chars.next_if_eq(&'?').is_some() {
                        while chars.next_if(char::is_ascii_digit).is_some() {}
                        let switched = chars.next();
                        assert!(
                            matches!(switched, Some('l' | 'h')),
                            "a mode the drawing switches: {switched:?}"
                        );
                        continue;
                    }
                    let mut count = String::new();
                    while let Some(digit) = chars.next_if(char::is_ascii_digit) {
                        count.push(digit);
                    }
                    let mut code = chars.next();
                    // Paint, its parameters joined by `;`, moves nothing.
                    while code == Some(';') {
                        while chars.next_if(char::is_ascii_digit).is_some() {}
                        code = chars.next();
                    }
                    match code {
                        Some('A') => self.row -= count.parse::<usize>().unwrap_or(1),
                        Some('J') => {
                            self.rows.truncate(self.row + 1);
                            if let Some(row) = self.rows.get_mut(self.row) {
                                row.truncate(self.column);
                            }
                        }
                        Some('m') => {}
                        other => panic!("no control the drawing writes: {other:?}"),
                    }
                }
                printed => {
                    while self.rows.len() <= self.row {
                        self.rows.push(Vec::new());
                    }
                    let row = &mut self.rows[self.row];
                    while row.len() < self.column {
                        row.push(' ');
                    }
                    match row.get_mut(self.column) {
                        Some(cell) => *cell = printed,
                        None => row.push(printed),
                    }
                    self.column += 1;
                }
            }
        }
    }

    /// What the screen shows, a line a row, past its last row with anything on it nothing.
    pub fn shown(&self) -> String {
        let mut rows = self
            .rows
            .iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<_>>();
        while rows.last().is_some_and(String::is_empty) {
            rows.pop();
        }

        rows.iter().map(|row| format!("{row}\n")).collect()
    }
}
