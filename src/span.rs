#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn render_snippet(&self, source: &str) -> String {
        let start = self.start;
        let end = self.end;

        let line_start = source[..start]
            .rfind('\n')
            .map(|index| 1 + index)
            .unwrap_or(0);

        let line_end = source[start..]
            .find('\n')
            .map(|index| start + index)
            .unwrap_or(source.len());

        let number = 1 + source[..line_start]
            .bytes()
            .filter(|&byte| byte == b'\n')
            .count();

        let width = end.max(start.saturating_add(1)).min(line_end) - start;
        let caret = format!(
            "{}{}",
            " ".repeat(start - line_start),
            "^".repeat(width.max(1))
        );

        format!(
            "{number:>5} | {line}\n{padding:>5} | {caret}",
            number = number,
            line = &source[line_start..line_end],
            padding = "",
        )
    }
}
