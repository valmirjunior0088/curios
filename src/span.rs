use std::{fs, io, path::PathBuf, rc::Rc};

#[derive(Debug)]
pub struct Source {
    pub path: Option<PathBuf>,
    pub text: String,
}

impl Source {
    pub fn new(path: impl Into<PathBuf>, text: impl Into<String>) -> Rc<Self> {
        Rc::new(Self {
            path: Some(path.into()),
            text: text.into(),
        })
    }

    pub fn inline(text: impl Into<String>) -> Rc<Self> {
        Rc::new(Self {
            path: None,
            text: text.into(),
        })
    }

    pub fn read(path: impl Into<PathBuf>) -> io::Result<Rc<Self>> {
        let path = path.into();
        let text = fs::read_to_string(&path)?;

        Ok(Self::new(path, text))
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pub source: Rc<Source>,
    pub start: usize,
    pub end: usize,
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.source, &other.source)
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for Span {}

impl std::hash::Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.source) as usize).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl Span {
    pub fn new(source: Rc<Source>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    pub fn render_snippet(&self) -> String {
        let source = &self.source.text;
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

        let snippet = format!(
            "{number:>5} | {line}\n{padding:>5} | {caret}",
            number = number,
            line = &source[line_start..line_end],
            padding = "",
        );

        match &self.source.path {
            Some(path) => format!("   --> {}:{number}\n{snippet}", path.display()),
            None => snippet,
        }
    }
}
