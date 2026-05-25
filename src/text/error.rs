use {
    crate::Span,
    std::fmt,
};

#[derive(Debug)]
pub enum Error {
    UnresolvedQualifier { qualifier: String },
    ModuleNotFound { path: String },
    ChildModuleNotFound { segment: String },
    PrivateChildModule { segment: String },
    BindingNotFound { binding: String },
    PrivateBinding { binding: String },
    CoercionOutsideDefBlock { label: String },
    Located { span: Span, error: Box<Error> },
}

impl Error {
    pub fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    pub fn format(&self, source: &str) -> String {
        match self {
            Self::Located { span, error } => {
                format!(
                    "{error}\n\n{}",
                    span.render_snippet(source)
                )
            }
            error => error.to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnresolvedQualifier { qualifier } => {
                write!(f, "unresolved qualifier: {qualifier}")
            }
            Error::ModuleNotFound { path } => write!(f, "module not found: {path}"),
            Error::ChildModuleNotFound { segment } => {
                write!(f, "child module not found: {segment}")
            }
            Error::PrivateChildModule { segment } => write!(f, "private child module: {segment}"),
            Error::BindingNotFound { binding } => write!(f, "binding not found: {binding}"),
            Error::PrivateBinding { binding } => write!(f, "private binding: {binding}"),
            Error::CoercionOutsideDefBlock { label } => {
                write!(f, "coercion outside def block: {label}")
            }
            Error::Located { error, .. } => write!(f, "{error}"),
        }
    }
}
