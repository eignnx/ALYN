use core::fmt;

pub trait PadWith: Sized {
    fn pad(self) -> PadOpts;
}

impl PadWith for char {
    fn pad(self) -> PadOpts {
        PadOpts::default()
            .alignment(fmt::Alignment::Center)
            .fill(self)
    }
}

#[derive(Clone, Copy, Default)]
pub struct PadOpts {
    options: fmt::FormattingOptions,
}

impl fmt::Display for PadOpts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value(""))
    }
}

impl PadOpts {
    pub fn fill(mut self, fill: char) -> Self {
        self.options.fill(fill);
        self
    }

    pub fn width(mut self, width: u16) -> Self {
        self.options.width(Some(width));
        self
    }
    pub fn alignment(mut self, alignment: fmt::Alignment) -> Self {
        self.options.align(Some(alignment));
        self
    }

    pub fn align(mut self, align: char) -> Self {
        self.options.align(match align {
            '<' => Some(fmt::Alignment::Left),
            '>' => Some(fmt::Alignment::Right),
            '^' => Some(fmt::Alignment::Center),
            ' ' => None,
            _ => panic!("invalid alignment character '{align}'"),
        });
        self
    }

    pub fn value<T>(self, value: T) -> Pad<T> {
        Pad {
            value,
            options: self.options,
        }
    }
}

pub struct Pad<T> {
    value: T,
    options: fmt::FormattingOptions,
}

impl<T: fmt::Debug> fmt::Debug for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rendered = format!("{:?}", self.value);
        let mut f = self.options.create_formatter(f);
        f.pad(rendered.as_ref())?;
        Ok(())
    }
}

impl<T: fmt::Display> fmt::Display for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rendered = format!("{}", self.value);
        let mut f = self.options.create_formatter(f);
        f.pad(rendered.as_ref())?;
        Ok(())
    }
}
