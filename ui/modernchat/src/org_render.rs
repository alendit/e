use egui::{Color32, RichText, Ui};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OrgBlock {
    Heading { level: usize, text: String },
    ListItem(String),
    Source { lang: String, body: String },
    Quote(String),
    Paragraph(String),
}

pub fn parse_blocks(input: &str) -> Vec<OrgBlock> {
    let mut blocks = Vec::new();
    let mut paragraph = Vec::new();
    let mut lines = input.lines().peekable();

    fn flush(blocks: &mut Vec<OrgBlock>, paragraph: &mut Vec<String>) {
        if !paragraph.is_empty() {
            blocks.push(OrgBlock::Paragraph(paragraph.join("\n")));
            paragraph.clear();
        }
    }

    while let Some(line) = lines.next() {
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            flush(&mut blocks, &mut paragraph);
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("#+begin_src") {
            flush(&mut blocks, &mut paragraph);
            let lang = rest.trim().to_string();
            let mut body = Vec::new();
            while let Some(src_line) = lines.next() {
                if src_line.trim_start().to_ascii_lowercase().starts_with("#+end_src") {
                    break;
                }
                body.push(src_line.to_string());
            }
            blocks.push(OrgBlock::Source { lang, body: body.join("\n") });
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("#+begin_quote") {
            let _ = rest;
            flush(&mut blocks, &mut paragraph);
            let mut body = Vec::new();
            while let Some(quote_line) = lines.next() {
                if quote_line.trim_start().to_ascii_lowercase().starts_with("#+end_quote") {
                    break;
                }
                body.push(quote_line.to_string());
            }
            blocks.push(OrgBlock::Quote(body.join("\n")));
            continue;
        }
        let star_count = trimmed.chars().take_while(|c| *c == '*').count();
        if star_count > 0 && trimmed.chars().nth(star_count) == Some(' ') {
            flush(&mut blocks, &mut paragraph);
            blocks.push(OrgBlock::Heading {
                level: star_count,
                text: trimmed[star_count + 1..].to_string(),
            });
            continue;
        }
        if let Some(item) = trimmed.strip_prefix("- ") {
            flush(&mut blocks, &mut paragraph);
            blocks.push(OrgBlock::ListItem(item.to_string()));
            continue;
        }
        paragraph.push(trimmed.to_string());
    }
    flush(&mut blocks, &mut paragraph);
    blocks
}

pub fn render_org(ui: &mut Ui, input: &str) {
    for block in parse_blocks(input) {
        match block {
            OrgBlock::Heading { level, text } => {
                let size = match level { 1 => 22.0, 2 => 19.0, _ => 16.0 };
                ui.label(RichText::new(text).strong().size(size));
            }
            OrgBlock::ListItem(text) => {
                ui.horizontal_wrapped(|ui| {
                    ui.label("•");
                    render_inline(ui, &text);
                });
            }
            OrgBlock::Source { lang, body } => {
                ui.group(|ui| {
                    if !lang.is_empty() { ui.label(RichText::new(lang).small().weak()); }
                    ui.monospace(body);
                });
            }
            OrgBlock::Quote(body) => {
                ui.group(|ui| { ui.label(RichText::new(body).italics().color(Color32::GRAY)); });
            }
            OrgBlock::Paragraph(text) => render_inline(ui, &text),
        }
        ui.add_space(4.0);
    }
}

fn render_inline(ui: &mut Ui, text: &str) {
    // First slice: render readable wrapped text.  Link clicks and richer inline
    // markup can be layered onto the same parser without changing the shell DTO.
    ui.label(text);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_common_blocks() {
        let blocks = parse_blocks("* Title\n\n- item\n\n#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n");
        assert!(matches!(blocks[0], OrgBlock::Heading { level: 1, .. }));
        assert!(matches!(blocks[1], OrgBlock::ListItem(_)));
        assert!(matches!(blocks[2], OrgBlock::Source { .. }));
    }
}
