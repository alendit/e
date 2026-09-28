mod org_render;

use emacs_egui_sdk::{emacs_post_message, EguiEmacsApp, ThemeColors};
use serde::Deserialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModernChatState {
    #[serde(default)]
    pub session: SessionState,
    #[serde(default)]
    pub messages: Vec<MessageState>,
    #[serde(default)]
    pub activities: Vec<ActivityState>,
    #[serde(default)]
    pub attachments: Vec<AttachmentState>,
    #[serde(default)]
    pub composer: ComposerState,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionState {
    pub id: Option<String>,
    pub name: Option<String>,
    pub project_root: Option<String>,
    pub active_turn_id: Option<String>,
    pub model: Option<String>,
    #[serde(default)]
    pub layers: Vec<String>,
    pub output_mode: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MessageState {
    pub id: Option<String>,
    pub turn_id: Option<String>,
    pub role: Option<String>,
    pub status: Option<String>,
    pub created_at: Option<String>,
    pub content: Option<String>,
    pub content_mode: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityState {
    pub id: Option<String>,
    pub turn_id: Option<String>,
    pub kind: Option<String>,
    pub status: Option<String>,
    pub title: Option<String>,
    pub summary: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AttachmentState {
    pub id: Option<String>,
    pub kind: Option<String>,
    pub label: Option<String>,
    pub uri: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ComposerState {
    #[serde(default)]
    pub text: String,
    #[serde(default)]
    pub enabled: bool,
    pub placeholder: Option<String>,
}

pub struct ModernChatApp {
    state: ModernChatState,
    composer_text: String,
    selected_activity: Option<String>,
    readable_fonts_applied: bool,
}

impl ModernChatApp {
    pub fn new() -> Self {
        Self {
            state: ModernChatState::default(),
            composer_text: String::new(),
            selected_activity: None,
            readable_fonts_applied: false,
        }
    }

    fn apply_readable_fonts(&mut self, ctx: &egui::Context) {
        if self.readable_fonts_applied {
            return;
        }

        let mut style = (*ctx.style()).clone();
        style.override_text_style = Some(egui::TextStyle::Body);
        style.text_styles.insert(
            egui::TextStyle::Heading,
            egui::FontId::new(22.0, egui::FontFamily::Proportional),
        );
        style.text_styles.insert(
            egui::TextStyle::Body,
            egui::FontId::new(16.0, egui::FontFamily::Proportional),
        );
        style.text_styles.insert(
            egui::TextStyle::Button,
            egui::FontId::new(15.0, egui::FontFamily::Proportional),
        );
        style.text_styles.insert(
            egui::TextStyle::Small,
            egui::FontId::new(14.0, egui::FontFamily::Proportional),
        );
        style.text_styles.insert(
            egui::TextStyle::Monospace,
            egui::FontId::new(15.0, egui::FontFamily::Monospace),
        );
        ctx.set_style(style);
        self.readable_fonts_applied = true;
    }

    fn send_action(action: &str, payload: serde_json::Value) {
        let mut map = serde_json::Map::new();
        map.insert(
            "action".to_string(),
            serde_json::Value::String(action.to_string()),
        );
        if let serde_json::Value::Object(extra) = payload {
            for (key, value) in extra {
                map.insert(key, value);
            }
        }
        emacs_post_message("ui-action", serde_json::Value::Object(map));
    }

    fn selected_activity(&self) -> Option<&ActivityState> {
        self.selected_activity.as_ref().and_then(|selected| {
            self.state
                .activities
                .iter()
                .find(|activity| activity.id.as_ref() == Some(selected))
        })
    }
}

impl EguiEmacsApp for ModernChatApp {
    type State = ModernChatState;

    fn on_state_update(&mut self, state: Self::State) {
        if self.composer_text.is_empty() && !state.composer.text.is_empty() {
            self.composer_text = state.composer.text.clone();
        }
        self.state = state;
    }

    fn on_theme_update(&mut self, _theme: ThemeColors) {
        self.readable_fonts_applied = false;
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.apply_readable_fonts(ctx);

        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            ui.horizontal_wrapped(|ui| {
                ui.heading(
                    self.state
                        .session
                        .name
                        .as_deref()
                        .unwrap_or("e modern chat"),
                );
                if let Some(model) = &self.state.session.model {
                    ui.label(format!("model: {}", model));
                }
                if let Some(mode) = &self.state.session.output_mode {
                    ui.label(format!("mode: {}", mode));
                }
                if self.state.session.active_turn_id.is_some() {
                    ui.spinner();
                    ui.label("running");
                }
            });
        });

        egui::SidePanel::left("left")
            .resizable(true)
            .default_width(220.0)
            .show(ctx, |ui| {
                ui.heading("Session");
                if let Some(id) = &self.state.session.id {
                    ui.small(id);
                }
                if let Some(root) = &self.state.session.project_root {
                    ui.label(root);
                }
                ui.separator();
                ui.heading("Layers");
                for layer in &self.state.session.layers {
                    ui.small(layer);
                }
                ui.separator();
                ui.heading("Attachments");
                for attachment in &self.state.attachments {
                    let label = attachment.label.as_deref().unwrap_or("attachment");
                    if ui.button(label).clicked() {
                        if let Some(uri) = &attachment.uri {
                            Self::send_action("open-resource", serde_json::json!({ "uri": uri }));
                        }
                    }
                }
            });

        egui::SidePanel::right("inspector")
            .resizable(true)
            .default_width(260.0)
            .show(ctx, |ui| {
                ui.heading("Inspector");
                if self.selected_activity.is_some() {
                    if let Some(activity) = self.selected_activity() {
                        ui.label(activity.title.as_deref().unwrap_or("activity"));
                        ui.label(activity.status.as_deref().unwrap_or(""));
                        ui.separator();
                        ui.label(activity.summary.as_deref().unwrap_or(""));
                    }
                } else {
                    ui.label("Select an activity card.");
                }
            });

        egui::TopBottomPanel::bottom("composer").show(ctx, |ui| {
            ui.horizontal(|ui| {
                let edit = egui::TextEdit::singleline(&mut self.composer_text)
                    .hint_text(
                        self.state
                            .composer
                            .placeholder
                            .as_deref()
                            .unwrap_or("Ask e..."),
                    )
                    .desired_width(f32::INFINITY);
                let response = ui.add_enabled(self.state.composer.enabled, edit);
                let send = ui.add_enabled(
                    self.state.composer.enabled && !self.composer_text.trim().is_empty(),
                    egui::Button::new("Send"),
                );
                if send.clicked()
                    || (response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)))
                {
                    let text = self.composer_text.trim().to_string();
                    if !text.is_empty() {
                        Self::send_action("send-message", serde_json::json!({ "text": text }));
                        self.composer_text.clear();
                    }
                }
                if ui
                    .add_enabled(
                        self.state.session.active_turn_id.is_some(),
                        egui::Button::new("Cancel"),
                    )
                    .clicked()
                {
                    Self::send_action("cancel-turn", serde_json::json!({}));
                }
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::vertical()
                .stick_to_bottom(true)
                .show(ui, |ui| {
                    for message in &self.state.messages {
                        ui.group(|ui| {
                            ui.horizontal(|ui| {
                                ui.strong(message.role.as_deref().unwrap_or("message"));
                                ui.small(message.status.as_deref().unwrap_or(""));
                            });
                            match message.content_mode.as_deref() {
                                Some("org") => org_render::render_org(
                                    ui,
                                    message.content.as_deref().unwrap_or(""),
                                ),
                                _ => {
                                    ui.label(message.content.as_deref().unwrap_or(""));
                                }
                            }
                        });
                        ui.add_space(6.0);
                    }
                    for activity in &self.state.activities {
                        let title = activity.title.as_deref().unwrap_or("activity");
                        if ui
                            .selectable_label(
                                self.selected_activity.as_ref() == activity.id.as_ref(),
                                title,
                            )
                            .clicked()
                        {
                            self.selected_activity = activity.id.clone();
                        }
                    }
                });
        });
    }
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn start_app(canvas_id: &str) -> Result<(), JsValue> {
    emacs_egui_sdk::launch_simple(canvas_id, ModernChatApp::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserializes_minimal_state() {
        let state: ModernChatState =
            serde_json::from_str(r#"{"session":{"id":"s1"},"messages":[]}"#).unwrap();
        assert_eq!(state.session.id.as_deref(), Some("s1"));
    }

    #[test]
    fn selects_and_inspects_activity_by_preserved_board_identity() {
        let state: ModernChatState = serde_json::from_str(
            r#"{
              "activities": [{
                "id": "board-activity-7",
                "turnId": "turn-1",
                "kind": "context-curated",
                "status": "ok",
                "title": "Agent updated context",
                "summary": "kept 1 · erased 2"
              }]
            }"#,
        )
        .unwrap();
        let mut app = ModernChatApp::new();
        app.state = state;
        app.selected_activity = Some("board-activity-7".to_string());

        let selected = app.selected_activity().unwrap();
        assert_eq!(selected.id.as_deref(), Some("board-activity-7"));
        assert_eq!(selected.title.as_deref(), Some("Agent updated context"));
        assert_eq!(selected.status.as_deref(), Some("ok"));
        assert_eq!(selected.summary.as_deref(), Some("kept 1 · erased 2"));
    }
}
