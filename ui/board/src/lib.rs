use emacs_egui_sdk::{emacs_post_message, EguiEmacsApp, ThemeColors};
use serde::Deserialize;
use serde_json::{json, Map, Value};

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BoardState {
    pub board_id: Option<String>,
    #[serde(default)]
    pub compact: bool,
    #[serde(default)]
    pub run_set_epoch: u64,
    #[serde(default)]
    pub run_set: RunSetState,
    pub selected_run_id: Option<String>,
    pub selected_task: Option<TaskIdentity>,
    #[serde(default)]
    pub detail: DetailState,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RunSetState {
    pub status: Option<String>,
    pub restore_state: Option<String>,
    #[serde(default)]
    pub ready: bool,
    #[serde(default)]
    pub browsing: bool,
    #[serde(default)]
    pub browse_available: bool,
    #[serde(default)]
    pub page_loading: bool,
    pub page_generation: Option<u64>,
    #[serde(default)]
    pub next_available: bool,
    #[serde(default)]
    pub selected_run_visible: bool,
    #[serde(default)]
    pub more_may_exist: bool,
    #[serde(default)]
    pub active_count: usize,
    #[serde(default)]
    pub omitted_count: usize,
    #[serde(default)]
    pub runs: Vec<RunSummary>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RunSummary {
    pub run_id: Option<String>,
    pub label: Option<String>,
    pub lifecycle: Option<String>,
    #[serde(default)]
    pub required_count: usize,
    #[serde(default)]
    pub optional_count: usize,
    #[serde(default)]
    pub required_states: StateCounts,
    #[serde(default)]
    pub optional_states: StateCounts,
    #[serde(default)]
    pub attention: bool,
    #[serde(default)]
    pub conflict_count: usize,
    pub deadline_label: Option<String>,
    pub restore_state: Option<String>,
    pub completion_delivery_state: Option<String>,
    pub completion_execution_state: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StateCounts {
    #[serde(default)]
    pub total: usize,
    #[serde(default)]
    pub pending: usize,
    #[serde(default)]
    pub running: usize,
    #[serde(default)]
    pub done: usize,
    #[serde(default)]
    pub failed: usize,
    #[serde(default)]
    pub cancelled: usize,
    #[serde(default)]
    pub other: usize,
}

#[derive(Debug, Clone, Default, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct TaskIdentity {
    pub run_id: Option<String>,
    pub task_key: Option<String>,
    pub attempt: Option<i64>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DetailState {
    pub state: Option<String>,
    pub board_id: Option<String>,
    pub run_id: Option<String>,
    pub label: Option<String>,
    pub terminal_status: Option<String>,
    pub generation: Option<i64>,
    pub revision: Option<i64>,
    pub error: Option<String>,
    #[serde(default)]
    pub required_tasks: Vec<TaskCard>,
    #[serde(default)]
    pub optional_tasks: Vec<TaskCard>,
    #[serde(default)]
    pub participants: Vec<ParticipantCard>,
    pub next_participant_cursor: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TaskCard {
    pub run_id: Option<String>,
    pub task_key: Option<String>,
    pub label: Option<String>,
    pub attempt: Option<i64>,
    #[serde(default)]
    pub required: bool,
    pub state: Option<String>,
    pub participant_id: Option<String>,
    pub participant_name: Option<String>,
    pub participant_state: Option<String>,
    pub outcome_status: Option<String>,
    pub outcome_summary: Option<String>,
    pub outcome_error: Option<String>,
    pub progress_sequence: Option<i64>,
    pub progress_summary: Option<String>,
    #[serde(default)]
    pub controls: TaskControls,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TaskControls {
    #[serde(default)]
    pub can_open_chat: bool,
    #[serde(default)]
    pub can_steer: bool,
    #[serde(default)]
    pub can_send: bool,
    #[serde(default)]
    pub can_interrupt: bool,
    #[serde(default)]
    pub can_shutdown: bool,
}

impl TaskControls {
    fn any_available(&self) -> bool {
        self.can_open_chat
            || self.can_steer
            || self.can_send
            || self.can_interrupt
            || self.can_shutdown
    }
}

impl TaskCard {
    fn identity(&self) -> TaskIdentity {
        TaskIdentity {
            run_id: self.run_id.clone(),
            task_key: self.task_key.clone(),
            attempt: self.attempt,
        }
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ParticipantCard {
    pub participant_id: Option<String>,
    pub name: Option<String>,
    pub state: Option<String>,
    pub run_id: Option<String>,
    pub task_key: Option<String>,
    pub attempt: Option<i64>,
    pub outcome_source: Option<String>,
    pub outcome_status: Option<String>,
    pub outcome_summary: Option<String>,
    pub progress_sequence: Option<i64>,
    pub progress_summary: Option<String>,
}

pub struct BoardApp {
    state: BoardState,
    selected_task: Option<TaskIdentity>,
    steer_prompt: String,
    steer_reason: String,
    send_prompt: String,
    fonts_for_compact: Option<bool>,
}

impl BoardApp {
    pub fn new() -> Self {
        Self {
            state: BoardState {
                compact: true,
                ..BoardState::default()
            },
            selected_task: None,
            steer_prompt: String::new(),
            steer_reason: String::new(),
            send_prompt: String::new(),
            fonts_for_compact: None,
        }
    }

    fn apply_readable_fonts(&mut self, ctx: &egui::Context) {
        let compact = self.state.compact;
        if self.fonts_for_compact == Some(compact) {
            return;
        }

        let mut style = (*ctx.style()).clone();
        style.override_text_style = Some(egui::TextStyle::Body);
        style.text_styles.insert(
            egui::TextStyle::Heading,
            egui::FontId::new(
                if compact { 13.0 } else { 22.0 },
                egui::FontFamily::Proportional,
            ),
        );
        style.text_styles.insert(
            egui::TextStyle::Body,
            egui::FontId::new(
                if compact { 12.0 } else { 16.0 },
                egui::FontFamily::Proportional,
            ),
        );
        style.text_styles.insert(
            egui::TextStyle::Button,
            egui::FontId::new(
                if compact { 11.0 } else { 15.0 },
                egui::FontFamily::Proportional,
            ),
        );
        style.text_styles.insert(
            egui::TextStyle::Small,
            egui::FontId::new(
                if compact { 11.0 } else { 14.0 },
                egui::FontFamily::Proportional,
            ),
        );
        style.text_styles.insert(
            egui::TextStyle::Monospace,
            egui::FontId::new(
                if compact { 11.0 } else { 15.0 },
                egui::FontFamily::Monospace,
            ),
        );
        style.spacing.item_spacing = if compact {
            egui::vec2(4.0, 3.0)
        } else {
            egui::vec2(8.0, 6.0)
        };
        ctx.set_style(style);
        self.fonts_for_compact = Some(compact);
    }

    fn detail_contains_task(&self, identity: &TaskIdentity) -> bool {
        self.state
            .detail
            .required_tasks
            .iter()
            .chain(self.state.detail.optional_tasks.iter())
            .any(|task| task.identity() == *identity)
    }

    fn action_payload(&self, action: &str, values: Value) -> Value {
        let mut payload = Map::new();
        payload.insert("action".to_string(), Value::String(action.to_string()));
        payload.insert(
            "boardId".to_string(),
            self.state
                .board_id
                .as_ref()
                .map(|value| Value::String(value.clone()))
                .unwrap_or(Value::Null),
        );
        payload.insert(
            "runSetEpoch".to_string(),
            Value::from(self.state.run_set_epoch),
        );
        payload.insert(
            "pageGeneration".to_string(),
            self.state
                .run_set
                .page_generation
                .map(Value::from)
                .unwrap_or(Value::Null),
        );
        if let Value::Object(values) = values {
            payload.extend(values);
        }
        Value::Object(payload)
    }

    fn task_action_payload(&self, action: &str, task: &TaskCard, values: Value) -> Value {
        let mut fields = Map::new();
        fields.insert(
            "runId".to_string(),
            task.run_id
                .as_ref()
                .map(|value| Value::String(value.clone()))
                .unwrap_or(Value::Null),
        );
        fields.insert(
            "taskKey".to_string(),
            task.task_key
                .as_ref()
                .map(|value| Value::String(value.clone()))
                .unwrap_or(Value::Null),
        );
        fields.insert(
            "attempt".to_string(),
            task.attempt.map(Value::from).unwrap_or(Value::Null),
        );
        fields.insert(
            "participantId".to_string(),
            task.participant_id
                .as_ref()
                .map(|value| Value::String(value.clone()))
                .unwrap_or(Value::Null),
        );
        fields.insert(
            "generation".to_string(),
            self.state
                .detail
                .generation
                .map(Value::from)
                .unwrap_or(Value::Null),
        );
        fields.insert(
            "revision".to_string(),
            self.state
                .detail
                .revision
                .map(Value::from)
                .unwrap_or(Value::Null),
        );
        if let Value::Object(extra) = values {
            fields.extend(extra);
        }
        self.action_payload(action, Value::Object(fields))
    }

    fn send_action(&self, action: &str, values: Value) {
        emacs_post_message("ui-action", self.action_payload(action, values));
    }

    fn send_task_action(&self, action: &str, task: &TaskCard, values: Value) {
        emacs_post_message("ui-action", self.task_action_payload(action, task, values));
    }

    fn clear_control_inputs(&mut self) {
        self.steer_prompt.clear();
        self.steer_reason.clear();
        self.send_prompt.clear();
    }

    fn select_run(&mut self, run_id: &str) {
        self.state.selected_run_id = Some(run_id.to_string());
        self.selected_task = None;
        self.clear_control_inputs();
        self.send_action("select-run", json!({ "runId": run_id }));
    }

    fn select_task(&mut self, task: &TaskCard) {
        let identity = task.identity();
        if self.selected_task.as_ref() != Some(&identity) {
            self.clear_control_inputs();
        }
        self.selected_task = Some(identity.clone());
        self.send_action(
            "select-task",
            json!({
                "runId": identity.run_id,
                "taskKey": identity.task_key,
                "attempt": identity.attempt,
                "generation": self.state.detail.generation,
                "revision": self.state.detail.revision,
            }),
        );
    }

    fn selected_task(&self) -> Option<&TaskCard> {
        let identity = self.selected_task.as_ref()?;
        self.state
            .detail
            .required_tasks
            .iter()
            .chain(self.state.detail.optional_tasks.iter())
            .find(|task| task.identity() == *identity)
    }

    fn run_status_labels(run: &RunSummary) -> Vec<String> {
        let mut labels = Vec::new();
        if run.attention {
            labels.push("Attention".to_string());
        }
        if run.conflict_count > 0 {
            labels.push(format!("Conflicts: {}", run.conflict_count));
        }
        if let Some(deadline) = &run.deadline_label {
            labels.push(format!("Deadline: {deadline}"));
        }
        if let Some(restore) = &run.restore_state {
            labels.push(format!("Restore: {restore}"));
        }
        if let Some(delivery) = &run.completion_delivery_state {
            labels.push(format!("Completion delivery: {delivery}"));
        }
        if let Some(execution) = &run.completion_execution_state {
            labels.push(format!("Coordinator outcome: {execution}"));
        }
        labels
    }

    fn task_admission_label(task: &TaskCard) -> String {
        let attempt = task
            .attempt
            .map(|attempt| attempt.to_string())
            .unwrap_or_else(|| "unknown".to_string());
        match &task.participant_id {
            Some(participant_id) => format!(
                "Attempt {attempt} → participant {} ({participant_id}) · {}",
                task.participant_name.as_deref().unwrap_or(participant_id),
                task.participant_state.as_deref().unwrap_or("unknown")
            ),
            None => format!("Attempt {attempt} · admission pending · no participant"),
        }
    }

    fn render_hud_tasks(ui: &mut egui::Ui, tasks: &[TaskCard], title: &str) {
        ui.label(egui::RichText::new(title).weak().size(11.0));
        if tasks.is_empty() {
            ui.small("None");
        }
        for task in tasks {
            let label = task
                .label
                .as_deref()
                .or(task.task_key.as_deref())
                .unwrap_or("Task");
            let state = task.state.as_deref().unwrap_or("unknown");
            ui.horizontal(|ui| {
                let label_width = (ui.available_width() - 82.0).max(60.0);
                ui.add_sized(
                    [label_width, 0.0],
                    egui::Label::new(format!("• {label}")).truncate(),
                );
                ui.label(egui::RichText::new(state).weak().size(11.0));
            });
            if task.participant_id.is_none() {
                ui.label(
                    egui::RichText::new(if matches!(state, "queued" | "pending") {
                        "Admission pending"
                    } else {
                        "No participant admitted"
                    })
                    .weak()
                    .size(11.0),
                );
            }
        }
    }

    fn render_hud(&self, ctx: &egui::Context) {
        egui::CentralPanel::default()
            .frame(egui::Frame::none().inner_margin(egui::Margin::same(8.0)))
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label(egui::RichText::new("Board activity").strong().size(12.0));
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if ui.add(egui::Button::new("×").frame(false)).clicked() {
                            self.send_action("dismiss", json!({}));
                        }
                        if ui.add(egui::Button::new("Details").frame(false)).clicked() {
                            self.send_action("show-details", json!({}));
                        }
                    });
                });
                ui.separator();

                let selected = self
                    .state
                    .run_set
                    .runs
                    .iter()
                    .find(|run| run.run_id.as_deref() == self.state.selected_run_id.as_deref());
                let label = self
                    .state
                    .detail
                    .label
                    .as_deref()
                    .or_else(|| selected.and_then(|run| run.label.as_deref()))
                    .or(self.state.selected_run_id.as_deref())
                    .unwrap_or("No active run");
                ui.label(egui::RichText::new("Run").weak().size(11.0));
                ui.add(egui::Label::new(egui::RichText::new(label).strong()).truncate());
                if let Some(run) = selected {
                    let lifecycle = run.lifecycle.as_deref().unwrap_or("unknown");
                    ui.small(format!(
                        "{lifecycle} · {} required · {} optional",
                        run.required_count, run.optional_count
                    ));
                    if run.attention || run.conflict_count > 0 {
                        ui.label("Attention required");
                    }
                    if let Some(delivery) = &run.completion_delivery_state {
                        ui.small(format!("Completion: {delivery}"));
                    }
                } else if let Some(status) = &self.state.detail.terminal_status {
                    ui.small(format!("Completion: {status}"));
                }

                ui.separator();
                match self.state.detail.state.as_deref() {
                    Some("ready") => {
                        egui::ScrollArea::vertical()
                            .max_height(120.0)
                            .show(ui, |ui| {
                                Self::render_hud_tasks(
                                    ui,
                                    &self.state.detail.required_tasks,
                                    "Required",
                                );
                                ui.add_space(4.0);
                                Self::render_hud_tasks(
                                    ui,
                                    &self.state.detail.optional_tasks,
                                    "Optional",
                                );
                            });
                    }
                    Some("empty") => {
                        ui.small("No active Board runs");
                    }
                    Some("error") => {
                        ui.label("Activity unavailable");
                        ui.small(
                            self.state
                                .detail
                                .error
                                .as_deref()
                                .unwrap_or("Unknown error"),
                        );
                    }
                    _ => {
                        ui.spinner();
                        ui.small("Loading Board activity…");
                    }
                }
            });
    }

    fn render_run_selector(&mut self, ui: &mut egui::Ui) {
        ui.heading("Board runs");
        if let Some(status) = &self.state.run_set.status {
            ui.label(format!("Run status: {status}"));
        }
        ui.label(format!("Active runs: {}", self.state.run_set.active_count));
        if !self.state.run_set.browsing && self.state.run_set.omitted_count > 0 {
            ui.small(format!(
                "{} active runs outside the current list",
                self.state.run_set.omitted_count
            ));
        }
        if self.state.run_set.browsing {
            ui.small("Browsing one indexed page of active runs");
        }
        if self.state.selected_run_id.is_some()
            && !self.state.run_set.page_loading
            && !self.state.run_set.selected_run_visible
        {
            ui.small("Selected run is outside this selector page; its activity remains open.");
        }
        if let Some(restore) = &self.state.run_set.restore_state {
            ui.label(format!("Board restore: {restore}"));
        }
        egui::ScrollArea::vertical()
            .max_height(170.0)
            .show(ui, |ui| {
            let runs = self.state.run_set.runs.clone();
            if runs.is_empty() && self.state.run_set.ready {
                ui.small("No actionable runs are currently projected.");
            }
            for run in runs {
                let run_id = run.run_id.as_deref().unwrap_or("");
                let label = run.label.as_deref().unwrap_or(run_id);
                let lifecycle = run.lifecycle.as_deref().unwrap_or("unknown");
                let text = format!(
                    "{label} · {lifecycle} · {} required · {} optional",
                    run.required_count, run.optional_count
                );
                ui.group(|ui| {
                    if ui
                        .selectable_label(
                            self.state.selected_run_id.as_deref() == Some(run_id),
                            text,
                        )
                        .clicked()
                    {
                        self.select_run(run_id);
                    }
                    for status in Self::run_status_labels(&run) {
                        ui.small(status);
                    }
                });
            }
            if self.state.run_set.browsing {
                if self.state.run_set.page_loading {
                    ui.label("Loading run page…");
                } else if self.state.run_set.next_available && ui.button("Next").clicked() {
                    self.send_action("next-runs", json!({}));
                }
                if ui.button("Current").clicked() {
                    self.send_action("current-runs", json!({}));
                }
            } else if self.state.run_set.browse_available
                && ui.button("Browse runs").clicked()
            {
                self.send_action("browse-runs", json!({}));
            }
        });
    }

    fn render_task_group(&mut self, ui: &mut egui::Ui, tasks: &[TaskCard], title: &str) {
        ui.heading(format!("{title} ({})", tasks.len()));
        for task in tasks {
            egui::Frame::group(ui.style()).show(ui, |ui| {
                let task_key = task.task_key.as_deref().unwrap_or("unknown task");
                let label = task.label.as_deref().unwrap_or(task_key);
                if ui
                    .selectable_label(
                        self.selected_task
                            .as_ref()
                            .is_some_and(|selected| *selected == task.identity()),
                        format!("{label} · {}", task.state.as_deref().unwrap_or("unknown")),
                    )
                    .clicked()
                {
                    self.select_task(task);
                }
                ui.small(format!(
                    "Task {} · attempt {}",
                    task_key,
                    task.attempt
                        .map(|attempt| attempt.to_string())
                        .unwrap_or_else(|| "not selected".to_string())
                ));
                ui.label(Self::task_admission_label(task));
                if let Some(status) = &task.outcome_status {
                    ui.label(format!("Outcome: {status}"));
                }
                if let Some(error) = &task.outcome_error {
                    ui.label(format!("Outcome error: {error}"));
                } else if let Some(summary) = &task.outcome_summary {
                    ui.label(format!("Outcome: {summary}"));
                }
            });
        }
    }

    fn render_participants(&self, ui: &mut egui::Ui) {
        ui.heading("Board participants");
        if self.state.detail.state.as_deref() == Some("loading") {
            ui.label("Loading participant page…");
            return;
        }

        for participant in &self.state.detail.participants {
            let id = participant.participant_id.as_deref().unwrap_or("");
            let name = participant.name.as_deref().unwrap_or(id);
            let state = participant.state.as_deref().unwrap_or("unknown");
            if ui.button(format!("{name} · {state}")).clicked() {
                self.send_action(
                    "open-participant",
                    json!({
                        "runId": self.state.detail.run_id,
                        "participantId": id,
                        "generation": self.state.detail.generation,
                        "revision": self.state.detail.revision,
                    }),
                );
            }
            ui.small(id);
        }

        if let Some(cursor) = &self.state.detail.next_participant_cursor {
            if ui.button("Next participants").clicked() {
                self.send_action(
                    "next-participants",
                    json!({
                        "runId": self.state.detail.run_id,
                        "cursor": cursor,
                        "generation": self.state.detail.generation,
                        "revision": self.state.detail.revision,
                    }),
                );
            }
        }
    }

    fn render_inspector(&mut self, ui: &mut egui::Ui) {
        ui.heading("Task inspector");
        let Some(task) = self.selected_task().cloned() else {
            ui.label("Select a task card to inspect its Board coordinates.");
            return;
        };

        ui.label(format!("Board: {}", self.state.board_id.as_deref().unwrap_or("unknown")));
        ui.label(format!("Run: {}", task.run_id.as_deref().unwrap_or("unknown")));
        ui.label(format!("Task: {}", task.task_key.as_deref().unwrap_or("unknown")));
        ui.label(format!(
            "Attempt: {}",
            task.attempt
                .map(|attempt| attempt.to_string())
                .unwrap_or_else(|| "not selected".to_string())
        ));
        ui.label(format!("Disposition: {}", task.state.as_deref().unwrap_or("unknown")));
        if let Some(participant_id) = &task.participant_id {
            ui.label(format!(
                "Participant: {} ({participant_id}) · {}",
                task.participant_name.as_deref().unwrap_or(participant_id),
                task.participant_state.as_deref().unwrap_or("unknown")
            ));
        } else {
            ui.label("Participant: none · admission pending");
        }
        if let Some(status) = &task.outcome_status {
            ui.label(format!("Outcome status: {status}"));
        }
        if let Some(error) = &task.outcome_error {
            ui.label(format!("Outcome error: {error}"));
        } else if let Some(summary) = &task.outcome_summary {
            ui.label(format!("Outcome summary: {summary}"));
        }
        if let Some(sequence) = task.progress_sequence {
            ui.label(format!(
                "Progress #{sequence}: {}",
                task.progress_summary.as_deref().unwrap_or("update")
            ));
        }

        if let Some(revision) = self.state.detail.revision {
            ui.separator();
            ui.label(format!("Board revision: {revision}"));
            if let Some(generation) = self.state.detail.generation {
                ui.small(format!("Board generation: {generation}"));
            }
        }

        let controls = &task.controls;
        if controls.any_available() {
            ui.separator();
            ui.heading("Participant controls");
            if controls.can_open_chat && ui.button("Open participant chat").clicked() {
                self.send_task_action("open-task-participant", &task, json!({}));
            }
            if controls.can_steer {
                ui.label("Steer prompt");
                ui.text_edit_multiline(&mut self.steer_prompt);
                ui.label("Reason (optional)");
                ui.text_edit_singleline(&mut self.steer_reason);
                if ui
                    .add_enabled(
                        !self.steer_prompt.trim().is_empty(),
                        egui::Button::new("Steer"),
                    )
                    .clicked()
                {
                    let prompt = std::mem::take(&mut self.steer_prompt);
                    let reason = std::mem::take(&mut self.steer_reason);
                    self.send_task_action(
                        "steer-participant",
                        &task,
                        json!({ "prompt": prompt, "reason": reason }),
                    );
                }
            }
            if controls.can_send {
                ui.label("Follow-up prompt");
                ui.text_edit_multiline(&mut self.send_prompt);
                if ui
                    .add_enabled(
                        !self.send_prompt.trim().is_empty(),
                        egui::Button::new("Send"),
                    )
                    .clicked()
                {
                    let prompt = std::mem::take(&mut self.send_prompt);
                    self.send_task_action(
                        "send-participant",
                        &task,
                        json!({ "prompt": prompt }),
                    );
                }
            }
            if controls.can_interrupt && ui.button("Interrupt").clicked() {
                self.send_task_action("interrupt-participant", &task, json!({}));
            }
            if controls.can_shutdown && ui.button("Shut down").clicked() {
                self.send_task_action("shutdown-participant", &task, json!({}));
            }
        }
    }
}

impl Default for BoardApp {
    fn default() -> Self {
        Self::new()
    }
}

impl EguiEmacsApp for BoardApp {
    type State = BoardState;

    fn on_state_update(&mut self, state: Self::State) {
        let previous = self.selected_task.take();
        self.state = state;
        let selected_task = previous
            .clone()
            .filter(|selected| self.detail_contains_task(selected))
            .or_else(|| {
                self.state
                    .selected_task
                    .clone()
                    .filter(|selected| self.detail_contains_task(selected))
            });
        if selected_task != previous {
            self.clear_control_inputs();
        }
        self.selected_task = selected_task;
    }

    fn on_theme_update(&mut self, _theme: ThemeColors) {
        self.fonts_for_compact = None;
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.apply_readable_fonts(ctx);

        if self.state.compact {
            self.render_hud(ctx);
            return;
        }

        egui::TopBottomPanel::top("board-header").show(ctx, |ui| {
            ui.horizontal_wrapped(|ui| {
                ui.heading("Board activity");
                if ui.button("HUD").clicked() {
                    self.send_action("show-hud", json!({}));
                }
                if ui.button("Close").clicked() {
                    self.send_action("dismiss", json!({}));
                }
                if let Some(board_id) = &self.state.board_id {
                    ui.small(format!("Board {board_id}"));
                }
                match self.state.detail.state.as_deref() {
                    Some("loading") => {
                        ui.spinner();
                        ui.label("Loading selected Board revision…");
                    }
                    Some("error") => {
                        ui.label(format!(
                            "Activity unavailable: {}",
                            self.state.detail.error.as_deref().unwrap_or("unknown error")
                        ));
                    }
                    Some("ready") => {
                        if let Some(revision) = self.state.detail.revision {
                            ui.label(format!("Revision {revision}"));
                        }
                        if let Some(state) = &self.state.detail.terminal_status {
                            ui.label(format!("Run completion: {state}"));
                        }
                    }
                    _ => {
                        ui.label("Waiting for Board run state");
                    }
                }
            });
            self.render_run_selector(ui);
        });

        egui::SidePanel::left("participants")
            .resizable(true)
            .default_width(210.0)
            .show(ctx, |ui| self.render_participants(ui));

        egui::SidePanel::right("task-inspector")
            .resizable(true)
            .default_width(270.0)
            .show(ctx, |ui| self.render_inspector(ui));

        egui::CentralPanel::default().show(ctx, |ui| {
            match self.state.detail.state.as_deref() {
                Some("loading") => {
                    ui.spinner();
                    ui.heading("Loading the selected run");
                    ui.label("Tasks and participants stay hidden until one coherent Board page settles.");
                }
                Some("error") => {
                    ui.heading("Board activity unavailable");
                    ui.label(self.state.detail.error.as_deref().unwrap_or("unknown error"));
                }
                Some("ready") => {
                    ui.columns(2, |columns| {
                        self.render_task_group(
                            &mut columns[0],
                            &self.state.detail.required_tasks.clone(),
                            "Required",
                        );
                        self.render_task_group(
                            &mut columns[1],
                            &self.state.detail.optional_tasks.clone(),
                            "Optional",
                        );
                    });
                }
                _ => {
                    ui.heading("No active run selected");
                    ui.label("Choose a Board run to load its bounded activity page.");
                }
            }
        });
    }
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn start_app(canvas_id: &str) -> Result<(), wasm_bindgen::JsValue> {
    emacs_egui_sdk::launch_simple(canvas_id, BoardApp::new())
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn announce_ready() {
    emacs_post_message("ui-ready", json!({}));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boot_state_uses_the_compact_hud_before_emacs_connects() {
        assert!(BoardApp::new().state.compact);
    }

    fn ready_state(revision: i64) -> BoardState {
        serde_json::from_value(json!({
            "boardId": "board-1",
            "runSetEpoch": 4,
            "selectedRunId": "run-1",
            "selectedTask": null,
            "detail": {
                "state": "ready",
                "boardId": "board-1",
                "runId": "run-1",
                "generation": 2,
                "revision": revision,
                "requiredTasks": [{
                    "runId": "run-1",
                    "taskKey": "calendar",
                    "attempt": 0,
                    "required": true,
                    "state": "running",
                    "participantId": "worker-1"
                }],
                "optionalTasks": [],
                "participants": []
            }
        }))
        .unwrap()
    }

    #[test]
    fn maps_run_selector_labels_and_bounded_counts() {
        let state: BoardState = serde_json::from_value(json!({
            "boardId": "board-1",
            "runSet": {
                "status": "attention",
                "restoreState": "ready",
                "ready": true,
                "browsing": true,
                "browseAvailable": false,
                "pageLoading": false,
                "pageGeneration": 2,
                "nextAvailable": true,
                "selectedRunVisible": true,
                "moreMayExist": true,
                "activeCount": 33,
                "omittedCount": 1,
                "runs": [{
                    "runId": "run-1",
                    "label": "Daily update",
                    "lifecycle": "attention",
                    "requiredCount": 2,
                    "optionalCount": 1,
                    "attention": true,
                    "conflictCount": 1,
                    "deadlineLabel": "expired",
                    "restoreState": "ready",
                    "completionDeliveryState": "failed",
                    "completionExecutionState": "cancelled"
                }]
            }
        }))
        .unwrap();

        assert_eq!(state.run_set.active_count, 33);
        assert_eq!(state.run_set.omitted_count, 1);
        let run = &state.run_set.runs[0];
        assert_eq!(run.label.as_deref(), Some("Daily update"));
        assert_eq!(run.lifecycle.as_deref(), Some("attention"));
        assert_eq!(run.required_count, 2);
        assert_eq!(run.optional_count, 1);
        assert_eq!(run.conflict_count, 1);
        assert_eq!(run.deadline_label.as_deref(), Some("expired"));
        assert_eq!(run.completion_delivery_state.as_deref(), Some("failed"));
        assert!(state.run_set.browsing);
        assert!(!state.run_set.browse_available);
        assert!(!state.run_set.page_loading);
        assert!(state.run_set.next_available);
        assert!(state.run_set.more_may_exist);
        let labels = BoardApp::run_status_labels(run);
        assert!(labels.contains(&"Attention".to_string()));
        assert!(labels.contains(&"Conflicts: 1".to_string()));
        assert!(labels.contains(&"Deadline: expired".to_string()));
        assert!(labels.contains(&"Restore: ready".to_string()));
        assert!(labels.contains(&"Completion delivery: failed".to_string()));
        assert!(labels.contains(&"Coordinator outcome: cancelled".to_string()));
    }

    #[test]
    fn pending_task_without_participant_keeps_its_admission_boundary_visible() {
        let task: TaskCard = serde_json::from_value(json!({
            "runId": "run-1",
            "taskKey": "calendar",
            "label": "Calendar",
            "attempt": 1,
            "required": true,
            "state": "queued",
            "participantId": null
        }))
        .unwrap();
        let mut app = BoardApp::new();
        app.on_state_update(ready_state(7));
        app.state.detail.required_tasks = vec![task.clone()];

        assert!(app.detail_contains_task(&task.identity()));
        assert_eq!(
            BoardApp::task_admission_label(&task),
            "Attempt 1 · admission pending · no participant"
        );
        assert!(!task.controls.any_available());
    }

    #[test]
    fn task_action_payload_carries_all_current_assignment_coordinates() {
        let mut app = BoardApp::new();
        app.state = ready_state(7);
        let task: TaskCard = serde_json::from_value(json!({
            "runId": "run-1",
            "taskKey": "calendar",
            "attempt": 2,
            "participantId": "worker-1",
            "controls": {
                "canOpenChat": true,
                "canSteer": true,
                "canSend": true,
                "canInterrupt": true,
                "canShutdown": true
            }
        }))
        .unwrap();

        assert!(task.controls.any_available());
        let payload = app.task_action_payload(
            "send-participant",
            &task,
            json!({ "prompt": "follow up" }),
        );

        assert_eq!(payload["action"], "send-participant");
        assert_eq!(payload["boardId"], "board-1");
        assert_eq!(payload["runSetEpoch"], 4);
        assert_eq!(payload["runId"], "run-1");
        assert_eq!(payload["taskKey"], "calendar");
        assert_eq!(payload["attempt"], 2);
        assert_eq!(payload["participantId"], "worker-1");
        assert_eq!(payload["generation"], 2);
        assert_eq!(payload["revision"], 7);
        assert_eq!(payload["prompt"], "follow up");
    }

    #[test]
    fn task_change_clears_local_control_input() {
        let mut app = BoardApp::new();
        app.on_state_update(ready_state(7));
        app.selected_task = Some(TaskIdentity {
            run_id: Some("run-1".to_string()),
            task_key: Some("calendar".to_string()),
            attempt: Some(0),
        });
        app.steer_prompt = "private prompt".to_string();
        app.steer_reason = "private reason".to_string();
        app.send_prompt = "follow up".to_string();
        let mut changed = ready_state(8);
        changed.detail.required_tasks[0].attempt = Some(1);

        app.on_state_update(changed);

        assert!(app.steer_prompt.is_empty());
        assert!(app.steer_reason.is_empty());
        assert!(app.send_prompt.is_empty());
        assert!(app.selected_task.is_none());
    }

    #[test]
    fn task_selection_survives_page_revision_when_durable_identity_remains() {
        let mut app = BoardApp::new();
        app.on_state_update(ready_state(7));
        app.selected_task = Some(TaskIdentity {
            run_id: Some("run-1".to_string()),
            task_key: Some("calendar".to_string()),
            attempt: Some(0),
        });
        app.steer_prompt = "keep this for the same task".to_string();

        app.on_state_update(ready_state(8));

        assert_eq!(app.state.detail.revision, Some(8));
        assert_eq!(app.selected_task.unwrap().attempt, Some(0));
        assert_eq!(app.steer_prompt, "keep this for the same task");
    }

    #[test]
    fn loading_state_carries_no_stale_task_rows() {
        let state: BoardState = serde_json::from_value(json!({
            "boardId": "board-1",
            "selectedRunId": "run-2",
            "detail": { "state": "loading", "runId": "run-2", "requiredTasks": [] }
        }))
        .unwrap();
        let mut app = BoardApp::new();
        app.on_state_update(ready_state(7));
        app.selected_task = Some(TaskIdentity {
            run_id: Some("run-1".to_string()),
            task_key: Some("calendar".to_string()),
            attempt: Some(0),
        });

        app.on_state_update(state);

        assert_eq!(app.state.detail.state.as_deref(), Some("loading"));
        assert!(app.state.detail.required_tasks.is_empty());
        assert!(app.selected_task.is_none());
    }

    #[test]
    fn action_payload_keeps_current_board_and_revision_identity() {
        let mut app = BoardApp::new();
        app.state = ready_state(7);
        app.state.run_set.page_generation = Some(3);
        let payload = app.action_payload(
            "select-task",
            json!({
                "runId": "run-1",
                "taskKey": "calendar",
                "attempt": 0,
                "generation": 2,
                "revision": 7
            }),
        );

        assert_eq!(payload["action"], "select-task");
        assert_eq!(payload["boardId"], "board-1");
        assert_eq!(payload["runSetEpoch"], 4);
        assert_eq!(payload["pageGeneration"], 3);
        assert_eq!(payload["runId"], "run-1");
        assert_eq!(payload["taskKey"], "calendar");
        assert_eq!(payload["attempt"], 0);
        assert_eq!(payload["revision"], 7);
    }
}
