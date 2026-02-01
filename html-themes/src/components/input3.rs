use leptos::prelude::*;

#[component]
pub fn Input3(
    id: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = "".to_string())] placeholder: String,
) -> impl IntoView {
    view! {
        <input
            id=id
            type="input"
            placeholder=placeholder
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 bg-primary border-4 border-4-focus color-primary"
                } else {
                    "text-1-1 padding-lr-1 bg-primary border-4-disabled color-secondary-disabled"
                }
            }
            disabled=disabled
        />
    }
}
