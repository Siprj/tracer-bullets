use leptos::prelude::*;

#[component]
pub fn Button1(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 border-1 border-1-hover color-secondary color-primary-hover bg-primary"
                } else {
                    "text-1-1 padding-lr-1 border-1-disabled color-bg-primary"
                }
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
