use leptos::prelude::*;

#[component]
pub fn Button4(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 bg-primary border-4 border-4-hover color-secondary color-primary-hover"
                } else {
                    "text-1-1 padding-lr-1 bg-primary border-4-disabled color-disabled-primary text-shadow-2rpx"
                }
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
