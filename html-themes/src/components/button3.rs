use leptos::prelude::*;

#[component]
pub fn Button3(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 bg-primary border-3 border-3-hover color-secondary color-primary-hover"
                } else {
                    "text-1-1 padding-lr-1 bg-primary border-3-disabled color-secondary-disabled text-shadow-2rpx"
                }
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
