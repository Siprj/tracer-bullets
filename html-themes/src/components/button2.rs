use leptos::prelude::*;

#[component]
pub fn Button2(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 bg-primary border-2 border-2-hover color-secondary color-primary-hover text-shadow-2rpx"
                } else {
                    "text-1-1 padding-lr-1 bg-primary border-2-disabled color-disabled-primary text-shadow-2rpx"
                }
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
