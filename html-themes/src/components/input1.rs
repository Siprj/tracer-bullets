use leptos::prelude::*;

#[component]
pub fn Input1(
    id: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = "".to_string(), into)] placeholder: String,
) -> impl IntoView {
    view! {
        <input
            id=id
            type="input"
            placeholder=placeholder
            class=move || {
                if !disabled {
                    "text-1-1 padding-lr-1 bg-primary border-2 border-2-focus color-primary"
                } else {
                    "text-1-1 padding-lr-1 border-2-disabled bg-primary color-disabled-primary"
                }
            }
            disabled=disabled
        />
    }
}
