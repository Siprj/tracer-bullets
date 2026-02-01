use leptos::prelude::*;

#[component]
pub fn Input2(
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
                    "text-1-1 padding-lr-1 bg-primary border-3 border-3-focus color-primary"
                } else {
                    "text-1-1 padding-lr-1 bg-primary input3 border-3-disabled color-secondary-disabled"
                }
            }
            disabled=disabled
        />
    }
}
