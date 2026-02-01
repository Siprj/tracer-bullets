use leptos::prelude::*;

use crate::components::label::Label;

#[component]
pub fn Radio3(
    id: String,
    #[prop(into)] name: String,
    #[prop(default = false)] disabled: bool,
    children: Children,
) -> impl IntoView {
    view! {
        <input
            id=id.clone()
            name=name
            type="radio"
            class=move || {
                if !disabled {
                    "text-1-1 border-4 border-4-hover color-primary align-middle size-1-5rem border-radius-50 align-middle radiomark-after"
                } else {
                    "text-1-1 border-4-disabled color-secondary-disabled align-middle size-1-5rem border-radius-50 align-middle radiomark-after"
                }
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle class:color-primary-disabled=disabled>
            {children()}
        </Label>
    }
}
