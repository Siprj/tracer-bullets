use leptos::prelude::*;

use crate::components::label::Label;

#[component]
pub fn Radio2(
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
                    "text-1-5 border-3 border-3-hover color-primary size-1-5rem border-radius-50 align-middle radiomark-after"
                } else {
                    "text-1-5 border-3-disabled color-disabled-primary size-1-5rem border-radius-50 align-middle radiomark-after"
                }
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle>
            {children()}
        </Label>
    }
}
