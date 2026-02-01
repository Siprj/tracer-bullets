use leptos::prelude::*;

use crate::components::label::Label;

#[component]
pub fn Checkbox3(
    id: String,
    #[prop(default = false)] disabled: bool,
    children: Children,
) -> impl IntoView {
    view! {
        <input
            id=id.clone()
            type="checkbox"
            class=move || {
                if !disabled {
                    "text-1-3 border-4 border-4-hover color-primary align-middle size-1-5rem checkmark-after"
                } else {
                    "text-1-3 border-4-disabled color-disabled-primary align-middle size-1-5rem checkmark-after"
                }
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle>
            {children()}
        </Label>
    }
}
