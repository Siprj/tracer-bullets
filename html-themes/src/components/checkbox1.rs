use leptos::prelude::*;

use crate::components::label::Label;

#[component]
pub fn Checkbox1(
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
                    "text-1-3 border-2 border-2-hover color-primary text-shadow-2rpx inline-block position-relative align-middle size-1-5rem checkmark-after"
                } else {
                    "text-1-3 border-2-disabled color-secondary-disabled text-shadow-2rpx align-middle size-1-5rem checkmark-after"
                }
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle class:color-primary-disabled=disabled>
            {children()}
        </Label>
    }
}
