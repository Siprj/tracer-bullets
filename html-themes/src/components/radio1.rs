use leptos::prelude::*;

use crate::components::label::Label;

#[component]
pub fn Radio1(
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
                    "text-1-5 border-2 border-2-hover color-primary text-shadow-2rpx inline-block align-middle size-1-5rem radiomark-after border-radius-50"
                } else {
                    "text-1-5 border-2-disabled color-secondary-disabled text-shadow-2rpx align-middle size-1-5rem radiomark-after border-radius-50"
                }
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle class:color-primary-disabled=disabled>
            {children()}
        </Label>
    }
}
