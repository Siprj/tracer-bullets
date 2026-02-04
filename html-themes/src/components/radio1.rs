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
                "inline-block appearance-none transition duration-150 ease-linear text-base w-4 h-4 box-content border2 !rounded-full text-primary cursor-pointer text-nowrap disabled:cursor-default disabled:text-secondary-disabled disabled:my-text-shadow radiomark align-middle"
            }
            disabled=disabled
        />
        <Label for_id=id class:align-middle class:text-primary-disabled=disabled>
            {children()}
        </Label>
    }
}
