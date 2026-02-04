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
                "outline-none transition duration-150 ease-linear text-base px-4 py-1 border3 text-primary cursor-text text-nowrap disabled:cursor-default disabled:text-secondary-disabled disabled:my-text-shadow"
            }
            disabled=disabled
        />
    }
}
