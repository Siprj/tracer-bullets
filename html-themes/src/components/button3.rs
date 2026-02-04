use leptos::prelude::*;

#[component]
pub fn Button3(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                "transition duration-150 ease-linear text-base px-4 border3 text-secondary cursor-pointer hover:text-primary disabled:cursor-default disabled:text-secondary-disabled disabled:my-text-shadow text-nowrap"
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
