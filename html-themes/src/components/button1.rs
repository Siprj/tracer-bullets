use leptos::prelude::*;

#[component]
pub fn Button1(text: String, #[prop(default = false)] disabled: bool) -> impl IntoView {
    view! {
        <button
            class=move || {
                "text-base px-4 border-solid rounded-full border-[calc(3rem_/_16)] hover:border-primary text-secondary hover:text-primary cursor-pointer disabled:cursor-default disabled:border-secondary disabled:text-bg disabled:bg-secondary text-nowrap"
            }
            disabled=disabled
        >
            {text}
        </button>
    }
}
