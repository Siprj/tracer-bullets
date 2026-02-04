use leptos::prelude::*;

#[component]
pub fn Showcases(children: Children) -> impl IntoView {
    view! { <div class="w-2/3 m-auto height-1.0">{children()}</div> }
}

#[component]
pub fn Showcase(children: Children) -> impl IntoView {
    view! { <div class="flex flex-col gap-3">{children()}</div> }
}

#[component]
pub fn Element(children: Children) -> impl IntoView {
    view! {
        <div class="inline-flex flex-row flex-wrap gap-2 items-center width-100">{children()}</div>
    }
}
