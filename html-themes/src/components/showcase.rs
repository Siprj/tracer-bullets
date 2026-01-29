use leptos::prelude::*;

#[component]
pub fn Showcases(children: Children) -> impl IntoView {
    view! { <div class="show">{children()}</div> }
}

#[component]
pub fn Showcase(children: Children) -> impl IntoView {
    view! { <div class="showcase">{children()}</div> }
}
