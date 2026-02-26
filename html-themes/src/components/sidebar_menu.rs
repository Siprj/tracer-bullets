use leptos::prelude::*;

#[component]
pub fn Menu(children: Children) -> impl IntoView {
    view! { <ul class="space-y-2 font-medium px-3 py-3 list-inside list-disc">{children()}</ul> }
}

#[component]
pub fn MenuItem(children: Children) -> impl IntoView {
    view! { <li>{children()}</li> }
}
