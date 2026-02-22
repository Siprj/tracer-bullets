use leptos::prelude::*;

#[component]
pub fn SideBar(children: Children) -> impl IntoView {
    view! { <aside class="fixed top-0 left-0 z-40 w-64 h-full border2-sidebar border-e-[calc(3rem_/_16)] by-none">{children()}</aside> }
}
