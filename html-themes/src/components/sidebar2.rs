use leptos::prelude::*;

#[component]
pub fn SideBar(children: Children) -> impl IntoView {
    view! {
<aside class="fixed top-0 left-0 z-40 w-64 h-full flex flex-col by-none bg-(--color-secondary-bg)">
            {children()}
        </aside>
    }
}
