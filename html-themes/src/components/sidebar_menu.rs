use leptos::prelude::*;
use leptos_icons::Icon;
use icondata;
use leptos_router::components::A;

#[component]
pub fn Menu(children: Children) -> impl IntoView {
    view! {
        <ul class="space-y-2 font-medium px-3 py-3 list-inside h-full overflow-y-auto">{children()} </ul>
    }
}

#[component]
pub fn MenuItem(icon: icondata::Icon, text: String, path: String) -> impl IntoView {
    view! {
        <li>
            <A href=path attr:class="w-full flex items-center gap-2" >
                <Icon icon=icon />
                <span>{text}</span>
            </A>
        </li>
    }.into_any()
}

#[component]
pub fn SubMenu<C>(icon: icondata::Icon, text: String, children: TypedChildrenFn<C>) -> impl IntoView
where
    C: IntoView + 'static,
{
    let (is_expanded, set_expanded) = signal(false);
    let children = children.into_inner();
    view! {
        <li>
            <button class="w-full flex justify-between" on:click=move |_| set_expanded.update(|e| *e = !*e) >
                <span class="w-full flex items-center gap-2">
                    <Icon icon=icon />
                    <span>{text}</span>
                </span>
                <span
                    class={move || format!("transition-transform duration-200 {}", if is_expanded.get() { "rotate-90" } else { "" })}
                >"▶"</span>
            </button>
        </li>
        <ul class="space-y-2 font-medium px-3 list-inside">

            <Show when=move || is_expanded.get()>
                {children()}
            </Show>
        </ul>
    }
}

#[component]
pub fn UserMenu(icon: icondata::Icon, text: String, path: String) -> impl IntoView {
    view! {
        <A href=path attr:class="text-xl font-medium px-3 py-3 mt-auto w-full flex items-center gap-2" >
            <Icon icon=icon />
            <span>{text}</span>
        </A>
    }
}
