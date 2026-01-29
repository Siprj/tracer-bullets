use leptos::prelude::*;

#[component]
pub fn Label(children: Children, for_id: String) -> impl IntoView {
    view! {
        <label for=for_id class="color-primary">
            {children()}
        </label>
    }
}
