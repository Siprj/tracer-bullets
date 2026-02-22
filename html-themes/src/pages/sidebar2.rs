use leptos::prelude::*;

use crate::components::sidebar2::SideBar;

/// Default Home Page
#[component]
pub fn SideBarPage() -> impl IntoView {
    view! {
        <ErrorBoundary fallback=|errors| {
            view! {
                <h1>"Uh oh! Something went wrong!"</h1>

                <p>"Errors: "</p>
                // Render a list of errors as strings - good for development purposes
                <ul>
                    {move || {
                        errors
                            .get()
                            .into_iter()
                            .map(|(_, e)| view! { <li>{e.to_string()}</li> })
                            .collect_view()
                    }}

                </ul>
            }
        }>
          <SideBar>
            <ul>
              <li>"Sidebar Item 1"</li>
              <li>"Sidebar Item 2"</li>
              <li>"Sidebar Item 3"</li>
              <li>"Sidebar Item 4"</li>
            </ul>
          </SideBar>
        </ErrorBoundary>
    }
}
