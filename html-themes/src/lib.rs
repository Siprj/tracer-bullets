use leptos::prelude::*;
use leptos_meta::*;
use leptos_router::{components::*, path};

// Modules
mod base;
mod components;
mod pages;

// Top-Level pages
use crate::base::BASE_URL;
use crate::pages::home::Home;

/// An app router which renders the homepage and handles 404's
#[component]
pub fn App() -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();

    view! {
        <Html attr:lang="en" attr:dir="ltr" attr:data-theme="dark" />

        // sets the document title
        <Title text="My crazy styles :)" />

        // injects metadata in the <head> of the page
        <Meta charset="UTF-8" />
        <Meta name="viewport" content="width=device-width, initial-scale=1.0" />

        <Router base=BASE_URL>
            <Routes fallback=|| view! { NotFound }>
                <Route path=path!("/") view=Home />
                <Route path=path!("/sidebar1") view=pages::sidebar1::SideBarPage />
                <Route path=path!("/sidebar2") view=pages::sidebar2::SideBarPage />
                <Route path=path!("/sidebar3") view=pages::sidebar3::SideBarPage />
                <Route path=path!("/sidebar4") view=pages::sidebar4::SideBarPage />
                <Route path=path!("/sidebar5") view=pages::sidebar5::SideBarPage />
                <Route path=path!("/sidebar6") view=pages::sidebar6::SideBarPage />
            </Routes>
        </Router>
    }
}
