#[cfg(not(debug_assertions))]
pub const BASE_URL: &str = "/tracer-bullets";
#[cfg(debug_assertions)]
pub const BASE_URL: &str = "";
