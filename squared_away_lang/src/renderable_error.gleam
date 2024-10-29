import gleam/option.{type Option}

/// This type represents an error we want to render. The idea is that the 
/// various error types implement converstion functions to this, rather than
/// individual to_string, to_html, etc. functions, which cuts down on work for
/// me as just one person. 
/// It will hopefully also help to keep error's looking consistent in the UI.
pub type RenderableError {
  RenderableError(
    title: String,
    info: String,
    hint: Option(String)
  )
}