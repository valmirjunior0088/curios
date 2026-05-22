fn main() -> Result<(), String> {
    #[cfg(feature = "cli")]
    curios::cli()?;

    Ok(())
}
