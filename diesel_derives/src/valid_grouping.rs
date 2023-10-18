use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;
use syn::Result;

// TODO: Remove me!
pub fn derive(mut _item: DeriveInput) -> Result<TokenStream> {
    Ok(quote! {})
}
