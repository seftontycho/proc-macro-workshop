use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{
    AngleBracketedGenericArguments, Data, DeriveInput, Fields, GenericArgument, PathArguments,
    Type, TypePath,
};

fn parse_field_type(ty: Type) -> (Type, bool) {
    let type_path = match ty.clone() {
        Type::Path(type_path) => type_path,
        _ => return (ty, false),
    };

    let path = match type_path {
        TypePath { qself: None, path } => path,
        _ => return (ty, false),
    };

    let segment = match path.segments.first() {
        Some(segment) => segment,
        None => return (ty, false),
    };

    if segment.ident != "Option" {
        return (ty, false);
    }

    let args = match &segment.arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
        _ => unreachable!(),
    };

    let contained_type = match args.first() {
        Some(GenericArgument::Type(ty)) => ty,
        _ => unreachable!(),
    };

    (contained_type.clone(), true)
}

fn parse_fields(data: Data) -> Option<(Vec<Ident>, Vec<Type>, Vec<bool>)> {
    let data = match data {
        Data::Struct(data) => data,
        _ => return None,
    };

    let named = match data.fields {
        Fields::Named(fields) => fields.named,
        _ => return None,
    };

    let mut names = Vec::new();
    let mut types = Vec::new();
    let mut required = Vec::new();

    for pair in named.pairs() {
        let value = pair.value();
        let name = value.ident.clone().unwrap();
        let (ty, optional) = parse_field_type(value.ty.clone());

        names.push(name.clone());
        types.push(ty);
        required.push(!optional)
    }

    Some((names, types, required))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = syn::parse_macro_input!(input as DeriveInput);
    let (field_names, field_types, required) = parse_fields(parsed.data).unwrap();
    let name = parsed.ident;

    let builder_name = format_ident!("{}Builder", name);

    let builder_func = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_names: None),*
                }
            }
        }
    };

    let builder_struct = quote!(
    pub struct #builder_name {
        #(#field_names: Option<#field_types>),*
    });

    let mut non_optional_fields = Vec::new();
    let mut populated_fields = Vec::new();

    for (field_name, is_required) in field_names.iter().zip(required) {
        if is_required {
            non_optional_fields.push(field_name);
            populated_fields.push(quote!(#field_name: self.#field_name.clone().unwrap()));
            continue;
        };

        populated_fields.push(quote!(#field_name: self.#field_name.clone()));
    }

    let builder_impl = quote!(
        impl #builder_name {
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                match (#(&self.#non_optional_fields),*) {
                    (#(Some(#non_optional_fields)),*) => {
                        Ok(#name {
                            #(#populated_fields),*
                        })
                    },
                    _ => Err("Not all fields initialised".into()),
                }
            }

            #(pub fn #field_names(&mut self, #field_names: #field_types) -> &mut Self {
                self.#field_names = Some(#field_names);
                self
            })
            *
        }
    );

    quote!(
        #builder_func
        #builder_struct
        #builder_impl
    )
    .into()
}
