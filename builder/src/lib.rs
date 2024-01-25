use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{
    AngleBracketedGenericArguments, Data, DeriveInput, Fields, GenericArgument, MetaList,
    PathArguments, Type, TypePath,
};

enum Field {
    Required(Ident, Type),
    Optional(Ident, Type),
    Repeated(Ident, Type, Ident),
    Error(syn::Error),
}

fn extract_contained_type<'a>(ty: &'a Type, container: &str) -> Option<&'a Type> {
    let path = match ty {
        Type::Path(TypePath {
            qself: None,
            ref path,
        }) => path,
        _ => return None,
    };

    let segment = match path.segments.first() {
        Some(segment) => segment,
        None => return None,
    };

    if segment.ident != container {
        return None;
    }

    let args = match segment.arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) => args,
        _ => return None,
    };

    match args.first() {
        Some(GenericArgument::Type(ty)) => Some(ty),
        _ => None,
    }
}

fn parse_repeated_field(field: &syn::Field) -> Result<Option<Field>, syn::Error> {
    let attrs = &field.attrs;
    let attr = match attrs.first() {
        Some(attr) => attr,
        None => return Ok(None),
    };

    if !attr.path().is_ident("builder") {
        return Err(syn::Error::new_spanned(
            attr,
            "expected `builder` attribute",
        ));
    };

    let tokens = match attr.meta {
        syn::Meta::List(MetaList { ref tokens, .. }) => tokens,
        _ => {
            return Err(syn::Error::new_spanned(
                attr,
                "expected `builder` attribute",
            ))
        }
    };

    let mut tokens = tokens.clone().into_iter();

    match tokens.next() {
        Some(proc_macro2::TokenTree::Ident(ident)) => {
            if ident != "each" {
                return Err(syn::Error::new_spanned(
                    attr,
                    "expected `builder(each = \"...\")`",
                ));
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                attr,
                "expected `builder(each = \"...\")`",
            ))
        }
    };

    match tokens.next() {
        Some(proc_macro2::TokenTree::Punct(punct)) => {
            if punct.as_char() != '=' {
                return Err(syn::Error::new_spanned(
                    attr,
                    "expected `builder(each = \"...\")`",
                ));
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                attr,
                "expected `builder(each = \"...\")`",
            ))
        }
    };

    println!("matched =");

    let literal = match tokens.next() {
        Some(proc_macro2::TokenTree::Literal(literal)) => literal,
        _ => {
            return Err(syn::Error::new_spanned(
                attr,
                "expected `builder(each = \"...\")`",
            ))
        }
    };

    let each = literal.to_string();
    let each = format_ident!("{}", each.trim_matches('"'));

    let contained_type = match extract_contained_type(&field.ty, "Vec") {
        Some(ty) => ty,
        None => return Err(syn::Error::new_spanned(attr, "expected type of `Vec<...>")),
    };

    Ok(Some(Field::Repeated(
        field.ident.clone().unwrap(),
        contained_type.clone(),
        each,
    )))
}

fn parse_optional_field(field: &syn::Field) -> Option<Field> {
    let contained_type = extract_contained_type(&field.ty, "Option")?;

    Some(Field::Optional(
        field.ident.clone().unwrap(),
        contained_type.clone(),
    ))
}

fn parse_field(field: &syn::Field) -> Field {
    if let Some(field) = parse_optional_field(field) {
        return field;
    };

    match parse_repeated_field(field) {
        Ok(Some(field)) => return field,
        Err(err) => return Field::Error(err),
        _ => {}
    }

    Field::Required(field.ident.clone().unwrap(), field.ty.clone())
}

fn parse_fields(data: Data) -> Option<Vec<Field>> {
    let data = match data {
        Data::Struct(data) => data,
        _ => return None,
    };

    let named = match data.fields {
        Fields::Named(fields) => fields.named,
        _ => return None,
    };

    Some(
        named
            .pairs()
            .map(|pair| parse_field(pair.value()))
            .collect(),
    )
}

fn generate_setters(fields: &[Field]) -> Vec<proc_macro2::TokenStream> {
    let mut field_setters = Vec::new();

    for field in fields.iter() {
        if let Field::Repeated(name, ty, each) = field {
            field_setters.push(quote!(
                pub fn #each(&mut self, #each: #ty) -> &mut Self {
                    self.#name.push(#each);
                    self
                }
            ));

            if each != name {
                field_setters.push(quote!(
                    pub fn #name(&mut self, #name: std::vec::Vec<#ty>) -> &mut Self {
                        self.#name = #name;
                        self
                    }
                ));
            }

            continue;
        }

        let (name, ty) = match field {
            Field::Required(name, ty) => (name, ty),
            Field::Optional(name, ty) => (name, ty),
            _ => unreachable!(),
        };

        field_setters.push(quote!(
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        ));
    }

    field_setters
}

fn generate_build_checks(fields: &[Field]) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().filter_map(|field| {
        let name = match field {
            Field::Required(name, _) => name,
            _ => return None,
        };

        let err_msg = format!("{} is required", name);

        Some(quote!(
            let #name = self.#name.clone().ok_or(#err_msg)?;
        ))
    })
}

fn generate_populated_fields(
    fields: &[Field],
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().map(|field| {
        if let Field::Required(name, _) = field {
            return quote!(#name: self.#name.clone().unwrap());
        }

        let name = match field {
            Field::Optional(name, _) => name,
            Field::Repeated(name, _, _) => name,
            _ => unreachable!(),
        };

        quote!(#name: self.#name.clone())
    })
}

fn generate_build_method(struct_name: &Ident, fields: &[Field]) -> proc_macro2::TokenStream {
    let build_checks = generate_build_checks(fields);
    let populated_fields = generate_populated_fields(fields);

    let build_method = quote!(
        pub fn build(&self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
            #(#build_checks)*

            std::result::Result::Ok(#struct_name {
                #(#populated_fields),*
            })
        }
    );

    build_method
}

fn generate_builder_definition(builder_name: &Ident, fields: &[Field]) -> proc_macro2::TokenStream {
    let builder_fields = fields.iter().map(|field| match field {
        Field::Required(name, ty) => quote!(#name: std::option::Option<#ty>),
        Field::Optional(name, ty) => quote!(#name: std::option::Option<#ty>),
        Field::Repeated(name, ty, _) => quote!(#name: Vec<#ty>),
        _ => unreachable!(),
    });

    quote!(
        pub struct #builder_name {
            #(#builder_fields),*
        }
    )
}

fn generate_builder_method(
    struct_name: &Ident,
    builder_name: &Ident,
    fields: &[Field],
) -> proc_macro2::TokenStream {
    let builder_defaults = fields.iter().map(|field| match field {
        Field::Required(name, _) => quote!(#name: std::option::Option::None),
        Field::Optional(name, _) => quote!(#name: std::option::Option::None),
        Field::Repeated(name, _, _) => quote!(#name: std::vec::Vec::new()),
        _ => unreachable!(),
    });

    quote!(
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_defaults),*
                }
            }
        }
    )
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = syn::parse_macro_input!(input as DeriveInput);

    let fields = parse_fields(parsed.data).unwrap();
    let struct_name = parsed.ident;
    let builder_name = format_ident!("{}Builder", struct_name);

    for field in fields.iter() {
        if let Field::Error(err) = field {
            return err.to_compile_error().into();
        }
    }

    let builder_method = generate_builder_method(&struct_name, &builder_name, &fields);
    let builder_definition = generate_builder_definition(&builder_name, &fields);
    let build_method = generate_build_method(&struct_name, &fields);
    let field_setters = generate_setters(&fields);

    let expanded = quote!(
        #builder_method

        #builder_definition

        impl #builder_name {
            #build_method

            #(#field_setters)*
        }
    );

    expanded.into()
}
